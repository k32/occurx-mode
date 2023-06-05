;;; occurx-mode.el --- Occur-like filtering of buffers with rx patterns  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  k32

;; Author: k32
;; Keywords: matching
;; Version: 0.1
;; Homepage: https://github.com/k32/occurx-mode
;; Package-Requires: ((emacs "27.1") (rbit "0.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Occurx mode is an alternative to native `occur' command that makes
;; it easier to inspect large logs by adding the following features:
;;
;; - Regexps are specified using the `rx' notation to make them more
;;   readable
;;
;; - Search patterns are composed in a dedicated buffer rather than
;;   minibuffer for easier editing
;;
;; - They can be easily saved and restored
;;
;; - It can search for multiple matches that can occur in any order
;;
;; - Occurences can span multiple lines
;;
;; - Search patterns can be easily negated
;;
;; - Different search patterns are highlighted in different colors

(require 'ert)
(require 'rx)
(require 'hi-lock)
(require 'cl-lib)
(require 'rbit)

;;; Code:

;;; Customizable settings

(defcustom occurx-max-matches
  1
  "How many matches should be highlighted per entry by default."
  :type 'integer
  :group 'occurx)

(defcustom occurx-delimiter-face
  font-lock-comment-face
  "Face used to display log delimiters."
  :type 'face
  :group 'occurx)

(defface occurx-ellipsis-face
  '((((type tty)) :inherit underline)
    (((type pc)) :inherit escape-glyph)
    (t :height 0.6))
  "Face used to display ellipsis."
  :group 'occurx)

(defcustom occurx-rx-bindings
  '((atom (or (seq ?' (+ (not ?')) ?')
              (seq lower-case (* (in alnum ?_)))))
    (=> (a b) (seq a (* blank) "=>" (* blank) b))
    (kind (a) (=> "'$kind'" a))
    (meta (k v) (=> "'~meta'" (seq "#{" (* nonl) (group (=> k v))))))
  "List of additional definitions that is passed to `rx-let-eval'."
  :type '(repeat sexp)
  :group 'occurx)

(defcustom occurx-default-faces
  '(hi-pink hi-green hi-blue hi-salmon)
  "List of faces used to highlight the patterns."
  :type '(repeat face)
  :group 'occurx)

(defcustom occurx-context
  10
  "Size of the context that shown around matches."
  :type 'integer
  :group 'occurx)

;;; Source buffer

(defvar-local occurx-pattern-buffer
    nil
  "Id of the dedicated pattern buffer.")

;;;###autoload
(define-minor-mode occurx-mode
  "Minor mode for viewing logs."
  :lighter "🪵"
  :keymap (list (cons (kbd "q") #'quit-window)
                (cons (kbd "o") #'occurx-pattern-buffer)
                (cons (kbd "<SPC>") #'scroll-down-command)
                (cons (kbd "C-c C-c") (lambda ()
                                        (occurx-pattern-buffer)
                                        (occurx-run))))
  (read-only-mode t))

(defun occurx--occur-buffer (source-buffer)
  "Create or get occur buffer for the given SOURCE-BUFFER."
  (get-buffer-create (concat "*Occur* " (buffer-name source-buffer)) t))

;; Pattern struct

(cl-defstruct (occurx-matcher (:constructor occurx-matcher--create))
  re n-matches sub-expr)

(cl-defun occurx-matcher-create (&key re &key n-matches &key sub-expr)
  "Constructor for `occurx-matcher'.
Optional argument RE rx expression.
Optional argument N-MATCHES maximum number of matches to highlight.
Optional argument SUB-EXPR sub-match pattern for the regexp."
  (occurx-matcher--create :re (occurx--rx-compile re)
                          :n-matches (or n-matches occurx-max-matches)
                          :sub-expr (or sub-expr 0)))

(defvar-local occurx--default-faces occurx-default-faces)

(defun occurx--default-face ()
  "Get default face."
  (unless occurx--default-faces
    (setq-local occurx--default-faces occurx-default-faces))
  (pop occurx--default-faces))

(cl-defstruct (occurx-pattern (:constructor occurx-pattern--create))
  include exclude face orig-pos)

(defun occurx-pattern-create (input)
  "Constructor for `occurx-pattern'.
Argument INPUT s-expression containing pattern specification ."
  (let (include exclude face n-matches negated (l input) pattern sub)
    (while l
      (pcase l
        (`(:n ,n . ,rest)    (setq n-matches n l rest))
        (`(:sub ,s . ,rest)  (setq sub s       l rest))
        (`(:face ,f . ,rest) (setq face f      l rest))
        (`(:not . ,rest)     (setq negated t   l rest))
        (`(,p . ,rest)
         (setq pattern (occurx-matcher-create :re p :n-matches n-matches :sub-expr sub))
         (if negated
             (push pattern exclude)
           (push pattern include))
         (setq l rest
               n-matches nil
               negated nil
               sub nil))))
    (occurx-pattern--create :face (or face (occurx--default-face))
                            :include include
                            :exclude exclude)))

;;; Occur

(defun occurx--run-pattern (p begin bound)
  "Run pattern P in the fragment of the current buffer.
Search is delimited by BEGIN and BOUND.
If all of `include' regexps match and none of `exclude' regexps
match, return list of positions of all matches, nil overwise."
  (let ((result
         (cl-loop for matcher in (occurx-pattern-include p)
                  for found = (let ((re (occurx-matcher-re matcher))
                                    ;(n (occurx-matcher-n-matches matcher)) TODO
                                    (e (occurx-matcher-sub-expr matcher)))
                                (goto-char begin)
                                (cl-loop while (re-search-forward re bound t)
                                         collect (list (match-beginning e)
                                                       (match-end e)
                                                       (occurx-pattern-face p))))
                  if found append found
                  else return nil)))
    (and result
         ;; If all include pattern were found, run exclusion patterns:
         (cl-loop for re in (occurx-pattern-exclude p)
                  for found = (progn
                                (goto-char begin)
                                (re-search-forward re bound t))
                  if found return nil
                  finally return t)
         ;; If exclusion patterns weren't found, return `result':
         result)))

(defun occurx--match-intervals (entry-beginning begin bound matches)
  "Add context to the intervals MATCHES.
ENTRY-BEGINNING specifies beginning of the entry.
BEGIN specifies beginning of the entry's body.
BOUND specifies upper bound of the entry."
  (let* (acc
         (push-interval (lambda (beg end face)
                          (if (= beg end)
                              acc
                            (setq acc (rbit-set acc beg end face (lambda (a b) (or a b))))))))
    (pcase-dolist (`(,beg ,end ,face) matches)
      (funcall push-interval beg end face)
      (funcall push-interval (max entry-beginning (- beg occurx-context)) beg nil)
      (funcall push-interval end (min bound (+ end occurx-context)) nil))
    (funcall push-interval entry-beginning begin occurx-delimiter-face)
    (rbit-to-list acc)))

(defun occurx--on-match (entry-beginning begin bound matches orig-buf occur-buf)
  "This function is called when a pattern match is found.
ENTRY-BEGINNING specifies beginning of the entry.
BEGIN specifies beginning of the entry's body.
BOUND specifies upper bound of the entry.
ORIG-BUF is id of the source buffer.
OCCUR-BUF is id of the occur buffer.
MATCHES is list of elements of type `(match-begin match-end face)'."
  (with-current-buffer occur-buf
    (let (chunk-begin offset prev-max)
      (pcase-dolist (`(,min ,max ,face) (occurx--match-intervals entry-beginning begin bound matches))
        ;; Insert ellipsis if fragment is skipped
        (when (and prev-max (> min prev-max))
          (insert "...")
          (add-face-text-property (- (point) 3) (point) 'occurx-ellipsis-face))
        ;; Copy contexts of the source buffer to the occur buffer:
        (setq chunk-begin (point)
              offset (- chunk-begin min)
              prev-max max)
        (insert-buffer-substring orig-buf min max)
        ;; Add property that allows to jump to the source
        (put-text-property chunk-begin (point) 'occurx-pointer min)
        ;; Highlight fragment:
        (when face
          (put-text-property chunk-begin (point) 'face face)))
      ;;   Insert newline if not at the end of line:
      (unless (bolp)
        (insert-char ?\n)))))

(defun occurx--run-patterns (delimiter patterns orig-buf occur-buf)
  "Run PATTERNS from OCCUR-BUF in ORIG-BUF.
DELIMITER specifies an rx expression separating entries."
  (let (entry-beginning body-beginning next-entry-beginning next-body-beginning)
    ;; Initialization of the loop:
    (setq body-beginning (re-search-forward delimiter nil t)
          entry-beginning (match-beginning 0))
    ;; Loop over buffer:
    (while body-beginning
      (forward-char)
      ;; Find next entry:
      (setq next-body-beginning (re-search-forward delimiter nil t)
            next-entry-beginning (if next-body-beginning (match-beginning 0) (point-max)))
      ;; Match entry's body against patterns, stop on first match:
      (cl-loop for pattern in patterns
               for matches = (occurx--run-pattern pattern
                                                   body-beginning
                                                   next-entry-beginning)
               if matches
               return (occurx--on-match entry-beginning body-beginning next-entry-beginning
                                         matches
                                         orig-buf occur-buf)
               end)
      ;; Move forward:
      (setq body-beginning next-body-beginning
            entry-beginning next-entry-beginning)
      (when body-beginning (goto-char body-beginning)))))

(defun occurx--occur (pattern-buf orig-buf delimiter patterns)
  "Search PATTERNS from PATTERN-BUF in ORIG-BUF separated by DELIMITER."
  (let ((occur-buf (occurx--occur-buffer (current-buffer))))
    (set-window-dedicated-p
     (display-buffer occur-buf
                     '((display-buffer-reuse-window display-buffer-in-direction)
                       (direction . leftmost)))
     t)
    (with-current-buffer occur-buf
      (occurx-occur-mode)
      (setq-local occurx-orig-buffer orig-buf)
      (setq-local occurx-pattern-buffer pattern-buf)
      (read-only-mode -1)
      (erase-buffer))
    (with-current-buffer orig-buf
      (save-excursion
        (goto-char (point-min))
        (occurx--run-patterns delimiter patterns orig-buf occur-buf)))
    (with-current-buffer occur-buf
      (read-only-mode))))

;;;; Occur major mode

(defvar-local occurx-orig-buffer nil
  "Id of the source buffer.")

(defun occurx-occur-visit-source ()
  "Jump to the occurrance in the original buffer."
  (interactive)
  (let ((pos (get-text-property (point) 'occurx-pointer)))
    (when pos
      (select-window
       (display-buffer occurx-orig-buffer '((display-buffer-reuse-window display-buffer-in-direction)
                                             (direction . right))))
      (goto-char pos))))

(defvar occurx-occur-mode-map nil "Keymap for `occurx-occur-mode'.")
(setq occurx-occur-mode-map (make-sparse-keymap))

(define-key occurx-occur-mode-map (kbd "<return>") #'occurx-occur-visit-source)
(define-key occurx-occur-mode-map (kbd "o") #'occurx-pattern-buffer)
(define-key occurx-occur-mode-map [mouse-1] #'occurx-occur-visit-source)

(define-derived-mode occurx-occur-mode fundamental-mode
  "🪡"
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo (current-buffer))
  (setq-local occurx-orig-buffer nil))

;;;; Pattern buffer

(defvar-local occurx-dependent-buffers
    nil
  "Ids of source buffers using this pattern.")

;;;###autoload
(defun occurx-run ()
  "Run pattern from the currently open buffer.
Read a set of `rx' patterns from the current buffer, read list of
``dependent buffers'' from a buffer variable and filter out
entries matching the patterns to occur buffer"
  (interactive "")
  (setq-local occurx--default-faces nil)
  (seq-let (delimiter &rest patterns) (occurx--read-patterns (current-buffer))
    (dolist (buf occurx-dependent-buffers)
      (occurx--occur (current-buffer) buf (occurx--rx-compile delimiter) patterns))))

(defun occurx--buffer-to-sexps (buffer)
  "Parse BUFFER into a list of sexps."
  (with-current-buffer buffer
    (save-excursion
      (let (sexps
            sexp
            (line-start 0)
            line-end)
        (goto-char (point-min))
        (ignore-errors
          (while (setq sexp (read (current-buffer)))
            (setq line-end (line-number-at-pos))
            (push (list line-start line-end sexp) sexps)
            (setq line-start line-end)))
        (reverse sexps)))))

(defun occurx--read-patterns (buffer)
  "Read patterns from BUFFER as s-exps."
  (let* ((sexps (occurx--buffer-to-sexps buffer))
         (delimiter '(or bos bol))
         patterns)
    (pcase-dolist (`(,_ ,_ ,sexp) sexps)
      (pcase-exhaustive sexp
        (`(delimiter ,del) (setq delimiter del))
        ((pred stringp) (push (occurx-pattern-create (list sexp)) patterns))
        ((pred listp)   (push (occurx-pattern-create sexp)       patterns))))
    (cons delimiter patterns)))

(defun occurx--intercalate (separator l)
  "Return a list where SEPARATOR is inserted between elements of L."
  (let (ret)
    (dolist (i l (nreverse (cdr ret)))
      (setq ret (cons separator (cons i ret))))))

(defun occurx--preprocess-rx (pat)
  "Preprocess `rx' pattern PAT.
Change behavior of `and' operation: it inserts `(* nonl)' between each operand.
Use `seq' if you need standard rx behavior."
  (pcase pat
    (`(and . ,rest) (cons 'and (occurx--intercalate '(* any) (occurx--preprocess-rx rest))))
    ((pred listp)   (mapcar #'occurx--preprocess-rx pat))
    (_              pat)))

(defun occurx--rx-compile (pat)
  "Compile rx pattern PAT to string."
  (rx-let-eval occurx-rx-bindings
    (rx-to-string (occurx--preprocess-rx pat) t)))

(defun occurx--find-pattern-buffer (change)
  "Find or create a buffer that stores the pattern for the current buffer.
If CHANGE is not nil then ask user to specify new name for the pattern file."
  (unless (and (boundp 'occurx-pattern-buffer)
               occurx-pattern-buffer
               (get-buffer occurx-pattern-buffer) ; Buffer's alive
               (not change))                      ; User doesn't want to change it
    (setq-local occurx-pattern-buffer
                (find-file-noselect (read-file-name "Buffer containing the pattern:" (concat (buffer-name) "-pattern.el")))))
  occurx-pattern-buffer)

;;;###autoload
(defun occurx-pattern-buffer (&optional change)
  "Switch to the pattern buffer.
If CHANGE is not nil then ask user to specify new file name for the pattern."
  (interactive "P")
  (let ((orig-buf (buffer-name))
        (pattern-buf (occurx--find-pattern-buffer change)))
    (select-window
     (display-buffer (get-buffer-create pattern-buf)
                     '((display-buffer-reuse-window display-buffer-below-selected)
                       (window-height . 8))))
    (set-window-dedicated-p (selected-window) t)
    (occurx-pattern-mode)
    (push orig-buf occurx-dependent-buffers)
    pattern-buf))

(defvar occurx-pattern-mode-map nil "Keymap for `occurx-pattern-mode'.")
(setq occurx-pattern-mode-map (make-sparse-keymap))
(define-key occurx-pattern-mode-map (kbd "C-c C-c") #'occurx-run)

(define-derived-mode occurx-pattern-mode emacs-lisp-mode
  "🔍"
  :syntax-table nil
  :abbrev-table nil
  (setq-local occurx-dependent-buffers nil))

(provide 'occurx-mode)
;;; occurx-mode.el ends here
