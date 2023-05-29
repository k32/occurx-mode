;;; logview-mode.el --- Interactive log filtering  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  k32

;; Author: k32 <example@example.com>
;; Keywords: logs occur
;; Version: 0.1
;; Homepage: https://github.com/k32/snabbkaffe-mode
;; Package-Requires: ((emacs "25.1") (rbit "0.1"))

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

;;

;;; Code:

(require 'ert)
(require 'rx)
(require 'hi-lock)
(require 'cl-lib)
(require 'rbit)

;;; Customizable settings

(setq logview--snk-bindings
      '((=> (a b) (seq a (* blank) "=>" (* blank) b))
        (kind (a) (=> "'$kind'" a))
        (meta (k v) (=> "'~meta'" (seq "#{" (* nonl) (=> k v))))))

(defcustom logview-delimiter-face
  font-lock-comment-face
  "Face used to display log delimiters"
  :type 'face
  :group 'logview)

(defface logview-ellipsis-face
  '((((type tty)) :inherit underline)
    (((type pc)) :inherit escape-glyph)
    (t :height 0.6))
   "Face used to display ellipsis replacing skipped fragment of the log entry"
  :group 'logview)

(defcustom logview-rx-bindings
  logview--snk-bindings
  "List of additional definitions that is passed to `rx-let-eval'"
  :type '(repeat sexp))

(defcustom logview-default-faces
  '(hi-pink hi-green hi-blue hi-salmon)
  "List of faces used to highlight the patterns."
  :type '(repeat face)
  :group 'logview)

(defcustom logview-context
  10
  "Size of the context"
  :type 'integer
  :group 'logview)

;;; Source buffer

;;;###autoload
(define-minor-mode logview-mode
  "Minor mode for viewing logs"
  :lighter "🪵"
  :keymap (list (cons (kbd "q") #'quit-window)
                (cons (kbd "o") #'logview-pattern-buffer)
                (cons (kbd "<SPC>") #'scroll-down-command)
                (cons (kbd "C-c C-c") (lambda ()
                                        (logview-pattern-buffer)
                                        (logview-run))))
  (read-only-mode t))

(defun logview--occur-buffer (source-buffer)
  "Create or get occur buffer for the given SOURCE-BUFFER"
  (get-buffer-create (concat "*Occur* " (buffer-name source-buffer)) t))

;; Pattern struct

(defvar-local logview--default-faces logview-default-faces)

(defun logview--default-face ()
  "Get default face"
  (unless logview--default-faces
    (setq-local logview--default-faces logview-default-faces))
  (pop logview--default-faces))

(cl-defstruct (logview-pattern (:constructor logview-pattern--create))
  include exclude face orig-pos)

(cl-defun logview-pattern-create (&key include &key exclude &key face)
  (let ((incl (mapcar #'logview--rx-compile include))
        (excl (mapcar #'logview--rx-compile exclude)))
    (logview-pattern--create :include incl
                             :exclude excl
                             :face (or face (logview--default-face)))))

(defun logview--parse-pattern (input beg-pos end-pos)
  (let (include exclude face head (l input))
    (while l
      (pcase-exhaustive l
        (`(:face ,face . ,rest) (setq face face l rest))
        (`(:not ,pat . ,rest)   (push pat exclude) (setq l rest))
        (`(,pat . ,rest)        (push pat include) (setq l rest))))
    (logview-pattern-create :face face :include include :exclude exclude)))

;;; Occur

(defun logview--run-pattern (p begin bound)
  "Run a list of regular expressions PATTERN.
If all of them match, return list of positions of all matches, `nil' overwise."
  (cl-loop for re in (logview-pattern-include p)
           for found = (progn
                         (goto-char begin)
                         (cl-loop while (re-search-forward re bound t)
                                  collect (list (match-beginning 0)
                                                (match-end 0)
                                                (logview-pattern-face p))))
           if found append found
           else return nil))

(defun logview--match-intervals (entry-beginning begin bound matches)
  (let* (acc
         (push-interval (lambda (beg end face)
                          (if (= beg end)
                              acc
                            (setq acc (rbit-set acc beg end face (lambda (a b) (or a b))))))))
    (pcase-dolist (`(,beg ,end ,face) matches)
      (funcall push-interval beg end face)
      (funcall push-interval (max entry-beginning (- beg logview-context)) beg nil)
      (funcall push-interval end (min bound (+ end logview-context)) nil))
    (funcall push-interval entry-beginning begin logview-delimiter-face)
    (rbit-to-list acc)))

(defun logview--on-match (entry-beginning begin bound matches orig-buf occur-buf)
  "This function is called when a pattern match is found"
  (with-current-buffer occur-buf
    (let (chunk-begin offset prev-max)
      (pcase-dolist (`(,min ,max ,face) (logview--match-intervals entry-beginning begin bound matches))
        ;; Insert ellipsis if fragment is skipped
        (when (and prev-max (> min prev-max))
          (insert "...")
          (add-face-text-property (- (point) 3) (point) 'logview-ellipsis-face))
        (setq chunk-begin (point)
              offset (- chunk-begin min)
              prev-max max)
        (insert-buffer-substring orig-buf min max)
        ;; Add property that allows to jump to the source
        (put-text-property chunk-begin (point) 'logview-pointer min)
        ;; Highlight fragment:
        (when face
          (put-text-property chunk-begin (point) 'face face)))
      ;;   Insert newline if not at the end of line:
      (unless (bolp)
        (insert-char ?\n)))))

(defun logview--run-patterns (delimiter patterns orig-buf occur-buf)
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
               for matches = (logview--run-pattern pattern
                                                   body-beginning
                                                   next-entry-beginning)
               if matches
               return (logview--on-match entry-beginning body-beginning next-entry-beginning
                                         matches
                                         orig-buf occur-buf)
               end)
      ;; Move forward:
      (setq body-beginning next-body-beginning
            entry-beginning next-entry-beginning)
      (when body-beginning (goto-char body-beginning)))))

(defun logview--occur (pattern-buf orig-buf delimiter patterns)
  (let ((occur-buf (logview--occur-buffer (current-buffer))))
    (set-window-dedicated-p
     (display-buffer occur-buf
                     `((display-buffer-reuse-window display-buffer-in-atom-window)
                       (side . left)
                       (window . ,(window-parent))))
     t)
    (with-current-buffer occur-buf
      (logview-occur-mode)
      (setq-local logview-orig-buffer orig-buf)
      (setq-local logview-pattern-buffer pattern-buf)
      (read-only-mode -1)
      (erase-buffer))
    (with-current-buffer orig-buf
      (save-excursion
        (goto-char (point-min))
        (logview--run-patterns delimiter patterns orig-buf occur-buf)))
    (with-current-buffer occur-buf
      (read-only-mode))))

;;;###autoload
(defun logview-run ()
  "Read a set of `rx' patterns from the current buffer, read list of
``dependent buffers'' from a buffer variable and filter out
entries matching the patterns to occur buffer"
  (interactive "")
  (setq-local logview--default-faces nil)
  (seq-let (delimiter &rest patterns) (logview--read-patterns (current-buffer))
    (dolist (buf logview-dependent-buffers)
      (logview--occur (current-buffer) buf (logview--rx-compile delimiter) patterns))))

;;;; Occur major mode
(defun logview-occur-visit-source ()
  "Jump to the occurrance in the original buffer"
  (interactive)
  (let ((pos (get-text-property (point) 'logview-pointer)))
    (when pos
      (select-window
       (display-buffer logview-orig-buffer '((display-buffer-reuse-window display-buffer-in-direction)
                                             (direction . right))))
      (goto-char pos))))

(defvar logview-occur-mode-map nil "Keymap for logview-occur-mode")
(setq logview-occur-mode-map (make-sparse-keymap))

(define-key logview-occur-mode-map (kbd "<return>") #'logview-occur-visit-source)
(define-key logview-occur-mode-map (kbd "o") #'logview-pattern-buffer)
(define-key logview-occur-mode-map [mouse-1] #'logview-occur-visit-source)

(define-derived-mode logview-occur-mode fundamental-mode
  "🪡"
  :syntax-table nil
  :abbrev-table nil
  (setq-local logview-orig-buffer nil))

;;;; Pattern buffer

(defun logview--buffer-to-sexps (buffer)
  "Parse BUFFER into a list of sexps"
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

(defun logview--read-patterns (buffer)
  "Read patterns from BUFFER as s-exps"
  (let* ((sexps (logview--buffer-to-sexps buffer))
         (delimiter '(or bos bol))
         patterns)
    (pcase-dolist (`(,beg-pos ,end-pos ,sexp) sexps)
      (pcase-exhaustive sexp
        (`(delimiter ,del) (setq delimiter del))
        ((pred stringp) (push (logview-pattern-create :include (list sexp))
                              patterns))
        ((pred listp) (push (logview--parse-pattern sexp beg-pos end-pos) patterns))))
    (cons delimiter patterns)))

(defun logview--intercalate (separator l)
  "Return a list where SEPARATOR is inserted between elements of L."
  (let (ret)
    (dolist (i l (nreverse (cdr ret)))
      (setq ret (cons separator (cons i ret))))))

(defun logview--preprocess-rx (pat)
  "Preprocess `rx' pattern PAT.
Change behavior of `and' operation: it inserts `(* nonl)' between each operand.
Use `seq' if you need standard rx behavior."
  (pcase pat
    (`(and . ,rest) (cons 'and (logview--intercalate '(* any) (logview--preprocess-rx rest))))
    ((pred listp)   (mapcar #'logview--preprocess-rx pat))
    (_              pat)))

(defun logview--rx-compile (pat)
  "Compile rx pattern PAT to string"
  (rx-let-eval logview-rx-bindings
    (rx-to-string (logview--preprocess-rx pat) t)))

;;;###autoload
(defun logview--find-pattern-buffer (change)
  "Find or create a buffer that stores the pattern for the current buffer."
  (unless (and (boundp 'logview-pattern-buffer)
               (get-buffer logview-pattern-buffer) ; Buffer's alive
               (not change))                       ; User doesn't want to change it
    (setq-local logview-pattern-buffer
                (find-file-noselect (read-file-name "Buffer containing the pattern:" (concat (buffer-name) "-pattern.el")))))
  logview-pattern-buffer)

;;;###autoload
(defun logview-pattern-buffer (&optional change)
  "Switch to the pattern buffer"
  (interactive "P")
  (let ((orig-buf (buffer-name))
        (pattern-buf (logview--find-pattern-buffer change)))
    (select-window
     (display-buffer (get-buffer-create pattern-buf)
                     '((display-buffer-reuse-window display-buffer-in-atom-window)
                       (window-height . 8)
                       (side . below))))
    (set-window-dedicated-p (selected-window) t)
    (logview-pattern-mode)
    (push orig-buf logview-dependent-buffers)
    pattern-buf))

(defvar logview-pattern-mode-map nil "Keymap for logview-pattern-mode")
(setq logview-pattern-mode-map (make-sparse-keymap))
(define-key logview-pattern-mode-map (kbd "C-c C-c") #'logview-run)

(define-derived-mode logview-pattern-mode emacs-lisp-mode
  "🔍"
  :syntax-table nil
  :abbrev-table nil
  (setq-local logview-dependent-buffers nil))

(provide 'logview-mode)
;;; logview-mode.el ends here
