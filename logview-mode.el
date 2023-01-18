;;; logview-mode.el --- Interactive log filtering  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  k32

;; Author: k32 <k32@example.com>
;; Keywords: log occur

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

(require 'rx)
(require 'hi-lock)

(setq logview--snk-bindings
      '((=> (a b) (seq a (* blank) "=>" (* blank) b))
        (kind (a) (=> "'$kind'" a))
        (meta (k v) (=> "'~meta'" (seq "#{" (* nonl) (=> k v))))))

(defcustom logview-rx-bindings
  logview--snk-bindings
  "List of additional definitions that is passed to `rx-let-eval'"
  :type '(repeat sexp))

(defcustom logview-default-faces
  '(hi-pink hi-green hi-yellow hi-salmon)
  "List of faces used to highlight the patterns."
  :type '(repeat face)
  :group 'logview)

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
    (`(and . ,rest) (cons 'and (logview--intercalate '(* nonl) (logview--preprocess-rx rest))))
    ((pred listp)   (mapcar #'logview--preprocess-rx pat))
    (_              pat)))

(defun logview--build-re (pat)
  "Transform pattern to a regexp."
  (rx-let-eval logview--snk-bindings
    (rx-to-string `(seq bol (* nonl) ,(logview--preprocess-rx pat) (* nonl) eol)
                  t)))

;;;###autoload
(define-minor-mode logview-mode
  "Minor mode for viewing traces"
  :lighter "☕"
  :keymap (list (cons (kbd "q") #'quit-window)
                (cons (kbd "o") #'logview-occur)
                (cons (kbd "<SPC>") #'scroll-up-command))
  (toggle-truncate-lines t)
  (read-only-mode t))

;;;###autoload
(defun logview--find-pattern-buffer (change)
  "Find or create a buffer that stores the pattern for the current buffer."
  (unless (and (boundp 'logview-pattern-buffer)
               (get-buffer logview-pattern-buffer) ; Buffer's alive
               (not change))                       ; User doesn't want to change it
    (setq-local logview-pattern-buffer
                (read-buffer "Buffer containing the pattern:" (concat (buffer-name) "-pattern.el"))))
  logview-pattern-buffer)

(defun logview--show-pattern-buffer (change)
  "Find or create a buffer that stores the pattern for the current buffer."
  (let ((orig-buf (buffer-name))
        (pattern-buf (logview--find-pattern-buffer change)))
    (select-window
     (display-buffer (get-buffer-create pattern-buf)
                     '((display-buffer-reuse-window display-buffer-in-direction)
                       (window-height . 8)
                       (direction . bottom))))
    (logview-pattern-mode)
    (push orig-buf logview-dependent-buffers)
    pattern-buf))

(defun logview--read-patterns (buffer)
  "Read regexps from the pattern buffer."
  ;; Parse sexp:
  (let ((patterns (with-current-buffer buffer
                    (goto-char (point-min))
                    (read (get-buffer buffer)))))
    (mapcar
     (lambda (body)
      ;; Parse the pattern:
       (let (face keyw)
         (while (keywordp (setq keyw (car body)))
          (setq body (cdr body))
          (pcase keyw
	          (:face (setq face (pop body)))))
        (list :face face :rx (car body))))
     patterns)))

(defun logview--build-occur-re (patterns)
  "Join all patterns with `or' operation to create a regexp for `occur'."
  (logview--build-re
   (cons 'or
         (mapcar (lambda (i) (plist-get i :rx)) patterns))))

(defun logview--colorize (patterns)
  (let ((faces logview-default-faces))
    (dolist (i patterns)
      (hi-lock-face-buffer (logview--build-re (plist-get i :rx))
                           (or (plist-get i :face) (pop faces))))))

;;;###autoload
(defun logview-occur (&optional change)
  "Switch to the pattern buffer"
  (interactive "P")
  (logview--show-pattern-buffer change))

;;;###autoload
(defun logview-run-pattern ()
  "Read a set of `rx' patterns from a specified buffer and run `occur' with them.
Colorize the occur buffer."
  (interactive "")
  (let*
      ((patterns (logview--read-patterns (current-buffer)))
       (occur-re (logview--build-occur-re patterns)))
    (message "Patterns %S" patterns)
    (dolist (buf logview-dependent-buffers)
      (with-current-buffer buf
        (occur occur-re)
        (with-current-buffer "*Occur*"
          (logview--colorize patterns))))))

(defvar logview-pattern-mode-map nil "Keymap for logview-pattern-mode")
(setq logview-pattern-mode-map (make-sparse-keymap))
(define-key logview-pattern-mode-map (kbd "C-c C-c") 'logview-run-pattern)

(define-derived-mode logview-pattern-mode emacs-lisp-mode
  "🔍"
  :syntax-table nil
  :abbrev-table nil

  (setq-local logview-dependent-buffers nil))

(provide 'logview-mode)
;;; logview-mode.el ends here
