;;; hook.el --- Utilities for generating hooks.
;;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Provide utilities for generating hooks.
;;;
;;; Code:

(defun hook--configure-post-command ()
  "Initialize a `post-command-hook' for the current `major-mode'."
  (let* ((mm-post-command-name (intern (format "hook-%s-post-command-hook" major-mode)))
         (mm-post-command-hook (when (boundp mm-post-command-name) (symbol-value mm-post-command-name)))
         (runner1 `(lambda () (run-hooks ',mm-post-command-name)))
         (runner2 `(lambda () (remove-hook 'post-command-hook ,runner1 t))))
    (add-hook 'post-command-hook runner1 nil t)
    (add-hook 'change-major-mode-hook runner2 nil t)))

(add-hook 'after-change-major-mode-hook #'hook--configure-post-command)

(defvar hook--watched-expressions-global nil
  "Alist of expressions known to hook.
Values are the last known values.")

(defvar hook--watched-expressions-buffer nil
  "Alist of variables known to hook in the current buffer.
Values are the last known values.")
(make-variable-buffer-local 'hook--watched-expressions-buffer)

(defun hook--update-watched-expression (var val &optional local)
  "Update the known value of VAR to VAL.
Update the buffer-local value if LOCAL is non-nil."
  (let ((hook-var (hook--watched-expressions-var local)))
    (eval `(setq ,hook-var (assq-delete-all var ,hook-var)))
    (eval `(push (cons var val) ,hook-var))))

(defun hook--watched-expressions-var (&optional local)
  "Return the correct watch variables based on whether LOCAL is non-nil."
  (intern (format "hook--watched-expressions-%s" (if local 'buffer 'global))))

(defun hook--watched-expressions-last-value (var &optional local)
  "Return the last known value of VAR.
Return the buffer-local version if LOCAL is non-nil."
  (eval `(cdr (assoc var ,(hook--watched-expressions-var local)))))

(defvar hook--monitored nil
  "Monitored expressions.")

(defun hook--monitor (pred &rest fns)
  "After `post-command-hook' check PRED for a non-NIL value.
If PRED evaluates to non-NIL, then run each function in FNS."
  (let (exist-fns (cdr (assoc pred hook--monitored)))
    (dolist (fn fns) (unless (member fn exist-fns) (push fn exist-fns)))
    (setq hook--monitored (--reject (equal (car it) pred) hook--monitored))
    (push (cons pred exist-fns) hook--monitored))
  nil)

(defun hook--check-monitored ()
  "Check each monitored expression."
  (let ((to-delete))
    (dolist (pexp hook--monitored)
      (when (condition-case var (eval (car pexp))
              (error (progn (message "error when evaluating %s (got %s)" (car pexp) (error-message-string var))
                            (push pexp to-delete) nil)))
        (dolist (f (cdr pexp)) (funcall f))))
    (when to-delete (setq hook--monitored (--reject (member it to-delete) hook--monitored)))))

(add-hook 'post-command-hook 'hook--check-monitored)

(provide 'hook)
;;; hook.el ends here
