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
  (let* ((mm-post-command-name (intern (concat (symbol-name major-mode) "-post-command-hook")))
         (mm-post-command-hook (when (boundp mm-post-command-name) (symbol-value mm-post-command-name)))
         (runner1 `(lambda () (run-hooks ',mm-post-command-name)))
         (runner2 `(lambda () (remove-hook 'post-command-hook ,runner1 t))))
    (add-hook 'post-command-hook runner1 nil t)
    (add-hook 'change-major-mode-hook runner2 nil t)))

(add-hook 'after-change-major-mode-hook #'hook--configure-post-command)

(defvar hook--watched-variables-global nil
  "Alist of variables known to hook.
Values are the last known values.")

(defvar hook--watched-variables-buffer nil
  "Alist of variables known to hook in the current buffer.
Values are the last known values.")
(make-variable-buffer-local 'hook--watched-variables-buffer)

(defvar hook--watched-variables-buffer-list nil
  "List of variables monitored for changes in buffer-local values.")

(defvar hook--watched-variables-global-list nil
  "List of variables monitored for changes in value.")

(defun hook--update-watched-variable (var val &optional local)
  "Update the known value of VAR to VAL.
Update the buffer-local value if LOCAL is non-nil."
  (let ((hook-var (hook--watched-variables-var local)))
    (eval `(setq ,hook-var (assq-delete-all var ,hook-var)))
    (eval `(push (cons var val) ,hook-var))))

(defun hook--watched-variables-var (&optional local)
  "Return the correct watch variables based on whether LOCAL is non-nil."
  (intern (format "hook--watched-variables-%s" (if local 'buffer 'global))))

(defun hook--watched-variables-var-list (&optional local)
  "Return the correct watch variables list based on whether LOCAL is non-nil."
  (intern (format "hook--watched-variables-%s-list" (if local 'buffer 'global))))

(defun hook--add-watched-variable (var &optional local)
  "Initialize the known value of VAR to NIL if it is not already known about.
Initialize the buffer-local value if LOCAL is non-nil."
  (let ((var-list (hook--watched-variables-var-list local)))
    (unless (eval `(memq var ,var-list))
      (eval `(push var ,var-list)))))

(defun hook--add-hook (hook fn &optional local)
  "Add to HOOK the function FN.
If LOCAL is non-nil, modify the hook's buffer-local value.
A wrapper around `add-hook'."
  (add-hook hook fn nil local))

(defun hook--run-hooks (&rest hooks)
  "Run each hook in HOOKS."
  (apply #'run-hooks hooks))

(defun hook--watched-variables-last-value (var &optional local)
  "Return the last known value of VAR.
Return the buffer-local version if LOCAL is non-nil."
  (eval `(cdr (assoc var ,(hook--watched-variables-var local)))))

(defun hook--list-watched-variables (&optional local)
  "List the variables watched by hook.
If LOCAL is non-nil then return the buffer-local variables."
  (symbol-value (hook--watched-variables-var-list local)))

(defun hook--check-watched-variables (&optional local)
  "Check watched variables for any change in value.
If LOCAL is non-nil then check the buffer-local values, otherwise check the global values."
    (dolist (var (hook--list-watched-variables local))
      (let ((change-hook (intern (format "hook-%s-after-var-changed-%s-hook" (if local 'local 'global) var))))
        (let ((old-val (hook--watched-variables-last-value var local))
              (new-val (when (boundp var) (if local (buffer-local-value var (current-buffer)) (symbol-value var)))))
        (unless (eq old-val new-val)
          (hook--update-watched-variable var new-val local)
          (hook--run-hooks change-hook))))))

(defun hook--check-watched-variables-buffer ()
  "Check watched variables for any change in value in the current buffer."
  (hook--check-watched-variables t))

(defun hook--check-watched-variables-global ()
  "Check watched variables for any change in value."
  (hook--check-watched-variables))

(defun hook--watch-variable (var fn &optional local)
  "Watch VAR for change in value, and run FN on a change.
Watch the buffer-local value if LOCAL is non-nil."
  (let ((var-hook (intern (format "hook-%s-after-var-changed-%s-hook" (if local 'local 'global) var))))
    (hook--add-watched-variable var local)
    (hook--add-hook var-hook fn)))

(defun hook--watch-variable-global (var fn)
  "Watch VAR for change in any buffer, and run FN on a change."
  (hook--watch-variable var fn))

(defun hook--watch-variable-buffer (var fn)
  "Watch VAR for change in any buffer, and run FN on a change."
  (hook--watch-variable var fn t))

(add-hook 'post-command-hook 'hook--check-watched-variables-global)
(add-hook 'post-command-hook 'hook--check-watched-variables-buffer)

(provide 'hook)
;;; hook.el ends here
