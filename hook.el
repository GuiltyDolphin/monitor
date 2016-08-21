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

(defun hook--update-watched-variable-global (var val)
  "Update the known value of VAR to VAL."
  (setq hook--watched-variables-global (assq-delete-all var hook--watched-variables-global))
  (push (list var val) hook--watched-variables-global))

(defun hook--add-watched-variable-global (var)
  "Initialize the known value of VAR to NIL if it is not already known about."
  (unless (assoc var hook--watched-variables-global)
    (hook--update-watched-variable-global var nil)))

(defun hook--add-hook (hook fn)
  "Add to HOOK the function FN.
A wrapper around `add-hook'."
  (add-hook hook fn))

(defun hook--watch-variable-global (var fn)
  "Watch VAR for change in any buffer, and run FN on a change."
  (let ((var-hook (intern (format "hook-global-after-var-changed-%s-hook" var))))
    (hook--add-watched-variable-global var)
    (hook--add-hook var-hook fn)))

(defun hook--run-hooks (&rest hooks)
  "Run each hook in HOOKS."
  (apply #'run-hooks hooks))

(defun hook--check-watched-variables-global ()
  "Check watched variables for any change in value."
  (dolist (var (mapcar 'car hook--watched-variables-global))
    (let ((old-val (cadr (assoc var hook--watched-variables-global)))
          (new-val (when (boundp var) (symbol-value var))))
      (unless (eq old-val new-val)
        (hook--update-watched-variable-global var new-val)
        (hook--run-hooks (intern (format "hook-global-after-var-changed-%s-hook" var)))))))

(add-hook 'post-command-hook 'hook--check-watched-variables-global)

(provide 'hook)
;;; hook.el ends here
