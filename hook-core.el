;;; hook-core.el --- Utilities for generating hooks.
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

(require 'dash)

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

(provide 'hook-core)
;;; hook-core.el ends here
