;;; monitor-core.el --- Utilities for generating monitors.
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
;;; Provide utilities for generating monitors.
;;;
;;; Code:

(require 'dash)

(defvar monitor--monitored nil
  "Monitored expressions.")

(defun monitor--monitor (pred &rest fns)
  "After `post-command-hook' check PRED for a non-NIL value.
If PRED evaluates to non-NIL, then run each function in FNS."
  (let ((exist-fns (cdr (assoc pred monitor--monitored))))
    (dolist (fn fns) (unless (member fn exist-fns) (push fn exist-fns)))
    (monitor--monitored-update-functions pred exist-fns)))

(defun monitor--monitored-update-functions (pred fns)
  "Update the functions of PRED to FNS.
If FNS is nil then this deletes the entry at PRED."
  (setq monitor--monitored (--reject (equal (car it) pred) monitor--monitored))
  (when fns (push (cons pred fns) monitor--monitored)))

(defun monitor--monitored-remove-function (pred &rest fns )
  "Remove from PRED, any functions `equal' to a member of FNS."
  (let ((exist-fns (cdr (assoc pred monitor--monitored))))
    (setq exist-fns (--reject (member it fns) exist-fns))
    (monitor--monitored-update-functions pred exist-fns)))

(defun monitor--monitored-remove-pred (pred)
  "Remove PRED from the monitored predicates."
  (monitor--monitored-update-functions pred nil))

(defun monitor--check-monitored ()
  "Check each monitored expression."
  (dolist (pexp (copy-alist monitor--monitored))
    (when (condition-case var (eval (car pexp))
            (error (progn (message "error when evaluating %s (got %s)" (car pexp) (error-message-string var))
                          (monitor--monitored-remove-pred (car pexp)) nil)))
      (dolist (f (cdr pexp))
        (condition-case var (funcall f)
          (error (progn (message "error when executing %s (got %s)" f (error-message-string var))
                        (monitor--monitored-remove-function (car pexp) f))))))))

(add-hook 'post-command-hook 'monitor--check-monitored)

(provide 'monitor-core)
;;; monitor-core.el ends here
