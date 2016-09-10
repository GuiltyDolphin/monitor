;;; monitor-change.el --- Monitor changes in values.
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

(require 'monitor-core)

(defvar monitor--watched-expressions-global nil
  "Alist of expressions known to monitor.
Values are the last known values.")

(defvar monitor--watched-expressions-buffer nil
  "Alist of variables known to monitor in the current buffer.
Values are the last known values.")
(make-variable-buffer-local 'monitor--watched-expressions-buffer)

(defun monitor--update-watched-expression (var val &optional local)
  "Update the known value of VAR to VAL.
Update the buffer-local value if LOCAL is non-nil."
  (let ((monitor-var (monitor--watched-expressions-var local)))
    (eval `(setq ,monitor-var (assq-delete-all var ,monitor-var)))
    (eval `(push (cons var val) ,monitor-var))))

(defun monitor--watched-expressions-var (&optional local)
  "Return the correct watch variables based on whether LOCAL is non-nil."
  (intern (format "monitor--watched-expressions-%s" (if local 'buffer 'global))))

(defun monitor--watched-expressions-last-value (var &optional local)
  "Return the last known value of VAR.
Return the buffer-local version if LOCAL is non-nil."
  (eval `(cdr (assoc var ,(monitor--watched-expressions-var local)))))

(defun monitor--monitor-in-mode (pred mode &rest fns)
  "Monitor PRED in `major-mode' MODE.
If MODE is nil then no mode restriction is applied.
FNS are run as in `monitor--monitor'."
  (let ((mode-pred (if mode `(and (eq major-mode ',mode) ,pred) pred)))
    (apply 'monitor--monitor mode-pred fns)))

(defun monitor--expression-value-changed (expr &optional local)
  "Check for a change in value of EXPR.
Update the known value of EXPR if it has changed.
If LOCAL is non-nil, check the buffer-local value."
  (let ((old-val (monitor--watched-expressions-last-value expr local))
        (new-val (eval expr)))
    (unless (eq old-val new-val)
      (monitor--update-watched-expression expr new-val local) t)))

(defun monitor--monitor-expression-value (expr fn &optional mode local)
  "Monitor EXPR and run FN if it's value is changed.
Optional MODE should specify a `major-mode'.
If LOCAL is non-nil then monitor the buffer-local value."
  (monitor--monitor-in-mode `(monitor--expression-value-changed ',expr ,local) mode fn))

(provide 'monitor-change)
;;; monitor-change.el ends here
