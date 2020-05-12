;;; monitor.el --- Utilities for monitoring expressions -*- lexical-binding: t -*-

;; Copyright (C) 2016, 2020 Ben Moon
;; Author: Ben Moon <software@guiltydolphin.com>
;; URL: https://github.com/guiltydolphin/monitor
;; Git-Repository: git://github.com/guiltydolphin/monitor.git
;; Created: 2016-08-17
;; Version: 0.4.0
;; Keywords: lisp, monitor, utility
;; Package-Requires: ((dash "2.17.0") (dash-functional "1.2.0") (emacs "25.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Monitor provides utilities for monitoring expressions.
;; A predicate-based system is used to determine when to run
;; specific functions - not unlike Emacs' built-in hooks (see Info node `Hooks').
;;
;; For example, if we wanted to print "foo" every time the value
;; of (point) changed in the current buffer, we could write:
;;
;;    (monitor-expression-value (point) (lambda () (print "foo")))
;;
;; A (rather convoluted) way of mimicking the functionality of the
;; standard `after-change-major-mode-hook' could be to use the
;; following expression:
;;
;;    (monitor-expression-value major-mode (...))
;;
;; Which would run whenever the value of `major-mode' changed.

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'eieio)


;;;;;;;;;;;;;;;;;;
;;;;; Errors ;;;;;
;;;;;;;;;;;;;;;;;;


(define-error 'monitor--missing-required-option
  "Missing required option(s)")

(define-error 'monitor--does-not-inherit-base-monitor-class
  "The class does not inherit from `monitor--base'")


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Customization ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(defgroup monitor nil
  "Monitor expressions."
  :group 'lisp
  :prefix 'monitor-)


;;;;;;;;;;;;;;;;;;;
;;;;; Helpers ;;;;;
;;;;;;;;;;;;;;;;;;;


(defun monitor--funcall (fn &rest args)
  "Call FN as a function with remaining ARGS, use the last arg as list of args.

Thus (monitor--funcall #'fn 'a '(b c)) is the same as (funcall #'fn 'a 'b 'c).

Returns the value FN returns."
  (funcall (-applify fn) (-concat (-drop-last 1 args) (car (-take-last 1 args)))))

(defconst monitor--instance-prop
  :monitor--instance
  "Property name under which instance information is stored.

Please don't modify this value manually.")

(defun monitor--require-monitor-obj (obj)
  "Get the monitor associated with OBJ, which must be something that satisfies `monitorp'.

This fails if `obj' does not satisfy `monitorp'."
  (cl-check-type obj monitorp)
  (if (symbolp obj)
      (monitor--symbol-monitor-object obj)
    obj))

(defun monitor--expand-define-args (args)
  "Parse ARGS as a monitor definition argument list."
  (let (class keys docstr)
    (when (stringp (car args))
      (setq docstr (pop args)))
    (while (keywordp (car args))
      (let ((k (pop args))
            (v (pop args)))
        (if (eq k :class)
            (setq class v)
          (push k keys)
          (push v keys))))
    (list (if (eq (car-safe class) 'quote) (cadr class) class)
          (nreverse keys)
          docstr
          args)))

(defun monitor--remove-monitor (monitor)
  "Remove MONITOR's definition as a monitor."
  (monitor-disable monitor)
  (put monitor monitor--instance-prop nil))

(defun monitor--symbol-monitor-object (symbol)
  "Get the monitor object associated with the symbol SYMBOL."
  (get symbol monitor--instance-prop))

(defun monitorp (monitor)
  "Return non-NIL if MONITOR is a monitor."
  (or (and (monitor--base--eieio-childp monitor) t)
      (and (symbolp monitor)
           (monitor--base--eieio-childp (monitor--symbol-monitor-object monitor))
           t)))

(defun monitor--enabled-p (monitor)
  "T if MONITOR is enabled."
  (slot-value monitor 'enabled))

(defun monitor--disabled-p (monitor)
  "T if MONITOR is disabled."
  (not (monitor--enabled-p monitor)))


;;;;;;;;;;;;;;;;;;;
;;;;; Classes ;;;;;
;;;;;;;;;;;;;;;;;;;


(defclass monitor--base ()
  ((enabled :initform nil
            :type booleanp
            :documentation "Non-NIL if the monitor is currently enabled (allowed to monitor).

Do not modify this value manually, instead use `monitor-enable' and `monitor-disable'."))
  :abstract t
  :documentation "Abstract base class for all monitors.")

(defclass monitor--trigger (monitor--base)
  ((trigger :initarg :trigger
            :initform #'ignore
            :type functionp
            :documentation "Trigger run whenever the monitor is activated."))
  :abstract t
  :documentation "Abstract class for monitors that support instantaneous triggering.")

(defclass monitor--hook (monitor--trigger)
  ((hook :initarg :hook
         :documentation "Hook variable to target."))
  :documentation "Monitor for triggering on hooks.")

(defclass monitor--guarded (monitor--trigger)
  ((pred :initarg :pred
         :type functionp
         :documentation "Predicate that determines whether the monitor should trigger."))
  :abstract t
  :documentation "Abstract class for monitors which can only trigger when a predicate is satisfied.")

(defclass monitor--expression-value (monitor--guarded)
  ((expr :initarg :expr
         :documentation "Expression to monitor. It's probably best to keep this free of side-effects.")
   (pred :initarg :pred
         :type functionp
         :documentation "Function used to compare the previous and current vaue of the expression.

The function is passed the old and new values and arguments, and should return non-NIL if the monitor should trigger.")
   (value :documentation "Last known value of `:expr' (don't set this manually)."))
  :abstract t
  :documentation "Abstract class for monitors which should only trigger if an expression has reached a desired state since the last tick.")


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Class methods ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enabling and disabling ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl-defgeneric monitor--enable (obj)
  "Enable monitoring with OBJ.

Note that you should only use this when implementing the method behaviour via `cl-defmethod', if you actually want to enable the monitor, use `monitor-enable' instead.")

(cl-defgeneric monitor--disable (obj)
  "Disable monitoring with OBJ.

Note that you should only use this when implementing the method behaviour via `cl-defmethod', if you actually want to disable the monitor, use `monitor-disable' instead.")

(defun monitor-enable (monitor)
  "Enable MONITOR."
  (let ((m (monitor--require-monitor-obj monitor)))
    (unless (monitor--enabled-p m) (monitor--enable m))))

(defun monitor-disable (monitor)
  "Disable MONITOR."
  (let ((m (monitor--require-monitor-obj monitor)))
    (unless (monitor--disabled-p m) (monitor--disable m))))


;;; Base


(cl-defmethod monitor--enable :after ((obj monitor--base))
  (oset obj enabled t))

(cl-defmethod monitor--disable :after ((obj monitor--base))
  (oset obj enabled nil))


;;; Hook


(defun monitor--hook-build-hook-fn (obj)
  "Build a form suitable for adding to a hook for OBJ."
  (lambda () (monitor--trigger--trigger obj)))

(cl-defmethod monitor--enable ((obj monitor--hook))
  (add-hook (oref obj hook) (monitor--hook-build-hook-fn obj)))

(cl-defmethod monitor--disable ((obj monitor--hook))
  (remove-hook (oref obj hook) (monitor--hook-build-hook-fn obj))
  (oset obj enabled nil))


;;; Expression-value


(cl-defmethod monitor--enable :before ((obj monitor--expression-value))
  (oset obj value (eval (oref obj expr))))

(cl-defmethod monitor--disable :before ((obj monitor--expression-value))
  (slot-makeunbound obj 'value))


;;;;;;;;;;;
;; Setup ;;
;;;;;;;;;;;


(cl-defgeneric monitor--setup (obj)
  "Initialize the monitor object OBJ.

This method is called when an instance is created with
`monitor-define-monitor', so it's a good place to put any
validation (e.g., checking for missing options) and
initialization you want to apply to all new instances.

You should usually either combine this method with `:before' or
`:after' (see `cl-defmethod'), or call `cl-call-next-method' in
the body.")

(defun monitor--validate-required-options (obj props)
  "Check that OBJ provides each option in PROPS, fail otherwise."
  (let ((missing-opts))
    (dolist (prop props)
      (unless (slot-boundp obj prop)
        (push prop missing-opts)))
    (unless (null missing-opts)
      (signal 'monitor--missing-required-option (nreverse missing-opts)))))


;;; Base


(cl-defmethod monitor--setup ((_ monitor--base))
  "No additional setup required for base monitor.")


;;; Hook


(cl-defmethod monitor--setup :after ((obj monitor--hook))
  "We require the :hook argument to be bound."
  (monitor--validate-required-options obj '(:hook)))


;;; Guarded


(cl-defmethod monitor--setup :after ((obj monitor--guarded))
  "We require the :pred option to be bound."
  (monitor--validate-required-options obj '(:pred)))


;;; Expression-value


(cl-defmethod monitor--setup ((obj monitor--expression-value))
  "We require the `:expr' and `:pred' arguments to be bound."
  (monitor--validate-required-options obj '(:expr :pred))
  (let* ((pred-old (oref obj pred))
         (pred-new (lambda (obj)
                     (let* ((expr (oref obj expr))
                            (old (oref obj value))
                            (new (eval expr)))
                       (when (funcall pred-old old new) (oset obj value new) t)))))
    ;; we wrap up the old predicate with a new predicate that tracks the expression value
    (oset obj pred pred-new))
  (cl-call-next-method))


;;;;;;;;;;;;;;;;
;; Triggering ;;
;;;;;;;;;;;;;;;;


(cl-defgeneric monitor--trigger--trigger (obj)
  "This method determines how to handle triggering a monitor, i.e., the moment the monitor becomes instantaneously active.")


;;; Trigger


(cl-defmethod monitor--trigger--trigger ((obj monitor--trigger) &rest args)
  "Run the `:trigger' function of OBJ with ARGS as arguments."
  (monitor--funcall (oref obj trigger) args))


;;; Guarded


(cl-defmethod monitor--trigger--trigger :around ((obj monitor--guarded) &rest args)
  "Triggering is guarded by a predicate (`:pred').

The monitor will only trigger if this predicate returns non-NIL when passed OBJ."
  (when (funcall (oref obj pred) obj)
    (apply #'cl-call-next-method args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Creating monitors ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro monitor-define-monitor (name arglist &rest args)
  "Define NAME as a monitor.

ARGLIST is currently ignored, but may be used in future.
DOCSTRING is the documentation string and is optional.

These arguments can optionally be followed by key-value pairs.
Each key has to be a keyword symbol, either `:class' or a keyword
argument supported by the constructor of that class. It is an error
not to specify a class.

\(fn NAME ARGLIST [DOCSTRING] [KEYWORD VALUE]...)"
  (declare (debug (&define name lambda-list
                           [&optional lambda-doc]
                           [&rest keywordp sexp]))
           (doc-string 3)
           (indent defun))
  (ignore arglist) ; to prevent warning about unused ARGLIST
  (let ((obj (make-symbol "obj")))
    (pcase-let ((`(,class ,slots ,docstr _)
                 (monitor--expand-define-args args)))
      (when (null class)
        (error "You must specify a non-NIL value for `:class'"))
      (unless (child-of-class-p class 'monitor--base)
        (signal 'monitor--does-not-inherit-base-monitor-class class))
      `(progn
         (let ((,obj (,class ,@slots)))
           (monitor--setup ,obj)
           (put ',name 'function-documentation ,docstr)
           (put ',name ,monitor--instance-prop ,obj))))))

(defalias 'define-monitor 'monitor-define-monitor)


(provide 'monitor)
;;; monitor.el ends here
