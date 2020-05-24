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
  "The class does not inherit from `monitor--monitor'")


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Customization ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(defgroup monitor nil
  "Monitor expressions."
  :group 'lisp
  :prefix 'monitor-)


;;;;;;;;;;;;;
;; Globals ;;
;;;;;;;;;;;;;


(defvar monitor--guard-classes nil
  "Alist of registered guard symbols and their respective classes.")

(defvar monitor--listener-classes nil
  "Alist of registered listener symbols and their respective classes.")


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

(defun monitor--parse-keyword-value-args (args &optional special-keys)
  "Parse ARGS as a series of keyword value pairs.

If SPECIAL-KEYS is specified, it should be a series of keyword
symbols to keep separate from the main keyword list, and will be
returned as a separate element.

The result is in the format (keyword-args special-args non-keyword-args)."
  (let (keys specials)
    (while (keywordp (car args))
      (let ((k (pop args))
            (v (pop args)))
        (if (memq k special-keys)
            (progn (push k specials) (push v specials))
          (push k keys)
          (push v keys))))
    (list (nreverse keys) (nreverse specials) args)))

(defun monitor--expand-define-args (args)
  "Parse ARGS as a monitor definition argument list."
  (pcase-let* ((`(,keys ,specials ,args) (monitor--parse-keyword-value-args args '(:class)))
               (class (plist-get specials :class)))
    (list (if (eq (car-safe class) 'quote) (cadr class) class) keys args)))

(defun monitor--remove-monitor (monitor)
  "Remove MONITOR's definition as a monitor."
  (monitor-disable monitor)
  (put monitor monitor--instance-prop nil))

(defun monitor--symbol-monitor-object (symbol)
  "Get the monitor object associated with the symbol SYMBOL."
  (get symbol monitor--instance-prop))

(defun monitorp (monitor)
  "Return non-NIL if MONITOR is a monitor."
  (or (and (monitor--monitor--eieio-childp monitor) t)
      (and (symbolp monitor)
           (monitor--monitor--eieio-childp (monitor--symbol-monitor-object monitor))
           t)))

(defun monitor--enabled-p (monitor)
  "T if MONITOR is enabled."
  (slot-value monitor 'enabled))

(defun monitor--disabled-p (monitor)
  "T if MONITOR is disabled."
  (not (monitor--enabled-p monitor)))

(defun monitor--parse-specs (spec-class specs owner)
  "Parse SPECS as specifications for SPEC-CLASS with given OWNER."
  (mapcar
   (lambda (spec)
     (let* ((sclass (monitor--get-class-for-alias spec-class (car spec)))
            (args (cdr spec))
            (instance (apply sclass (monitor--parse-spec sclass args))))
       (oset instance owner owner)
       (monitor--setup instance)
       instance)) specs))

(cl-defgeneric monitor--get-class-for-alias (class alias)
  "Retrieve the class associated with the symbol ALIAS, for a given CLASS.")

(cl-defmethod monitor--get-class-for-alias ((_ (subclass monitor--listener)) alias)
  (or (alist-get alias monitor--listener-classes)
      (error "%s is not known to be a listener" alias)))

(cl-defmethod monitor--get-class-for-alias ((_ (subclass monitor--guard)) alias)
  (or (alist-get alias monitor--guard-classes)
      (error "%s is not known to be a guard" alias)))

(defun monitor--parse-listeners (listener-spec owner)
  "Parse LISTENER-SPEC into appropriate listeners for the given OWNER."
  (monitor--parse-specs 'monitor--listener listener-spec owner))

(defun monitor--parse-guards (guard-spec owner)
  "Parse GUARD-SPEC into appropriate guards for the given OWNER."
  (monitor--parse-specs 'monitor--guard guard-spec owner))

(defun monitor--register-listener (class &optional alias)
  "Register CLASS as a listener with optional alias ALIAS.

You need to do this if you want to use CLASS in `define-monitor' listener specifications."
  (unless (child-of-class-p class 'monitor--listener)
    (error "%s does not inherit from 'monitor--listener" class))
  (let ((alias (or alias (eieio-class-name class))))
    (add-to-list 'monitor--listener-classes (cons alias class))))

(defun monitor--register-guard (class &optional alias)
  "Register CLASS as a guard with optional alias ALIAS.

You need to do this if you want to use CLASS in `define-monitor' guard specifications."
  (unless (child-of-class-p class 'monitor--guard)
    (error "%s does not inherit from 'monitor--guard" class))
  (let ((alias (or alias (eieio-class-name class))))
    (add-to-list 'monitor--guard-classes (cons alias class))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Guards ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass monitor--guard ()
  ((enabled :initform nil
            :type booleanp
            :documentation "Non-NIL if the listener is currently enabled (allowed to listen).

Do not modify this value manually, instead use `monitor-enable' and `monitor-disable' on the parent monitor.")
   (owner :documentation "Object that this guard is guarding."))
  :abstract t
  :documentation "Base class for guards which can be used to refine when other components can trigger or activate.")

(defclass monitor--expression-value-guard (monitor--guard)
  ((expr :initarg :expr
         :documentation "Expression to monitor. It's probably best to keep this free of side-effects.")
   (pred :initarg :pred
         :type functionp
         :documentation "Function used to compare the previous and current vaue of the expression.

The function is passed the old and new values and arguments, and should return non-NIL if the monitor should trigger.")
   (value :documentation "Last known value of `:expr' (don't set this manually)."))
  :documentation "Guard which allows triggering only if an expression has reached a desired state.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Listeners ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass monitor--listener ()
  ((enabled :initform nil
            :type booleanp
            :documentation "Non-NIL if the listener is currently enabled (allowed to listen).

Do not modify this value manually, instead use `monitor-enable' and `monitor-disable' on the parent monitor.")
   (guard-trigger
    :initarg :guard-trigger
    :initform nil
    :documentation "Specification used to guard triggering.")
   (trigger-pred
    :initarg :trigger-pred
    :type functionp
    :initform (-const t)
    :documentation "Predicate that determines whether the monitor should trigger. It is passed the current monitor object, and may perform side-effects.")
   (owner :type monitorp
          :documentation "The monitor associated with this listener. Do not modify this value manually."))
  :abstract t
  :documentation "Abstract base class for all listeners.")

(defclass monitor--hook-listener (monitor--listener)
  ((hook :initarg :hook
         :documentation "Hook variable to target."))
  :documentation "Listener for triggering on hooks.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes - Monitors ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass monitor--monitor ()
  ((enabled :initform nil
            :type booleanp
            :documentation "Non-NIL if the monitor is currently enabled (allowed to monitor).

Do not modify this value manually, instead use `monitor-enable' and `monitor-disable'.")
   (trigger-on :initarg :trigger-on
               :initform nil
               :documentation "Specification for listeners that should trigger the monitor.")
   (on-trigger :initarg :on-trigger
               :initform #'ignore
               :type functionp
               :documentation "Run whenever one of the listeners in `:trigger-on' is triggered.")
   (listeners :initform nil)
   (trigger-pred
    :initarg :trigger-pred
    :type functionp
    :initform (-const t)
    :documentation "Predicate that determines whether the monitor should trigger. It is passed the current monitor object, and may perform side-effects."))
  :documentation "Base class for all monitors.")


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


;;; Monitor (monitor)


(cl-defmethod monitor--enable :after ((obj monitor--monitor))
  (oset obj enabled t))

(cl-defmethod monitor--disable :after ((obj monitor--monitor))
  (oset obj enabled nil))

(cl-defmethod monitor--enable ((obj monitor--monitor))
  (dolist (listener (oref obj listeners))
    (monitor--enable listener)))

(cl-defmethod monitor--disable ((obj monitor--monitor))
  (dolist (listener (oref obj listeners))
    (monitor--disable listener)))


;;; Listener (listener)


(cl-defmethod monitor--enable :after ((obj monitor--listener))
  (dolist (guard (oref obj guard-trigger))
    (monitor--enable guard))
  (oset obj enabled t))

(cl-defmethod monitor--disable :after ((obj monitor--listener))
  (dolist (guard (oref obj guard-trigger))
    (monitor--disable guard))
  (oset obj enabled nil))


;;; Hook (listener)


(defun monitor--hook-build-hook-fn (obj)
  "Build a form suitable for adding to a hook for the OBJ."
  (lambda () (monitor--trigger--trigger obj)))

(cl-defmethod monitor--enable ((obj monitor--hook-listener))
  (add-hook (oref obj hook) (monitor--hook-build-hook-fn obj)))

(cl-defmethod monitor--disable ((obj monitor--hook-listener))
  (remove-hook (oref obj hook) (monitor--hook-build-hook-fn obj)))


;;; Guard (guard)


(cl-defmethod monitor--enable :after ((obj monitor--guard))
  (oset obj enabled t))

(cl-defmethod monitor--disable :after ((obj monitor--guard))
  (oset obj enabled nil))


;;; Expression-value (guard)


(cl-defmethod monitor--enable ((obj monitor--expression-value-guard))
  (oset obj value (eval (oref obj expr))))

(cl-defmethod monitor--disable ((obj monitor--expression-value-guard))
  (slot-makeunbound obj 'value))


;;;;;;;;;;;
;; Setup ;;
;;;;;;;;;;;


(cl-defgeneric monitor--setup (obj)
  "Initialize OBJ.

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


;;; Monitor (monitor)


(cl-defmethod monitor--setup ((obj monitor--monitor))
  (let ((trigger-on (monitor--parse-listeners (oref obj trigger-on) obj)))
    (oset obj trigger-on trigger-on))
  (oset obj listeners (oref obj trigger-on)))


;;; Listener (listener)


(cl-defmethod monitor--setup :after ((obj monitor--listener))
  (let ((guard-trigger (monitor--parse-guards (oref obj guard-trigger) obj)))
    (oset obj guard-trigger guard-trigger)))

(cl-defmethod monitor--setup ((_ monitor--listener)))


;;; Hook (listener)


(cl-defmethod monitor--setup :before ((obj monitor--hook-listener))
  "We require the :hook argument to be bound."
  (monitor--validate-required-options obj '(:hook)))


;;; Guard (guard)


(cl-defmethod monitor--setup ((_ monitor--guard))
  "No additional setup required for base guard.")


;;; Expression-value (guard)


(cl-defmethod monitor--setup ((obj monitor--expression-value-guard))
  "We require the `:expr' and `:pred' arguments to be bound."
  (monitor--validate-required-options obj '(:expr :pred))
  (let* ((owner (oref obj owner))
         (pred-old (oref owner trigger-pred))
         (pred-new (lambda ()
                     (let* ((expr (oref obj expr))
                            (old (oref obj value))
                            (new (eval expr)))
                       (when (funcall (oref obj pred) old new) (oset obj value new) t)))))
    ;; we wrap up the old predicate with a new predicate that tracks the expression value
    (oset owner trigger-pred (lambda () (and (funcall pred-old) (funcall pred-new)))))
  (cl-call-next-method))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specification parsing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cl-defgeneric monitor--parse-spec (obj args)
  "Specify how to parse ARGS as a specification for OBJ.")


;;; Listener (listener)


(cl-defmethod monitor--parse-spec ((_ (subclass monitor--listener)) args)
  args)


;;; Hook (listener)


(cl-defmethod monitor--parse-spec ((_ (subclass monitor--hook-listener)) args)
  (pcase-let* ((`(,keys ,specials ,args)
                (monitor--parse-keyword-value-args args '(:hook)))
               (hook (if (plist-member specials :hook)
                         (plist-get specials :hook)
                       (pop args))))
    (when hook (setq keys (plist-put keys :hook hook)))
    (-concat keys args)))


;;; Guard (guard)


(cl-defmethod monitor--parse-spec ((_ (subclass monitor--guard)) args)
  args)


;;;;;;;;;;;;;;;;
;; Triggering ;;
;;;;;;;;;;;;;;;;


(cl-defgeneric monitor--trigger--trigger (obj)
  "This method determines how to handle triggering a monitor, i.e., the moment the monitor becomes instantaneously active.")


;;; Listener (listener)


(cl-defmethod monitor--trigger--trigger ((obj monitor--listener) &optional args)
  "Run the `:on-trigger' function of the owner of OBJ with ARGS as arguments.

Only triggers if the predicate in `:trigger-pred' returns non-NIL."
  (when (funcall (oref obj trigger-pred))
    (monitor--trigger--trigger (oref obj owner) args)))


;;; Monitor (monitor)


(cl-defmethod monitor--trigger--trigger ((obj monitor--monitor) &optional args)
  "Run the `:trigger' function of OBJ with ARGS as arguments.

The monitor will only trigger if the predicate in `:trigger-pred' returns non-NIL."
  (when (funcall (oref obj trigger-pred))
    (monitor--funcall (oref obj on-trigger) args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Creating monitors ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun monitor-create (&rest args)
  "Create a new monitor.

ARGS is a series of keyword-value pairs.  Each key has to be a
keyword symbol, either `:class' or a keyword argument supported
by the constructor of that class.  If no class is specified, it
defaults to `monitor--monitor'."
  (declare (indent 1))
  (pcase-let* ((`(,class ,slots _)
                (monitor--expand-define-args args))
               (class (or class 'monitor--monitor)))
    (unless (child-of-class-p class 'monitor--monitor)
      (signal 'monitor--does-not-inherit-base-monitor-class class))
    (let ((obj (monitor--funcall class slots)))
      (monitor--setup obj)
      obj)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Default setup ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(monitor--register-listener 'monitor--hook-listener 'hook)
(monitor--register-guard 'monitor--expression-value-guard 'expression-value)


(provide 'monitor)
;;; monitor.el ends here
