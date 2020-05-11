;;; monitor-tests.el --- Tests for monitor.el -*- lexical-binding: t -*-
;;; Code:

(require 'ert)
(require 'monitor)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes for testing ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass monitor-test--not-a-monitor ()
  (()))

(defclass monitor-test--empty-monitor (monitor--base)
  (())
  :documentation "Empty monitor for testing without side effects.")

(cl-defmethod monitor--enable ((_ monitor-test--empty-monitor)))

(cl-defmethod monitor--disable ((_ monitor-test--empty-monitor)))

(defclass monitor-test--enable-disable-monitor (monitor--base)
  ((enable-var :initarg :enable-var)
   (disable-var :initarg :disable-var)))

(cl-defmethod monitor--enable ((obj monitor-test--enable-disable-monitor))
  (eval `(set ',(oref obj enable-var) (1+ ,(oref obj enable-var)))))

(cl-defmethod monitor--disable ((obj monitor-test--enable-disable-monitor))
  (eval `(set ',(oref obj disable-var) (1+ ,(oref obj disable-var)))))

(defclass monitor-test--enable-disable-monitor-child (monitor-test--enable-disable-monitor)
  (()))

(cl-defmethod monitor--enable ((_ monitor-test--enable-disable-monitor-child)))


;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Test helpers ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;


(defun monitor--test-build-test-monitor (name &optional parent &rest args)
  "Build a test monitor named NAME."
  (apply 'define-monitor name parent "Test monitor." args))

(defmacro monitor--test-with-uninterned-symbols (symbols &rest body)
  "Provide an uninterned version of each symbol in SYMBOLS for use in BODY."
  (declare (indent 1))
  `(let (,@(--map `(,it (make-symbol (symbol-name ',it))) symbols))
     ,@body))

(defmacro monitor-test--should-error-missing-options (opts form)
  "Assert that FORM throws an error indicating that the options in OPTS should be present."
  (declare (indent 1))
  `(should (equal '(monitor--missing-required-option . ,opts)
                  (should-error ,form :type 'monitor--missing-required-option))))

(defun monitor-test--define-monitor (name-sym class &rest args)
  "Define a new monitor whose name is NAME-SYM with ARGS as the remaining args.

This is a simple wrapper around `monitor-define-monitor'.

\(fn NAME-SYM CLASS [KEYWORD VALUE]...)"
  (declare (indent defun))
  (eval `(monitor-define-monitor ,name-sym () :class ',class ,@args)))


;;;;;;;;;;;;;;;;;
;;;;; Tests ;;;;;
;;;;;;;;;;;;;;;;;


(ert-deftest monitor-test-monitorp ()
  "Tests for `monitorp'."
  (monitor--test-with-uninterned-symbols (monitor-symbol)

    ;; initially, we don't expect it to be a monitor.
    (should (eq nil (monitorp monitor-symbol)))

    (let ((instance (monitor-test--define-monitor monitor-symbol 'monitor-test--empty-monitor)))
      ;; now we've created a monitor, it should recognize it as one
      (should (eq t (monitorp monitor-symbol)))

      ;; the instance should be a monitor
      (should (eq t (monitorp instance)))

      (monitor--remove-monitor monitor-symbol)
      ;; now it is no longer a monitor
      (should (eq nil (monitorp monitor-symbol))))))

(ert-deftest monitor-test-monitor-enable-disable ()
  "Tests for `monitor-enable' and `monitor-disable'."
  (monitor--test-with-uninterned-symbols (monitor-symbol monitor-child counter-enabled counter-disabled)
    (set counter-enabled 0)
    (set counter-disabled 0)
    (let ((instance (eval `(monitor-define-monitor ,monitor-symbol ()
                             :class 'monitor-test--enable-disable-monitor
                             :enable-var ',counter-enabled
                             :disable-var ',counter-disabled))))
      (should (= 0 (symbol-value counter-enabled)))
      (should (= 0 (symbol-value counter-disabled)))
      (should (eq t (monitor--disabled-p instance)))
      (monitor-enable monitor-symbol)
      (should (= 1 (symbol-value counter-enabled)))
      (should (eq t (monitor--enabled-p instance)))
      (monitor-enable monitor-symbol)
      (should (= 1 (symbol-value counter-enabled)))
      (monitor-disable monitor-symbol)
      (should (= 1 (symbol-value counter-disabled)))
      (should (eq t (monitor--disabled-p instance)))
      (monitor-disable monitor-symbol)
      (should (= 1 (symbol-value counter-disabled)))
      (eval `(monitor-define-monitor ,monitor-child ()
               :class 'monitor-test--enable-disable-monitor-child
               :enable-var ',counter-enabled
               :disable-var ',counter-disabled))
      (should (= 1 (symbol-value counter-enabled)))
      (should (= 1 (symbol-value counter-disabled)))
      (monitor-enable monitor-child)
      (should (= 1 (symbol-value counter-enabled)))
      (monitor-disable monitor-child)
      (should (= 2 (symbol-value counter-disabled))))))

(ert-deftest monitor-test:define-monitor:basic ()
  "Basic tests for `define-monitor'."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    ;; 'monitor-test--not-a-monitor does not inherit from the base monitor class
    (should-error (monitor-test--define-monitor monitor-symbol 'monitor-test--not-a-monitor)
                  :type 'monitor--does-not-inherit-base-monitor-class)))

(ert-deftest monitor-test:define-monitor:return-is-instance ()
  "Test that the return value of `monitor-define-monitor' is the actual monitor instance."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    (let* ((ret-instance (monitor-test--define-monitor monitor-symbol
                           'monitor-test--empty-monitor))
           (sym-instance (monitor--symbol-monitor-object monitor-symbol)))
      (should (eq sym-instance ret-instance))
      (should (eq nil (monitor--enabled-p sym-instance)))
      (should (eq nil (monitor--enabled-p ret-instance)))
      (monitor-enable monitor-symbol)
      (should (eq t (monitor--enabled-p sym-instance)))
      (should (eq t (monitor--enabled-p ret-instance)))
      (monitor-disable monitor-symbol)
      (should (eq nil (monitor--enabled-p sym-instance)))
      (should (eq nil (monitor--enabled-p ret-instance))))))

(ert-deftest monitor-test:monitor-enable:accepts-only-monitors ()
  "Test that `monitor-enable' only accepts valid monitors or monitor symbols."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    (should-error (monitor-enable nil)
                  :type 'wrong-type-argument)
    (should-error (monitor-enable monitor-symbol)
                  :type 'wrong-type-argument)
    (monitor-test--define-monitor monitor-symbol 'monitor-test--empty-monitor)
    ;; this shouldn't error
    (monitor-enable monitor-symbol)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for specific monitors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ert-deftest monitor-test-hook-monitor ()
  "Tests for the 'hook monitor."
  (monitor--test-with-uninterned-symbols (monitor-symbol hook-symbol counter-a)
    (set counter-a 0)
    (set hook-symbol nil)

    ;; the :hook option is required
    (monitor-test--should-error-missing-options (:hook)
      (monitor-test--define-monitor monitor-symbol 'monitor--hook))

    (let* ((instance (eval `(monitor-define-monitor ,monitor-symbol ()
                              :class 'monitor--hook
                              :trigger (lambda () (setq ,counter-a (1+ ,counter-a)))
                              :hook ',hook-symbol))))
      ;; disabled
      (should (eq nil (monitor--enabled-p instance)))
      (should (eq nil (symbol-value hook-symbol)))

      (run-hooks hook-symbol)
      ;; counter should remain unchanged, as the monitor should not be
      ;; running anything from the hook
      (should (= 0 (symbol-value counter-a)))
      (monitor-enable monitor-symbol)

      ;; enabled
      (should (eq t (monitor--enabled-p instance)))
      (should (= 1 (length (symbol-value hook-symbol))))

      ;; instances trigger when hook runs
      (run-hooks hook-symbol)
      ;; counter should now be incremented, as the monitor was enabled
      ;; and should have tied into the hook
      (should (= 1 (symbol-value counter-a)))

      (monitor-disable monitor-symbol)

      ;; disabled
      (should (eq nil (monitor--enabled-p instance)))
      (should (eq nil (symbol-value hook-symbol))))))


(ert-deftest monitor-test:expression-value-monitor ()
  "Tests for the 'expression-value monitor."
  (monitor--test-with-uninterned-symbols (monitor-symbol counter-a counter-b)
    (set counter-a 0)
    (set counter-b 0)

    ;; the :expr and :pred options are required
    (monitor-test--should-error-missing-options (:expr :pred)
      (monitor-test--define-monitor monitor-symbol 'monitor--expression-value))

    (unwind-protect
        (let* ((instance (eval `(monitor-define-monitor ,monitor-symbol ()
                                  :class 'monitor--expression-value
                                  :trigger (lambda () (setq ,counter-a (1+ ,counter-a)))
                                  :expr ',counter-b
                                  :pred (lambda (old new) (> new old))))))
          (monitor-enable monitor-symbol)

          ;; enabled
          (should (eq t (monitor--enabled-p instance)))

          ;; value not yet checked
          (should (= 0 (symbol-value counter-a)))

          (monitor--trigger--trigger instance)

          ;; value checked, but same
          (should (= 0 (symbol-value counter-a)))

          (set counter-b (1+ (symbol-value counter-b)))

          ;; should trigger (value increased)
          (monitor--trigger--trigger instance)
          (should (= 1 (symbol-value counter-a)))

          (monitor-disable monitor-symbol)

          ;; disabled
          (should (eq nil (monitor--enabled-p instance)))

          ;; cannot trigger when disabled
          (should-error (monitor--trigger--trigger instance))))))


(provide 'monitor-tests)
;;; monitor-tests.el ends here
