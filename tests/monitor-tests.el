;;; monitor-tests.el --- Tests for monitor.el -*- lexical-binding: t -*-
;;; Code:

(require 'ert)
(require 'monitor)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Classes for testing ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass monitor-test--not-a-monitor ()
  (()))

(defclass monitor-test--empty-monitor (monitor--monitor)
  (())
  :documentation "Empty monitor for testing without side effects.")

(cl-defmethod monitor--enable ((_ monitor-test--empty-monitor)))

(cl-defmethod monitor--disable ((_ monitor-test--empty-monitor)))

(defclass monitor-test--enable-disable-monitor (monitor--monitor)
  ((enable-var :initarg :enable-var)
   (disable-var :initarg :disable-var)))

(cl-defmethod monitor--enable ((obj monitor-test--enable-disable-monitor))
  (eval `(set ',(oref obj enable-var) (1+ ,(oref obj enable-var)))))

(cl-defmethod monitor--disable ((obj monitor-test--enable-disable-monitor))
  (eval `(set ',(oref obj disable-var) (1+ ,(oref obj disable-var)))))

(defclass monitor-test--enable-disable-monitor-child (monitor-test--enable-disable-monitor)
  (()))

(cl-defmethod monitor--enable ((_ monitor-test--enable-disable-monitor-child)))

(defclass monitor-test--dummy-listener (monitor--listener)
  (())
  :documentation "Dummy listener which doesn't do anything special.")


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

(defun monitor-test--define-monitor (&rest args)
  "Define a new monitor with ARGS as arguments.

This is a simple wrapper around `monitor-create'.

\(fn [KEYWORD VALUE]...)"
  (declare (indent 1))
  (apply #'monitor-create args))


;;;;;;;;;;;;;;;;;
;;;;; Tests ;;;;;
;;;;;;;;;;;;;;;;;


(ert-deftest monitor-test:monitorp ()
  "Tests for `monitorp'."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    (let ((instance (monitor-test--define-monitor :class 'monitor-test--empty-monitor)))
      ;; the instance should be a monitor
      (should (eq t (monitorp instance))))))

(ert-deftest monitor-test:monitor-enable-disable ()
  "Tests for `monitor-enable' and `monitor-disable'."
  (monitor--test-with-uninterned-symbols (counter-enabled counter-disabled)
    (set counter-enabled 0)
    (set counter-disabled 0)
    (let ((instance (monitor-test--define-monitor
                     :class 'monitor-test--enable-disable-monitor
                     :enable-var counter-enabled
                     :disable-var counter-disabled)))
      (should (= 0 (symbol-value counter-enabled)))
      (should (= 0 (symbol-value counter-disabled)))
      (should (eq t (monitor--disabled-p instance)))
      (monitor-enable instance)
      (should (= 1 (symbol-value counter-enabled)))
      (should (eq t (monitor--enabled-p instance)))
      (monitor-enable instance)
      (should (= 1 (symbol-value counter-enabled)))
      (monitor-disable instance)
      (should (= 1 (symbol-value counter-disabled)))
      (should (eq t (monitor--disabled-p instance)))
      (monitor-disable instance)
      (should (= 1 (symbol-value counter-disabled)))
      (let ((child (monitor-test--define-monitor
                     :class 'monitor-test--enable-disable-monitor-child
                     :enable-var counter-enabled
                     :disable-var counter-disabled)))
        (should (= 1 (symbol-value counter-enabled)))
        (should (= 1 (symbol-value counter-disabled)))
        (monitor-enable child)
        (should (= 1 (symbol-value counter-enabled)))
        (monitor-disable child)
        (should (= 2 (symbol-value counter-disabled)))))))

(ert-deftest monitor-test:create-monitor:basic ()
  "Basic tests for `define-monitor'."
  ;; 'monitor-test--not-a-monitor does not inherit from the base monitor class
  (should-error (monitor-create :class 'monitor-test--not-a-monitor)
                :type 'monitor--does-not-inherit-base-monitor-class))

(ert-deftest monitor-test:monitor-enable:accepts-only-monitors ()
  "Test that `monitor-enable' only accepts valid monitors or monitor symbols."
  (should-error (monitor-enable nil)
                :type 'wrong-type-argument)
  ;; this shouldn't error
  (monitor-enable (monitor-test--define-monitor () :class 'monitor-test--empty-monitor)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for specific monitors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ert-deftest monitor-test:hook-monitor ()
  "Tests for the 'hook listener."
  (monitor--test-with-uninterned-symbols (hook-symbol counter-a)
    (set counter-a 0)
    (set hook-symbol nil)

    ;; the :hook option is required
    (monitor-test--should-error-missing-options (:hook)
      (monitor-test--define-monitor :trigger-on [(hook)]))

    ;; first argument is taken to be the hook (this should not error)
    (eval `(monitor-test--define-monitor :trigger-on [(hook ,hook-symbol)]))

    ;; you can use the full name of the listener if you wish
    (eval `(monitor-test--define-monitor :trigger-on [(monitor--hook-listener ,hook-symbol)]))

    (let* ((instance (eval `(monitor-test--define-monitor
                              :trigger-on [(hook :hook ,hook-symbol)]
                              :on-trigger (lambda () (setq ,counter-a (1+ ,counter-a)))))))
      ;; disabled
      (should (eq nil (monitor--enabled-p instance)))
      (should (eq nil (symbol-value hook-symbol)))

      (run-hooks hook-symbol)
      ;; counter should remain unchanged, as the monitor should not be
      ;; running anything from the hook
      (should (= 0 (symbol-value counter-a)))
      (monitor-enable instance)

      ;; enabled
      (should (eq t (monitor--enabled-p instance)))
      (should (= 1 (length (symbol-value hook-symbol))))

      ;; instances trigger when hook runs
      (run-hooks hook-symbol)
      ;; counter should now be incremented, as the monitor was enabled
      ;; and should have tied into the hook
      (should (= 1 (symbol-value counter-a)))

      (monitor-disable instance)

      ;; disabled
      (should (eq nil (monitor--enabled-p instance)))
      (should (eq nil (symbol-value hook-symbol))))))


(ert-deftest monitor-test:expression-value ()
  "Tests for the 'expression-value guard."
  (monitor--test-with-uninterned-symbols (counter-a counter-b counter-c hook-symbol)
    (set counter-a 0)
    (set counter-b 0)
    (set counter-c 0)
    (set hook-symbol nil)

    ;; the :expr and :pred options are required
    (monitor-test--should-error-missing-options (:expr :pred)
      (monitor-test--define-monitor :trigger-on [(monitor-test--dummy-listener :guard-trigger [(expression-value)])]))

    ;; we can specify the guard locally to a listener
    (let* ((instance1 (eval `(monitor-test--define-monitor
                               :trigger-on [(hook ,hook-symbol
                                             :guard-trigger [(expression-value
                                                              :expr ,counter-b
                                                              :pred (lambda (old new) (> new old)))])]
                               :on-trigger (lambda () (setq ,counter-a (1+ ,counter-a))))))
           ;; we can specify the guard globally to a monitor
           (instance2 (eval `(monitor-test--define-monitor
                               :trigger-on [(hook ,hook-symbol)]
                               :guard-trigger [(expression-value
                                                :expr ,counter-b
                                                :pred (lambda (old new) (> new old)))]
                               :on-trigger (lambda () (setq ,counter-c (1+ ,counter-c)))))))

      (monitor-enable instance1)
      (monitor-enable instance2)

      ;; enabled
      (should (eq t (monitor--enabled-p instance1)))
      (should (eq t (monitor--enabled-p instance2)))

      ;; value not yet checked
      (should (= 0 (symbol-value counter-a)))
      (should (= 0 (symbol-value counter-c)))

      (run-hooks hook-symbol)

      ;; value checked, but same
      (should (= 0 (symbol-value counter-a)))
      (should (= 0 (symbol-value counter-c)))

      (set counter-b (1+ (symbol-value counter-b)))

      ;; should trigger (value increased)
      (run-hooks hook-symbol)
      (should (= 1 (symbol-value counter-a)))
      (should (= 1 (symbol-value counter-c)))

      (monitor-disable instance1)
      (monitor-disable instance2)

      ;; disabled
      (should (eq nil (monitor--enabled-p instance1)))
      (should (eq nil (monitor--enabled-p instance2))))))


(provide 'monitor-tests)
;;; monitor-tests.el ends here
