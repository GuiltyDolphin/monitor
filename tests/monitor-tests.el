;;; monitor-tests.el
;;; Code:

(require 'ert)
(require 'monitor)

(defun monitor--test-build-test-monitor (name &optional parent &rest args)
  "Build a test monitor named NAME."
  (apply 'monitor--define-monitor name parent "Test monitor." args))

(defmacro monitor--test-build-get-put-tests (getter putter)
  "Build standard tests for a getter GETTER and putter PUTTER."
  (let* ((monitor-symbol (make-symbol "monitor-symbol"))
         (get-form `(funcall ,getter ,monitor-symbol :test-slot-a))
         (set-form (lambda (to) `(funcall ,putter ,monitor-symbol :test-slot-a ,to))))
    `(let ((,monitor-symbol (make-symbol "monitor--test-build-get-put-tests-tvar")))
       (should-error ,get-form :type '(wrong-type-argument monitorp))
       (should-error ,(funcall set-form "test") :type 'wrong-type-argument)
       (monitor--test-build-test-monitor ,monitor-symbol)
       (should (eq nil ,get-form))
       ,(funcall set-form "test")
       (should (equal "test" (funcall ,getter ,monitor-symbol :test-slot-a)))
       ,(funcall set-form "test2")
       (should (equal "test2" ,get-form)))))

(ert-deftest monitor-test-monitorp ()
  "Tests for `monitorp'."
  (let ((monitor-symbol (make-symbol "monitor-test-monitorp-tvar")))
  ; initially, we don't expect it to be a monitor.
  (should (eq nil (monitorp monitor-symbol)))
  (monitor--test-build-test-monitor monitor-symbol)
  ; now we've created a monitor, it should recognize it as one
  (should-not (eq nil (monitorp monitor-symbol)))
  (monitor--remove-monitor monitor-symbol)
  ; now it is no longer a monitor
  (should (eq nil (monitorp monitor-symbol)))))

(ert-deftest monitor-test-monitor-meta-get-put ()
  "Tests for `monitor--meta-put' and `monitor--meta-get'."
  (monitor--test-build-get-put-tests 'monitor--meta-get 'monitor--meta-put))

(ert-deftest monitor-test-monitor-decl-get-put ()
  "Tests for `monitor--decl-put' and `monitor--decl-get'."
  (monitor--test-build-get-put-tests 'monitor--decl-get 'monitor--decl-put)
  (let ((monitor-parent-symbol (make-symbol "monitor-parent-symbol"))
        (monitor-child-symbol (make-symbol "monitor-child-symbol")))
    (monitor--test-build-test-monitor monitor-parent-symbol nil :test-arg-a 'parent-a :test-arg-b 'parent-b)
    (monitor--test-build-test-monitor monitor-child-symbol monitor-parent-symbol :test-arg-b 'child-b)
    ; not defined in child, so fall back to parent
    (should (eq 'parent-a (monitor--decl-get monitor-child-symbol :test-arg-a)))
    ; defined in both parent and child, so use child
    (should (eq 'child-b (monitor--decl-get monitor-child-symbol :test-arg-b)))))

(ert-deftest monitor-test-make-plist ()
  "Tests for `monitor--make-plist'."
  ; all plists should be different
  (should-not (eq (monitor--make-plist) (monitor--make-plist))))

(ert-deftest monitor-test-monitor-enable-disable ()
  "Tests for `monitor--enable' and `monitor--disable'."
  (let ((monitor-symbol (make-symbol "monitor-symbol"))
        (counter-enabled 0)
        (counter-disabled 0))
    (monitor--test-build-test-monitor monitor-symbol nil
                                      :enable (lambda (monitor) (incf counter-enabled))
                                      :disable (lambda (monitor) (incf counter-disabled)))
    (should (= 0 counter-enabled))
    (should (= 0 counter-disabled))
    (should (eq t (monitor--disabled-p monitor-symbol)))
    (monitor--enable monitor-symbol)
    (should (= 1 counter-enabled))
    (should (eq t (monitor--enabled-p monitor-symbol)))
    (monitor--enable monitor-symbol)
    (should (= 1 counter-enabled))
    (monitor--disable monitor-symbol)
    (should (= 1 counter-disabled))
    (should (eq t (monitor--disabled-p monitor-symbol)))
    (monitor--disable monitor-symbol)
    (should (= 1 counter-disabled))
    (monitor--enable monitor-symbol)))

(ert-deftest monitor-test-monitor-instance-p ()
  "Tests for `monitor--instance-p'."
  (let ((monitor-symbol (make-symbol "monitor-symbol")))
    ; monitors aren't instances of themselves
    (monitor--test-build-test-monitor monitor-symbol)
    (should (eq nil (monitor--instance-p monitor-symbol)))
    (let ((instance-a (monitor--instance-create monitor-symbol)))
      ; explicit instance
      (should (eq t (monitor--instance-p instance-a))))))

(ert-deftest monitor-test-monitor-instance-equal ()
  "Tests for `monitor--instance-equal'."
  (let ((monitor-symbol (make-symbol "monitor-symbol"))
        (monitor-symbol-b (make-symbol "monitor-symbol-b")))
    (monitor--test-build-test-monitor monitor-symbol)
    (monitor--test-build-test-monitor monitor-symbol-b)
    (let ((instance-a (monitor--instance-create monitor-symbol :a 6 :b 7))
          (instance-b (monitor--instance-create monitor-symbol :a 6 :b 7))
          (instance-c (monitor--instance-create monitor-symbol :b 7 :a 6))
          (instance-d (monitor--instance-create monitor-symbol :a 5 :b 7))
          (instance-e (monitor--instance-create monitor-symbol :a 6 :c 7))
          (instance-f (monitor--instance-create monitor-symbol-b :a 6 :b 7)))
      ; same instance
      (should (eq t (monitor--instance-equal instance-a instance-a)))
      ; same form
      (should (eq t (monitor--instance-equal instance-a instance-b)))
      ; same key-values, different order
      (should (eq t (monitor--instance-equal instance-a instance-c)))
      ; different value for a key
      (should-not (eq t (monitor--instance-equal instance-a instance-d)))
      ; different keys
      (should-not (eq t (monitor--instance-equal instance-a instance-e)))
      ; different monitors
      (should-not (eq t (monitor--instance-equal instance-a instance-f))))))

(ert-deftest monitor-test-monitor-instance-get ()
  "Tests for `monitor--instance-get'."
  (let ((monitor-symbol (make-symbol "monitor-symbol")))
    (monitor--test-build-test-monitor monitor-symbol nil :test-arg-a 'ma :test-arg-b 'mb)
    (let ((instance (monitor--instance-create monitor-symbol :test-arg-a 'ia)))
      ; not declared in any
      (should (eq nil (monitor--instance-get instance :test-arg)))
      ; declared in instance
      (should (eq 'ia (monitor--instance-get instance :test-arg-a)))
      ; not in instance, fall back to monitor
      (should (eq 'mb (monitor--instance-get instance :test-arg-b))))))

(ert-deftest monitor-test-monitor-instance-get-arg ()
  "Tests for `monitor--instance-get-arg'."
  (let ((monitor-symbol (make-symbol "monitor-symbol")))
    (monitor--test-build-test-monitor monitor-symbol nil :test-arg-a 'ma :test-arg-b 'mb)
    (let ((instance (monitor--instance-create monitor-symbol :test-arg-a 'ia)))
      ; not declared in any
      (should (eq nil (monitor--instance-get-arg instance :test-arg)))
      ; declared in instance
      (should (eq 'ia (monitor--instance-get-arg instance :test-arg-a)))
      ; not in instance (in monitor), but don't fall back
      (should (eq nil (monitor--instance-get-arg instance :test-arg-b))))))

(provide 'monitor-tests)
;;; monitor-tests.el ends here
