;;; monitor-tests.el
;;; Code:

(require 'ert)
(require 'monitor)

(defun monitor--test-build-test-monitor (name &optional parent &rest args)
  "Build a test monitor named NAME."
  (apply 'define-monitor name parent "Test monitor." args))

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

(defmacro monitor--test-with-uninterned-symbols (symbols &rest body)
  "Provide an uninterned version of each symbol in SYMBOLS for use in BODY."
  (declare (indent 1))
  `(let (,@(--map `(,it (make-symbol (symbol-name ',it))) symbols))
     ,@body))

(defun monitor--test-same-instances-p (lista listb)
  "T if LISTA has the same monitor instances as LISTB."
  (let ((-compare-fn 'monitor--instance-equal)) (-same-items-p lista listb)))

(ert-deftest monitor-test-monitorp ()
  "Tests for `monitorp'."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    ; initially, we don't expect it to be a monitor.
    (should (eq nil (monitorp monitor-symbol)))
    (monitor--test-build-test-monitor monitor-symbol)
    ; now we've created a monitor, it should recognize it as one
    (should (eq t (monitorp monitor-symbol)))
    (monitor--remove-monitor monitor-symbol)
    ; now it is no longer a monitor
    (should (eq nil (monitorp monitor-symbol)))))

(ert-deftest monitor-test-monitor-meta-get-put ()
  "Tests for `monitor--meta-put' and `monitor--meta-get'."
  (monitor--test-build-get-put-tests 'monitor--meta-get 'monitor--meta-put))

(ert-deftest monitor-test-monitor-decl-get-put ()
  "Tests for `monitor--decl-put' and `monitor--decl-get'."
  (monitor--test-build-get-put-tests 'monitor--decl-get 'monitor--decl-put)
  (monitor--test-with-uninterned-symbols (monitor-parent-symbol monitor-child-symbol)
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
  (monitor--test-with-uninterned-symbols (monitor-symbol monitor-child)
    (let ((counter-enabled 0)
          (counter-disabled 0))
      (monitor--test-build-test-monitor monitor-symbol nil
        :enable (lambda (monitor) (setq counter-enabled (1+ counter-enabled)))
        :disable (lambda (monitor) (setq counter-disabled (1+ counter-disabled))))
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
      (monitor--test-build-test-monitor monitor-child monitor-symbol :enable nil)
      (should (= 1 counter-enabled))
      (should (= 1 counter-disabled))
      (monitor--enable monitor-child)
      (should (= 1 counter-enabled))
      (monitor--disable monitor-child)
      (should (= 2 counter-disabled)))))

(ert-deftest monitor-test-monitor-instance-p ()
  "Tests for `monitor--instance-p'."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    ; monitors aren't instances of themselves
    (monitor--test-build-test-monitor monitor-symbol)
    (should (eq nil (monitor--instance-p monitor-symbol)))
    (let ((instance-a (monitor-instance-create monitor-symbol)))
      ; explicit instance
      (should (eq t (monitor--instance-p instance-a))))))

(ert-deftest monitor-test-monitor-instance-equal ()
  "Tests for `monitor--instance-equal'."
  (monitor--test-with-uninterned-symbols (monitor-symbol monitor-symbol-b)
    (monitor--test-build-test-monitor monitor-symbol)
    (monitor--test-build-test-monitor monitor-symbol-b)
    (let ((instance-a (monitor-instance-create monitor-symbol :a 6 :b 7))
          (instance-b (monitor-instance-create monitor-symbol :a 6 :b 7))
          (instance-c (monitor-instance-create monitor-symbol :b 7 :a 6))
          (instance-d (monitor-instance-create monitor-symbol :a 5 :b 7))
          (instance-e (monitor-instance-create monitor-symbol :a 6 :c 7))
          (instance-f (monitor-instance-create monitor-symbol-b :a 6 :b 7)))
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

(ert-deftest monitor-test-monitor-plist-equal-p ()
  "Tests for `monitor--monitor-plist-equal-p'."
  (monitor--test-with-uninterned-symbols (monitor-parent)
    (monitor--test-build-test-monitor monitor-parent)
    (let ((monitor-a (monitor--create-monitor-plist nil "foo" :a 7))
          (monitor-b (monitor--create-monitor-plist nil "foo" :a 7))
          (monitor-c (monitor--create-monitor-plist nil "bar" :a 7))
          (monitor-d (monitor--create-monitor-plist nil "foo" :a 8))
          (monitor-e (monitor--create-monitor-plist monitor-parent "foo" :a 7))
          (monitor-f (monitor--create-monitor-plist nil "foo" :a 7 :b 8))
          (monitor-g (monitor--create-monitor-plist nil "foo" :b 8 :a 7))
          (monitor-h (monitor--create-monitor-plist nil "foo" :b 7)))
    ; same monitor
    (should (eq t (monitor--monitor-plist-equal-p monitor-a monitor-a)))
    ; same form
    (should (eq t (monitor--monitor-plist-equal-p monitor-a monitor-b)))
    ; different parents
    (should (eq nil (monitor--monitor-plist-equal-p monitor-a monitor-e)))
    ; same key-values, different order
    (should (eq t (monitor--monitor-plist-equal-p monitor-f monitor-g)))
    ; different value for a key
    (should-not (eq t (monitor--monitor-plist-equal-p monitor-a monitor-d)))
    ; different keys
    (should-not (eq t (monitor--monitor-plist-equal-p monitor-a monitor-h)))
    ; different docs
    (should (eq nil (monitor--monitor-plist-equal-p monitor-a monitor-c))))))

(ert-deftest monitor-test-monitor-instance-get ()
  "Tests for `monitor--instance-get'."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    (monitor--test-build-test-monitor monitor-symbol nil :test-arg-a 'ma :test-arg-b 'mb)
    (let ((instance (monitor-instance-create monitor-symbol :test-arg-a 'ia)))
      ; not declared in any
      (should (eq nil (monitor--instance-get instance :test-arg)))
      ; declared in instance
      (should (eq 'ia (monitor--instance-get instance :test-arg-a)))
      ; not in instance, fall back to monitor
      (should (eq 'mb (monitor--instance-get instance :test-arg-b))))))

(ert-deftest monitor-test-monitor-instance-get-arg ()
  "Tests for `monitor--instance-get-arg'."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    (monitor--test-build-test-monitor monitor-symbol nil :test-arg-a 'ma :test-arg-b 'mb)
    (let ((instance (monitor-instance-create monitor-symbol :test-arg-a 'ia)))
      ; not declared in any
      (should (eq nil (monitor--instance-get-arg instance :test-arg)))
      ; declared in instance
      (should (eq 'ia (monitor--instance-get-arg instance :test-arg-a)))
      ; not in instance (in monitor), but don't fall back
      (should (eq nil (monitor--instance-get-arg instance :test-arg-b))))))

(ert-deftest monitor-test-instance-existing-p ()
  "Tests for `monitor--instance-existing-p'."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    (monitor--test-build-test-monitor monitor-symbol)
    (should-error (monitor--instance-existing-p nil) :type 'wrong-type-argument)
    (let ((instance (monitor-instance-create monitor-symbol)))
      (should (eq t (monitor--instance-existing-p instance)))
      (monitor--instance-destroy instance)
      (should (eq nil (monitor--instance-existing-p instance))))))

(ert-deftest monitor-test-instance-create ()
  "Tests for `monitor-instance-create'."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    (let (instances)
      (monitor--test-build-test-monitor monitor-symbol nil
        :create (lambda (instance)
                  (push instance instances)))
      (should (equal nil instances))
      (let ((instance (monitor-instance-create monitor-symbol)))
        (should (monitor--test-same-instances-p (list instance) instances))
        (monitor-instance-create monitor-symbol)
        ; already have an exact instance
        (should (monitor--test-same-instances-p (list instance) instances))
        (let ((instance-b (monitor-instance-create monitor-symbol :a 1)))
          (should (monitor--test-same-instances-p (list instance instance-b) instances)))))))

(ert-deftest monitor-test-instance-destroy ()
  "Tests for `monitor--instance-destroy'."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    (let (instances)
      (monitor--test-build-test-monitor monitor-symbol nil
        :create (lambda (instance)
                  (push instance instances))
        :destroy (lambda (instance)
                   (setq instances (--reject (monitor--instance-equal it instance) instances))))
      (should (equal nil instances))
      (let ((instance (monitor-instance-create monitor-symbol)))
        (should (monitor--test-same-instances-p (list instance) instances))
        (monitor--instance-destroy instance)
        (should (eq nil instances))))))

(ert-deftest monitor-test-define-monitor-basic ()
  "Basic tests for `define-monitor'."
  (monitor--test-with-uninterned-symbols (monitor-symbol monitor-symbol-a monitor-symbol-b)
    (should-error (define-monitor monitor-symbol-a monitor-symbol-b "") :type 'wrong-type-argument)
    (let ((count-disable 0))
      (define-monitor monitor-symbol nil "" :disable (lambda (monitor) (setq count-disable (1+ count-disable))))
      (monitor--enable monitor-symbol)
      (should (= 0 count-disable))
      (define-monitor monitor-symbol nil "" :disable (lambda (monitor) (setq count-disable (1+ count-disable))))
      ; 'same' monitor, so don't overwrite it
      (should (= 0 count-disable))
      (define-monitor monitor-symbol nil "" :enable nil :disable (lambda (monitor) (setq count-disable (1+ count-disable))))
      ; different monitors - so destroy the old one
      (should (= 1 count-disable)))))

(ert-deftest monitor-test-monitor-remove ()
  "Tests for `monitor--remove-monitor'."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    (let ((count-destroy 0)
          (count-disable 0))
      (monitor--test-build-test-monitor monitor-symbol nil
        :disable (lambda (monitor) (setq count-disable (1+ count-disable)))
        :destroy (lambda (instance) (setq count-destroy (1+ count-destroy))))
      (monitor--enable monitor-symbol)
      (monitor-instance-create monitor-symbol)
      (monitor-instance-create monitor-symbol :a 7)
      (monitor--remove-monitor monitor-symbol)
      ; it should destroy all instances
      (should (= 2 count-destroy))
      ; it should be 'disabled'
      (should (= 1 count-disable)))))

(ert-deftest monitor-test-monitor-instance-run ()
  "Tests for `monitor--instance-run'."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    (monitor--test-build-test-monitor monitor-symbol nil :test-fn-b (lambda () "bar"))
    (let ((instance-a (monitor-instance-create monitor-symbol :test-fn nil))
          (instance-b (monitor-instance-create monitor-symbol :test-fn '1+))
          (instance-c (monitor-instance-create monitor-symbol :test-fn (lambda () "foo"))))
      ; shouldn't error with no function
      (monitor--instance-run instance-a :test-fn)
      ; function with arguments
      (should (= 8 (monitor--instance-run instance-b :test-fn 7)))
      ; function with no arguments
      (should (equal "foo" (monitor--instance-run instance-c :test-fn)))
      ; don't fall back to monitor
      ; different interpretation for instance functions and monitor functions.
      (should (eq nil (monitor--instance-run instance-a :test-fn-b))))))

(ert-deftest monitor-test-base-monitor ()
  "Tests for the 'base monitor."
  (monitor--test-with-uninterned-symbols (monitor-symbol)
    ;; we should be able to make children
    (define-monitor monitor-symbol 'base "")
    ;; we don't need any arguments for an instance
    (monitor monitor-symbol)))

(provide 'monitor-tests)
;;; monitor-tests.el ends here
