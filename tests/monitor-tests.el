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

(provide 'monitor-tests)
;;; monitor-tests.el ends here
