;;; monitor-tests.el
;;; Code:

(require 'ert)
(require 'monitor)

(defun monitor--test-build-test-monitor (name)
  "Build a test monitor named NAME."
  (monitor--define-monitor name 'default "Test monitor."))

(defmacro monitor--test-build-get-put-tests (getter putter)
  "Build standard tests for a getter GETTER and putter PUTTER."
  (let* ((monitor-symbol (intern "monitor-symbol"))
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

(provide 'monitor-tests)
;;; monitor-tests.el ends here
