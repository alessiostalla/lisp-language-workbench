(defpackage lisp-language-workbench/tests/main
  (:use :cl
        :lisp-language-workbench
        :rove))
(in-package :lisp-language-workbench/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :lisp-language-workbench)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
