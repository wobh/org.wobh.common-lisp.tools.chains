;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.tools.chains-test
  (:nicknames #:chains-test)
  (:use #:common-lisp)
  (:local-nicknames (#:chains #:org.wobh.common-lisp.tools.chains))
  (:export #:test-all
           #:test-sizef-stack
           #:test-peekf-stack
           #:test-nextf-stack
           #:test-sizef-queue
           #:test-peekf-queue
           #:test-nextf-queue
           #:test-sizef-cycle
           #:test-peekf-cycle
           #:test-nextf-cycle)
  (:documentation "ORG.WOBH.COMMON-LISP.TOOL.CHAINS-TEST

Basic tests for CHAINS. If this file loads without errors, tests
passed."))

(in-package #:org.wobh.common-lisp.tools.chains-test)

;;; Stack tests

(defun test-sizef-stack
    (&key
       (expects 4)
       (subject (make-instance 'chains:stack :items '("foo" "bar" "baz" "qux"))))
  (let ((results (chains:sizef subject)))
    (assert (eql expects results)
	    (subject expects results)
	    "sizef stack ~S~&~
             expects: ~A~&~
             results: ~A"
	    subject
	    expects
	    results)))

(defun test-peekf-stack
    (&key
       (expects "foo")
       (subject (make-instance 'chains:stack :items '("foo" "bar" "baz" "qux"))))
  (let ((results (chains:peekf subject)))
    (assert (string= expects results)
	    (subject expects results)
	    "peekf stack ~S~&~
             expects: ~A~&~
             results: ~A"
	    subject
	    expects
	    results)))

(defun test-nextf-stack
    (&key
       (expects '("foo" "bar" "baz" "qux"))
       (subject (make-instance 'chains:stack :items '("foo" "bar" "baz" "qux"))))
  (let ((results (loop repeat 4 collect (chains:nextf subject))))
    (assert (equal expects results)
	    (subject expects results)
	    "nextf stack ~S~&~
             expects: ~A~&~
             results: ~A"
	    subject
	    expects
	    results)))


;;; Queue tests

(defun test-sizef-queue
    (&key
       (expects 4)
       (subject (make-instance 'chains:queue :items '("foo" "bar" "baz" "qux"))))
  (let ((results (chains:sizef subject)))
    (assert (eql expects results)
	    (subject expects results)
	    "sizef queue ~S~&~
             expects: ~A~&~
             results: ~A"
	    subject
	    expects
	    results)))

(defun test-peekf-queue
    (&key
       (expects "foo")
       (subject (make-instance 'chains:queue :items '("foo" "bar" "baz" "qux"))))
  (let ((results (chains:peekf subject)))
    (assert (string= expects results)
	    (subject expects results)
	    "peekf queue ~S~&~
             expects: ~A~&~
             results: ~A"
	    subject
	    expects
	    results)))

(defun test-nextf-queue
    (&key
       (expects '("foo" "bar" "baz" "qux"))
       (subject (make-instance 'chains:queue :items '("foo" "bar" "baz" "qux"))))
  (let ((results (loop repeat 4 collect (chains:nextf subject))))
    (assert (equal expects results)
	    (subject expects results)
	    "nextf queue ~S~&~
             expects: ~A~&~
             results: ~A"
	    subject
	    expects
	    results)))


;;; Cycle tests

(defun test-sizef-cycle
    (&key
       (expects 2)
       (subject (make-instance 'chains:cycle :items '("foo" "bar"))))
  (let ((results (chains:sizef subject)))
    (assert (eql expects results)
	    (subject expects results)
	    "sizef cycle ~S~&~
             expects: ~A~&~
             results: ~A"
	    subject
	    expects
	    results)))

(defun test-peekf-cycle
    (&key
       (expects "foo")
       (subject (make-instance 'chains:cycle :items '("foo" "bar"))))
  (let ((results (chains:peekf subject)))
    (assert (string= expects results)
	    (subject expects results)
	    "peekf cycle ~S~&~
             expects: ~A~&~
             results: ~A"
	    subject
	    expects
	    results)))

(defun test-nextf-cycle
    (&key
       (expects '("foo" "bar" "foo" "bar"))
       (subject (make-instance 'chains:cycle :items '("foo" "bar"))))
  (let ((results (loop repeat 4 collect (chains:nextf subject))))
    (assert (equal expects results)
	    (subject expects results)
	    "nextf cycle ~S~&~
             expects: ~A~&~
             results: ~A"
	    subject
	    expects
	    results)))

(defun test-all ()
  (loop
    with tests = '(test-sizef-stack
                   test-peekf-stack
                   test-nextf-stack
                   test-sizef-queue
                   test-peekf-queue
                   test-nextf-queue
                   test-sizef-cycle
                   test-peekf-cycle
                   test-nextf-cycle)
    for test in tests
    do (funcall test)
    finally (return t)))
