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

(flet ((make-test (name subject expected actually &key (test #'eql))
	 (assert (funcall test expected actually)
		 (subject expected actually)
		 "~A ~S~&~
                  expected: ~A~&~
                  actually: ~A"
		 name
		 subject
		 expected
		 actually)))
  (let ((subject (let ((a-stack (make-instance 'chains:stack)))
		   (chains:pushf a-stack :foo)
		   (chains:pushf a-stack :bar)
		   (chains:pushf a-stack :baz)
		   (chains:pushf a-stack :qux))))
    (make-test 'sizef subject 4 (chains:sizef subject))
    (make-test 'peekf subject :qux (chains:peekf subject))
    (make-test 'nextf
	       subject
	       '(:qux :baz :bar :foo)
	       (loop
		 for item = (chains:nextf subject)
		 while item
		 collect item)
	       :test #'equal))

  (let ((subject (chains:make-stack-from #(:foo :bar :baz))))
    (check-type subject chains:stack)
    (make-test 'sizef subject 3 (chains:sizef subject))
    (make-test 'peekf subject
		'(0 :foo)
		(chains:peekf subject)
		:test #'equal)
    (make-test 'nextf
	       subject
	       '((0 :foo) (1 :bar) (2 :baz))
	       (loop
		 for item = (chains:nextf subject)
		 while item
		 collect item)
	       :test #'equal))

  (let ((subject (chains:make-chain-from #(:foo :bar :baz) 'chains:stack)))
    (check-type subject chains:stack))

  (let ((subject (chains:make-stack-from
		  (let ((a-hash-table (make-hash-table)))
		    (setf (gethash :bar a-hash-table) :foo)
		    (setf (gethash :baz a-hash-table) :qux)
		    a-hash-table))))
    (check-type subject chains:stack)
    (make-test 'sizef subject 2 (chains:sizef subject))
    (make-test 'nextf
	       subject
	       '((:bar :foo) (:baz :qux))
	       (loop
		 for item = (chains:nextf subject)
		 while item
		 collect item into pairs
		 finally
		    (return (sort pairs #'string< :key #'first)))
	       :test #'equal))

  (let ((subject (let ((a-queue (make-instance 'chains:queue)))
		   (chains:pushf a-queue :foo)
		   (chains:pushf a-queue :bar)
		   (chains:pushf a-queue :baz)
		   (chains:pushf a-queue :qux))))
    (make-test 'sizef subject 4 (chains:sizef subject))
    (make-test 'peekf subject :foo (chains:peekf subject))
    (make-test 'nextf
	       subject
	       '(:foo :bar :baz :qux)
	       (loop
		 for item = (chains:nextf subject)
		 while item
		 collect item)
	       :test #'equal))

  (let ((subject (chains:make-queue-from #(:foo :bar :baz))))
    (check-type subject chains:queue)
    (make-test 'sizef subject 3 (chains:sizef subject))
    (make-test 'peekf subject
		'(0 :foo)
		(chains:peekf subject)
		:test #'equal)
    (make-test 'nextf
	       subject
	       '((0 :foo) (1 :bar) (2 :baz))
	       (loop
		 for item = (chains:nextf subject)
		 while item
		 collect item)
	       :test #'equal))

  (let ((subject (chains:make-chain-from #(:foo :bar :baz) 'chains:queue)))
    (check-type subject chains:queue))

  (let ((subject (let ((a-cycle (make-instance 'chains:cycle)))
		   (chains:pushf a-cycle :foo)
		   (chains:pushf a-cycle :bar))))
    (make-test 'sizef subject 2 (chains:sizef subject))
    (make-test 'peekf subject :foo (chains:peekf subject))
    (make-test 'nextf
	       subject
	       '(:foo :bar :foo :bar)
	       (loop
		 repeat 4
		 collect (chains:nextf subject))
	       :test #'equal))

  (let ((subject (chains:make-cycle-from #(:foo :bar))))
    (check-type subject chains:cycle)
    (make-test 'sizef subject 2 (chains:sizef subject))
    (make-test 'peekf
	       subject
	       '(0 :foo)
	       (chains:peekf subject)
	       :test #'equal)
    (make-test 'nextf
	       subject
	       '((0 :foo) (1 :bar) (0 :foo) (1 :bar))
	       (loop
		 repeat 4
		 collect (chains:nextf subject))
	       :test #'equal))

  (let ((subject (chains:make-chain-from #(:foo :bar :baz) 'chains:cycle)))
    (check-type subject chains:cycle)))


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
