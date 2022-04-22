;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.tools.chains-test
  (:use #:common-lisp)
  (:local-nicknames (#:chains #:org.wobh.common-lisp.tools.chains))
  (:documentation "ORG.WOBH.COMMON-LISP.TOOL.CHAINS-TEST

Basic tests for CHAINS. If this file loads without errors, tests
passed."))

(in-package #:org.wobh.common-lisp.tools.chains-test)

;;; Stack tests

(let* ((subject (let ((a-stack (make-instance 'chains:stack)))
		  (chains:pushf a-stack :foo)
		  (chains:pushf a-stack :bar)
		  (chains:pushf a-stack :baz)
		  (chains:pushf a-stack :qux)))
       (expected :qux)
       (actually (chains:peekf subject)))
  (assert (eql expected actually)
	  (subject expected actually)
	  "peekf stack ~A~&~
          expected: ~A~&~
          actually: ~A"
	  subject
	  expected
	  actually))

(let* ((subject (let ((a-stack (make-instance 'chains:stack)))
		  (chains:pushf a-stack :foo)
		  (chains:pushf a-stack :bar)
		  (chains:pushf a-stack :baz)
		  (chains:pushf a-stack :qux)))
       (expected 4)
       (actually (chains:sizef subject)))
  (assert (eql expected actually)
	  (subject expected actually)
	  "sizef stack ~A~&~
          expected: ~A~&~
          actually: ~A"
	  subject
	  expected
	  actually))

(let* ((subject (let ((a-stack (make-instance 'chains:stack)))
		  (chains:pushf a-stack :foo)
		  (chains:pushf a-stack :bar)
		  (chains:pushf a-stack :baz)
		  (chains:pushf a-stack :qux)))
       (expected '(:qux :baz :bar :foo))
       (actually (loop repeat 4 collect (chains:nextf subject))))
  (assert (equal expected actually)
	  (subject expected actually)
	  "nextf stack ~A~&~
          expected: ~A~&~
          actually: ~A"
	  subject
	  expected
	  actually))


;;; Queue tests

(let* ((subject (let ((a-queue (make-instance 'chains:queue)))
		  (chains:pushf a-queue :foo)
		  (chains:pushf a-queue :bar)
		  (chains:pushf a-queue :baz)
		  (chains:pushf a-queue :qux)))
       (expected 4)
       (actually (chains:sizef subject)))
  (assert (eql expected actually)
	  (subject expected actually)
	  "sizef queue ~A~&~
          expected: ~A~&~
          actually: ~A"
	  subject
	  expected
	  actually))

(let* ((subject (let ((a-queue (make-instance 'chains:queue)))
		  (chains:pushf a-queue :foo)
		  (chains:pushf a-queue :bar)
		  (chains:pushf a-queue :baz)
		  (chains:pushf a-queue :qux)))
       (expected :foo)
       (actually (chains:peekf subject)))
  (assert (eql expected actually)
	  (subject expected actually)
	  "peekf queue ~A~&~
          expected: ~A~&~
          actually: ~A"
	  subject
	  expected
	  actually))

(let* ((subject (let ((a-queue (make-instance 'chains:queue)))
		  (chains:pushf a-queue :foo)
		  (chains:pushf a-queue :bar)
		  (chains:pushf a-queue :baz)
		  (chains:pushf a-queue :qux)))
       (expected '(:foo :bar :baz :qux))
       (actually (loop repeat 4 collect (chains:nextf subject))))
  (assert (equal expected actually)
	  (subject expected actually)
	  "nextf queue ~A~&~
          expected: ~A~&~
          actually: ~A"
	  subject
	  expected
	  actually))


;;; Cycle tests

(let* ((subject (let ((a-cycle (make-instance 'chains:cycle)))
		  (chains:pushf a-cycle :foo)
		  (chains:pushf a-cycle :bar)))
       (expected :foo)
       (actually (chains:peekf subject)))
  (assert (eql expected actually)
	  (subject expected actually)
	  "peekf cycle ~A~&~
          expected: ~A~&~
          actually: ~A"
	  subject
	  expected
	  actually))

(let* ((subject (let ((a-cycle (make-instance 'chains:cycle)))
		  (chains:pushf a-cycle :foo)
		  (chains:pushf a-cycle :bar)))
       (expected 2)
       (actually (chains:sizef subject)))
  (assert (eql expected actually)
	  (subject expected actually)
	  "sizef cycle ~A~&~
          expected: ~A~&~
          actually: ~A"
	  subject
	  expected
	  actually))

(let* ((subject (let ((a-cycle (make-instance 'chains:cycle)))
		  (chains:pushf a-cycle :foo)
		  (chains:pushf a-cycle :bar)))
       (expected '(:foo :bar :foo :bar))
       (actually (loop repeat 4 collect (chains:nextf subject))))
  (assert (equal expected actually)
	  (subject expected actually)
	  "nextf cycle ~A~&~
          expected: ~A~&~
          actually: ~A"
	  subject
	  expected
	  actually))
