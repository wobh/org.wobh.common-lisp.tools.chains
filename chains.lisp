;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.tools.chains
  (:use #:common-lisp)
  (:nicknames #:chains)
  (:export #:chain #:stack #:queue #:cycle)
  (:export #:sizef #:peekf)
  (:export #:pushf #:pullf #:nextf)
  (:documentation "ORG.WOBH.COMMON-LISP.TOOLS.CHAINS

Provides sequential structure types implemented with lists. I've tried
for the simplest formalization. Current classes:

- chain :: an abstract sequence class
- stack :: a first-in-first-out (FIFO) structure
- queue :: a last-in-first-out (LIFO) structure
- cycle :: a circular structure

Some helpful methods which have also been defined for lists:

- sizef
- peekf
- pushf
- pullf
- nextf"))

(in-package #:org.wobh.common-lisp.tools.chains)

;;; generic interface

(defgeneric sizef (container)
  (:documentation "sizef

Given a container, `sizef' returns the number of all elements in the
container.")
  (:method ((list list))
    (list-length list)))

(defgeneric peekf (container)
  (:documentation "peekf

Given a container, `peekf' returns the 'first' object in the container,
and the unmodified container.")
  (:method ((list list))
    (let ((out (car list)))
      (values out list))))

(defgeneric pullf (container)
  (:documentation "pullf

Given a container, `pullf' removes the 'first' object in the
container, returning the removed object and modified container.")
  (:method ((list list))
    (let ((out (pop list)))
      (values out list))))

(defgeneric nextf (container)
  (:documentation "nextf

Given a container, `nextf' may remove the `first' object in the
container, returning the object and container, which may be further
modified according to the properties of the container. See: `cycle'.")
  (:method ((list list))
    (pullf list)))

(defgeneric pushf (container object)
  (:documentation "pushf

Given a container and an object, `pushf' stores the object in the
container and returns the container.")
  (:method ((list list) object)
    (push object list)
    list))


;; Chain

(defclass chain () ())

(defmethod print-object ((chain chain) stream)
  (if *print-readably*
      (multiple-value-bind (make-form setup-form)
          (make-load-form chain)
        (format stream "~W~@[~&~W~]" make-form setup-form))
      (if *print-escape*
	  (print-unreadable-object (chain stream :type t :identity t)
	    (format stream
                    (if *print-pretty*
                        "~<~@{~S ~S~^ ~@_~}~:>"
                        "~{~S ~S~^ ~}")
                    (cddr (make-load-form chain))))
	  (format stream
                  "~(~A~)"
                  (class-name (class-of chain))))))


;;; Stack

(defclass stack (chain)
  ((outgoing :initform ())
   (o-length :initform 0)))

(defmethod initialize-instance :around
    ((stack stack) &key items &allow-other-keys)
  (call-next-method)
  (with-slots (outgoing o-length) stack
    (setf outgoing items)
    (incf o-length (length outgoing)))
  stack)

(defmethod make-load-form
    ((stack stack) &optional environment)
  (declare (ignore environment))
  (with-slots (outgoing) stack
    `(make-instance ',(class-name (class-of stack))
		    :items ,outgoing)))

(defmethod sizef ((stack stack))
  (with-slots (o-length) stack
    o-length))

(defmethod peekf ((stack stack))
  (with-slots (outgoing) stack
    (let ((out (car outgoing)))
      (values out stack))))

(defmethod pullf ((stack stack))
  (with-slots (outgoing o-length) stack
    (let ((out (pop outgoing)))
      (decf o-length)
      (values out stack))))

(defmethod nextf ((stack stack))
  (pullf stack))

(defmethod pushf ((stack stack) object)
  (with-slots (outgoing o-length) stack
    (push object outgoing)
    (incf o-length)
    stack))


;;; Queue

(defclass queue (stack)
  ((incoming :initform ())
   (i-length :initform 0)))

(defmethod make-load-form
    ((queue queue) &optional environment)
  (declare (ignore environment))
  (with-slots (outgoing incoming) queue
    `(make-instance ',(class-name (class-of queue))
		    :items ,(append outgoing
				    (reverse incoming)))))

(defmethod sizef ((queue queue))
  (with-slots (o-length i-length) queue
    (+ o-length i-length)))

(defmethod peekf ((queue queue))
  (with-slots (outgoing o-length incoming i-length) queue
    (let ((out (car outgoing)))
      (values out queue))))

(flet ((queue-turn (queue)
	 (with-slots (outgoing o-length incoming i-length) queue
	   (when (and (<= o-length i-length)
		      (< 0 i-length))
	     (setq outgoing
		   (nconc outgoing
			  (nreverse (shiftf incoming ()))))
	     (incf o-length
		   (shiftf i-length 0))))))
  (defmethod pullf ((queue queue))
    (with-slots (outgoing o-length) queue
      (let ((out (pop outgoing)))
	(decf o-length)
	(queue-turn queue)
	(values out queue))))

  (defmethod pushf ((queue queue) object)
    (with-slots (incoming i-length) queue
      (push object incoming)
      (incf i-length))
    (queue-turn queue)
    queue))


;;; Cycle

(defclass cycle (queue) ())

(defmethod nextf ((cycle cycle))
  (let ((out (pullf cycle)))
    (pushf cycle out)
    (values out cycle)))
