;; -*- mode: lisp -*-

(defpackage #:org.wobh.common-lisp.tools.chains
  (:use #:common-lisp)
  (:nicknames #:chains)
  (:export #:chain #:stack #:queue #:cycle)
  (:export #:sizef #:peekf)
  (:export #:pushf #:pullf #:nextf)
  (:export #:make-stack-from
	   #:make-queue-from
	   #:make-cycle-from
	   #:make-chain-from)
  (:documentation "ORG.WOBH.COMMON-LISP.TOOLS.CHAINS

Provides sequential structure types implemented with lists. I've tried
for the simplest formalization. Current classes:

- chain :: an abstract sequence class
- stack :: a first-in-first-out (FIFO) structure
- queue :: a last-in-first-out (LIFO) structure
- cycle :: a circular structure

Some factory methods have been defined for creating chain objects from
other collection types:

- make-chain-from : collection chain-type -> chain:chain-type
- make-stack-from : collection -> stack
- make-queue-from : collection -> queue
- make-cycle-from : collection -> cycle

These take a simple-vector or hash and turn it into one of the chain
types whose items is a pair of values cooresponding to the index/key
and value.

Some other helpful methods for 'succession' objects which have also
been defined for lists and streams:

- peekf : succession -> item, succession
- pullf : succession -> item, succession
- nextf : succession -> item, succession
- pushf : succession object -> succession
"))

(in-package #:org.wobh.common-lisp.tools.chains)

;;; generic interface

(defgeneric sizef (container)
  (:documentation "sizef

Given a container, `sizef' returns the number of all elements in the
container.")
  (:method ((container list))
    (list-length container))
  (:method ((container sequence))
    (length container))
  (:method ((container hash-table))
    (hash-table-count container)))

(defgeneric peekf (succession)
  (:documentation "peekf

Given a succession, `peekf' returns the 'first' object in the succession,
and the unmodified succession.")
  (:method ((succession list))
    (let ((out (car succession)))
      (values out succession)))
  (:method ((succession stream))
    (let ((out (peek-char nil stream nil)))
      (values out succession))))

(defgeneric pullf (succession)
  (:documentation "pullf

Given a succession, `pullf' removes the 'first' object in the
succession, returning the removed object and modified succession.")
  (:method ((succession list))
    (let ((out (pop succession)))
      (values out succession)))
  (:method ((succession stream))
    (let ((out (read-char-no-hang stream nil)))
      (values out succession))))

(defgeneric nextf (succession)
  (:documentation "nextf

Given a succession, `nextf' may remove the `first' object in the
succession, returning the object and succession, which may be further
modified according to the properties of the succession. See: `cycle'.")
  (:method ((succession list))
    (pullf succession))
  (:method ((succession stream))
    (pullf succession)))

(defgeneric pushf (succession object)
  (:documentation "pushf

Given a succession and an object, `pushf' stores the object in the
succession and returns the succession.")
  (:method ((succession list) object)
    (push object succession)
    succession)
  (:method ((succession stream) (object character))
    (write-char object stream)
    succession))


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

(defgeneric make-chain-from (collection chain-type)
  (:method ((collection t) (chain-type (eql 'stack)))
    (make-stack-from collection))
  (:method ((collection t) (chain-type (eql 'queue)))
    (make-queue-from collection))
  (:method ((collection t) (chain-type (eql 'cycle)))
    (make-cycle-from collection)))


;;; Stack

(defclass stack (chain)
  ((outgoing :initform ())
   (o-length :initform 0)))

(defmethod initialize-instance :around
    ((stack stack) &key items &allow-other-keys)
  (call-next-method)
  (with-slots (outgoing o-length) stack
    (setf outgoing (copy-list items))
    (incf o-length (length outgoing)))
  stack)

(defmethod make-load-form
    ((stack stack) &optional environment)
  (declare (ignore environment))
  (with-slots (outgoing) stack
    `(make-instance ',(class-name (class-of stack))
                    :items ,outgoing)))

(defmethod sizef ((container stack))
  (with-slots (o-length) container
    o-length))

(defmethod peekf ((succession stack))
  (with-slots (outgoing) succession
    (let ((out (car outgoing)))
      (values out succession))))

(defmethod pullf ((succession stack))
  (with-slots (outgoing o-length) succession
    (let ((out (pop outgoing)))
      (decf o-length)
      (values out succession))))

(defmethod nextf ((succession stack))
  (pullf succession))

(defmethod pushf ((succession stack) object)
  (with-slots (outgoing o-length) succession
    (push object outgoing)
    (incf o-length)
    succession))

(defgeneric make-stack-from (collection)
  (:method ((collection simple-vector))
    (loop
      for index = 0 then (incf index)
      for item across collection
      collect (list index item) into a-list
      finally
         (return (make-instance 'stack
                                :items a-list))))
  (:method ((collection hash-table))
    (loop
      for index being the hash-key
        using (hash-value item) of collection
      collect (list index item) into a-list
      finally
         (return (make-instance 'stack
                                :items a-list)))))


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

(defmethod sizef ((container queue))
  (with-slots (o-length i-length) container
    (+ o-length i-length)))

(defmethod peekf ((succession queue))
  (with-slots (outgoing o-length incoming i-length) succession
    (let ((out (car outgoing)))
      (values out succession))))

(flet ((queue-turn (queue)
         (with-slots (outgoing o-length incoming i-length) queue
           (when (and (<= o-length i-length)
                      (< 0 i-length))
             (setq outgoing
                   (nconc outgoing
                          (nreverse (shiftf incoming ()))))
             (incf o-length
                   (shiftf i-length 0))))))
  (defmethod pullf ((succession queue))
    (with-slots (outgoing o-length) succession
      (let ((out (pop outgoing)))
        (decf o-length)
        (queue-turn succession)
        (values out succession))))

  (defmethod pushf ((succession queue) object)
    (with-slots (incoming i-length) succession
      (push object incoming)
      (incf i-length))
    (queue-turn succession)
    succession))

(defgeneric make-queue-from (collection)
  (:method ((collection simple-vector))
    (loop
      for index = 0 then (incf index)
      for item across collection
      collect (list index item) into a-list
      finally
         (return (make-instance 'queue
                                :items a-list))))
  (:method ((collection hash-table))
    (loop
      for index being the hash-key
        using (hash-value item) of collection
      collect (list index item) into a-list
      finally
         (return (make-instance 'queue
                                :items a-list)))))


;;; Cycle

(defclass cycle (queue) ())

(defmethod nextf ((succession cycle))
  (let ((out (pullf succession)))
    (pushf succession out)
    (values out succession)))

(defgeneric make-cycle-from (collection)
  (:method ((collection simple-vector))
    (loop
      for index = 0 then (incf index)
      for item across collection
      collect (list index item) into a-list
      finally
         (return (make-instance 'cycle
                                :items a-list))))
  (:method ((collection hash-table))
    (loop
      for index being the hash-key
        using (hash-value item) of collection
      collect (list index item) into a-list
      finally
         (return (make-instance 'cycle
                                :items a-list)))))
