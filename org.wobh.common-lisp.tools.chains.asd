;; -*- mode: lisp -*-

(defsystem "org.wobh.common-lisp.tools.chains"
  :description "Chains: sequential access types: stack, queue, cycle"
  :version "0.0.1"
  :license "Copyright © 2018-2021 William Clifford All rights reserved."
  :author "William Clifford <will@wobh.org"
  :in-order-to ((test-op (test-op "org.wobh.common-lisp.tools.chains/test")))
  :components ((:file "chains")))

(defsystem "org.wobh.common-lisp.tools.chains/test"
  :description "Chains testing"
  :depends-on ("org.wobh.common-lisp.tools.chains")
  :perform (test-op (o c) (symbol-call 'chains-test
                                       'test-all))
  :components ((:file "chains-test")))
