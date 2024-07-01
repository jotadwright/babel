(in-package :cl-user)

(defpackage :au-benchmark.msg
  (:use :common-lisp :cl-user :au-benchmark.base))

(defpackage :au-benchmark.msg.baseline
  (:use :common-lisp :cl-user :au-benchmark.base :au-benchmark.msg)
  (:export :anti-unify-predicate-networks))

(defpackage :au-benchmark.msg.exhaustive
  (:use :common-lisp :cl-user :au-benchmark.base :au-benchmark.msg)
  (:export :anti-unify-predicate-networks))

(defpackage :au-benchmark.msg.kswap-heuristic
  (:use :common-lisp :cl-user :au-benchmark.base :au-benchmark.msg)
  (:export :anti-unify-predicate-networks))

(defpackage :au-benchmark.msg.1swap-omega
  (:use :common-lisp :cl-user :au-benchmark.base :au-benchmark.msg)
  (:export :anti-unify-predicate-networks))

(defpackage :au-benchmark.msg.kswap-omega
  (:use :common-lisp :cl-user :au-benchmark.base :au-benchmark.msg)
  (:export :anti-unify-predicate-networks))

