(in-package :cl-user)

(defpackage :au-benchmark.lcg
  (:use :common-lisp :cl-user :au-benchmark.base))

(defpackage :au-benchmark.lcg.baseline
  (:use :common-lisp :cl-user :au-benchmark.base :au-benchmark.lcg)
  (:export :anti-unify-predicate-networks))

(defpackage :au-benchmark.lcg.exhaustive
  (:use :common-lisp :cl-user :au-benchmark.base :au-benchmark.lcg)
  (:export :anti-unify-predicate-networks))

(defpackage :au-benchmark.lcg.kswap
  (:use :common-lisp :cl-user :au-benchmark.base :au-benchmark.lcg)
  (:export :anti-unify-predicate-networks))