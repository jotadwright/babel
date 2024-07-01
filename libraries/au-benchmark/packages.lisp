(in-package :cl-user)

(defpackage :au-benchmark.base
  (:use :common-lisp :cl-user))

(defpackage :au-benchmark.benchmark
  (:use :common-lisp :cl-user :au-benchmark.base)
  (:export :run-anti-unification-benchmark
           :run-anti-unification-benchmark-in-parallel
           :generate-benchmark))

(defpackage :au-benchmark
  (:use :common-lisp :cl-user
        :au-benchmark.base
        :au-benchmark.benchmark))
