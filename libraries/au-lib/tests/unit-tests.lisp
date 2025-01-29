(in-package :au-benchmark-tests)

(def-suite au-benchmark-tests)

(def-suite utils-suite :in au-benchmark-tests)
(in-suite utils-suite)

;; TO DO: add unit tests for utils/

(def-suite msg-utils-suite :in au-benchmark-tests)
(in-suite msg-utils-suite)

;; TO DO: add unit tests for algorithms/msg/msg-utils.lisp
;(test gen-test
;  (is-true t))