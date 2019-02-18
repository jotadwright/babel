(in-package :amr-grammar)


 ;;Functions for AMR-FCG package:
(defun equivalent-amr-predicate-networks (fcg-amr-network amr-penman)
  (irl:equivalent-irl-programs? fcg-amr-network
                                (mapcar #'(lambda (predicate)
                                            (cons (first predicate)
                                                  (mapcar #'(lambda (symbol)
                                                              (variablify symbol))
                                                          (rest predicate))))
                                        (amr:penman->predicates amr-penman))))