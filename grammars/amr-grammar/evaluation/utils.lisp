(in-package :amr-grammar)


 ;;Functions for AMR-FCG package:
(defun equivalent-amr-predicate-networks (fcg-amr-network amr-predicates)
  (irl:equivalent-irl-programs? fcg-amr-network
                                (mapcar #'(lambda (predicate)
                                            (cons (first predicate)
                                                  (mapcar #'(lambda (symbol)
                                                              (if (stringp symbol)
                                                                symbol
                                                                (utils::variablify symbol)
                                                                ))
                                                          (rest predicate))))
                                        amr-predicates)))