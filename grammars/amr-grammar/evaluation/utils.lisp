(in-package :amr-grammar)


 ;;Functions for AMR-FCG package:
(defun equivalent-amr-predicate-networks (fcg-amr-network amr-predicates)
  (irl:equivalent-irl-programs? fcg-amr-network
                                (mapcar #'(lambda (predicate)
                                            (cons (first predicate)
                                                  (mapcar #'(lambda (symbol)
                                                              (cond ((stringp symbol)
                                                                     symbol)
                                                                    ((numberp symbol)
                                                                     symbol)
                                                                    ((or (equal symbol '-)
                                                                         (equal symbol '+))
                                                                     symbol)
                                                                    (t
                                                                     (utils::variablify symbol))
                                                                ))
                                                          (rest predicate))))
                                        amr-predicates)))


(defparameter *amr-corpus* (utils:babel-pathname :directory '("grammars" "amr-grammar" "data")
                                                 :name "amr-corpus"
                                                 :type "csv"))

(defun evaluate-amr-grammar (&key (filepath *amr-corpus*) (series 5) (cxn-inventory *fcg-constructions*))
  (let ((amr-sentences-meanings (with-open-file (inputstream filepath)
                                  (loop for line = (read-line inputstream nil nil)
                                        while line
                                        collect (list (first (split-sequence:split-sequence #\Tab line))
                                                      (read-from-string (second (split-sequence:split-sequence #\Tab line))))))))
    (format t "~%======== The following sentences were not always comprehended correctly ========~%~%")
    (loop for sentence-meaning in amr-sentences-meanings
          for sentence = (first sentence-meaning)
          for meaning = (second sentence-meaning)
          for parses = (loop for n from 1 upto series
                             collect (equivalent-amr-predicate-networks
                                      (comprehend sentence :cxn-inventory cxn-inventory :silent t)
                                      meaning))
          when (member nil parses)
          do (format t "Sentence: ~a~%Gold meaning:~a~%~%" sentence meaning))
    (format t "=================================================================================~%")))

; (evaluate-amr-grammar)

