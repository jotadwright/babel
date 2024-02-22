
(in-package :moral-foundations)

(load-mf-dictionary 'english)

(setf *raise-errors* t)

(deftest test-dictionary ()
  (test-assert *moral-foundations*)
  (test-assert *mf-vocabulary-list*)
  (loop for word in '("threatens" "victim" "equal" "reparation")
        do (test-assert (token-in-dictionary-p word))))
;; (test-dictionary)
