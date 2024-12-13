(in-package :irl)

;;;;;;; some tests for evaluate-primitive
(deftest test-evaluate-primitive ()
         ;; give zero arguments
         (test-assert (null (evaluate-primitive (find-primitive 'pick-apples *apple-counting-inventory*)
                                                (make-bindings
                                                 '((?var-1) (?var-2)))
                                                *test-ontology*)))
         ;; give first
         (test-assert (= 1 (length (evaluate-primitive
                                    (find-primitive 'pick-apples *apple-counting-inventory*)
                                    (make-bindings
                                     `((?var-1)
                                       (?var-2 0.5 ,(make-instance 'quantity :n 12))))
                                    *test-ontology*))))
         ;; give second
         (test-assert (= 1 (length (evaluate-primitive
                                    (find-primitive 'pick-apples *apple-counting-inventory*)
                                    (make-bindings
                                     `((?var-1 0.5 ,(make-apples-set 20))
                                       (?var-2)))
                                    *test-ontology*))))
         ;; give both consistent
         (test-assert (= 1 (length (evaluate-primitive
                                    (find-primitive 'pick-apples *apple-counting-inventory*)
                                    (make-bindings
                                     `((?var-1 0.5 ,(make-apples-set 20))
                                       (?var-2 0.5 ,(make-instance 'quantity :n 20))))
                                    *test-ontology*))))
         ;; give both inconsistent
         (test-equal 'inconsistent (evaluate-primitive
                                    (find-primitive 'pick-apples *apple-counting-inventory*)
                                    (make-bindings
                                     `((?var-1 0.5 ,(make-apples-set 20))
                                       (?var-2 0.5 ,(make-instance 'quantity :n 12))))
                                    *test-ontology*))


         ;; cleanup
         (makunbound '*test-ontology*)
         (setf (find-class 'quantity) nil)
         (fmakunbound 'make-quantity)
         (fmakunbound 'add-quantities-to-ontology)
         (setf (find-class 'apples-set) nil)
         (fmakunbound 'make-apples-set)
         (makunbound '*apple-counting-inventory*))

;;(test-evaluate-primitive)