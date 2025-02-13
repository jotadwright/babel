(ql:quickload :concept-representations)
(in-package :concept-representations)

;; This script demonstrates how to use to
;;   create, compare, update and visualise
;;   multidimensional concepts.

;; setting up entities

(progn
  (progn
    (setf features (make-hash-table))
    (loop for feature-name in (list 'r 'g 'key 'a 'width 'c 'd 'e 'f 'g 'h 'i 'j)
          for feature-value in (list 1 1 'A 0 0 0 0 0 0 0 0 0 0 0 0)
          do (setf (gethash feature-name features) feature-value))
           
    (setf description (make-hash-table))
    (setf entity1 (create-entity features description)))

  (progn
    (setf features (make-hash-table))
    (loop for feature-name in (list 'r 'g 'key 'a 'width 'c 'd 'e 'f 'g 'h 'i 'j)
          for feature-value in (list 1 1 'C 0 0 0 0 0 0 0 0 0 0 0 0)
          do (setf (gethash feature-name features) feature-value))
           
    (setf description (make-hash-table))
    (setf entity2 (create-entity features description)))

  (progn
    (setf features (make-hash-table))
    (loop for feature-name in (list 'r 'g 'key 'a 'width 'c 'd 'e 'f 'g 'h 'i 'j)
          for feature-value in (list 1 0 'A 0 0 0 0 0 0 0 0 0 0 0 0)
          do (setf (gethash feature-name features) feature-value))
           
    (setf description (make-hash-table))
    (setf entity3 (create-entity features description))))

;; CREATE => instantiating concepts based on an entity
(setf concept1 (create-concept-representation entity1 :weighted-multivariate-distribution))
(setf concept2 (create-concept-representation entity2 :weighted-multivariate-distribution))

;; COMPARE => calculating the similarity between two concepts
(concept-similarity concept1 concept2)

;; COMPARE => calculating the similarity between a concept and an entity
(concept-entity-similarity concept1 entity1)

;; UPDATE => updating a concept towards an entity (that is embedded in a context of other entities)
(update-concept concept1 entity2 (list entity1 entity2 entity3))

;; VISUALISE => visualising a concept
(add-concept-to-interface concept1 :weight-threshold 0.1)


