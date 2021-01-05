;; (ql:quickload :utils)

(in-package :utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;








;;;;;;;;;;;;;;;
;; Actions   ;;
;;;;;;;;;;;;;;;

;; (get-kitchen ?kitchen-state)
;; (setf *A* (multiple-value-list (fetch-ingredient (make-var) (make-var) *kitchen* 'milk 500 'ml)))




;;(defparameter *irl-primitive* '(fetch-ingredient ?ingredient ?new-kitchen-state ?kitchen milk 500 'ml))


;; (funcall (fetch-ingredient ?ingredient ?new-kitchen-state ?kitchen milk 500 'ml))



;; Bind: lookup in ontology










#|
 (list (evaluate-bind-predicate (make-var 'milk-class) (class-of (make-instance 'milk)))
       (evaluate-bind-predicate (make-var 'amount) 500)
       (evaluate-bind-predicate (make-var 'unit-class) (class-of (make-instance 'ml)))


       
       


       )

 |#


(type-of (clos::class-designator (class-of (make-instance 'milk))))

(type-of (clos::class-prototype (class-of (make-instance 'milk))))
=> UTILS::MILK

(setf *A* (clos::class-prototype (class-of (make-instance 'milk))))

(type-of (class-string (make-instance 'milk)))

(evaluate-primitive `(fetch-ingredient ,(make-var 'ingredient)
                                       ,(make-var 'new-kitchen)
                                       ,*kitchen*
                                       ,(make-instance 'milk)
                                       500
                                       ,(make-instance 'ml)))


(defmacro def-irl-predicate (predicate)
  `(do-def-irl-predicate ',predicate))

(defun do-def-irl-predicate (predicate)
  (loop with predicate-name = (first predicate)
        for arg in (rest predicate)
        if (numberp arg)
        collect arg into args
        else if (string= (subseq (symbol-name arg) 0 1) "?")
        collect (make-var (subseq (symbol-name arg) 1)) into args
        else
        collect (class-of (make-instance arg)) into args
        finally return (cons predicate-name args)))

(def-irl-predicate  (bind ?milk-class milk))
(def-irl-predicate  (get-context ?kitchen))

(bind ?milk-class milk)

(bind ?milk-class milk)
(bind ?amount-class 500)
(bind ?unit-class ml)

(get-context ?kitchen)

(fetch-ingredient ?ingredient ?new-kitchen ?kitchen ?milk-class ?amount ?unit-class)










(bind ?milk-class milk)

<binding ?milk-class <milk-class>



   




