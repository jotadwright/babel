(in-package :fcg)

;;;;;;;;;;;;;
;;         ;;
;; Repairs ;;
;;         ;;
;;;;;;;;;;;;;



(defclass learn-cxns (repair)
  ((trigger :initform 'routine-processing-finished)))


(defmethod repair ((repair learn-cxns)
                   (problem gold-standard-not-in-search-space)
                   (cip construction-inventory-processor)
                   &key &allow-other-keys)
  ""

  (let ((speech-act (get-data (blackboard (construction-inventory cip)) :speech-act)))

    (cond ((= (node-counter cip) 0) ;;no construction could apply => learn a holophrase
           (let ((holophrastic-cxn (learn-holophrastic-cxn (fresh-variables (form speech-act))
                                                           (fresh-variables (meaning speech-act))
                                                           :cxn-inventory (original-cxn-set (construction-inventory cip)))))
             (make-instance 'cxn-fix
                            :repair repair
                            :problem problem
                            :restart-data holophrastic-cxn)))
          (t 
           nil))))


(defmethod handle-fix ((fix cxn-fix) (repair learn-cxns) (problem problem) (cip construction-inventory-processor) &key &allow-other-keys)
;;  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (call-next-method)
 ; (with-disabled-monitor-notifications

    (set-data fix 'fixed-cars
              (fcg-apply (get-processing-cxn (restart-data fix))
                         (car-source-cfs (cipn-car (top-node cip)))
                         (direction cip))))

  
#|  (let ((uw (first (get-data problem 'strings)))) 
    (multiple-value-bind (cxn-set lex-cxn)
        (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append uw "-cxn")))
                            ((?word-unit
                              (args (?ref))
                              (syn-cat (lex-class ?lex-class))
                              (sem-cat (sem-class ?sem-class)))
                             <-
                             (?word-unit
                              (HASH meaning ((,(intern (upcase uw)) ?ref)))
                              --
                              (HASH form ((string ?word-unit ,uw)))))
                            :cxn-inventory ,(copy-object (original-cxn-set (construction-inventory node)))
                            :cxn-set lex))
      (declare (ignore cxn-set))
      (make-instance 'cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data lex-cxn)))) |#