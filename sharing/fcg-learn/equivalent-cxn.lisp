(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                            ;;
;; Checking equivalency between constructions ;;
;;                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric equivalent-cxn (cxn-1 cxn-2)
  (:documentation "Returns t if cxn-1 and cxn-2 are considered equivalent."))

(defmethod equivalent-cxn ((cxn-1 t) (cxn-2 t))
  "If cxns are of a different class, they are not equivalent."
  ;; If cxn-1 and cxn-2 are of the same type, this method should probably not have been called.
  (when (eql (type-of cxn-1) (type-of cxn-2))
    (warn "equivalent-cxn: cxn-1 and cxn-2 are of the same type but no dedicated method exists."))
  ;; return nil
  nil)

(defmethod equivalent-cxn ((cxn-1 holophrastic-cxn) (cxn-2 holophrastic-cxn))
  "Holophrastic cxns should have the same form and the same meaning upto variable renamings."
  (and
   ;; same form
   (string= (attr-val cxn-1 :form-hash-key) (attr-val cxn-2 :form-hash-key))
   ;; for efficiency
   (eql (attr-val cxn-1 :meaning-hash-key) (attr-val cxn-2 :meaning-hash-key))
   ;; same meaning
   (pn::equivalent-predicate-networks-p (attr-val cxn-1 :meaning) (attr-val cxn-2 :meaning))))

(defmethod equivalent-cxn ((cxn-1 linking-cxn) (cxn-2 linking-cxn))
  "Linking cxns are equivalent if they have the same contributing-slot-args and conditional-slot-args on both slots."
  (and ;; for efficiency, check that length of all args features is the same.
       (= (length (attr-val cxn-1 :meaning-args)) (length (attr-val cxn-2 :meaning-args)))
       (= (length (attr-val cxn-1 :form-args)) (length (attr-val cxn-2 :form-args)))
       (= (length (attr-val cxn-1 :meaning-args-slot-1)) (length (attr-val cxn-2 :meaning-args-slot-1)))
       (= (length (attr-val cxn-1 :form-args-slot-1)) (length (attr-val cxn-2 :form-args-slot-1)))
       (= (length (attr-val cxn-1 :meaning-args-slot-2)) (length (attr-val cxn-2 :meaning-args-slot-2)))
       (= (length (attr-val cxn-1 :form-args-slot-2)) (length (attr-val cxn-2 :form-args-slot-2)))

       ;; check whether all contributing args are bound in the same way in both slots for meaning
       (loop for arg-1 in (attr-val cxn-1 :meaning-args)
             for arg-2 in (attr-val cxn-2 :meaning-args)
             always (and (eql (position arg-1 (attr-val cxn-1 :meaning-args-slot-1))
                              (position arg-2 (attr-val cxn-2 :meaning-args-slot-1)))
                         (eql (position arg-1 (attr-val cxn-1 :meaning-args-slot-2))
                              (position arg-2 (attr-val cxn-2 :meaning-args-slot-2)))))

       ;; check whether all contributing args are bound in the same way in both slots for form
       (loop for arg-1 in (attr-val cxn-1 :form-args)
             for arg-2 in (attr-val cxn-2 :form-args)
             always (and (eql (position arg-1 (attr-val cxn-1 :form-args-slot-1))
                              (position arg-2 (attr-val cxn-2 :form-args-slot-1)))
                         (eql (position arg-1 (attr-val cxn-1 :form-args-slot-2))
                              (position arg-2 (attr-val cxn-2 :form-args-slot-2)))))))
                           
(defmethod equivalent-cxn ((cxn-1 filler-cxn) (cxn-2 filler-cxn))
  (let ((meaning-cxn-1 (attr-val cxn-1 :meaning))
        (meaning-cxn-2 (attr-val cxn-2 :meaning))
        (form-cxn-1 (attr-val cxn-1 :form))
        (form-cxn-2 (attr-val cxn-2 :form))
        (meaning-args-cxn-1 (attr-val cxn-1 :meaning-args))
        (meaning-args-cxn-2 (attr-val cxn-2 :meaning-args))
        (form-args-cxn-1 (attr-val cxn-1 :form-args))
        (form-args-cxn-2 (attr-val cxn-2 :form-args)))
        
    (and (= (length meaning-cxn-1 )(length meaning-cxn-2))
         (= (length form-cxn-1 )(length form-cxn-2))
         (= (length meaning-args-cxn-1) (length meaning-args-cxn-2))
         (= (length form-args-cxn-1 )(length form-args-cxn-2))
         
         (let ((bindings-meaning (pn::equivalent-predicate-networks meaning-cxn-1 meaning-cxn-2)))
           (when bindings-meaning (if (and meaning-args-cxn-1 meaning-args-cxn-2)
                                    (loop
                                       for arg-1 in meaning-args-cxn-1
                                       for arg-2 in meaning-args-cxn-2
                                       always (eql (cdr (assoc arg-1 bindings-meaning)) arg-2))
                                    t)))
         (let ((bindings-form (pn::equivalent-predicate-networks form-cxn-1 form-cxn-2)))
           (when bindings-form (if (and form-args-cxn-1 form-args-cxn-2)
                                 (loop
                                    for arg-1 in form-args-cxn-1
                                    for arg-2 in form-args-cxn-2
                                    always (eql (cdr (assoc arg-1 bindings-form)) arg-2))
                                 t))))))

(defmethod find-cxn ((cxn holophrastic-cxn) (hashed-fcg-construction-set hashed-fcg-construction-set)
                     &key (key #'identity) (test #'eql))
  "More efficient find-cxn method for holophrastic-cxns."
  (find (funcall key cxn)
        (gethash (attr-val cxn :form-hash-key) (constructions-hash-table hashed-fcg-construction-set))
        :test test :key key))
