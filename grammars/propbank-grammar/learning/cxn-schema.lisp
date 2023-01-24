(in-package :propbank-grammar)


(defgeneric make-cxn-schema (core-units-with-role cxn-units-with-role mode &key &allow-other-keys)
  (:documentation "Makes an abstract cxn schema"))



(defmethod make-cxn-schema (core-units-with-role cxn-units-with-role 
                                                 (mode (eql :core-roles))
                                                 &key passive?
                                                 &allow-other-keys)
  (declare (ignore mode))
  
  (loop for (role . unit) in core-units-with-role
        for cxn-unit in cxn-units-with-role
        collect (cons (intern (role-type role))
                      (cond
                       ;; unit contains a phrase-type
                       ((and passive?
                             (find 'v (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name))))
                        (intern (format nil "~{~a~}(pass)" (unit-feature-value unit 'syn-class))))
                       ((feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name)))))))


(defmethod make-cxn-schema (units-with-role cxn-units-with-role 
                                                 (mode (eql :argm-leaf))
                                                 &key lemma
                                                 &allow-other-keys)

  (declare (ignore mode))
  
  (loop for (role . nil) in units-with-role
      for cxn-unit in cxn-units-with-role
                       collect (cons (intern (role-type role))
                                     (if (equal (feature-value (find 'lemma (cddr cxn-unit) :key #'feature-name)) lemma)
                                       lemma
                                       (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name))))))



(defmethod make-cxn-schema (core-units-with-role cxn-units-with-role
                                                 (mode (eql :argm-pp))
                                                 &key cxn-preposition-units passive? cxn-s-bar-units)
  (loop with pp-unit-number = 0
        with s-bar-unit-number = 0
        for (role . unit) in core-units-with-role
        for cxn-unit in cxn-units-with-role
        collect (cons (intern (role-type role))
                     
                      (cond
                       ;; unit is a pp
                       ((find 'pp (unit-feature-value (unit-body unit) 'syn-class))
                        (incf pp-unit-number)
                        (if (= 1 (length (nth1 pp-unit-number cxn-preposition-units)))
                          (intern (format nil "~{~a~}(~a)" (unit-feature-value unit 'syn-class)
                                          (second (find 'lemma
                                                        (nthcdr 2 (first (nth1 pp-unit-number cxn-preposition-units)))
                                                        :key #'feature-name))))
                          (intern (format nil "~{~a~}(cc-~a)" (unit-feature-value unit 'syn-class)
                                          (second (find 'lemma
                                                        (nthcdr 2 (third (nth1 pp-unit-number cxn-preposition-units)))
                                                        :key #'feature-name))))))
                       ;; unit is an s-bar
                       ((or (find 'sbar (unit-feature-value (unit-body unit) 'syn-class))
                            (find 's (unit-feature-value (unit-body unit) 'syn-class)))
                        (incf s-bar-unit-number)
                        (intern (format nil "~{~a~}(~a)" (unit-feature-value unit 'syn-class)
                                        (or (second (find 'lemma
                                                          (nthcdr 2 (nth1 s-bar-unit-number cxn-s-bar-units))
                                                          :key #'feature-name))
                                            (second (find 'string
                                                          (nthcdr 2 (nth1 s-bar-unit-number cxn-s-bar-units))
                                                          :key #'feature-name))))))
                       ;; Unit contains a lemma
                       ((feature-value (find 'lemma (cddr cxn-unit) :key #'feature-name)))
                       ;; unit contains a phrase-type
                       ((and passive?
                             (find 'v (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name))))
                        (intern (format nil "~{~a~}(pass)" (unit-feature-value unit 'syn-class))))
                       ((feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name)))))))