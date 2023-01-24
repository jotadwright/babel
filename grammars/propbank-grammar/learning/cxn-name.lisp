(in-package :propbank-grammar)


(defgeneric make-cxn-name (ts-units-with-role 
                           cxn-units-with-role
                           cxn-units-without-role mode &key &allow-other-keys)
  (:documentation "Creates a unique construction name for an argument structure construction"))


(defmethod make-cxn-name ((ts-units-with-role list)
                          (cxn-units-with-role list)
                          (cxn-units-without-role list)
                          (mode (eql :argm-leaf))
                          &key lemma &allow-other-keys)
  "Creates a unique construction name for an argument structure
construction that links a predicate to a modifier through the
constituency path that links them."

  (declare (ignore mode))
  
  (loop for (role . unit) in ts-units-with-role
        for cxn-unit = (find (variablify (unit-name unit)) cxn-units-with-role :key #'unit-name)
        collect (format nil "~a(~a)"
                        (role-type role)
                        (if (equal (feature-value (find 'lemma (cddr cxn-unit) :key #'feature-name)) lemma)
                          lemma
                          (format nil "~{~a~}" (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name))) ))
        into roles
        finally (return (intern (symbol-name (make-id (upcase (format nil "~{~a~^+~}+~a-cxn" roles (length cxn-units-without-role)))))))))

(defmethod make-cxn-name ((ts-units-with-role list)
                          (cxn-units-with-role list)
                          (cxn-units-without-role list)
                          (mode (eql :argm-pp))
                          &key pp-units lemma s-bar-units &allow-other-keys)
  "Creates a unique construction name for an argument structure
construction that connects a predicate to a modifier phrase of type PP."

  (declare (ignore mode))

  (loop with pp-unit-number = 0
        for (role . unit) in ts-units-with-role
        for cxn-unit = (find (variablify (unit-name unit)) cxn-units-with-role :key #'unit-name)
        collect (format nil "~a:~a"
                        (role-type role)
                        (cond
                         ;; unit is a pp
                         ((find 'pp (unit-feature-value (unit-body unit) 'syn-class))
                          (incf pp-unit-number)
                          (if (= 1 (length (nth1 pp-unit-number pp-units)))
                            (format nil "~{~a~}(~a)" (unit-feature-value unit 'syn-class )
                                    (or lemma
                                        (second (find 'lemma
                                                      (nthcdr 2 (first (nth1 pp-unit-number pp-units)))
                                                      :key #'feature-name))))
                            (format nil "~{~a~}(cc-~a)" (unit-feature-value unit 'syn-class )
                                    (or lemma
                                        (second (find 'lemma
                                                      (nthcdr 2 (third (nth1 pp-unit-number pp-units)))
                                                      :key #'feature-name))))))
                        
                         ;; unit contains a phrase-type
                         ((feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name))
                          (format nil "~{~a~}" (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name))))))
        into roles
        finally (return (intern (symbol-name (make-id (upcase (format nil "~{~a~^+~}+~a-cxn" roles (length cxn-units-without-role)))))))))
