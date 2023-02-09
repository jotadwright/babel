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
                          (mode (eql :argm-phrase))
                          &key phrase &allow-other-keys)
  "Creates a unique construction name for an argument structure
construction that links a predicate to a modifier through the
constituency path that links them."

  (declare (ignore mode))

  (assert (= (length cxn-units-with-role) 2)) ;;v-unit + single argm-unit with string
  (assert phrase)
  
  (loop for (role . unit) in ts-units-with-role
        for cxn-unit = (find (variablify (unit-name unit)) cxn-units-with-role :key #'unit-name)
        if (string= (role-type role) "V")
          collect (format nil "V(~a)" (format nil "~{~a~}" (feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name)))) into roles
        else
          collect (format nil "~a(~a)" (role-type role) phrase) into roles
        finally (return (intern (symbol-name (make-id (upcase (format nil "~{~a~^+~}+~a-cxn" roles (length cxn-units-without-role)))))))))
