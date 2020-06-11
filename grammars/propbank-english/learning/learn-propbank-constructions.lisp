(in-package :propbank-english)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; Learning constructions based on Propbank annotated corpora.  ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn-propbank-grammar (list-of-propbank-sentences &key
                                                          (selected-rolesets nil) 
                                                          (cxn-inventory '*propbank-learned-cxn-inventory*)
                                                          (fcg-configuration nil))
  "Learns a Propbank Grammar."
  (let ((cxn-inventory (eval `(def-fcg-constructions propbank-learned-english
                                :fcg-configurations ,fcg-configuration
                                :visualization-configurations ((:show-constructional-dependencies . nil))
                                :hierarchy-features (constituents dependents)
                                :feature-types ((constituents sequence)
                                                (dependents sequence)
                                                (span sequence)
                                                (phrase-type set)
                                                (word-order set-of-predicates)
                                                (meaning set-of-predicates)
                                                (footprints set))
                                :cxn-inventory ,cxn-inventory
                                :hashed t))))
    (loop for sentence in list-of-propbank-sentences
          for sentence-number from 1
          for syntactic-analysis = (syntactic-analysis sentence)
          for rolesets = (if selected-rolesets
                           (intersection selected-rolesets (all-rolesets sentence) :test #'equalp)
                           (all-rolesets sentence))
          do
          (format t "~%---> Sentence ~a." sentence-number)
          (loop for roleset in rolesets
                if (spacy-benepar-compatible-annotation sentence roleset :syntactic-analysis syntactic-analysis)
                do
                (loop for mode in (get-configuration cxn-inventory :learning-modes)
                      do
                      (learn-cxn-from-propbank-annotation sentence roleset cxn-inventory mode)))
          finally
          return cxn-inventory)))

                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learning a single cxn ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric learn-cxn-from-propbank-annotation (propbank-sentence roleset cxn-inventory mode))

(defmethod learn-cxn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :multi-argument-core-roles)))
  "Learns a construction capturing all core roles."
  ;; Looping over all frame instances for roleset annotated in sentence
  (loop with gold-frames = (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)
        with ts-unit-structure = (ts-unit-structure propbank-sentence cxn-inventory)
        for gold-frame in gold-frames
        for core-units-with-role = (remove-if #'(lambda (unit-with-role)
                                                  (search "ARGM" (role-type (car unit-with-role))))
                                              (units-with-role ts-unit-structure gold-frame))
        for footprint = (make-id "footprint")
        for v-lemma = (v-lemma core-units-with-role)
        for pp-units-with-role = (remove-if-not #'(lambda (unit-w-role)
                                                    (find 'pp (unit-feature-value (cdr unit-w-role) 'phrase-type)))
                                                core-units-with-role)
        for contributing-unit = (make-propbank-contributing-unit core-units-with-role gold-frame footprint :include-frame-name t)
        for cxn-units-with-role = (loop for unit in core-units-with-role
                                        collect
                                        (make-propbank-conditional-unit-with-role unit footprint :include-v-lemma t :include-fe-lemma nil))
        for cxn-units-without-role  = (make-propbank-conditional-units-without-role core-units-with-role
                                                                                    cxn-units-with-role ts-unit-structure)
        for cxn-preposition-units = (loop for pp-unit in pp-units-with-role
                                          collect (make-preposition-unit pp-unit ts-unit-structure))
        for cxn-name = (make-cxn-name roleset core-units-with-role cxn-units-without-role cxn-units-without-role cxn-preposition-units)
        do
        ;; assertions
        (assert (and cxn-units-with-role v-lemma))
        ;;;
        (when (and cxn-units-with-role v-lemma)
          ;;create a new construction and add it to the cxn-inventory
          (eval `(def-fcg-cxn ,cxn-name
                              (,contributing-unit
                               <-
                               ,@cxn-units-with-role
                               ,@cxn-units-without-role
                               ,@(loop for unit in cxn-preposition-units
                                       append unit))
                              :disable-automatic-footprints t
                              :attributes (:lemma ,v-lemma
                                           :score ,(length cxn-units-with-role)
                                           :label multi-argument-core-roles)
                              :cxn-inventory ,cxn-inventory)))
        finally
        return cxn-inventory))

(defun make-cxn-name (roleset ts-units-with-role cxn-units-with-role cxn-units-without-role preposition-units)
  (loop for (role . unit) in ts-units-with-role
        for cxn-unit = (find (variablify (unit-name unit)) cxn-units-with-role :key #'unit-name)
        for pp-unit-number = 0  
        collect (format nil "~a:~a"
                        (role-type role)
                        (cond
                         ;; unit is a pp
                         ((find 'pp (unit-feature-value (unit-body unit) 'phrase-type))
                          (incf pp-unit-number)
                          (format nil "~{~a~}(~a)" (unit-feature-value 'phrase-type unit)
                                  (second (find 'lemma
                                                (if (= 1 (length preposition-units))
                                                  (nthcdr 2 (first (nth1 pp-unit-number preposition-units)))
                                                  (format nil "cc-~a" (nthcdr 2 (third (nth1 pp-unit-number preposition-units)))))
                                                :key #'feature-name))))
                         ;; unit contains a lemma
                         ((feature-value (find 'lemma (cddr cxn-unit) :key #'feature-name)))
                         ;; unit contains a phrase-type
                         ((feature-value (find 'phrase-type (cddr cxn-unit) :key #'feature-name))
                          (format nil "~{~a~}" (feature-value (find 'phrase-type (cddr cxn-unit) :key #'feature-name))))
                          ;; unit contains a lex-class
                          ((feature-value (find 'lex-class (cddr cxn-unit) :key #'feature-name)))))
        into roles
        finally return (make-id (upcase (format nil "~a-~{~a~^+~}+~a-cxn" roleset roles (length cxn-units-without-role))))))



  

#|

(defmethod learn-cxn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :multi-argument-with-lemma)))
  "Adds a new construction to the cxn-inventory based on a propbank
sentence object and a roleset (e.g. 'believe.01')"
  (let ((frames (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)))
      (dolist (frame frames cxn-inventory)
        (let* ((unit-structure (left-pole-structure (de-render (sentence-string propbank-sentence)
                                                               (get-configuration cxn-inventory :de-render-mode)
                                                               :syntactic-analysis syntactic-analysis)))
                (units-with-role (loop for role in (frame-roles frame) ;;find all units that correspond to annotated frame elements
                                       for role-start = (first (indices role))
                                       for role-end = (+ (last-elt (indices role)) 1)
                                       for unit = (find-unit-by-span unit-structure (list role-start role-end))
                                       when unit
                                       collect (cons role unit)))
                (pp-units-with-role (loop for unit-w-role in units-with-role
                                          when (find 'pp (unit-feature-value (cdr unit-w-role) 'phrase-type))
                                          collect unit-w-role))
                (preposition-units (when pp-units-with-role
                                     (loop for pp-unit in pp-units-with-role
                                           collect (make-preposition-unit pp-unit unit-structure))))
                (cxn-name-list (loop for (role . unit) in units-with-role
                                     for i = -1
                                     collect (format nil "~a:~a" (role-type role) ;;create a name based on role-types and lex-class/phrase-type
                                                     (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                                       (format nil "~a" (cadr (find 'lemma (unit-body unit) :key #'feature-name)))
                                                       (if (find 'pp (unit-feature-value (cdr unit) 'phrase-type))
                                                         (progn (incf i)
                                                           (format nil "~{~a~}(~a)" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name))
                                                                 (cadr (find 'lemma (nthcdr 2 (nth i preposition-units)) :key #'feature-name))))
                                                         (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name))))))))
                (lemma (loop for (role . unit) in units-with-role
                             when (string= "V" (role-type role))
                             return (feature-value (find 'lemma (unit-body unit) :key #'feature-name))))
                (footprint-name (format nil "~a-~{~a~^+~}-cxn" roleset cxn-name-list))
                (contributing-unit (make-propbank-contributing-unit units-with-role frame footprint-name))
                (cxn-units-with-role
                 (loop for unit in units-with-role collect (make-propbank-conditional-unit-with-role-with-lemma unit footprint-name)))
                (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role unit-structure))
                (cxn-name (format nil "~a-~{~a~^+~}+~a-cxn" roleset cxn-name-list (length cxn-units-without-role))))

           (when (and cxn-units-with-role lemma)
             ;;create a new construction and add it to the cxn-inventory
             (eval `(def-fcg-cxn ,(make-id (upcase cxn-name))
                                 (,contributing-unit
                                  <-
                                  ,@cxn-units-with-role
                                  ,@cxn-units-without-role
                                  ,@preposition-units)
                                 :disable-automatic-footprints t
                                 :attributes (:lemma ,lemma :score ,(length cxn-units-with-role) :label multi-argument-with-lemma)
                                 :cxn-inventory ,cxn-inventory)))))))

(defmethod learn-cxn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :multi-argument-without-lemma)))
  "Adds a new construction to the cxn-inventory based on a propbank
sentence object and a roleset (e.g. 'believe.01')"
  (let ((frames (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)))
      (dolist (frame frames cxn-inventory)
        (let* ((unit-structure (left-pole-structure (de-render (sentence-string propbank-sentence)
                                                               (get-configuration cxn-inventory :de-render-mode)
                                                               :syntactic-analysis syntactic-analysis)))
                (units-with-role (loop for role in (frame-roles frame) ;;find all units that correspond to annotated frame elements
                                       for role-start = (first (indices role))
                                       for role-end = (+ (last-elt (indices role)) 1)
                                       for unit = (find-unit-by-span unit-structure (list role-start role-end))
                                       when unit
                                       collect (cons role unit)))
                (cxn-name-list (loop for (role . unit) in units-with-role
                                     collect (format nil "~a:~a" (role-type role) ;;create a name based on role-types and lex-class/phrase-type
                                                     (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                                       (if (equalp (role-type role) "V")
                                                         (format nil "~a" (cadr (find 'lemma (unit-body unit) :key #'feature-name)))
                                                         (format nil "~a" (cadr (find 'lex-class (unit-body unit) :key #'feature-name))))
                                                       (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))))
                (lemma (loop for (role . unit) in units-with-role
                             when (string= "V" (role-type role))
                             return (feature-value (find 'lemma (unit-body unit) :key #'feature-name))))
                (footprint-name (format nil "~a-~{~a~^+~}-cxn" roleset cxn-name-list))
                (contributing-unit (make-propbank-contributing-unit units-with-role frame footprint-name))
                (cxn-units-with-role (loop for unit in units-with-role collect (make-propbank-conditional-unit-with-role unit footprint-name)))
                (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role unit-structure))
                (cxn-name (format nil "~a-~{~a~^+~}+~a-cxn" roleset cxn-name-list (length cxn-units-without-role))))

           (when (and cxn-units-with-role lemma)
             ;;create a new construction and add it to the cxn-inventory
             (eval `(def-fcg-cxn ,(make-id (upcase cxn-name))
                                 (,contributing-unit
                                  <-
                                  ,@cxn-units-with-role
                                  ,@cxn-units-without-role
                                  )
                                 :disable-automatic-footprints t
                                 :attributes (:lemma ,lemma :score ,(length cxn-units-with-role) :label multi-argument-without-lemma)
                                 :cxn-inventory ,cxn-inventory)))))))


(defmethod learn-cxn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :multi-argument-core-only)))
  "Adds a new construction to the cxn-inventory based on a propbank
sentence object and a roleset (e.g. 'believe.01')"
  (let ((frames (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)))
      (dolist (frame frames cxn-inventory)
        (let* ((unit-structure (left-pole-structure (de-render (sentence-string propbank-sentence)
                                                               (get-configuration cxn-inventory :de-render-mode)
                                                               :syntactic-analysis syntactic-analysis)))
                (units-with-role (loop for role in (frame-roles frame) ;;find all units that correspond to annotated frame elements
                                       for role-start = (first (indices role))
                                       for role-end = (+ (last-elt (indices role)) 1)
                                       for unit = (find-unit-by-span unit-structure (list role-start role-end))
                                       when (and unit (not (search "ARGM" (role-type role)))) ;;do not learn non-core roles
                                       collect (cons role unit)))
                (pp-units-with-role (loop for unit-w-role in units-with-role
                                          when (find 'pp (unit-feature-value (cdr unit-w-role) 'phrase-type))
                                          collect unit-w-role))
                (preposition-units (loop for pp-unit in pp-units-with-role
                                           collect (make-preposition-unit pp-unit unit-structure)))
                (cxn-name-list (loop for (role . unit) in units-with-role
                                     for i = 0
                                     collect (format nil "~a:~a" (role-type role) ;;create a name based on role-types and lex-class/phrase-type
                                                     (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                                       ; leaf:
                                                       (if (equalp (role-type role) "V")
                                                         ;; V:
                                                         (format nil "~a" (cadr (find 'lemma (unit-body unit) :key #'feature-name)))
                                                         ;; not V:
                                                         (format nil "~a" (cadr (find 'lex-class (unit-body unit) :key #'feature-name))))
                                                       ;; not a leaf
                                                       (if (find 'pp (unit-feature-value (cdr unit) 'phrase-type))
                                                         ;pp
                                                         (progn (incf i)
                                                           (format nil "~{~a~}(~a)" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name))
                                                                 (cadr (find 'lemma
                                                                             (if (= 1 (length preposition-units))
                                                                               (nthcdr 2 (first (nth1 i preposition-units)))
                                                                               (format nil "cc-~a" (nthcdr 2 (third (nth1 i preposition-units)))))
                                                                             :key #'feature-name))))
                                                         ;; geen pp
                                                         (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name))))))))
                (lemma (loop for (role . unit) in units-with-role
                             when (string= "V" (role-type role))
                             return (feature-value (find 'lemma (unit-body unit) :key #'feature-name))))
                (footprint-name (format nil "~a-~{~a~^+~}-cxn" roleset cxn-name-list))
                (contributing-unit (make-propbank-contributing-unit units-with-role frame footprint-name))
                (cxn-units-with-role (loop for unit in units-with-role collect (make-propbank-conditional-unit-with-role unit footprint-name)))
                (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role unit-structure))
                (cxn-name (format nil "~a-~{~a~^+~}+~a-cxn" roleset cxn-name-list (length cxn-units-without-role))))

           (when (and cxn-units-with-role lemma)
             ;;create a new construction and add it to the cxn-inventory
             (eval `(def-fcg-cxn ,(make-id (upcase cxn-name))
                                 (,contributing-unit
                                  <-
                                  ,@cxn-units-with-role
                                  ,@cxn-units-without-role
                                  ,@(loop for unit in preposition-units
                                          append unit))
                                 :disable-automatic-footprints t
                                 :attributes (:lemma ,lemma :score ,(length cxn-units-with-role) :label multi-argument-core-only)
                                 :cxn-inventory ,cxn-inventory)))))))




(defmethod learn-cxn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :argm-with-lemma-with-v-lex-class)))
  "Adds a new construction to the cxn-inventory based on a propbank
sentence object and a roleset (e.g. 'believe.01')"
  (let ((frames (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)))
    (dolist (frame frames cxn-inventory)
      (let* ((unit-structure (left-pole-structure (de-render (sentence-string propbank-sentence)
                                                             (get-configuration cxn-inventory :de-render-mode)
                                                             :syntactic-analysis syntactic-analysis)))
             (units-with-role (loop for role in (frame-roles frame) ;;find all units that correspond to annotated frame elements
                                    for role-start = (first (indices role))
                                    for role-end = (+ (last-elt (indices role)) 1)
                                    for unit = (find-unit-by-span unit-structure (list role-start role-end))
                                    when (and unit
                                              (search "ARGM" (role-type role)) ;;role is modifier
                                              (find '(node-type leaf) (unit-body unit) :test #'equal)) ;;and unit is a leaf
                                    collect (cons role unit)))
             (v-unit (loop for role in (frame-roles frame) ;;find all units that correspond to annotated frame elements
                           for role-start = (first (indices role))
                           for role-end = (+ (last-elt (indices role)) 1)
                           for unit = (find-unit-by-span unit-structure (list role-start role-end))
                           when (and unit
                                     (equalp "V" (role-type role)))
                           return (cons role unit))))
        (loop for (role . unit) in units-with-role
              for lemma = (feature-value (find 'lemma (unit-body unit) :key #'feature-name))
              for footprint-name = (format nil "~a-~a+~a-cxn" roleset (format nil "~a:~a" (role-type role) 
                                                                              (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                                                                (format nil "~a" (cadr (find 'lemma (unit-body unit) :key #'feature-name)))
                                                                                (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))
                                           (format nil "V:~a" (cadr (find 'lemma (unit-body v-unit) :key #'feature-name))))
              for cxn-units-with-role = (append (list (make-propbank-conditional-unit-with-role-without-v-lemma (cons role unit) footprint-name))
                                                (unless (equalp (cons role unit) v-unit)
                                                  (list (make-propbank-conditional-unit-with-role-without-v-lemma v-unit footprint-name))))
              for cxn-units-without-role = (make-propbank-conditional-units-without-role (if (equalp v-unit (cons role unit))
                                                                                           (list (cons role unit))
                                                                                           (list (cons role unit) v-unit))
                                                                                         cxn-units-with-role unit-structure)
              for cxn-name = (format nil "~a-~a+~a+~a-cxn" "all-frames"
                                     (format nil "~a:~a" (role-type role) 
                                             (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                               (format nil "~a" (cadr (find 'lemma (unit-body unit) :key #'feature-name)))
                                               (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))
                                     (format nil "V:~a"
                                             (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                               (format nil "~a" (cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                               (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))
                                     (length cxn-units-without-role))
              for contributing-unit = (make-propbank-contributing-unit-without-frame-name (if (equalp v-unit (cons role unit))
                                                                        (list (cons role unit))
                                                                        (list (cons role unit) v-unit)) frame footprint-name)
              do
              (when (and cxn-units-with-role lemma)
                (eval `(def-fcg-cxn ,(make-id (upcase cxn-name))
                                    (,contributing-unit
                                     <-
                                     ,@cxn-units-with-role
                                     ,@cxn-units-without-role)
                                    :disable-automatic-footprints t
                                    :attributes (:lemma ,lemma :score ,(length cxn-units-with-role) :label argm-with-lemma)
                                    :cxn-inventory ,cxn-inventory))))))))


(defmethod learn-cxn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :argm-pp-with-v-lemma)))
  "Adds a new construction to the cxn-inventory based on a propbank
sentence object and a roleset (e.g. 'believe.01')"
  (let ((frames (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)))
    (dolist (frame frames cxn-inventory)
      (let* ((unit-structure (left-pole-structure (de-render (sentence-string propbank-sentence)
                                                             (get-configuration cxn-inventory :de-render-mode)
                                                             :syntactic-analysis syntactic-analysis)))
             (units-with-role (loop for role in (frame-roles frame) ;;find all units that correspond to annotated frame elements
                                    for role-start = (first (indices role))
                                    for role-end = (+ (last-elt (indices role)) 1)
                                    for unit = (find-unit-by-span unit-structure (list role-start role-end))
                                    when (and unit
                                              (search "ARGM" (role-type role)) ;;role is modifier
                                              (find '(phrase-type (pp)) (unit-body unit) :test #'equal)) ;;and unit is a pp
                                    collect (cons role unit)))
             (v-unit (loop for role in (frame-roles frame) ;;find all units that correspond to annotated frame elements
                           for role-start = (first (indices role))
                           for role-end = (+ (last-elt (indices role)) 1)
                           for unit = (find-unit-by-span unit-structure (list role-start role-end))
                           when (and unit
                                     (equalp "V" (role-type role)))
                           return (cons role unit)))
             (lemma (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name))))
        
        (loop for (role . unit) in units-with-role
              for footprint-name = (format nil "~a-~a+~a-cxn" roleset (format nil "~a:~a" (role-type role) 
                                                                              (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                                                                (format nil "~a" (cadr (find 'lemma (unit-body unit) :key #'feature-name)))
                                                                                (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))
                                           (format nil "V:~a" (cadr (find 'lemma (unit-body v-unit) :key #'feature-name))))
              for cxn-units-with-role = (append (list (make-propbank-conditional-unit-with-role-with-lemma (cons role unit) footprint-name))
                                                (unless (equalp (cons role unit) v-unit)
                                                  (list (make-propbank-conditional-unit-with-role-with-lemma v-unit footprint-name))))
              for cxn-units-without-role = (make-propbank-conditional-units-without-role (if (equalp v-unit (cons role unit))
                                                                                           (list (cons role unit))
                                                                                           (list (cons role unit) v-unit))
                                                                                         cxn-units-with-role unit-structure)
              for cxn-preposition-units = (make-preposition-unit (cons role unit) unit-structure)
              for cxn-name = (format nil "~a-~a+~a+~a-cxn" roleset
                                     (format nil "~a:~a(~a)" (role-type role)
                                               (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))
                                               (cadr (find 'lemma (if (= 1 (length cxn-preposition-units))
                                                                    (nthcdr 2 (first cxn-preposition-units))
                                                                    (format nil "cc-~a" (nthcdr 2 (third cxn-preposition-units)))) :key #'feature-name)))
                                     (format nil "V:~a"
                                             (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                               (format nil "~a" (cadr (find 'lemma (unit-body v-unit) :key #'feature-name)))
                                               (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body v-unit) :key #'feature-name)))))
                                     (length cxn-units-without-role))
              for contributing-unit = (make-propbank-contributing-unit (if (equalp v-unit (cons role unit))
                                                                        (list (cons role unit))
                                                                        (list (cons role unit) v-unit)) frame footprint-name)
              do
              (when (and cxn-units-with-role lemma cxn-preposition-units)
                (eval `(def-fcg-cxn ,(make-id (upcase cxn-name))
                                    (,contributing-unit
                                     <-
                                     ,@cxn-units-with-role
                                     ,@cxn-units-without-role
                                     ,@cxn-preposition-units)
                                    :disable-automatic-footprints t
                                    :attributes (:lemma ,lemma :score ,(length cxn-units-with-role) :label argm-pp-with-lemma)
                                    :cxn-inventory ,cxn-inventory))))))))


(defmethod learn-cxn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :single-argument-without-lemma)))
  "Adds a new construction to the cxn-inventory based on a propbank
sentence object and a roleset (e.g. 'believe.01')"
  (let ((frames (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)))
    (dolist (frame frames cxn-inventory)
      (let* ((unit-structure (left-pole-structure (de-render (sentence-string propbank-sentence)
                                                             (get-configuration cxn-inventory :de-render-mode)
                                                             :syntactic-analysis syntactic-analysis)))
             (units-with-role (loop for role in (frame-roles frame) ;;find all units that correspond to annotated frame elements
                                    for role-start = (first (indices role))
                                    for role-end = (+ (last-elt (indices role)) 1)
                                    for unit = (find-unit-by-span unit-structure (list role-start role-end))
                                    when unit
                                    collect (cons role unit)))
             (v-unit (find "V" units-with-role
                           :key #'(lambda (unit-with-role)
                                    (role-type (car unit-with-role)))
                           :test #'equalp))
             (lemma (loop for (role . unit) in units-with-role
                             when (string= "V" (role-type role))
                             return (feature-value (find 'lemma (unit-body unit) :key #'feature-name)))))
        (loop for (role . unit) in units-with-role
              for footprint-name = (format nil "~a-~a+~a-cxn" roleset (format nil "~a:~a" (role-type role) 
                                                                              (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                                                                (format nil "~a" (cadr (find 'lemma (unit-body unit) :key #'feature-name)))
                                                                                (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))
                                           (format nil "V:~a" (cadr (find 'lemma (unit-body v-unit) :key #'feature-name))))
              for cxn-units-with-role = (append (list (make-propbank-conditional-unit-with-role (cons role unit) footprint-name))
                                                (unless (equalp (cons role unit) v-unit)
                                                  (list (make-propbank-conditional-unit-with-role v-unit footprint-name))))
              for cxn-units-without-role = (make-propbank-conditional-units-without-role (if (equalp v-unit (cons role unit))
                                                                                           (list (cons role unit))
                                                                                           (list (cons role unit) v-unit))
                                                                                         cxn-units-with-role unit-structure)
              for cxn-name = (format nil "~a-~a+~a+~a-cxn" roleset (format nil "~a:~a" (role-type role) 
                                                                           (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                                                             (format nil "~a" (cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                                                             (format nil "~{~a~}" (cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))
                                     (format nil "V:~a" (cadr (find 'lemma (unit-body v-unit) :key #'feature-name)))
                                     (length cxn-units-without-role))
              for contributing-unit = (make-propbank-contributing-unit (if (equalp v-unit (cons role unit))
                                                                        (list (cons role unit))
                                                                        (list (cons role unit) v-unit)) frame footprint-name)
              do
              (when (and cxn-units-with-role lemma)
                (eval `(def-fcg-cxn ,(make-id (upcase cxn-name))
                                    (,contributing-unit
                                     <-
                                     ,@cxn-units-with-role
                                     ,@cxn-units-without-role)
                                    :disable-automatic-footprints t
                                    :attributes (:lemma ,lemma :score ,(length cxn-units-with-role) :label single-argument-without-lemma)
                                    :cxn-inventory ,cxn-inventory))))))))
|#

  
(defun find-unit-by-span (transient-structure span)
  "Return a unit with span span"
  (loop for unit in transient-structure
        for unit-span = (cadr (find 'span (unit-body unit) :key #'first))
        when (equal unit-span span)
        return unit))

(defun make-propbank-contributing-unit (units-with-role gold-frame footprint &key (include-frame-name t))
  "Make a contributing unit based on a gold-frame and units-with-role."
  (let* ((v-unit (cdr (assoc "V" units-with-role :key #'role-type :test #'equalp)))
         (v-unit-name (variablify (unit-name v-unit)))
         (args (loop for r in (frame-roles gold-frame)
                     if (string= (role-type r) "V")
                     collect `(referent ,v-unit-name)
                     else
                     if (find (role-type r) units-with-role :key #'(lambda (unit-with-role)
                                                                     (role-type (car unit-with-role))) :test #'equalp)

                     collect `(,(make-kw (role-type r))
                               ,(variablify (unit-name (cdr (assoc r units-with-role)))))))
         (meaning (loop for r in (frame-roles gold-frame)
                        for frame-name = (if include-frame-name
                                           (intern (upcase (frame-name gold-frame)))
                                           '?frame)
                        if (string= (role-type r) "V")
                        collect `(frame ,frame-name ,v-unit-name)
                        else
                        if (find (role-type r) units-with-role :key #'(lambda (unit-with-role)
                                                                        (role-type (car unit-with-role))) :test #'equalp)
                        collect `(frame-element ,(intern (upcase (role-type r))) ,v-unit-name
                                                ,(variablify (unit-name (cdr (assoc r units-with-role))))))))
    `(,v-unit-name
      (args ,@args)
      (frame-evoking +)
      (meaning ,meaning)
      (footprints (,footprint)))))

(defun make-propbank-conditional-unit-with-role (unit-with-role footprint &key (include-v-lemma t) (include-fe-lemma nil))
  "Makes a conditional unit for a propbank cxn based on a unit in the
initial transient structure that plays a role in the frame."
  (let* ((unit (cdr unit-with-role))
         (unit-name (variablify (unit-name unit)))
         (parent (when (cadr (find 'parent (unit-body unit) :key #'feature-name))
                   (variablify (cadr (find 'parent (unit-body unit) :key #'feature-name)))))
         (phrase-type-or-lex-class (if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                     `(lex-class ,(cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                     `(phrase-type ,(cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))
         (v-unit? (string= (role-type (car unit-with-role)) "V"))
         (leaf? (find '(node-type leaf) (unit-body unit) :test #'equal)))
      ;;a FEE unit also has the features lemma and footprints
      `(,unit-name
        --
        (parent ,parent)
        ,phrase-type-or-lex-class
        ,@(when v-unit? `((footprints (NOT ,footprint))))
        ,@(when (and v-unit? include-v-lemma leaf?)
            `((lemma ,(cadr (find 'lemma (unit-body unit) :key #'feature-name)))))
        ,@(when (and (not v-unit?) include-fe-lemma leaf?)
            `((lemma ,(cadr (find 'lemma (unit-body unit) :key #'feature-name))))))))


(defun make-preposition-unit (unit-with-role unit-structure)
  (let* ((pp-unit (cdr unit-with-role))
         (preposition-unit-in-ts (loop for unit in unit-structure
                                       when (and (or (find (list 'lex-class 'in) (unit-body unit) :test #'equalp)
                                                     (find (list 'dependency-label 'prep) (unit-body unit) :test #'equalp))
                                                 (equal (cadr (find 'parent (unit-body unit) :key #'feature-name)) (unit-name pp-unit)))
                                       return unit)))

    (cond (preposition-unit-in-ts
           (list
            `(,(variablify (unit-name preposition-unit-in-ts))
              --
              (parent ,(variablify (unit-name pp-unit)))
              (lemma ,(cadr (find 'lemma (unit-body preposition-unit-in-ts) :key #'feature-name))))))
          (t ;; no prep child of pp
           (let* ((coordination-unit (loop for unit in unit-structure
                                          when (and (find (list 'lex-class 'cc) (unit-body unit) :test #'equalp)
                                                    (equal (cadr (find 'parent (unit-body unit) :key #'feature-name)) (unit-name pp-unit)))
                                          return unit))
                 
                 (sub-pp-unit (loop for unit in unit-structure
                                    when (and (find `(phrase-type (pp)) (unit-body unit) :test #'equalp)
                                              (equal (cadr (find 'parent (unit-body unit) :key #'feature-name)) (unit-name pp-unit)))
                                    return unit))
                 (prep-unit (loop for unit in unit-structure
                                  when (and (or (find (list 'lex-class 'in) (unit-body unit) :test #'equalp)
                                                (find (list 'dependency-label 'prep) (unit-body unit) :test #'equalp))
                                            (equal (cadr (find 'parent (unit-body unit) :key #'feature-name)) (unit-name sub-pp-unit)))
                                  return unit)))
             (when prep-unit
             (list
              `(,(variablify (unit-name coordination-unit))
                --
                (parent ,(variablify (unit-name pp-unit)))
                (lex-class cc))
              `(,(variablify (unit-name sub-pp-unit))
                --
                (parent ,(variablify (unit-name coordination-unit)))
                (phrase-type (pp)))
              `(,(variablify (unit-name prep-unit))
                --
                (parent ,(variablify (unit-name sub-pp-unit)))
                ;;(lex-class in)
                (lemma ,(cadr (find 'lemma (unit-body prep-unit) :key #'feature-name)))))))))))

             
(defun make-propbank-conditional-units-without-role (units-with-role cxn-units-with-role unit-structure)
  "Makes conditional units that are needed in a propbank cxn to encode
the paths in the syntactic tree between units that function as slot
fillers (arg0, arg1) and the frame-evoking element unit."
  (remove-duplicates
   (loop with fee-unit = (cdr (find-if #'(lambda(unit-with-role) (string= (role-type (car unit-with-role)) "V"))
                                                               units-with-role))
         for unit-with-role in (remove fee-unit units-with-role :test #'equal) ;;discard the frame-evoking element (FEE) unit
         for path = (find-path-in-syntactic-tree (cdr unit-with-role) fee-unit unit-structure) ;;find path between a unit in the transient structure and the FEE unit

         append (loop for unit-name in path
                      for unit = (find unit-name unit-structure :key #'unit-name)
                      for parent = (when (cadr (find 'parent (unit-body unit) :key #'feature-name))
                                      (variablify (cadr (find 'parent (unit-body unit) :key #'feature-name))))
                      for form-constraints-for-children-with-role-and-same-type = (make-form-constraints-for-children-with-role-and-same-type unit cxn-units-with-role)

                      unless (find (variablify unit-name) cxn-units-with-role :key #'unit-name) ;;check that the unit is not a frame-element
                      collect `(,(variablify unit-name)
                                --
                                (parent ,parent)
                                ,(when form-constraints-for-children-with-role-and-same-type
                                   `(word-order ,form-constraints-for-children-with-role-and-same-type))
                                ,(if (find '(node-type leaf) (unit-body unit) :test #'equal)
                                   `(lex-class ,(cadr (find 'lex-class (unit-body unit) :key #'feature-name)))
                                   `(phrase-type ,(cadr (find 'phrase-type (unit-body unit) :key #'feature-name)))))))
                                                    :key #'unit-name))


(defun make-form-constraints-for-children-with-role-and-same-type (unit cxn-units-with-role)
  (let* ((children (loop for role-unit in cxn-units-with-role
                         when (equalp (subseq (symbol-name (feature-value (find 'parent (cddr role-unit) :key #'first :test #'equalp))) 1)
                                      (symbol-name (unit-name unit)))
                         collect role-unit))
         (children-per-type (mapcar #'rest
                                    (group-by children #'(lambda (unit)
                                                           (if (find '(node-type leaf) (cddr unit) :test #'equal)
                                                             (feature-value (find 'lex-class (cddr unit) :key #'first :test #'equalp))
                                                             (feature-value (find 'phrase-type (cddr unit) :key #'first :test #'equalp))))
                                              :test #'equalp )))
         (form-constraints-in-unit (feature-value (find 'word-order (unit-body unit) :key #'first :test #'equalp))))

    (loop with fcs-to-keep = nil
          for fc in form-constraints-in-unit
          for first-unit-name = (variablify (second fc))
          for second-unit-name = (variablify (third fc))
          do (let ((fc-to-keep (loop for group in children-per-type
                                  if (and (find first-unit-name group :key #'unit-name :test #'string=)
                                          (find second-unit-name group :key #'unit-name :test #'string=))
                                  collect (list (first fc) first-unit-name second-unit-name))))
               (when fc-to-keep 
                 (setf fcs-to-keep (append fcs-to-keep fc-to-keep))))
          
          finally return fcs-to-keep)))


(defun find-path-in-syntactic-tree (unit v-unit unit-structure)
  "A search process that finds a path between two units in a transient
structure. The path is returned as a list of unit names, ordered from
start to end(v-unit)"
  (let ((queue (list `((:unit-name . ,(unit-name unit))
                       (:parent . ,(cadr (find 'parent (unit-body unit) :key #'feature-name)))
                       (:constituents .  ,(cadr (find 'constituents (unit-body unit) :key #'feature-name)))
                       (:path . ((,(unit-name unit) . :initial)))))))

    (loop while queue
          for state = (pop queue)
          if (equal (cdr (assoc :unit-name state)) (unit-name v-unit)) ;;solution found
          return (mapcar #'car (cdr (assoc :path state)))
          else do (setf queue (append queue (make-constituent-and-parent-states state unit-structure))))))

(defun make-constituent-and-parent-states (state unit-structure)
  "Creates new states for parent and constituents of current state"
  (let ((new-states nil))
    ;;First create states based on the constituents of the current state
    (loop for constituent-name in (cdr (assoc :constituents state)) 
          for constituent-unit = (find constituent-name unit-structure :key #'unit-name)
          for new-state = `((:unit-name . ,constituent-name)
                            (:parent . ,(cadr (find 'parent (unit-body constituent-unit) :key #'feature-name)))
                            (:constituents . ,(cadr (find 'constituents (unit-body constituent-unit) :key #'feature-name)))
                            (:path . ,(append (cdr (assoc :path state)) (list (cons constituent-name :constituent)))))
          unless (find constituent-name (cdr (assoc :path state)) :key #'car) ;;avoid circular paths
          do (push new-state new-states))

    ;;Now create a state for the parent of the current state
    (when (assoc :parent state)
      (let* ((parent-name (cdr (assoc :parent state)))
             (parent-unit (find parent-name unit-structure :key #'unit-name))
             (new-state `((:unit-name . ,parent-name)
                          (:parent . ,(cadr (find 'parent (unit-body parent-unit) :key #'feature-name)))
                          (:constituents . ,(cadr (find 'constituents (unit-body parent-unit) :key #'feature-name)))
                          (:path . ,(append (cdr (assoc :path state)) (list (cons parent-name :parent)))))))
        (unless (find parent-name (cdr (assoc :path state)) :key #'car) ;;avoid circular paths
          (push new-state new-states))))
    new-states))

(defmethod all-rolesets ((sentence conll-sentence))
  "Returns all propbank frames with which a sentence was annotated."
  (mapcar #'frame-name (propbank-frames sentence)))


(defmethod ts-unit-structure ((sentence conll-sentence) (cxn-inventory fcg-construction-set))
  "Returns the unit structure based on the syntactic analysis."
  (left-pole-structure (de-render sentence (get-configuration cxn-inventory :de-render-mode))))

(defun units-with-role (ts-unit-structure gold-frame)
  "Returns (cons . units) for each unit in the unit-structure that is an elements of the frame."
  (loop for role in (frame-roles gold-frame) ;;find all units that correspond to annotated frame elements
        for role-start = (first (indices role))
        for role-end = (+ (last-elt (indices role)) 1)
        for unit = (find-unit-by-span ts-unit-structure (list role-start role-end))
        collect (cons role unit)))

(defun v-lemma (units-with-role)
  "Returns the lemma of the V."
  (loop for (role . unit) in units-with-role
        when (string= "V" (role-type role))
        return (feature-value (find 'lemma (unit-body unit) :key #'feature-name))))



