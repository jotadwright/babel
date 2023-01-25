(in-package :propbank-grammar)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                              ;;
;; Learning constructions based on Propbank annotated corpora.  ;;
;;                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun learn-propbank-grammar (list-of-propbank-sentences &key
                                                          (selected-rolesets nil)
                                                          (excluded-rolesets nil)
                                                          (cxn-inventory '*propbank-learned-cxn-inventory*)
                                                          (fcg-configuration nil))
  "Learns a Propbank Grammar."
  (let ((cxn-inventory (eval `(def-fcg-constructions propbank-learned-english
                                :fcg-configurations ,fcg-configuration
                                :visualization-configurations ((:show-constructional-dependencies . nil)
                                                               (:show-categorial-network . nil)
                                                               (:hide-attributes . t))
                                :hierarchy-features (constituents dependents)
                                :feature-types ((constituents sequence)
                                                (dependents sequence)
                                                (span sequence)
                                                (syn-class set)
                                                (args set-of-predicates)
                                                (word-order set-of-predicates)
                                                (meaning set-of-predicates)
                                                (footprints set))
                                :cxn-inventory ,cxn-inventory
                                :hashed t))))
    (set-data (blackboard cxn-inventory) :training-corpus-size 0)
    (loop for sentence in list-of-propbank-sentences
          for sentence-number from 1
          for training-corpus-size = (get-data (blackboard cxn-inventory) :training-corpus-size)
          for rolesets = (cond (selected-rolesets
                                (intersection selected-rolesets (all-rolesets sentence) :test #'equalp))
                               (excluded-rolesets
                                 (loop for roleset in (all-rolesets sentence)
                                       unless (find roleset excluded-rolesets :test #'equalp)
                                       collect roleset))
                               (t
                                (all-rolesets sentence)))
          do
          (when (= 0 (mod sentence-number 100))
            (format t "~%---> Sentence ~a." sentence-number))
          (when rolesets
            (set-data (blackboard cxn-inventory) :training-corpus-size (incf training-corpus-size)))
          (loop for roleset in rolesets
                do
                (loop for mode in (get-configuration cxn-inventory :learning-modes)
                      do
                      (learn-from-propbank-annotation sentence roleset cxn-inventory mode)))
          finally
          (return cxn-inventory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learning constructions from an annotated frame instance. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric learn-from-propbank-annotation (propbank-sentence roleset cxn-inventory mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core roles.           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod learn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :core-roles)))
  
  (loop with gold-frames = (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)
        for gold-frame in gold-frames
        if (spacy-benepar-compatible-annotation propbank-sentence roleset :selected-role-types 'core-only)
        do (learn-constructions-for-gold-frame-instance propbank-sentence gold-frame cxn-inventory mode)))

(defmethod learn-constructions-for-gold-frame-instance (propbank-sentence gold-frame cxn-inventory (mode (eql :core-roles)))
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (core-units-with-role (remove-if #'(lambda (unit-with-role)
                                              (search "ARGM" (role-type (car unit-with-role))))
                                          (units-with-role ts-unit-structure gold-frame)))
         (lex-category (add-lexical-cxn gold-frame (v-unit core-units-with-role) cxn-inventory propbank-sentence))
         (gram-category (when lex-category
                          (add-grammatical-cxn gold-frame core-units-with-role cxn-inventory propbank-sentence lex-category))))
    (when gram-category
      (add-word-sense-cxn gold-frame (v-unit core-units-with-role) cxn-inventory propbank-sentence lex-category gram-category))))

(defun find-lexical-cxn (v-unit cxn-inventory)
  "Finds a lexical construction based on a v-unit."
  (let* ((lemma (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name)))
         (syn-class (feature-value (find 'syn-class (unit-body v-unit) :key #'feature-name)))
         (cxn-name (intern (upcase (format nil "~a~a-cxn" lemma syn-class)))))
    (find-cxn cxn-name cxn-inventory :hash-key lemma :key #'name)))

(defun add-lexical-cxn (gold-frame v-unit cxn-inventory propbank-sentence)
  "Creates a new lexical construction if necessary, otherwise increments frequency of existing cxn."
  (let* ((lemma (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name)))
         (syn-class (feature-value (find 'syn-class (unit-body v-unit) :key #'feature-name)))
         (lex-category (intern (symbol-name (make-id (format nil "~a~a" (truncate-frame-name (frame-name gold-frame)) syn-class)))))
         (cxn-name (intern (upcase (format nil "~a~a-cxn" lemma syn-class))))
         (equivalent-cxn (find-cxn cxn-name cxn-inventory :hash-key lemma :key #'name)))
    (if equivalent-cxn
      ;; if cxn already exists: increment frequency
      (progn
        (incf (attr-val equivalent-cxn :frequency))
        (attr-val equivalent-cxn :lex-category))
      ;; Else make new cxn
      (when lemma
        (if (equalp syn-class '(vp))
          (let ((lex-lemma (intern (subseq (symbol-name lemma) 0 (search "-" (symbol-name lemma))))))
            (eval
             `(def-fcg-cxn ,cxn-name
                           ((?phrasal-unit
                             (footprints (lex))
                             (lex-category ,lex-category))
                            (?lex-unit
                             (footprints (lex)))
                            <-
                            (?phrasal-unit
                             --
                             (footprints (NOT lex))
                             (lemma ,lemma)
                             (syn-class ,syn-class))
                            (?lex-unit
                             --
                             (footprints (NOT lex))
                             (lemma ,lex-lemma)
                             (parent ?phrasal-unit)))
                          
                           :attributes (:lemma ,lemma
                                        :lex-category ,lex-category
                                        :score 2
                                        :label lexical-cxn
                                        :frequency 1)
                           :description ,(sentence-string propbank-sentence)
                           :disable-automatic-footprints t
                           :cxn-inventory ,cxn-inventory)))
            (eval
             `(def-fcg-cxn ,cxn-name
                           ((?lex-unit
                             (footprints (lex))
                             (lex-category ,lex-category))
                            <-
                            (?lex-unit
                             --
                             (footprints (NOT lex))
                             (lemma ,lemma)
                             (syn-class ,syn-class)))
                           :attributes (:lemma ,lemma
                                        :lex-category ,lex-category
                                        :score 1
                                        :label lexical-cxn
                                        :frequency 1)
                           :description ,(sentence-string propbank-sentence)
                           :disable-automatic-footprints t
                           :cxn-inventory ,cxn-inventory)))
        (add-category lex-category cxn-inventory :recompute-transitive-closure nil)
          lex-category))))



(defun add-grammatical-cxn (gold-frame core-units-with-role cxn-inventory propbank-sentence lex-category)
  "Learns a construction capturing all core roles."
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (gram-category (make-gram-category core-units-with-role))
         (cxn-units-with-role (loop for unit in core-units-with-role
                                    collect (make-propbank-conditional-unit-with-role unit gram-category 'fee)))
         (cxn-units-without-role (make-propbank-conditional-units-without-role core-units-with-role
                                                                               cxn-units-with-role ts-unit-structure))
         (passive (loop for unit in cxn-units-without-role
                        when (eql '+ (unit-feature-value (cdr unit) 'passive))
                        return t))
         (contributing-unit (make-propbank-contributing-unit core-units-with-role gold-frame gram-category 'fee))
         (schema (make-cxn-schema core-units-with-role cxn-units-with-role :core-roles :passive? passive))
         (cxn-name (intern (upcase (format nil "~a+~a-cxn" gram-category (length cxn-units-without-role)))))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role))
                                              cxn-inventory)))
    
    (if equivalent-cxn
      
      ;; Grammatical construction already exists
      ;;----------------------------------------
      (progn
        ;;1) Increase its frequency
        (incf (attr-val equivalent-cxn :frequency))
        ;;2) Check if there was already a link in the categorial network between the lex-category and the gram-category:
        (if (link-exists-p lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory)
          ;;a) If yes, increase edge weight
          (progn
            (incf-link-weight lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :delta 1.0 :link-type 'lex-gram))
          ;;b) Otherwise, add new connection (weight 1.0)
          (progn
            (add-link lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :weight 1.0 :link-type nil
                      :recompute-transitive-closure nil)
            (add-link lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :weight 1.0 :link-type 'lex-gram
                      :recompute-transitive-closure nil)))
        ;;3) Return gram-category
        (attr-val equivalent-cxn :gram-category))

      ;; Else: Create a new grammatical category for the observed pattern + add category and link to the categorial network
      ;;--------------------------------------------------------------------------------------------------------------------
      (when (and cxn-units-with-role (v-lemma core-units-with-role))
        
        (add-category gram-category cxn-inventory :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)
        
        (eval `(def-fcg-cxn ,cxn-name
                            (,contributing-unit
                             <-
                             ,@cxn-units-with-role
                             ,@cxn-units-without-role
                             )
                            :disable-automatic-footprints t
                            :attributes (:schema ,schema
                                         :lemma nil
                                         :score ,(length cxn-units-with-role)
                                         :label argument-structure-cxn
                                         :frequency 1
                                         :gram-category ,gram-category)
                            :description ,(sentence-string propbank-sentence)
                            :cxn-inventory ,cxn-inventory))
        gram-category))))


(defun add-word-sense-cxn (gold-frame v-unit cxn-inventory propbank-sentence lex-category gram-category)
  "Creates a new lexical construction if necessary, otherwise increments frequency of existing cxn."
  (let* ((lemma (or (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name))
                    (feature-value (find 'string (unit-body v-unit) :key #'feature-name))))
         (cxn-name (intern (upcase (format nil "~a(~a)-cxn" (frame-name gold-frame) lemma))))
         
         (equivalent-cxn (find-cxn cxn-name cxn-inventory :hash-key (if (stringp lemma) ;;WERKT NIET MEER!
                                                                      (intern (upcase lemma))
                                                                      lemma) :key #'name))
         (sense-category (intern (symbol-name (make-id (frame-name gold-frame))))))
    
    (if equivalent-cxn
      
      ;; If word sense cxn already exists
      ;;---------------------------------
      (progn
        (incf (attr-val equivalent-cxn :frequency))
        
        ;; edge between gram-category and sense-category
        (if (link-exists-p gram-category (attr-val equivalent-cxn :sense-category) cxn-inventory)
          ;;connection between gram and sense category exists: increase edge weight
          (progn
            (incf-link-weight gram-category (attr-val equivalent-cxn :sense-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight gram-category (attr-val equivalent-cxn :sense-category) cxn-inventory :delta 1.0 :link-type 'gram-sense))
          ;;add new link
          (progn
            (add-link gram-category
                      (attr-val equivalent-cxn :sense-category) cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
            (add-link gram-category
                      (attr-val equivalent-cxn :sense-category) cxn-inventory :weight 1.0 :link-type 'gram-sense :recompute-transitive-closure nil)))
        
        ;; edge between lex-category and sense-category
        (if (link-exists-p lex-category (attr-val equivalent-cxn :sense-category) cxn-inventory)
          (progn
            (incf-link-weight lex-category (attr-val equivalent-cxn :sense-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight lex-category (attr-val equivalent-cxn :sense-category) cxn-inventory :delta 1.0 :link-type 'lex-sense))
          (progn
            (add-link lex-category
                      (attr-val equivalent-cxn :sense-category) cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
            (add-link lex-category
                      (attr-val equivalent-cxn :sense-category) cxn-inventory :weight 1.0 :link-type 'lex-sense :recompute-transitive-closure nil)))

        (attr-val equivalent-cxn :sense-category))
      
      ;; Else make new cxn
      ;;-------------------
      (progn (assert lemma)
        (eval
         `(def-fcg-cxn ,cxn-name
                       ((?lex-unit
                         (footprints (ws)))
                        <-
                        (?lex-unit
                         --
                         ,@(if (stringp lemma)
                             `((string ,lemma))
                             `((lemma ,lemma)))
                         (gram-category ,sense-category)
                         (lex-category ,sense-category)
                         (frame ,(intern (upcase (frame-name gold-frame))))
                         (footprints (NOT ws))))
                       :disable-automatic-footprints t
                       :attributes (:lemma ,(if (stringp lemma)
                                              (intern (upcase lemma))
                                              lemma)
                                    :sense-category ,sense-category
                                    :score 1
                                    :label word-sense-cxn
                                    :frequency 1)
                       :description ,(sentence-string propbank-sentence)
                       :cxn-inventory ,cxn-inventory))
        
        (add-category sense-category cxn-inventory :recompute-transitive-closure nil)
        (add-link gram-category sense-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link gram-category sense-category cxn-inventory :weight 1.0 :link-type 'gram-sense :recompute-transitive-closure nil)
        (add-link lex-category sense-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link lex-category sense-category cxn-inventory :weight 1.0 :link-type 'lex-sense :recompute-transitive-closure nil)
        
        sense-category))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARGM single word      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod learn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :argm-leaf)))
  (loop with gold-frames = (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)
        for gold-frame in gold-frames
        if (spacy-benepar-compatible-annotation propbank-sentence roleset :selected-role-types 'argm-only)
        do
        (learn-constructions-for-gold-frame-instance propbank-sentence gold-frame cxn-inventory mode)))
       
(defmethod learn-constructions-for-gold-frame-instance (propbank-sentence gold-frame cxn-inventory (mode (eql :argm-leaf)))
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (units-with-role (units-with-role ts-unit-structure gold-frame))
         (argm-leafs (remove-if-not #'(lambda (unit-with-role)
                                      (and (search "ARGM" (role-type (car unit-with-role)))
                                           (equalp (unit-feature-value (cdr unit-with-role) 'node-type) 'leaf)))
                                  units-with-role)))

    (loop with v-unit-with-role = (v-unit-with-role units-with-role)
          for argm-leaf in argm-leafs
          for argm-unit-name = (unit-name (cdr argm-leaf))
          append (loop with v-unit-found? = nil
                       for (role . unit) in units-with-role
                       for unit-name = (unit-name unit)
                       if (string= (role-type role) "V")
                       do (setf v-unit-found? t)
                       else if (equal unit-name argm-unit-name)
                       collect (let ((units-with-role (if v-unit-found? ;;v-unit precedes argm-leaf unit
                                                        (list v-unit-with-role argm-leaf)
                                                        (list argm-leaf v-unit-with-role))))
                                 (add-argm-leaf-cxn gold-frame units-with-role cxn-inventory propbank-sentence ts-unit-structure))))))
   

(defun add-argm-leaf-cxn (gold-frame units-with-role cxn-inventory propbank-sentence ts-unit-structure)
  "Learns a construction capturing V + ARGM-argm."
  (let* ((argm-unit (find "ARGM" units-with-role :key #'(lambda (unit-w-role)
                                                          (role-type (car unit-w-role))) :test #'search))
         (argm-lemma (unit-feature-value (unit-body argm-unit) 'lemma))
         (footprint (make-const 'argm))
         (cxn-units-with-role
          (loop for unit-w-role in units-with-role
                if (equal (role-type (car unit-w-role)) "V")
                collect (make-propbank-conditional-unit-with-role unit-w-role nil footprint :frame-evoking t)
                else collect (make-propbank-conditional-unit-with-role unit-w-role nil footprint :lemma argm-lemma)))
         (contributing-unit (make-propbank-contributing-unit units-with-role gold-frame nil footprint :include-gram-category? nil))
         (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role ts-unit-structure))
         (cxn-name (make-cxn-name units-with-role cxn-units-with-role cxn-units-without-role :argm-leaf :lemma argm-lemma))
         (schema (make-cxn-schema units-with-role cxn-units-with-role :argm-leaf :lemma argm-lemma))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role))
                                              cxn-inventory
                                              :hash-key argm-lemma)))

    (if equivalent-cxn
      
      ;; argm-leaf cxn already exists, update its frequency
      (incf (attr-val equivalent-cxn :frequency))
      
      ;; create a argm-leaf cxn
      (when (and cxn-units-with-role (v-lemma units-with-role))
        (eval `(def-fcg-cxn ,cxn-name
                            (,contributing-unit
                             <-
                             ,@cxn-units-with-role
                             ,@cxn-units-without-role)
                            :disable-automatic-footprints t
                            :attributes (:schema ,schema
                                         :lemma ,argm-lemma
                                         :score 1
                                         :label argm-leaf-cxn
                                         :frequency 1)
                            :description ,(sentence-string propbank-sentence)
                            :cxn-inventory ,cxn-inventory))))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARGM PPs              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod learn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :argm-pp)))
  (loop with gold-frames = (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)
        for gold-frame in gold-frames
        if (spacy-benepar-compatible-annotation propbank-sentence roleset :selected-role-types 'argm-only)
        do
        (learn-constructions-for-gold-frame-instance propbank-sentence gold-frame cxn-inventory mode)))
       
(defmethod learn-constructions-for-gold-frame-instance (propbank-sentence gold-frame cxn-inventory (mode (eql :argm-pp)))
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (units-with-role (units-with-role ts-unit-structure gold-frame))
         (argm-pps (remove-if-not #'(lambda (unit-with-role)
                                      (and (search "ARGM" (role-type (car unit-with-role)))
                                           (find 'pp (unit-feature-value (cdr unit-with-role) 'syn-class))))
                                  units-with-role))
         (v-unit (v-unit units-with-role))
         (lex-cxn (find-lexical-cxn v-unit cxn-inventory))
         (lex-category (when lex-cxn (attr-val lex-cxn :lex-category)))
         (gram-categories
          (when lex-category
            (loop with v-unit-with-role = (v-unit-with-role units-with-role)
                  for argm-pp in argm-pps
                  for pp-unit-name = (unit-name (cdr argm-pp))
                  append (loop with v-unit-found? = nil
                                for (role . unit) in units-with-role
                                for unit-name = (unit-name unit)
                                if (string= (role-type role) "V")
                                do (setf v-unit-found? t)
                                else if (equal unit-name pp-unit-name)
                                collect (let ((units-with-role (if v-unit-found? ;;v-unit precedes argm-pp-unit
                                                                 (list v-unit-with-role argm-pp)
                                                                 (list argm-pp v-unit-with-role))))
                                          (add-pp-cxn gold-frame units-with-role cxn-inventory propbank-sentence lex-category ts-unit-structure)))))))
    
    (loop for gram-category in gram-categories
          for word-sense-cxn = (find-word-sense-cxn gold-frame v-unit cxn-inventory)
          if word-sense-cxn
          do (update-categorial-network lex-category gram-category (attr-val word-sense-cxn :sense-category) cxn-inventory)
          else do (add-word-sense-cxn gold-frame v-unit cxn-inventory propbank-sentence lex-category gram-category))))




(defun find-word-sense-cxn (gold-frame v-unit cxn-inventory)
  "Find a word sense construction."
  (let* ((lemma (or (feature-value (find 'lemma (unit-body v-unit) :key #'feature-name))
                    (intern (upcase (feature-value (find 'string (unit-body v-unit) :key #'feature-name))))))
         (cxn-name (intern (upcase (format nil "~a-cxn" (frame-name gold-frame))))))
    (find-cxn cxn-name cxn-inventory :hash-key lemma :key #'name)))


(defun update-categorial-network (lex-category gram-category sense-category cxn-inventory)

  (if (cxn-inventory gram-category sense-category cxn-inventory)
    ;;connection between gram and sense category exists: increase edge weight
    (progn
      (incf-link-weight gram-category sense-category cxn-inventory :delta 1.0 :link-type nil)
      (incf-link-weight gram-category sense-category cxn-inventory :delta 1.0 :link-type 'gram-sense))
    ;;add new link
    (progn
      (add-link gram-category sense-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
      (add-link gram-category sense-category cxn-inventory :weight 1.0 :link-type 'gram-sense :recompute-transitive-closure nil)))

  (if (link-exists-p lex-category sense-category cxn-inventory)
    ;;connection between gram and sense category exists: increase edge weight
    (progn
      (incf-link-weight lex-category sense-category cxn-inventory :delta 1.0 :link-type nil )
      (incf-link-weight lex-category sense-category cxn-inventory :delta 1.0 :link-type 'lex-sense))
    ;;add new link
    (progn
      (add-link lex-category sense-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
      (add-link lex-category sense-category cxn-inventory :weight 1.0 :link-type 'lex-sense :recompute-transitive-closure nil))))




(defun add-pp-cxn (gold-frame units-with-role cxn-inventory propbank-sentence lex-category ts-unit-structure)
  "Learns a construction capturing V + ARGM-pp."
  (let* ((pp-unit (find "ARGM" units-with-role :key #'(lambda (unit-w-role)
                                                         (role-type (car unit-w-role))) :test #'search))
         (cxn-preposition-units (make-preposition-unit pp-unit ts-unit-structure)) ;;list with 1 or 3 units
         (preposition-lemma
          (if (= 1 (length cxn-preposition-units))
            (or (second (find 'lemma (nthcdr 2 (first cxn-preposition-units))
                              :key #'feature-name))
                (second (find 'string (nthcdr 2 (first cxn-preposition-units))
                              :key #'feature-name)))
            (second (find 'lemma (nthcdr 2 (third cxn-preposition-units))
                                          :key #'feature-name))))
         (gram-category (make-gram-category units-with-role preposition-lemma))
         (footprint (make-const 'pp))
         (cxn-units-with-role (loop for unit in units-with-role
                                     if (equal (role-type (car unit)) "V")
                                     collect (make-propbank-conditional-unit-with-role unit gram-category footprint :frame-evoking t)
                                     else collect (make-propbank-conditional-unit-with-role unit gram-category footprint)))
         (contributing-unit (make-propbank-contributing-unit units-with-role gold-frame gram-category footprint :include-gram-category? nil))
         (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role ts-unit-structure))
         
         (cxn-name (intern (upcase (format nil "~a+~a-cxn" gram-category (length cxn-units-without-role)))))
         (schema (make-cxn-schema units-with-role cxn-units-with-role :argm-pp :cxn-preposition-units (list cxn-preposition-units)))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role
                                                                   cxn-preposition-units))
                                              cxn-inventory
                                              :hash-key preposition-lemma)))
    
    (if equivalent-cxn
      
      ;; Grammatical construction already exists
      ;;----------------------------------------
      (progn
        ;;1) Increase its frequency
        (incf (attr-val equivalent-cxn :frequency))
        ;;2) Check if there was already a link in the type hierarchy between the lex-category and the gram-category:
        (if (link-exists-p lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :link-type nil)
          ;;a) If yes, increase edge weight
          (progn
            (incf-link-weight lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :delta 1.0 :link-type 'lex-gram))
          ;;b) Otherwise, add new connection (weight 1.0)
          (progn
            (add-link lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
          (add-link lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)))
        ;;3) Return gram-category
        (attr-val equivalent-cxn :gram-category))
      
      ;; Else: Create a new grammatical category for the observed pattern + add category and link to the categorial network
      ;;-------------------------------------------------------------------------------------------------------------------
      (when (and cxn-units-with-role (v-lemma units-with-role))
        (assert preposition-lemma)
        (add-category gram-category cxn-inventory :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)
        
        (eval `(def-fcg-cxn ,cxn-name
                            (,contributing-unit
                                  <-
                                  ,@cxn-units-with-role
                                  ,@cxn-units-without-role
                                  ,@cxn-preposition-units)
                            :disable-automatic-footprints t
                            :attributes (:schema ,schema
                                         :lemma ,preposition-lemma
                                         :score ,(length cxn-units-with-role)
                                         :label argm-phrase-cxn
                                         :frequency 1
                                         :gram-category ,gram-category)
                            :description ,(sentence-string propbank-sentence)
                            :cxn-inventory ,cxn-inventory))
        gram-category))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARGM S-BARs           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod learn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :argm-sbar)))
  (loop with gold-frames = (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)
        for gold-frame in gold-frames
        if (spacy-benepar-compatible-annotation propbank-sentence roleset :selected-role-types 'argm-only)
        do
        (learn-constructions-for-gold-frame-instance propbank-sentence gold-frame cxn-inventory mode)))
       
(defmethod learn-constructions-for-gold-frame-instance (propbank-sentence gold-frame cxn-inventory (mode (eql :argm-sbar)))
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (units-with-role (units-with-role ts-unit-structure gold-frame))
         (argm-sbars (remove-if-not #'(lambda (unit-with-role)
                                        (and (search "ARGM" (role-type (car unit-with-role)))
                                             (or (find 'sbar (unit-feature-value (cdr unit-with-role) 'syn-class))
                                                 (find 's (unit-feature-value (cdr unit-with-role) 'syn-class)))))
                                  units-with-role))
         (v-unit (v-unit units-with-role))
         (lex-cxn (find-lexical-cxn v-unit cxn-inventory))
         (lex-category (when lex-cxn (attr-val lex-cxn :lex-category)))
         (gram-categories
          (when lex-category
            (loop with v-unit-with-role = (v-unit-with-role units-with-role)
                  for argm-sbar in argm-sbars
                  for sbar-unit-name = (unit-name (cdr argm-sbar))
                  append (loop with v-unit-found? = nil
                                for (role . unit) in units-with-role
                                for unit-name = (unit-name unit)
                                if (string= (role-type role) "V")
                                do (setf v-unit-found? t)
                                else if (equal unit-name sbar-unit-name)
                                collect (let ((units-with-role (if v-unit-found? ;;v-unit precedes argm-pp-unit
                                                                 (list v-unit-with-role argm-sbar)
                                                                 (list argm-sbar v-unit-with-role))))
                                          (add-sbar-cxn gold-frame units-with-role cxn-inventory propbank-sentence lex-category ts-unit-structure)))))))
            
    (loop for gram-category in gram-categories
          do (add-word-sense-cxn gold-frame v-unit cxn-inventory propbank-sentence lex-category gram-category)))) ;;only one cxn, multiple links in th


(defun add-sbar-cxn (gold-frame units-with-role cxn-inventory propbank-sentence lex-category ts-unit-structure)
  "Learns a construction capturing V + ARGM-sbar."
  (let* ((sbar-unit (find "ARGM" units-with-role :key #'(lambda (unit-w-role)
                                                          (role-type (car unit-w-role))) :test #'search))
         (cxn-sbar-unit (make-subclause-word-unit sbar-unit ts-unit-structure))
         (sbar-lemma (second (or (find 'lemma (nthcdr 2 cxn-sbar-unit) :key #'feature-name)
                                 (find 'string (nthcdr 2 cxn-sbar-unit) :key #'feature-name))))
         (gram-category (make-gram-category units-with-role sbar-lemma))
         (footprint (make-const 'sbar))
          ;;1 unit
         (cxn-units-with-role (loop for unit in units-with-role
                                    if (equal (role-type (car unit)) "V")
                                      collect (make-propbank-conditional-unit-with-role unit gram-category footprint :frame-evoking t)
                                    else collect (make-propbank-conditional-unit-with-role unit gram-category footprint)))
         (contributing-unit (make-propbank-contributing-unit units-with-role gold-frame gram-category footprint :include-gram-category? nil))
         (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role ts-unit-structure))
         (cxn-name (intern (upcase (format nil "~a+~a-cxn" gram-category (length cxn-units-without-role)))))
         (schema (make-cxn-schema units-with-role cxn-units-with-role :argm-sbar :cxn-s-bar-units (list cxn-sbar-unit)))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role
                                                                   (list cxn-sbar-unit)))
                                              cxn-inventory
                                              :hash-key (if (stringp sbar-lemma)
                                                          (intern (upcase sbar-lemma))
                                                          sbar-lemma))))
    (if equivalent-cxn
      
      ;;Grammatical construction already exists
      (progn
        ;;1) Increase its frequency
        (incf (attr-val equivalent-cxn :frequency))
        
        ;;2) Check if there was already a link in the type hierarchy between the lex-category and the gram-category:
        (if (link-exists-p lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :link-type nil)
          ;;a) If yes, increase edge weight
          (progn
            (incf-link-weight lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :delta 1.0 :link-type nil)
            (incf-link-weight lex-category (attr-val equivalent-cxn :gram-category) cxn-inventory :delta 1.0 :link-type 'lex-gram))
          ;;b) Otherwise, add new connection (weight 1.0)
          (progn
            (add-link lex-category
                    (attr-val equivalent-cxn :gram-category) cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
          (add-link lex-category
                    (attr-val equivalent-cxn :gram-category) cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)))
        
        ;;3) Return gram-category
        (attr-val equivalent-cxn :gram-category))
      
      ;;Create a new grammatical category for the observed pattern + add category and link to the type hierarchy
      (when (and cxn-units-with-role (v-lemma units-with-role))
        (add-category gram-category cxn-inventory :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type nil :recompute-transitive-closure nil)
        (add-link lex-category gram-category cxn-inventory :weight 1.0 :link-type 'lex-gram :recompute-transitive-closure nil)

        (eval `(def-fcg-cxn ,cxn-name
                            (,contributing-unit
                             <-
                             ,@cxn-units-with-role
                             ,@cxn-units-without-role
                             ,cxn-sbar-unit)
                            :disable-automatic-footprints t
                            :attributes (:schema ,schema
                                         :lemma ,(if (stringp sbar-lemma)
                                                   (intern (upcase sbar-lemma))
                                                   sbar-lemma)
                                         :score ,(length cxn-units-with-role)
                                         :label argm-phrase-cxn
                                         :frequency 1
                                         :gram-category ,gram-category)
                            :description ,(sentence-string propbank-sentence)
                            :cxn-inventory ,cxn-inventory))
        gram-category))))






#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ARGM phrase with string          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod learn-from-propbank-annotation (propbank-sentence roleset cxn-inventory (mode (eql :argm-phrase-with-string)))
  (loop with gold-frames = (find-all roleset (propbank-frames propbank-sentence) :key #'frame-name :test #'equalp)
        for gold-frame in gold-frames
        if (spacy-benepar-compatible-annotation propbank-sentence roleset :selected-role-types 'argm-only)
        do
        (learn-constructions-for-gold-frame-instance propbank-sentence gold-frame cxn-inventory mode)))
       
(defmethod learn-constructions-for-gold-frame-instance (propbank-sentence gold-frame cxn-inventory (mode (eql :argm-phrase-with-string)))
  (let* ((ts-unit-structure (ts-unit-structure propbank-sentence cxn-inventory))
         (units-with-role (units-with-role ts-unit-structure gold-frame))
         (argm-phrases (remove-if-not #'(lambda (unit-with-role)
                                        (and (search "ARGM" (role-type (car unit-with-role)))
                                             (equalp (unit-feature-value (cdr unit-with-role) 'node-type) 'phrase)
                                             (not (or (find 'sbar (unit-feature-value (cdr unit-with-role) 'syn-class))
                                                      (find 's (unit-feature-value (cdr unit-with-role) 'syn-class))
                                                      (find 'pp (unit-feature-value (cdr unit-with-role) 'syn-class))))))
                                  units-with-role)))


    (loop with v-unit-with-role = (v-unit-with-role units-with-role)
          for argm-phrase in argm-phrases
          for argm-unit-name = (unit-name (cdr argm-phrase))
          append (loop with v-unit-found? = nil
                       for (role . unit) in units-with-role
                       for unit-name = (unit-name unit)
                       if (string= (role-type role) "V")
                       do (setf v-unit-found? t)
                       else if (equal unit-name argm-unit-name)
                       collect (let ((units-with-role (if v-unit-found? ;;v-unit precedes argm-pp-unit
                                                        (list v-unit-with-role argm-phrase)
                                                        (list argm-phrase v-unit-with-role))))
                                 (add-argm-phrase-with-string-cxn gold-frame units-with-role cxn-inventory propbank-sentence ts-unit-structure))))))

    
  
(defun add-argm-phrase-with-string-cxn (gold-frame units-with-role cxn-inventory propbank-sentence ts-unit-structure)
  "Learns a construction capturing V + ARGM that is a phrase (but not pp/sbar/s)."
  (let* ((argm-unit (find "ARGM" units-with-role :key #'(lambda (unit-w-role)
                                                          (role-type (car unit-w-role))) :test #'search))
         (argm-string (unit-feature-value (cdr argm-unit) 'string))
         (footprint (make-const 'argm))
         (cxn-units-with-role
          (loop for unit-w-role in units-with-role
                if (equal (role-type (car unit-w-role)) "V")
                collect (make-propbank-conditional-unit-with-role unit-w-role nil footprint :frame-evoking t)
                else collect (make-propbank-conditional-unit-with-role unit-w-role nil footprint :string argm-string)))
         (contributing-unit (make-propbank-contributing-unit units-with-role gold-frame nil footprint :include-gram-category? nil))
         (cxn-units-without-role (make-propbank-conditional-units-without-role units-with-role cxn-units-with-role ts-unit-structure))
         (cxn-name (make-cxn-name units-with-role cxn-units-with-role cxn-units-without-role nil nil nil))
         (schema (loop for (role . nil) in units-with-role
                       for cxn-unit in cxn-units-with-role
                       collect (cons (intern (role-type role))
                                     (cond
                                      ;; unit contains a string
                                      ((feature-value (find 'string (cddr cxn-unit) :key #'feature-name)))
                                      ;; unit contains a phrase-type
                                      ((feature-value (find 'syn-class (cddr cxn-unit) :key #'feature-name)))))))
         (equivalent-cxn (find-equivalent-cxn schema
                                              (syn-classes (append cxn-units-with-role
                                                                   cxn-units-without-role))
                                              cxn-inventory
                                              :hash-key (intern (upcase argm-string)))))

    (if equivalent-cxn
      
      ;;argm-leaf cxn already exists, update its frequency
      (incf (attr-val equivalent-cxn :frequency))
      
      ;;create a argm-leaf cxn
      (when (and cxn-units-with-role (v-lemma units-with-role))
        (eval `(def-fcg-cxn ,cxn-name
                            (,contributing-unit
                             <-
                             ,@cxn-units-with-role
                             ,@cxn-units-without-role)
                            :disable-automatic-footprints t
                            :attributes (:schema ,schema
                                         :lemma ,(intern (upcase argm-string))
                                         :score ,(length cxn-units-with-role)
                                         :label argm-leaf-cxn
                                         :frequency 1)
                            :description ,(sentence-string propbank-sentence)
                            :cxn-inventory ,cxn-inventory))))))


|#
