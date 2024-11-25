(in-package :geoquery-lsfb)

(defun load-data-for-generation (data-path)
  
  ;; vocabulary of naming signs used in the dataset
  (defparameter *naming-signs*
    (jsonl->list-of-json-alists
     (concatenate 'string
      data-path
      "/naming-signs.jsonl")))
  
  ;; the fingerspelling alphabet used to create fingerspellings in hamnosys
  (defparameter *fingerspelling-alphabet*
    (jsonl->list-of-json-alists
     (concatenate 'string
      data-path
      "/fingerspelling-alphabet.jsonl")))

  ;; vocabulary of all state entities in the Geoquery database
  (defparameter *states*
    (jsonl->list-of-json-alists
     (concatenate 'string
      data-path
      "/states.jsonl")))

  ;; vocabulary of all city entities in the Geoquery database
  (defparameter *cities*
    (jsonl->list-of-json-alists
     (concatenate 'string
      data-path
      "/cities.jsonl")))

  ;; vocabulary of all river entities in the Geoquery database
  (defparameter *rivers*
    (jsonl->list-of-json-alists
     (concatenate 'string
      data-path
      "/rivers.jsonl")))

  ;; set a counter for geo-ids
  (defparameter *geo-id-counter*
    880)

  ;; deepl translations for all instances in the large dataset
  (defparameter *deepl-translations*
    (jsonl->list-of-json-alists
     (concatenate 'string
      data-path
      "/translations.jsonl"))))

  


(defun reset-geo-id-counter ()
  "resets the counter used to create new geoquery-ids"
  (setf *geo-id-counter* 880))


(defun find-french-name (geographical-entity type)
  "finds the french name for the given geographical entity that is of a specified type"
  (cond
   ((string= type "state")
    (loop for item in *states*
          do (when (string=
                    (cdr
                     (assoc :name item))
                    geographical-entity)
               (return
                (cdr
                 (assoc :french item))))))
   ((or
     (string= type "city")
     (string= type "capital")
     (string= type "river"))
    geographical-entity)))

(defun find-naming-sign-by-entity (entity)
  "finds the naming sign for an entity in the vocabulary of naming signs"
  (loop for item in *naming-signs*
        do (when (string= (cdr (assoc :meaning item)) entity)
             (return item ))))

(defun find-naming-sign-by-gloss (gloss)
  "finds the naming sign for a gloss in the vocabulary of naming signs"
  (loop for item in *naming-signs*
        do (when (string= (cdr (assoc :id-gloss item)) (string-upcase gloss))
             (return item ))))

(defun create-new-gloss (gloss-prefix naming-sign french-name)
  (cond
   ;; A: the sign is a fingerspelled sign, so create fs gloss
   ((eql
     gloss-prefix
     'fs)
    (replace-spaces
     (concatenate
     'string
     "fs\:"
     french-name)))
   ;; B: the sign is a naming-sign, so extract id-gloss from vocabulary
   (naming-sign
    (cdr
     (assoc
      :id-gloss
      naming-sign)))
   ;; C: no naming sign exists for the entity so we fall back on a fingerspelled form
   (t
    (replace-spaces
     (concatenate
      'string
      "fs\:"
      french-name)))
   )
  )

(defun determine-handedness (gloss-prefix naming-sign)
  "finds the handedness of a sign for which a gloss and gloss-prefix are given"
  (cond
   ;; A: the sign is a fingerspelled sign, so right-handed
   ((eql
     gloss-prefix
     'fs)
    "right-hand-articulation")
   ;; B: there is a naming sign for the entity so we extract its handedness
   (naming-sign
    (cdr
     (assoc
      :handedness
      naming-sign)))
   ;; C: there is no naming sign for the entity so we fall back on fingerspelled form (right-handed)
   (t
    "right-hand-articulation")))

(defun make-hamnosys (gloss-prefix naming-sign french-name)
  (cond
   ;; A: the sign is a fingerspelled sign, so make fingerspelled form
   ((eql gloss-prefix 'fs)
    (make-fingerspelling
     french-name
     :input-type 'name))
   ;; B: there is a naming sign for the entity so we extract its hamnosys
   (naming-sign
    (cdr
     (assoc
      :hamnosys
      naming-sign)))
   ;; C: there is no naming sign for the entity so we fall back on fingerspelled form
   (t
    (make-fingerspelling
     french-name
     :input-type 'name))))

       
(defun replace-gloss-and-hamnosys (original-lsfb original-gloss french-state-name state-name)
  "Replaces original-gloss with an equivalent gloss for state-name. It also replaces
   the hamnosys related to the gloss."
    (loop with output = '()

          ;; collecting necessary information
          with new-naming-sign = (find-naming-sign-by-entity state-name)
          with gloss-prefix = (get-gloss-prefix original-gloss)
          with old-naming-sign = (when (eql gloss-prefix 'ns)
                                   (find-naming-sign-by-gloss original-gloss))
          with old-handedness = (determine-handedness gloss-prefix old-naming-sign)
          with new-handedness = (determine-handedness gloss-prefix new-naming-sign)
          with new-gloss = (create-new-gloss gloss-prefix new-naming-sign french-state-name)
          with new-tag = (make-const new-gloss)
          with new-hamnosys = (make-hamnosys gloss-prefix new-naming-sign french-state-name)


          ;; looping over predicates and replacing things where necessary  
          for predicate in original-lsfb
          for predicate-name = (first predicate)
          for first-arg = (second predicate)
          for second-arg = (third predicate)

          do
            (cond

              ;; Condition A: the predicate name = old handedness and first argument is original-gloss
              ;; --> replace old-handedness with new-handedness and original-gloss with new-gloss
              ((and
                (eql
                 predicate-name
                 (read-from-string old-handedness))
                (string=
                 (utils::get-base-name first-arg)
                 (string-upcase original-gloss)))
               (pushend
                `(,(read-from-string new-handedness)
                  ,new-tag
                  ,new-hamnosys)
                output))

              ;; Condition B: predicate name refers to an alignment predicate and first argument is the original gloss
              ;; --> replace first argument with new gloss
              ((and
                (member
                 predicate-name
                 '(fcg:adjacent
                   start-coincides
                   end-coincides
                   during))
                (string=
                 (utils::get-base-name first-arg)
                 (string-upcase original-gloss)))
               (pushend
                `(,predicate-name
                  ,new-tag
                  ,second-arg)
                output ))
              ;; Condition C: predicate name refers to an alignment predicate and second argument is the original gloss
              ;; --> replace second argument with new gloss
              ((and
                (member
                 predicate-name
                 '(fcg:adjacent
                   start-coincides
                   end-coincides
                   during))
                (string=
                 (utils::get-base-name second-arg)
                 (string-upcase original-gloss)))
               (pushend
                `(,predicate-name
                  ,first-arg
                  ,new-tag)
                output))

              ;; Condition D: all other cases
              ;; --> replace nothing and copy original predicate
              (t
               (pushend predicate output)))

             (reset-id-counters)
          finally (return output)))


(defun geo-replace (reference original-entity new-entity &key (tag-type "en"))
  "extracts original-entity with new-entity in the relevant tag of reference.
   The type of the relevant tag is indicated using tag-type"
  (let ((old-string
         (loop for line in (xmls::node-children reference)
               when (string=
                     (second
                      (first
                       (xmls::node-attrs line)))
                     tag-type)
                 do (return
                     (first
                      (xmls::node-children line))))))
    (string-replace
     old-string
     original-entity
     new-entity)))

(defun replace-prolog (reference original-entity new-entity)
  "extracts the prolog meaning representation from reference (xmls) and replaces
   the occurrence of original-entity with new-entity"
  (let ((old-prolog
         (loop for line in (xmls::node-children reference)
               when (string=
                     (second
                      (first
                       (xmls::node-attrs line)))
                     "geo-prolog")
                 do (return
                     (first
                      (xmls::node-children line)))))
        (geo-new-entity (if
                            (>
                             (length
                              (split-sequence::split-sequence
                               #\space
                               new-entity))
                             1)
                          (format nil "'~a'" new-entity)
                          new-entity))
        (geo-original-entity (if
                                 (>
                                  1
                                  (length
                                   (split-sequence::split-sequence
                                    #\space
                                    original-entity)))
                           (format nil "'~a'" original-entity)
                           original-entity)))
    (string-replace
     old-prolog
     geo-original-entity
     geo-new-entity)))


(defun add-all-variants (reference ;; the xmls-node of the reference annotation
                         representation-number ;; number for the type of meaning representation
                         original-gloss ;; the gloss of the original to-be replaced entity
                         original-meaning-entity ;; the meaning of the original to-be replaced entity
                         250-variants ;; overview of all variants included in multilingual geoquery
                         &key
                         (variable-type 'state)) ;; indicates the type of the to be expanded entity
  
  "adds all possible variants for the provided reference and that are not yet included in the set of 250 variants.
   The keyword variable-type indicates of which type the to be replaced gloss and meaning-entity are
   (i.e. 'state, 'river or 'city)"
  
  (loop with output = '()
          
        with type-lexicon = ;; lexicon with all entities for a given type included in geoquery
          (case variable-type
            ('state *states*)
            ('city *cities*)
            ('river *rivers*))
          
        with 250-variant-names = ;; collect the names of entities included in multilingual set
          (loop for item in 250-variants
                collect
                  (second
                   (split-sequence::split-sequence
                    #\:
                    item)))
          
        for entity in type-lexicon ;; loop over all entities included in the type-lexicon
          
        for entity-name = ;; the name of the new entity
          (cdr (assoc :name entity))
          
        for french-entity-name = ;; the french name of the new entity
          (find-french-name
           entity-name
           (string-downcase
            (string variable-type)))
          
        for new-english = ;; create the new english form
          (geo-replace
           reference
           original-meaning-entity
           (capitalise entity-name)
           :tag-type "en")
          
        for new-funql = ;; create the new funql meaning representation
          (geo-replace
           reference
           original-meaning-entity
           entity-name
           :tag-type "geo-funql")
          
        for new-prolog = ;; create the new prolog meaning representation
          (replace-prolog
           reference
           original-meaning-entity
           entity-name)
          
        for elan-ref = (find-elan-ref reference) ;; find the elan-reference
          
        for original-lsfb = ;; find the original-lsfb-representation
          (string->predicates
           (find-lsfb reference))
          
        for new-lsfb = ;; the new lsfb-form 
          (predicates->string
           (replace-gloss-and-hamnosys
            original-lsfb
            original-gloss
            french-entity-name
            entity-name))
          
        for new-id = *geo-id-counter* ;; the id for the new variant 
          
        do (unless
               ;; check whether variant is not yet included in the dataset
               (or (member
                    entity-name
                    250-variant-names
                    :test #'string=)
                   (string=
                    entity-name
                    original-meaning-entity))
             ;; push the new node for the variant to the output dataset file
             (push
              (xmls:make-node
               :name "example"
               :attrs `(("id" ,new-id)
                        ("type" ,representation-number))
               :children (list
                          (xmls:make-node :name "nl"
                                          :attrs `(("lang" "en"))
                                          :children `(,new-english))
                          (xmls:make-node :name "nl"
                                          :attrs `(("lang" "es"))
                                          :children `())
                          (xmls:make-node :name "nl"
                                          :attrs `(("lang" "ja"))
                                          :children `())
                          (xmls:make-node :name "nl"
                                          :attrs `(("lang" "tr"))
                                          :children `())
                          (xmls:make-node :name "nl"
                                          :attrs `(("lang" "lsfb")
                                                   ("elan-ref" ,elan-ref))
                                          :children `(,new-lsfb))
                          (xmls:make-node :name "nl"
                                          :attrs `(("lang" "fr"))
                                          :children `())
                          (xmls:make-node :name "mrl"
                                          :attrs `(("lang" "geo-funql"))
                                          :children `(,new-funql))
                          (xmls:make-node :name "mrl"
                                          :attrs `(("lang" "geo-prolog"))
                                          :children `(,new-prolog))))
              output)
             ;; increase the id-counter in preparation of the next iteration
             (incf *geo-id-counter*))
        ;; return the reversed output
        finally (return (reverse output))))

(defun add-all-capital-variants (reference ;; the xmls-node of the reference annotation
                                 representation-number ;; number for the type of meaning representation
                                 original-gloss ;; the gloss of the original to-be replaced entity
                                 original-meaning-entity ;; the meaning of the original to-be replaced entity
                                 250-variants) ;; overview of all variants included in multilingual geoquery
  
  "adds all possible variants for the provided reference and that are not yet included in the set of 250 variants.
  The function can only be used for references where a capital needs to be replaced."
  
  (loop with 250-variant-names = ;; collect the names of entities included in multilingual set
          (loop for item in 250-variants
                collect
                  (second
                   (split-sequence::split-sequence
                    #\:
                    item)))

        with output = '()

        ;; loop through all states in the states vocabulary
        for state in *states*
          
        for capital-name = ;; select the name of the state's capital
          (cdr
           (assoc
            :capital
            state)) 

         
        for new-english = ;; create the new english form
          (geo-replace
           reference
           original-meaning-entity
           (capitalise capital-name)
           :tag-type "en")
          
        for new-funql = ;; create the new funql meaning representation
          (geo-replace
           reference
           original-meaning-entity
           capital-name
           :tag-type
           "funql")
          
        for new-prolog = ;; create the new prolog meaning representation
          (replace-prolog
           reference
           original-meaning-entity
           capital-name)
          
        for elan-ref = (find-elan-ref reference) ;; find the elan-reference
          
        for original-lsfb = ;; find the original-lsfb-representation
          (string->predicates
           (find-lsfb reference))
          
        for new-lsfb = ;; the new lsfb-form 
          (predicates->string
           (replace-gloss-and-hamnosys
            original-lsfb
            original-gloss
            capital-name
            capital-name))
          
        for new-id = *geo-id-counter* ;; the id for the new variant 
          
        do (unless
                ;; check whether variant is not yet included in the dataset
               (or
                (member
                 capital-name
                 250-variant-names
                 :test #'string=)
                (string=
                 capital-name
                 original-meaning-entity))
             
             (pushend
              (xmls:make-node
               :name "example"
               :attrs `(("id" ,new-id)
                        ("type" ,representation-number))
               :children (list
                          (xmls:make-node :name "nl"
                                          :attrs `(("lang" "en"))
                                          :children `(,new-english))
                          (xmls:make-node :name "nl"
                                          :attrs `(("lang" "es"))
                                          :children `())
                          (xmls:make-node :name "nl"
                                          :attrs `(("lang" "ja"))
                                          :children `())
                          (xmls:make-node :name "nl"
                                          :attrs `(("lang" "tr"))
                                          :children `())
                          (xmls:make-node :name "nl"
                                          :attrs `(("lang" "lsfb")
                                                   ("elan-ref" ,elan-ref))
                                          :children `(,new-lsfb))
                          (xmls:make-node :name "nl"
                                          :attrs `(("lang" "fr"))
                                          :children `())
                          (xmls:make-node :name "mrl"
                                          :attrs `(("lang" "geo-funql"))
                                          :children `(,new-funql))
                          (xmls:make-node :name "mrl"
                                          :attrs `(("lang" "geo-prolog"))
                                          :children `(,new-prolog))))
                      output)
             ;; increase the id-counter in preparation of the next iteration
             (incf *geo-id-counter*))
        finally (return output)))

(defun make-type-overview (xml-path)
  "creates an overview of the number of instances included in the 250 and large dataset version"
(loop with 250-counter = 0 ;; a counter for all variants included in 250 dataset
      with large-counter = 0 ;; a counter for all variants included in large dataset
      with type-counters = '() ;; a list keeping counts of variants per meaning type

      ;; loop over all examples in the xml dataset
      for example in (xmls:node-children (read-xml xml-path))
        
      for id = (find-id example) ;; the id of the example
        
      for lsfb = (find-lsfb example) ;; the lsfb of the example
      for type-nr = (find-type-nr example) ;; the lsfb of the example
      
      ;; when the id is smaller than 880 and contains an lsfb-form, it is part of the
      ;; 250 dataset so we increase its counter
      when (and
            (< id 880)
            lsfb)
        do (incf 250-counter)

      ;; when the example contains an lsfb form, it is part of the large dataset so
      ;; we increase its counter
      when lsfb
        do (incf large-counter)
           
      do (if (assoc
              (parse-integer type-nr)
              type-counters)
           ;; if the type-nr is already in the overview-list, increase its counter
           (incf
            (cdr
             (assoc
              (parse-integer type-nr)
              type-counters)))
           ;; if the type-nr is not yet in the overview-list, add new counter of 1
           (pushend
            (cons
             (parse-integer type-nr)
             1)
            type-counters))
        
      finally
        ;; pretty printing the list of all types and their counters
        (pprint type-counters)
        ;; pretty printing the number of different types
        (pprint (loop with output-nr = 0
                            for counter in type-counters
                            do (incf output-nr (cdr counter))
                            finally (return output-nr)))
        ;; pretty printing the number of examples included in 250 and large datasets
        (pprint `(,250-counter ,large-counter))))