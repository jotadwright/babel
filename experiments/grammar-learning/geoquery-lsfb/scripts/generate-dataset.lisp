(ql:quickload :geoquery-lsfb)
(in-package :geoquery-lsfb)

;(reset-geo-id-counter)
;-------------------------------------------------------------------------------
; Step 1: extract lsfb and french from elan-files and add them to GeoQuery.xml
;-------------------------------------------------------------------------------

(defun templates->xml (elan-files-directory ;; directory containing all elan-files
                       path-to-original-xml ;; path the the original xml file 
                       path-to-new-xml) ;; path where new xml file should be saved
  
  "extracts lsfb and french from the elan-files in elan-files directory and adds them to the original-xml
   located at path-to-original-xml. The newly created xml is saved to a new location (path-to-new-xml)"
  
    (loop with templates = (directory elan-files-directory) ;; get all templates from the directory
          with new-xml = (read-xml path-to-original-xml) ;; copy original-xml which will be modified
          
          ;; loop over all templates in the directory
          for template in templates
          
          ;; get the xmls of the template
          for template-xmls = (read-xml template)

          ;; get the elan-ref of the template 
          for elan-ref = (pathname-name template)

          ;; get the id of the template
          for template-id =
            (third
             (split-sequence::split-sequence
              #\_
              elan-ref))
          
          ;; get the type-nr of the template  
          for type-nr =
            (first
             (split-sequence::split-sequence
              #\_
              elan-ref))

          ;; make predicates for the annotation in the file
          for lsfb-predicates = (elan->predicates template-xmls)

          ;; create string form of the predicates
          for lsfb-string =
             (list-of-predicates->string-of-predicates
              lsfb-predicates)
          
          ;; extract the french translation
          for french =
            (find-tier-by-tier-id
             template-xmls
             :target-tier-id 'french-translation)

          ;; find the example of the xml that corresponds to the template id
          ;; add the french and lsfb tags
          do (loop for example in (xmls:node-children new-xml)
                   for example-id =
                     (second
                      (first
                       (xmls:node-attrs example)))
                   when
                     (string=
                      example-id
                      template-id)
                     do
                       
                       ;; insert the lsfb form at position 4
                       (insert-after
                         (xmls:node-children example)
                         3
                         (xmls:make-node
                          :name "nl"
                          :attrs `(("lang" "lsfb")
                                   ("elan-ref" ,elan-ref))
                          :children `(,lsfb-string)))
                       
                       ;; insert french form at position 5
                        (insert-after
                         (xmls:node-children example)
                         4
                         (xmls:make-node
                          :name "nl"
                          :attrs `(("lang" "fr"))
                          :children `(,french)))
                        
                        (push `("type" ,type-nr)
                              (xmls::node-attrs example))
                       
                        (return))
          finally
            ;; write the modified xml to file
            (with-open-file
                (out-stream
                 path-to-new-xml
                 :direction :output
                 :if-exists :supersede
                 :if-does-not-exist :create
                 :external-format :utf-8
                 :element-type 'cl:character)
              (xmls:write-xml
               new-xml
               out-stream
               :indent t))))


;(templates->xml "/Users/liesbetdevos/Projects/GeoQuery-LSFB/elan-files/*.eaf" "/Users/liesbetdevos/Projects/geoquery-sign/original-geoquery.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-templates.xml")

;-------------------------------------------------------------------------------
; Step 2: extend the templates towards all 250 examples of multilingual GeoQuery
;-------------------------------------------------------------------------------

(defun create-250-xml (path-to-templates-xml ;; path to the xml that has lsfb and french for all templates
                       path-to-new-xml ;; path where new xml file should be saved
                       path-to-overview) ;; path to the overview of all variants included in 250 dataset
  
  "reads in the xml file containing the generated templates and uses the overview of all instances in GeoQuery multilingual
   (located at path-to-overview) to generate a new xml that contains an lsfb form for all these instances.
   The new xml is saved at a new location (path-to-new-xml)"
  
    (loop with overview = (jsonl->list-of-json-alists path-to-overview)
          with new-xml = (read-xml path-to-templates-xml) ;; copy original-xml which will be modified
          
          ;; loop over all examples in the overview
          for example in overview

          ;; the variants for the meaning-type that are included in the 250 version
          for variants = 
            (cdr
             (assoc
              :250-instances
              example))

          ;; the id of the example
          for example-id = 
            (parse-integer
             (cdr
              (assoc
               :id
               example)))

          ;;find the reference
          for reference =
            (when variants
              (find-example-by-id new-xml example-id))

          ;; the type-nr
          for type-nr =
            (cdr
             (assoc
              :type
              example))
          
          ;; find the original lsfb
          for original-lsfb = (when reference
                                (find-lsfb reference))
          
          ;; list version of predicates
          for predicates-lsfb =
            (when original-lsfb
              (string->predicates original-lsfb))
          
          ;; find the original gloss in the overview
          for original-gloss = (cdr (assoc :original-gloss example))

          ;; check whether the example has variants that need to be extended
          when variants
            do
              ;; loop over each variant
              (loop for variant in variants
                    for variant-characteristics =
                      (split-sequence::split-sequence
                       #\:
                       variant)
                    
                    ;; the id of the variant
                    for variant-id =
                      (parse-integer
                       (first variant-characteristics))


                    ;; get the new meaning entity from the overview
                    for new-meaning = (second variant-characteristics)

                    ;; get the french name for the new entity
                    for new-french-name =
                      (find-french-name
                       new-meaning
                        (cdr
                         (assoc
                          :variable-type
                          example)))
                    ;; create a new lsfb for the variant
                    for new-lsfb =
                       (list-of-predicates->string-of-predicates
                        (replace-gloss-and-hamnosys
                         predicates-lsfb
                         original-gloss
                         new-french-name
                         new-meaning))

                    ;;find the elan-ref
                    for elan-ref = (find-elan-ref reference)

                    ;; find the example of the variant in the xml and add lsfb and french to it
                    do (loop for xml-line in (xmls::node-children new-xml)
                              when (=
                                    ;; find the id of the example
                                    (find-id xml-line)
                                    variant-id)
                                do

                                 ;; insert lsfb at position 4
                                 (insert-after
                                   (xmls:node-children xml-line)
                                   3
                                   (xmls:make-node
                                    :name "nl"
                                    :attrs `(("lang" "lsfb")
                                             ("elan-ref" ,elan-ref))
                                    :children `(,new-lsfb)))

                                 ;; insert french at position 5
                                 (insert-after
                                  (xmls:node-children xml-line)
                                  4
                                  (xmls:make-node
                                   :name "nl"
                                   :attrs `(("lang" "fr"))
                                   :children `()))

                                 ;; add type attribute to the example's attributes
                                 (pushend
                                  `("type" ,type-nr)
                                  (xmls::node-attrs xml-line))
                                 (return)))
                    
            ;; write the new xml to file
                    finally
            (with-open-file
                (out-stream
                 path-to-new-xml
                 :direction :output
                 :if-exists :supersede
                 :if-does-not-exist :create
                 :external-format :utf-8
                 :element-type 'cl:character)
              (xmls:write-xml
               new-xml
               out-stream
               :indent t))))

;(create-250-xml "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-templates.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-250-new.xml" "/Users/liesbetdevos/Projects/babel/experiments/grammar-learning/geoquery-lsfb/data/overview.jsonl")

;--------------------------;
; Step 3: add special case ;
;--------------------------;
; for this example, the name of a city "des moines" is signed by combining a fingerspelled form with a regular sign (FS:DES + MOINE).
; To extend this example to the other variants, both should be replaced which is not captured by the regular expand-all-variants function.
; This function handles this case

(defun expand-single-type (path-to-original-xml ;; path to the original xml dataset
                           path-to-new-xml ;; path where the new xml should be saved
                           path-to-template ;; path to the template of the special case
                           original-gloss ;; the original to-be replaced gloss
                           original-meaning-entity ;; the original to-be replaced meaning-entity
                           type-nr ;; the original type-nr
                           variants-not-to-include 
                           elan-ref)
  
  "creates all variants for one single geoquery meaning representation type."
  
  (let* (;; read in the original xml
         (original-xml
          (read-xml path-to-original-xml)) 

         ;; copy original-xml which will be modified
         (new-xml original-xml)

         ;; read the template 
         (template-xmls
          (read-xml path-to-template))

         ;; create lsfb predicates for the template  
         (template-predicates
          (elan->predicates template-xmls))

         ;; string version of the predicates
         (template-string-predicates
           (list-of-predicates->string-of-predicates template-predicates))

         ;; retrieve the english translation
         (english
          (find-tier-by-tier-id
           template-xmls
           :target-tier-id 'english-translation))

         ;; retrieve the funql meaning representation
         (funql
          (find-tier-by-tier-id
           template-xmls
           :target-tier-id 'geofunql))

         ;; retrieve the prolog meaning representation
         (prolog
          (find-tier-by-tier-id
           template-xmls
           :target-tier-id 'geoprolog))

         ;; the id for the reference
         (new-id *geo-id-counter*)

         ;; create node for the reference
         (reference
          (xmls:make-node
           :name "example"
           :attrs `(("id" ,new-id)("type" ,type-nr))
           :children
           (list
            (xmls:make-node :name "nl"
                            :attrs `(("lang" "en"))
                            :children `(,english))
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
                            :children `(,template-string-predicates))
            (xmls:make-node :name "nl"
                            :attrs `(("lang" "fr"))
                            :children `())
            (xmls:make-node :name "mrl"
                            :attrs `(("lang" "geo-funql"))
                            :children `(,funql))
            (xmls:make-node :name "mrl"
                            :attrs `(("lang" "geo-prolog"))
                            :children `(,prolog))))))
    ;; increase geo-counter for next iteration
    (incf *geo-id-counter*)

    ;; push the example to the list of examples
    (pushend reference (xmls:node-children new-xml))

    ;; extend the example and add all variants to the list of examples
    (setf
     (xmls:node-children new-xml)
     (append
      (xmls::node-children new-xml)
      (add-all-capital-variants
       reference
       type-nr
       original-gloss
       original-meaning-entity
       variants-not-to-include)))
    
    ;; write the new xml to file
    (with-open-file
        (out-stream
         path-to-new-xml
         :direction :output
         :if-exists :supersede
         :if-does-not-exist :create
         :external-format :utf-8
         :element-type 'cl:character)
      (xmls:write-xml
       new-xml
       out-stream
       :indent t))))

;(expand-single-type "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-250-new.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-250-new-special.xml" "/Users/liesbetdevos/Documents/46_0_881.eaf" "fs:montgomery" "montgomery" "45" '("392:des moines" "881:montgomery") "45_0_392")

;---------------------------------;
; Step 4: create extended dataset ;
;---------------------------------;

(defun 250-to-large-xml (path-to-original-xml path-to-new-xml path-to-overview)
  
  "extends the 250 version of the dataset located at path-to-original-xml to the large version by adding all possible variants.
   It saves the new version at path-to-new-xml. It makes sure not to enclude variants already icluded in the 250 version using the overview
   located at path to overview"
  (loop
     
     ;; read the overview
     with overview = (jsonl->list-of-json-alists
                         path-to-overview)
     ;; new-xml is a copy of the original xml we will modify
     with new-xml = (read-xml
                    path-to-original-xml)

     ;; loop over all the examples in the overview
     for example in overview

     ;; the id of the example
     for reference-id = (parse-integer (cdr (assoc :id example)))

     ;; the type-nr of the example 
     for type-nr = (cdr (assoc :type example))

     ;; the variants included in the 250 dataset
     for 250-variants = (cdr (assoc :250-instances example))

     ;; the type of the entity that will be extended
     for variable-type =
       (read-from-string
        (cdr
         (assoc
          :variable-type
          example)))

     ;; the original to-be replaced gloss
     for original-gloss =
       (cdr
        (assoc
         :original-gloss
         example))

     ;; the original to-be replaced meaning entity
     for original-meaning-entity =
       (cdr
        (assoc
         :original-meaning
         example))
                              
     ;; the reference
     for reference = (find-example-by-id new-xml reference-id)

     ;; create the new variants
     for new-variants =
       (case variable-type
         
         ;; Case A: the to-be replaced entity is a state,
         ;; so extend it with all states 
         ('state
          (add-all-variants
           reference
           type-nr
           original-gloss
           original-meaning-entity
           250-variants
           :variable-type 'state))
         
         ;; Case A: the to-be replaced entity is a city,
         ;; so extend it with all cities
         ('city
          (add-all-variants
           reference
           type-nr
           original-gloss
           original-meaning-entity
           250-variants
           :variable-type
           'city))
         
         ;; Case A: the to-be replaced entity is a river,
         ;; so extend it with all rivers 
         ('river
          (add-all-variants
           reference
           type-nr
           original-gloss
           original-meaning-entity
           250-variants
           :variable-type
           'river))
         
         ;; Case A: the to-be replaced entity is a capital,
         ;; so extend it with all capitals 
         ('capital
          (add-all-capital-variants
           reference
           type-nr
           original-gloss
           original-meaning-entity
           250-variants)))

     ;; append the new examples to the example list
     do (setf
         (xmls:node-children
          new-xml)
         (append
          (xmls::node-children new-xml)
          new-variants))
     
     ;; write the new xml to file
     finally
       (with-open-file
           (out-stream
            path-to-new-xml
            :direction :output
            :if-exists :supersede
            :if-does-not-exist :create
            :external-format :utf-8
            :element-type 'cl:character)
         (xmls:write-xml
          new-xml
          out-stream
          :indent t))))

; (250-to-large-xml "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-250-new-special.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-large.xml" "/Users/liesbetdevos/Projects/babel/experiments/grammar-learning/geoquery-lsfb/data/overview.jsonl")

;------------------------------------;
; Step 5: create second special type ;
;------------------------------------;

; another special type where both a city and a state should be replaced in the original example to create the variants. 

; overview of the variants of this type included in the 250 version. It shows the id of the example, the city name and state name.
(defparameter *18-0-87-overview*
  '(("97" "spokane" "washington")
    ("510" "boston" "massachusetts")
    ("515" "erie" "pennsylvania")
    ("528" "portland" "maine")
    ("533" "seattle" "washington")
    ("535" "springfield" "missouri")
    ("537" "tempe" "arizona")))


(defun already-included-variant (state-name ;; the name of the new state
                                 city-name ;; the name of the new city
                                 example-overview ;; the overview for the example
                                 old-state-meaning ;; the original state
                                 old-city-meaning ;; the original city
                                 )
  
  "checks whether there already is a variant with the given state-name and city-name included in the set of 250 variants (or if it is the same as the original template reference"
  
  (let ((included? nil))
    ;; check whether the city and state combination are not the same as the original example one's
    (when
        (and
         (string=
          state-name
          old-state-meaning)
         (string=
          city-name
          old-city-meaning))
      (setf included? t))

    ;; check whether the state and city combination are not yet included in the 250 variants
    (unless included?
      (loop for item in example-overview
            when
              (and
               (string=
                state-name
                (third item))
               (string=
                city-name
                (second item)))
              do (setf included? t)))
    included?))

(defun extend-city-state (old-xml-path ;; path to the original xml
                          path-to-template ;; path to the template of the example
                          new-xml-path ;; path where the new xml should be saved
                          example-overview ;; the overview of the specific example
                          old-state-gloss ;; the to be replaced gloss for a state
                          old-city-gloss ;; the to be replaced gloss for the city
                          old-state-meaning ;; the meaning entity for the to-be replaced state
                          old-city-meaning ;; the meaning entity for the to-be replaced city
                          reference-id ;; the id of the reference
                          type-nr ;; the type-nr of the example
                          old-state-abbreviation ;; the abbreviation of the to-be replaced state
                          )
  
  "extends an example where both a city and a state should be replaced by all possible variants"
  
  (let* ( ;; a copy of the original xml which we will modify
         (new-xml (read-xml old-xml-path))

         ;; read the template 
         (template-xmls (read-xml path-to-template))

         ;; create predicates for the template
         (template-predicates (elan->predicates template-xmls))

         ;; create an elan-ref
         (elan-ref (pathname-name path-to-template))

         ;; find the reference-example
         (reference-example (find-example-by-id new-xml (parse-integer reference-id))))

         ;; loop over all the items in the overview
         (loop for item in example-overview

               ;; the id of the new variant
               for variant-id = (first item)

               ;; the city entity of the new variant
               for new-city-meaning = (second item)

               ;; the state entity of the new variant
               for new-state-meaning = (third item)

               ;; the french name for the new state
               for new-french-state-meaning =
                 (find-french-name
                  new-state-meaning
                  "state")

               ;; the new lsfb
               for new-lsfb =
                 (list-of-predicates->string-of-predicates
                  (replace-gloss-and-hamnosys
                   (replace-gloss-and-hamnosys
                    template-predicates
                    old-state-gloss
                    new-french-state-meaning
                    new-state-meaning)
                   old-city-gloss
                   new-city-meaning
                   new-city-meaning))

               
               do (loop for xml-line in (xmls::node-children new-xml)
                        ;; find the example with the id in the xml-file  
                        when (=
                             (find-id xml-line)
                             (parse-integer variant-id))
                          do
                            
                            ;; add lsfb to the example at position 4
                            (insert-after
                             (xmls:node-children xml-line)
                             3
                             (xmls:make-node
                              :name "nl"
                              :attrs `(("lang" "lsfb")
                                       ("elan-ref" ,elan-ref))
                              :children `(,new-lsfb)))

                            ;; add french to the example at position 5
                            (insert-after
                             (xmls:node-children xml-line)
                             4
                             (xmls:make-node :name "nl"
                                             :attrs `(("lang" "fr"))
                                             :children `()))
                             (pushend `("type" ,type-nr) (xmls::node-attrs xml-line))
                            
                             ;; once added, end the loop early
                             (return)))
                  
         ;; extend the example further with the variants not included in 250 dataset       
         (loop for city in *cities*
               
               ;; the new city name
               for city-name =
                 (cdr
                  (assoc
                   :name city))

               ;; the new state name
               for state-name =
                 (cdr
                  (assoc
                   :state city))

               ;; the new state-abbreviation
               for state-abbr =
                 (loop for state in *states*
                       do (when
                              (string=
                               (cdr
                                (assoc
                                 :name
                                 state))
                               state-name)
                            (return
                             (cdr
                              (assoc
                               :abbreviation
                               state)))))

               ;; the french name of the new state
               for new-french-state-meaning =
                 (find-french-name
                  state-name
                  "state")

               ;; the new english
               for new-english =
                 (string-replace
                  (geo-replace
                   reference-example
                   old-state-meaning
                   (capitalise state-name)
                   :tag-type "en")
                  old-city-meaning
                  (capitalise city-name))

               ;; new funql
               for new-funql =
                 (string-replace
                  (geo-replace
                   reference-example
                   old-city-meaning
                   city-name
                   :tag-type "geo-funql")
                  old-state-abbreviation state-abbr)

               ;; new prolog
               for new-prolog =
                 (string-replace
                  (replace-prolog
                   reference-example
                   old-city-meaning
                   city-name)
                  old-state-abbreviation
                  state-abbr)

               ;; new lsfb
               for new-lsfb =
                 (list-of-predicates->string-of-predicates
                  (replace-gloss-and-hamnosys
                   (replace-gloss-and-hamnosys
                    template-predicates
                    old-state-gloss
                    new-french-state-meaning
                    state-name)
                   old-city-gloss
                   city-name
                   city-name))

              ;; the id of the new example
              for new-id = *geo-id-counter*
               do
                 ;; unless the variant is already included in the dataset 
                 (unless
                      (already-included-variant
                       state-name
                       city-name
                       example-overview
                       old-state-meaning
                       old-city-meaning)
                    ;; create a new xmls node for it 
                    (pushend
                     (xmls:make-node
                      :name "example"
                      :attrs `(("id" ,new-id)
                               ("type" ,type-nr))
                      :children
                      (list
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
                                       :attrs `(("lang" "lsfb")("elan-ref" ,elan-ref))
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
                     (xmls::node-children new-xml))
                    ;; increase the geo-id-counter for the next iteration
                    (incf *geo-id-counter*)))
         ;; write the new xml to file
         (with-open-file
             (out-stream
              new-xml-path
              :direction :output
              :if-exists :supersede
              :if-does-not-exist :create
              :external-format :utf-8
              :element-type 'cl:character)
           (xmls:write-xml new-xml out-stream :indent t))))

;(extend-city-state "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-large.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/elan-files/18_0_87.eaf" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-large-special-1.xml" *18-0-87-overview* "fs:minnesota" "fs:minneapolis" "minnesota" "minneapolis" "87" "18" "mn")

;-----------------------------------------------------------------------------------------------
; step 5: remove instances from the dataset file that are not part of the multilingual version
;-----------------------------------------------------------------------------------------------

(defun remove-non-multilingual-instances (path-to-original ;; path to the original large dataset
                                          path-to-new-250 ;; path to the new 250 version of the dataset
                                          path-to-new-large) ;; path to the new large version of the dataset
  
  "removes all instances from the dataset that are not part of the multilingual version.
   It also creates the 250 version from the large version of the dataset."

    (loop
       
       ;; read the original dataset
       with original = (read-xml path-to-original)
       
       ;; a new xmls-node for the 250 version dataset
       with 250-data = (xmls:make-node :name "examples"
                                       :children `())
       
       ;; a new xmls-node for the large version of the dataset
       with large-data = (xmls:make-node :name "examples"
                                         :children `())
       ;; loop over all examples in original dataset
       for example in (xmls:node-children original)

       ;; the id of the example
       for id = (find-id example)

       ;; the lsfb of the example 
       for lsfb = (find-lsfb example)

       
       do (cond
           
           ;; Condition A: the example has an lsfb form and an id smaller than 880
           ;; the example is part of both dataset versions, so push it to both
           ((and
             (< id 880)
             lsfb)
            (push
             example
             (xmls:node-children
              250-data))
            (push
             example
             (xmls:node-children
              large-data)))
           
           ;; Condition B: the example has an lsfb form but id higher than 880
           ;; the example is part of the larger dataset only, so only push it to that one
           (lsfb
            (push
             example
             (xmls:node-children
              large-data))))
          
          ;; writing 250 version to file
          finally
         (setf (xmls:node-children 250-data)
               (reverse (xmls:node-children 250-data)))
         (setf (xmls:node-children large-data)
               (reverse (xmls:node-children large-data)))
         (with-open-file
             (out-stream
              path-to-new-250
              :direction :output
              :if-exists :supersede
              :if-does-not-exist :create
              :external-format :utf-8
              :element-type 'cl:character)
           (xmls:write-xml
            250-data
            out-stream
            :indent t))
         
         ;; writing large version to file
         (with-open-file
             (out-stream
              path-to-new-large
              :direction :output
              :if-exists :supersede
              :if-does-not-exist :create
              :external-format :utf-8
              :element-type 'cl:character)
           (xmls:write-xml
            large-data
            out-stream
            :indent t))))
                    
                    
;(remove-non-multilingual-instances  "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-large-special-1.xml" "/Users/liesbetdevos/Projects/geoquery-LSFB/250-final.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/large-final.xml")


;---------------------------;
; step 6: remove parse data ;
;---------------------------;

(defun remove-parse-data (path-to-original
                          path-to-new)
  "removes syntactic parses from the dataset"
  (loop with original-xml = (read-xml path-to-original)
        with new-xml-examples = '()
        for example in (xmls:node-children original-xml)
        for example-id = (write-to-string (find-id example))
        for example-type = (write-to-string (find-type-nr example))
        for example-children = '()
        do (loop for tag in (xmls:node-children example)
                 unless (member
                         (xmls:node-name tag)
                         '("syn""mrl-parse""augsyn")
                         :test #'string= )
                   do (push tag example-children))
           (push
            (xmls:make-node
             :name "example"
             :attrs `(("type" ,example-type)
                      ("id" ,example-id))
             :children (reverse example-children))
            new-xml-examples)
        finally
          (with-open-file
              (out-stream
               path-to-new
               :direction :output
               :if-exists :supersede
               :if-does-not-exist :create
               :external-format :utf-8
               :element-type 'cl:character)
            (xmls:write-xml
             (xmls:make-node
              :name "examples"
              :children (reverse new-xml-examples))
             out-stream
             :indent t))))

;(remove-parse-data "/Users/liesbetdevos/Projects/geoquery-LSFB/250-final.xml" "/Users/liesbetdevos/Projects/geoquery-LSFB/250-final-2.xml")

;(remove-parse-data "/Users/liesbetdevos/Projects/geoquery-LSFB/large-final.xml" "/Users/liesbetdevos/Projects/geoquery-LSFB/large-final-2.xml")

;--------------------------------;
; step 7: add deepl translations ;
;--------------------------------;

(defun add-deepl-translations (path-to-original
                               path-to-new)
  (loop with new-xml = (read-xml path-to-original)
        for example in (xmls:node-children new-xml)
        for example-id = (find-id example)
        for translations = (loop for item in *deepl-translations*
                                 for item-id = (second
                                                 (assoc :id item))
                                 when (=
                                       item-id
                                       example-id)
                                   do (return item))
        for new-spanish = (second (assoc :spanish translations))
        for new-turkish = (second (assoc :turkish translations))
        for new-japanese = (second (assoc :japanese translations))
        do (loop for child in (xmls:node-children example)
                 for node-name = (xmls:node-name child)
                 for node-attr = (second (first (xmls:node-attrs child)))
                 do (cond
                     ((and (string= node-name "nl")
                           (string= node-attr "es"))
                      (setf (xmls::node-children child)
                            `(,new-spanish)))
                     ((and (string= node-name "nl")
                           (string= node-attr "tr"))
                      (setf (xmls::node-children child)
                            `(,new-turkish)))
                     ((and (string= node-name "nl")
                           (string= node-attr "ja"))
                      (setf (xmls::node-children child)
                            `(,new-japanese)))))
        finally
          (with-open-file
              (out-stream
               path-to-new
               :direction :output
               :if-exists :supersede
               :if-does-not-exist :create
               :external-format :utf-8
               :element-type 'cl:character)
            (xmls:write-xml
             new-xml
             out-stream
             :indent t))))
                                       
;(add-deepl-translations "/Users/liesbetdevos/Projects/GeoQuery-LSFB/large-final-2.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/large-final-3.xml")

;(add-deepl-translations "/Users/liesbetdevos/Projects/GeoQuery-LSFB/250-final-2.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/250-final-3.xml")
           

;--------------------------------------------;
; step 6: create json version of the dataset ;
;--------------------------------------------;


(defun xml->json (xml-path json-path)
  "transforms an xml dataset file into json format"
  
  ;;creating a stream for the output
  (with-open-file
      (out-stream
       json-path
       :direction :output
       :if-exists :supersede
       :if-does-not-exist :create
       :external-format :utf-8
       :element-type 'cl:character)

    
    (loop with xml-data = (read-xml xml-path) ;; reading the original dataset

          ;; loop over the examples in the original dataset
          for example in (xmls:node-children xml-data)
            
          for id = nil
          for type = nil
          for english = nil
          for spanish = nil
          for turkish = nil
          for japanese = nil
          for french = nil
          for german = nil
          for greek = nil
          for thai = nil
          for chinese = nil
          for farsi = nil
          for indonesian = nil
          for swedish = nil
          for lsfb = nil
          for funql = nil
          for prolog = nil
          for elan-ref = nil
            
          do
            ;; extracting attributes from the node 
            (loop for attr in (xmls:node-attrs example)
                  do (cond
                      ((string=
                        (first attr)
                        "id")
                       (setf
                        id
                        (second
                         attr)))
                      ((string=
                        (first attr)
                        "type")
                       (setf
                        type
                        (second attr)))))

            ;; extracting information from the node's child tags
            (loop for child in (xmls:node-children example)
                  do
                    (cond
                     ;; Condition A: the node is the english translation node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "nl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "en"))
                      (setf
                       english
                       (first
                        (xmls:node-children child))))
                     
                     ;; Condition B: the node is the spanish translation node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "nl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "es"))
                      (setf
                       spanish
                       (first
                        (xmls:node-children child))))

                     ;; Condition C: the node is the turkish translation node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "nl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "tr"))
                      (setf
                       turkish
                       (first
                        (xmls:node-children child))))

                     ;; Condition D: the node is the japanese translation node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "nl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "ja"))
                      (setf
                       japanese
                       (first
                        (xmls:node-children child))))

                     ;; Condition E: the node is the french translation node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "nl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "fr"))
                      (setf
                       french
                       (first
                        (xmls:node-children child))))
                     ;; Condition F: the node is the german translation node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "nl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "de"))
                      (setf
                       german
                       (first
                        (xmls:node-children child))))
                     ;; Condition E: the node is the greek translation node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "nl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "el"))
                      (setf
                       greek
                       (first
                        (xmls:node-children child))))
                     ;; Condition F: the node is the thai translation node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "nl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "th"))
                      (setf
                       thai
                       (first
                        (xmls:node-children child))))
                     ;; Condition G: the node is the chinese translation node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "nl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "zh"))
                      (setf
                       chinese
                       (first
                        (xmls:node-children child))))
                     ;; Condition H: the node is the farsi translation node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "nl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "fa"))
                      (setf
                       farsi
                       (first
                        (xmls:node-children child))))
                     ;; Condition I: the node is the indonesian translation node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "nl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "id"))
                      (setf
                       indonesian
                       (first
                        (xmls:node-children child))))
                     ;; Condition J: the node is the swedish translation node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "nl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "sv"))
                      (setf
                       swedish
                       (first
                        (xmls:node-children child))))

                     ;; Condition K: the node is the lsfb translation node
                     ((string=
                       (xmls:node-name child)
                       "nl")
                      (loop for attr in (xmls:node-attrs child)
                            when (string= (first attr) "elan-ref")
                              do (setf elan-ref (second attr))
                                 (setf lsfb (xmls:node-children child))))

                      ;; Condition L: the node is the funql meaning node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "mrl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "geo-funql"))
                      (setf
                       funql
                       (first
                        (xmls:node-children child))))

                     ;; Condition M: the node is the prolog meaning node
                     ((and
                       (string=
                        (xmls:node-name child)
                        "mrl")
                       (string=
                        (second
                         (first
                          (xmls:node-attrs child)))
                        "geo-prolog"))
                      (setf
                       prolog
                       (first
                        (xmls:node-children
                         child))))))
          
          ;; write the information in json format to stream
          ;; note: use format instead of json-encode so that hamnosys strings can be encoded 
          do (format
              out-stream
              "{\"id\":\"~a\",\"type\":\"~a\",\"elan-ref\":\"~a\",\"english\":\"~a\",\"spanish\":\"~a\",\"turkish\":\"~a\",\"japanese\":\"~a\",\"german\":\"~a\",\"greek\":\"~a\",\"thai\":\"~a\",\"indonesian\":\"~a\",\"swedish\":\"~a\",\"chinese\":\"~a\",\"farsi\":\"~a\",\"french\":\"~a\",\"lsfb\":\"~a\",\"geo-prolog\":\"~a\",\"geo-funql\":\"~a\"}~%"
              id
              type
              elan-ref
              english
              spanish
              turkish
              japanese
              german
              greek
              thai
              indonesian
              swedish
              chinese
              farsi
              french
              lsfb
              prolog
              funql))))

;(xml->json "/Users/liesbetdevos/Projects/GeoQuery-LSFB/xml-files/geoquery-lsfb-250.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/json-files/geoquery-lsfb-250-new.jsonl")

;(xml->json "/Users/liesbetdevos/Projects/GeoQuery-LSFB/large-final-3.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/large-final-3.jsonl")



