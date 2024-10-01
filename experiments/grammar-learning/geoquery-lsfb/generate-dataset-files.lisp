(in-package :geoquery-lsfb)


;---------------------------;
; Necessary background data ;
;---------------------------;

; to be able to load these parameters, make sure the right data files are present within the data folder of the project

(defun read-lexicon (lexicon-path)
  "reads in a lexicon of type jsonl and returns it as a list of json-alists"
  (with-open-file (in-stream lexicon-path :direction :input)
    (loop with json-alist = nil
          for line = (read-line in-stream nil)
          while line do (pushend (cl-json::decode-json-from-string line) json-alist )
          finally (return json-alist)
          )))


; point towards the data folder of the project
(defparameter *data-folder*
  (babel-pathname :directory '("experiments""grammar-learning""geoquery-lsfb""data")))

; jsonl-file containing a lexicon of all naming signs used in the dataset
(defparameter *naming-signs*
  (read-lexicon (merge-pathnames *data-folder* (babel-pathname :name "naming-signs" :type "jsonl"))))

; jsonl-file containing the fingerspelling alphabet for lsfb  
(defparameter *fingerspelling-alphabet*
  (read-lexicon (merge-pathnames *data-folder* (babel-pathname :name "fingerspelling-alphabet" :type "jsonl"))))

; jsonl file containing an overview of all states used in Geobase
(defparameter *states*
  (read-lexicon (merge-pathnames *data-folder* (babel-pathname :name "states" :type "jsonl"))))

; json-file containing an overview of all cities used in Geobase
(defparameter *cities*
  (read-lexicon (merge-pathnames *data-folder* (babel-pathname :name "cities" :type "jsonl"))))

; jsonl-file containing an overview of all rivers used in Geobase
(defparameter *rivers*
  (read-lexicon (merge-pathnames *data-folder* (babel-pathname :name "rivers" :type "jsonl"))))

; counter that will be used for creating new geoquery-id. Original GeoQuery goes up to 879, so new ids start at 880
(defparameter *id-counter*
  880)

;------------------;
; Helper functions ;
;------------------;


(defun xmls->prolog (xmls)
  "extracts geo-prolog meaning representation from elan xmls structure and returns it as a string"
  (loop for child in (xmls:node-children xmls)
        for tier-id = (loop for attribute in (xmls:node-attrs child)
                            when (string= (first attribute) "TIER_ID")
                              do (return (read-from-string (second attribute))))
        when (eql tier-id 'geoprolog)
          do (return (string-replace (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children child)))))))) "\"" ""))))

(defun xmls->funql (xmls)
  "extracts geo-funql meaning representation from elan xmls structure and returns it as a string"
  (loop for child in (xmls:node-children xmls)
        for tier-id = (loop for attribute in (xmls:node-attrs child)
                            when (string= (first attribute) "TIER_ID")
                              do (return (read-from-string (second attribute))))
        when (eql tier-id 'GeoFunQL)
          do (return (string-replace (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children child)))))))) "\"" ""))))

(defun xmls->translation (xmls &key (language-tier-id "english translation"))
  "extracts a the language specified using language-tier-id from elan xmls structure and returns it as a string"
  (loop for child in (xmls:node-children xmls)
        for tier-id = (loop for attribute in (xmls:node-attrs child)
                            when (string= (first attribute) "TIER_ID")
                              do (return (second attribute)))
        when (string= (downcase tier-id) (downcase language-tier-id))
          do (return (string-replace (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children child)))))))) "\"" ""))))

(defun make-fingerspelling (input &key (input-type 'gloss))
  "takes a gloss or geographical name as input (string) and returns its fingerspelled hamnosys form as a string"
  (let ((fingerspelling-name (case input-type
                               ('gloss (remove #\: (second (split-sequence:split-sequence #\\ (string input)))))
                               ('name input))))
    (loop with output = ""
          for c across fingerspelling-name
          for c-hamnosys = (loop for letter in *fingerspelling-alphabet*
                                 do (when (string= (cdr (assoc :letter letter)) (string-upcase (string c)))
                                      (return (cdr (assoc :hamnosys letter)))))
          do (if (string= output "")
               (setf output (concatenate 'string output c-hamnosys))
               (setf output (concatenate 'string output (string (code-char  57514)) c-hamnosys)))
          finally (return output))))


(defun predicates->string (list)
  "Turns a list of meaning predicates into a string of predicates of
the following form: 'predicate-1(arg-x), predicate-2(arg-y)'. It escapes any colons present in the structure."
  (loop for predicate in list
        collect (format nil "~(~a~)(~{~(~a~)~^, ~})" (first predicate) (rest predicate)) into predicate-strings
        finally (return (string-replace (format nil "~{~a~^, ~}" predicate-strings) ":""\:"))))


(defun read-jsonl (json-path)
  "read a jsonl file located at json-path. The function returns a list of alists"
  (with-open-file (in-stream json-path :direction :input)
    (loop with output = '()
          for line = (read-line in-stream nil)
          while line do (pushend (decode-json-from-string line) output)
          finally (return output))))

(defun find-french-name (geographical-entity type)
  "finds the french name for the given geographical entity that is of a specified type" 
  (cond
   ((string= type "state")
    (loop for item in *states*
          do (when (string= (cdr (assoc :name item)) geographical-entity)
               (return (cdr (assoc :french item))))))
   ((or (string= type "city")(string= type "capital"))
    geographical-entity)
   ((string= type "river")
    geographical-entity)))

(defun replace-gloss-and-hamnosys (original-lsfb original-gloss french-state-name state-name)
  "Replaces original-gloss with an equivalent gloss for state-name. It also replaces the hamnosys related to the gloss."
  (let* ((original-gloss-type (first (split-sequence:split-sequence #\: (string original-gloss))))
         (new-hamnosys nil)
         (new-handedness nil)
         (old-handedness (if (string= original-gloss-type "fs")
                           "right-hand-articulation"
                           (loop for item in *naming-signs*
                                 do (when (string= (cdr (assoc :meaning item)) state-name)
                                      (return (cdr (assoc :handedness item)))))))
         (new-gloss (cond ((string= original-gloss-type "fs")
                           (progn
                             (setf new-handedness "right-hand-articulation")
                             (setf new-hamnosys (make-fingerspelling french-state-name :input-type 'name))
                             (concatenate 'string "fs\:" french-state-name)))
                          ((member state-name '("texas""florida""california""new-mexico""des-moines"))
                           (loop for item in *naming-signs*
                                   do (when (string= (cdr (assoc :meaning item)) state-name)
                                        (setf new-handedness (cdr (assoc :handedness item)))
                                        (setf new-hamnosys (cdr (assoc :hamnosys item)))
                                        (return (cdr (assoc :id-gloss item))))))
                          (t
                           (progn
                             (setf new-handedness "right-hand-articulation")
                             (setf new-hamnosys (make-fingerspelling french-state-name :input-type 'name))
                             (concatenate 'string "fs\:" french-state-name)))))
         (new-tag (make-const new-gloss)))
    (loop with output = '()
          for predicate in original-lsfb
          for predicate-name = (first predicate)
          for first-arg = (second predicate)
          for second-arg = (third predicate)
          do
            (unless old-handedness
              (setf old-handedness "right-hand-articulation"))

            (cond
              ((and (eql predicate-name (read-from-string old-handedness)) (string= (utils::get-base-name first-arg) (string-upcase original-gloss)))
               (pushend `(,(read-from-string new-handedness) ,new-tag ,new-hamnosys) output))
              ((and (member predicate-name '(fcg:adjacent start-coincides end-coincides during)) (string= (utils::get-base-name first-arg) (string-upcase original-gloss)))
               (pushend `(,predicate-name ,new-tag ,second-arg) output ))
              ((and (member predicate-name '(fcg:adjacent start-coincides end-coincides during)) (string= (utils::get-base-name second-arg) (string-upcase original-gloss)))
               (pushend  `(,predicate-name  ,first-arg ,new-tag) output))
              (t
               (pushend predicate output)))
             (reset-id-counters)
          finally (return output))))



(defun replace-english (reference original-entity new-entity)
  "extracts the english translation from reference (xmls) and replaces the occurrence of original-entity with new-entity"
  (let ((old-english (loop for line in (xmls::node-children reference)
                                   do (when (string= (second (first (xmls::node-attrs line))) "en")
                                        (return (first (xmls::node-children line)))))))
    (string-replace old-english original-entity (capitalise new-entity))))

(defun replace-funql (reference original-entity new-entity)
  "extracts the funql meaning representation from reference (xmls) and replaces the occurrence of original-entity with new-entity"
  (let ((old-funql (loop for line in (xmls::node-children reference)
                                   do (when (string= (second (first (xmls::node-attrs line))) "geo-funql")
                                        (return (first (xmls::node-children line)))))))
        (string-replace old-funql original-entity new-entity)))

(defun replace-prolog (reference original-entity new-entity)
  "extracts the prolog meaning representation from reference (xmls) and replaces the occurrence of original-entity with new-entity"
  (when (string= new-entity "new york")
    (print "what's happening?"))
                 
  (let ((old-prolog (loop for line in (xmls::node-children reference)
                                   do (when (string= (second (first (xmls::node-attrs line))) "geo-prolog")
                                        (return (first (xmls::node-children line))))))
        (geo-new-entity (if (>  (length (split-sequence::split-sequence #\space new-entity)) 1)
                           (format nil "'~a'" new-entity)
                           new-entity))
        (geo-original-entity (if (> 1 (length (split-sequence::split-sequence #\space original-entity)))
                           (format nil "'~a'" original-entity)
                           original-entity)))
    (string-replace old-prolog geo-original-entity geo-new-entity)))

(defun add-all-variants (reference type original-gloss original-meaning-entity 250-variants &key (variable-type 'state))
  "adds all possible variants for the provided reference and that are not yet included in the set of 250 variants. The keyword variable-type indicates of which type the to be replaced gloss and meaning-entity are (i.e. 'state, 'river or 'city)"
  (let ((type-lexicon (case variable-type
                        ('state *states*)
                        ('city *cities*)
                        ('river *rivers*)))
        (250-variant-names (loop for item in 250-variants
                                 collect (second (split-sequence::split-sequence #\: item)))))
  (loop with output = '()
        for state in type-lexicon
        for state-name = (cdr (assoc :name state))
        for french-state-name = (find-french-name state-name (string-downcase  (string variable-type)))
        for new-english = (replace-english reference original-meaning-entity state-name)
        for new-funql = (replace-funql reference original-meaning-entity state-name)
        for new-prolog = (replace-prolog reference original-meaning-entity state-name)
        for elan-ref = (loop with elan-id = nil
                             for line in (xmls::node-children reference)
                             do (loop for attr in (xmls::node-attrs line)
                                            do (when (string= (first attr) "elan-ref")
                                                 (setf elan-id (second attr))
                                                 (return)))
                                (when elan-id (return elan-id)))
        for original-lsfb = (string->predicates
                             (loop with lsfb = nil
                                   for line in (xmls::node-children reference)
                                   do (loop for attr in (xmls::node-attrs line)
                                            do (when (string= (second attr) "lsfb")
                                                 (setf lsfb (first (xmls::node-children line)))
                                                 (return)))
                                      (when lsfb (return lsfb))))
        for new-lsfb = (predicates->string (replace-gloss-and-hamnosys original-lsfb original-gloss french-state-name state-name))
        for new-id = *id-counter*
        do (unless (or (member state-name 250-variant-names :test #'string=)
                       (string= state-name original-meaning-entity))
             (pushend (xmls:make-node :name "example"
                                      :attrs `(("id" ,new-id)("type" ,type))
                                      :children (list (xmls:make-node :name "nl"
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
                      output)
             (incf *id-counter*))
             finally (return output))))

(defun add-all-capitals (reference type original-gloss original-meaning-entity 250-variants)
  "adds all possible variants for the provided reference and that are not yet included in the set of 250 variants. The function can only be used for references where a capital needs to be replaced."
  (let ((250-variant-names (loop for item in 250-variants
                                 collect (second (split-sequence::split-sequence #\: item)))))
  (loop with output = '()
        for state in *states*
        for capital-name = (cdr (assoc :capital state))
        for new-english = (replace-english reference original-meaning-entity capital-name)
        for new-funql = (replace-funql reference original-meaning-entity capital-name)
        for new-prolog = (replace-prolog reference original-meaning-entity capital-name)
        for elan-ref = (loop with elan-id = nil
                             for line in (xmls::node-children reference)
                             do (loop for attr in (xmls::node-attrs line)
                                            do (when (string= (first attr) "elan-ref")
                                                 (setf elan-id (second attr))
                                                 (return)))
                                (when elan-id (return elan-id)))
        for original-lsfb = (string->predicates
                             (loop with lsfb = nil
                                   for line in (xmls::node-children reference)
                                   do (loop for attr in (xmls::node-attrs line)
                                            do (when (string= (second attr) "lsfb")
                                                 (setf lsfb (first (xmls::node-children line)))
                                                 (return)))
                                      (when lsfb (return lsfb))))
        for new-lsfb = (predicates->string (replace-gloss-and-hamnosys original-lsfb original-gloss capital-name capital-name))
        for new-id = *id-counter*
        do (unless (or (member capital-name 250-variant-names :test #'string=)
                       (string= capital-name original-meaning-entity))
             (pushend (xmls:make-node :name "example"
                                      :attrs `(("id" ,new-id)("type" ,type))
                                      :children (list (xmls:make-node :name "nl"
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
                      output)
             (incf *id-counter*))
             finally (return output))))

(defun reset-id-counter ()
  "resets the counter used to create new geoquery-ids"
  (setf *id-counter* 880))

(defun make-type-overview (xml-path)
  "creates an overview of the number of instances included in the 250 and large dataset version"
(loop with 250-counter = 0
      with large-counter = 0
      with type-counters = '()for example in (xmls:node-children (read-xml xml-path))      
      for id = (parse-integer (loop for attr in (xmls:node-attrs example)
                                    do (when (string= (first attr) "id")
                                         (return (second attr)))))
      for lsfb = (loop with output = nil
                       for line in (xmls::node-children example)
                       do (loop for attr in (xmls::node-attrs line)
                                do (when (string= (second attr) "lsfb")
                                     (setf output (first (xmls::node-children line)))
                                     (return)))
                          (when output (return output)))
      for type = (loop for attr in (xmls:node-attrs example)
                                    do (when (string= (first attr) "type")
                                         (return (second attr))))
      do (when (and (< id 880) lsfb)
           (incf 250-counter))
         (when lsfb
           (incf large-counter)
           (if (assoc (parse-integer type) type-counters)
             (incf (cdr (assoc (parse-integer type) type-counters)))
             (pushend (cons (parse-integer type) 1) type-counters)))
        
      finally (pprint type-counters)
              (pprint (loop with output-nr = 0
                            for counter in type-counters
                            do (incf output-nr (cdr counter))
                            finally (return output-nr)))
        (return `(,250-counter ,large-counter))))


(defun read-jsonl-dataset (dataset-path)
  "reads in a jsonl located at dataset-path and returns it as a list of json-alists"
  (with-open-file (in-stream dataset-path :direction :input :external-format :utf-8 :element-type 'cl:character)
    (loop with output = '()
          for line = (read-line in-stream nil)
          while line do (pushend (decode-json-from-string line) output)
          finally (return output)
          )))

(defun string->predicates (string)
  "transforms a string of predicates into a list of predicates. It escapes colons."
  (let ((split-string (split-sequence::split-sequence #\) string ))
        (output-list '()))
    (delete "" split-string :test #'string=)
    (loop for item in split-string
          for sublist = '()
          for cleaned-item = (string-replace item ":" "\\:")
          for split-predicate = (delete "" (split-sequence::split-sequence #\( cleaned-item) :test #'string=)
          for first-arg = (remove #\, (first split-predicate))
          for remaining-args = (split-sequence::split-sequence #\, (second split-predicate))
          do (pushend (read-from-string first-arg :geoquery-lsfb) sublist)
             (loop for remaining-arg in remaining-args
                   do (pushend (read-from-string remaining-arg) sublist))
             (pushend sublist output-list))
    output-list))



             
;-------------------------------------------------------------------------------
; Step 1: extract lsfb and french from elan-files and add them to GeoQuery.xml
;-------------------------------------------------------------------------------

(defun templates->xml (elan-files-directory path-to-original-xml path-to-new-xml)
  "extracts lsfb and french from the elan-files in elan-files directory and adds them to the original-xml located at path-to-original-xml. The newly created xml is saved to a new location (path-to-new-xml)"
  (let* ((templates (directory elan-files-directory))
         (new-xml (read-xml path-to-original-xml)))
    (loop for template in templates
          for template-xmls = (read-xml template)
          for elan-ref = (pathname-name template)
          for template-id = (third (split-sequence::split-sequence #\_ elan-ref))
          for type = (first (split-sequence::split-sequence #\_ elan-ref))
          for lsfb-predicates = (xmls->hamnosyspredicates template-xmls)
          for lsfb-string = (list-of-predicates->string-of-predicates lsfb-predicates)
          for french = (xmls->translation template-xmls :language-tier-id "French Translation")
          do (loop for example in (xmls:node-children new-xml)
                   for example-id = (second (first (xmls:node-attrs example)))
                   do (when (string= example-id template-id)
                        (insert-after (xmls:node-children example)  3 (xmls:make-node :name "nl" :attrs `(("lang" "lsfb")("elan-ref" ,elan-ref)) :children `(,lsfb-string)))
                        
                        (insert-after (xmls:node-children example) 4 (xmls:make-node :name "nl" :attrs `(("lang" "fr")) :children `(,french)))
                        (pushend `("type" ,type) (xmls::node-attrs example))
                        (return))))
    (with-open-file (out-stream path-to-new-xml :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8 :element-type 'cl:character)
      (xmls:write-xml new-xml out-stream :indent t))))


;(templates->xml "/Users/liesbetdevos/Projects/GeoQuery-LSFB/elan-files/*.eaf" "/Users/liesbetdevos/Projects/geoquery-sign/original-geoquery.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-templates.xml")

;-------------------------------------------------------------------------------
; Step 2: extend the templates towards all 250 examples of multilingual GeoQuery
;-------------------------------------------------------------------------------

(defun create-250-xml (path-to-templates-xml path-to-new-xml path-to-overview)
  "reads in the xml file containing the generated templates and uses the overview of all instances in GeoQuery multilingual (located at path-to-overview) to generate a new xml that contains an lsfb form for all these instances. The new xml is saved at a new location (path-to-new-xml)" 
  (let* ((overview (read-jsonl path-to-overview))
         (new-xml (read-xml path-to-templates-xml)))
    (loop for example in overview
          for variants = (cdr (assoc :250-instances example)) 
          for reference = (when variants
                            (loop for xml-line in (xmls::node-children new-xml)
                                  do (when (string= (cdr (assoc :id example))
                                                    (loop for attr in (xmls::node-attrs xml-line)
                                                          do (when (string= (first attr) "id")
                                                               (return (second attr)))))
                                       (return xml-line))))
          do (when variants
               (loop for variant in variants
                     for variant-characteristics = (split-sequence::split-sequence #\: variant)
                     for variant-id = (first variant-characteristics)
                     for variant-type = (cdr (assoc :type example)) 
                     for original-lsfb = (string->predicates
                                          (loop for line in (xmls::node-children reference)
                                                do (when (string= (second (second (xmls::node-attrs line))) "lsfb")
                                                     (return (first (xmls::node-children line))))))
                     for original-gloss = (cdr (assoc :original-gloss example))
                     for new-meaning = (second variant-characteristics)
                     for new-french-name = (find-french-name new-meaning (cdr (assoc :variable-type example)))
                     for new-lsfb = (list-of-predicates->string-of-predicates (replace-gloss-and-hamnosys original-lsfb original-gloss new-french-name new-meaning))
                     for elan-ref = (loop for line in (xmls:node-children reference)
                                          do (when (string= (second (second (xmls::node-attrs line))) "lsfb")
                                               (return (second (first (xmls::node-attrs line))))))
                       
                     do (loop for xml-line in (xmls::node-children new-xml)
                              do (when (string= (loop for attr in (xmls:node-attrs xml-line)
                                                      do (when (string= (first attr) "id")
                                                           (return  (second attr))))
                                                variant-id)
                                   (insert-after (xmls:node-children xml-line)  3 (xmls:make-node :name "nl" :attrs `(("lang" "lsfb")("elan-ref" ,elan-ref)) :children `(,new-lsfb)))
                                   (insert-after (xmls:node-children xml-line) 4 (xmls:make-node :name "nl" :attrs `(("lang" "fr")) :children `()))
                                   (pushend `("type" ,variant-type) (xmls::node-attrs xml-line))
                                   (return))))))
        (with-open-file (out-stream path-to-new-xml :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8 :element-type 'cl:character)
          (xmls:write-xml new-xml out-stream :indent t))))

;(create-250-xml "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-250.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-250-new.xml" "/Users/liesbetdevos/Projects/babel/experiments/grammar-learning/geoquery-lsfb/data/overview.jsonl")

;--------------------------;
; Step 3: add special case ;
;--------------------------;
; for this example, the name of a city "des moines" is signed by combining a fingerspelled form with a regular sign (FS:DES + MOINE). To extend this example to the other variants, both should be replaced which is not captured by the regular expand-all-variants function. This function handles this case

(defun expand-single-type (path-to-original-xml path-to-new-xml path-to-template original-gloss original-meaning-entity variant-type variants-not-to-include elan-ref)
  "creates all variants for one single geoquery meaning representation type."
  (let* ((original-xml (read-xml path-to-original-xml))
        (new-xml original-xml)
        (template-xmls (read-xml path-to-template))
        (template-predicates (xmls->hamnosyspredicates template-xmls))
        (template-string-predicates (list-of-predicates->string-of-predicates template-predicates))
        (english (xmls->translation template-xmls :language-tier-id "English Translation"))
        (funql (xmls->funql template-xmls))
        (prolog (xmls->prolog template-xmls))
        (new-id *id-counter*)
        (reference (xmls:make-node :name "example"
                                   :attrs `(("id" ,new-id)("type" ,variant-type))
                                   :children (list (xmls:make-node :name "nl"
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
                                                                   :attrs `(("lang" "lsfb")("elan-ref" ,elan-ref))
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
    (incf *id-counter*)
    (pushend reference (xmls:node-children new-xml))
    (setf (xmls:node-children new-xml)(append (xmls::node-children new-xml)
                                              (add-all-capitals reference variant-type original-gloss original-meaning-entity variants-not-to-include)))
    
    (with-open-file (out-stream path-to-new-xml :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8 :element-type 'cl:character)
      (xmls:write-xml new-xml out-stream :indent t))))

;(expand-single-type "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-250-new.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-250-new-special.xml" "/Users/liesbetdevos/Documents/46_0_881.eaf" "fs:montgomery" "montgomery" "45" 'capital '("392:des moines" "881:montgomery")"45_0_392")

;---------------------------------;
; Step 4: create extended dataset ;
;---------------------------------;

(defun 250-large-xml (path-to-original-xml path-to-new-xml path-to-overview)
  (let* ((overview (read-jsonl path-to-overview))
         (original-xml (read-xml path-to-original-xml))
         (output-xml (loop with new-xml = original-xml
                           for example in overview
                           for reference-id = (cdr (assoc :id example))
                           for type = (cdr (assoc :type example))
                           for 250-variants = (cdr (assoc :250-instances example))
                           for variable-type = (read-from-string (cdr (assoc :variable-type example)))
                           for original-gloss = (cdr (assoc :original-gloss example))
                           for original-meaning-entity = (cdr (assoc :original-meaning-entity example))
                           for reference = (loop for xml-line in (xmls::node-children original-xml)
                                                  do (when (string= reference-id
                                                                    (loop for attr in (xmls::node-attrs xml-line)
                                                                          do (when (string= (first attr) "id")
                                                                               (return (second attr)))))
                                                       (return xml-line)))
                           for new-variants = (case variable-type
                                              ('state (add-all-variants reference type original-gloss original-meaning-entity 250-variants :variable-type 'state))
                                              ('city (add-all-variants reference type original-gloss original-meaning-entity 250-variants :variable-type 'city))
                                              ('river (add-all-variants reference type  original-gloss original-meaning-entity 250-variants :variable-type 'river))
                                              ('capital (add-all-capitals reference type original-gloss original-meaning-entity 250-variants)))
                           do (setf (xmls:node-children new-xml)(append (xmls::node-children new-xml) new-variants))
                           finally (return new-xml))))
    (with-open-file (out-stream path-to-new-xml :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8 :element-type 'cl:character)
      (xmls:write-xml output-xml out-stream :indent t))))

;(250-large-xml "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-250-new-special.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-large.xml" "/Users/liesbetdevos/Projects/babel/experiments/grammar-learning/geoquery-lsfb/data/overview.jsonl")

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

(defun already-included-variant (state-name city-name *18-0-87-overview* old-state-meaning old-city-meaning)
  "checks whether there already is a variant with the given state-name and city-name included in the set of 250 variants (or if it is the same as the original template reference"
  (let ((included? nil))
    (when (and (string= state-name  old-state-meaning)
               (string= city-name old-city-meaning))
      (setf included? t))
    (unless included? (loop for item in *18-0-87-overview*
                            do (when (and (string= state-name  (third item))
                                          (string= city-name (second item)))
                                 (setf included? t))))
    included?))

(defun extend-city-state (old-xml-path path-to-template new-xml-path example-overview old-state-gloss old-city-gloss old-state-meaning old-city-meaning reference-id variant-type old-state-abbreviation)
  "extends an example where both a city and a state should be replaced by all possible variants"
  (let* ((original-xml (read-xml old-xml-path))
         (new-xml original-xml)
         (template-xmls (read-xml path-to-template))
         (template-predicates (xmls->hamnosyspredicates template-xmls))
         (elan-ref (pathname-name path-to-template))
         (reference-example (loop for xml-line in (xmls::node-children new-xml)
                                  do (when (string= reference-id
                                                    (loop for attr in (xmls::node-attrs xml-line)
                                                          do (when (string= (first attr) "id")
                                                               (return (second attr)))))
                                       (return xml-line)))))
         (loop for item in example-overview
               for variant-id = (first item)
               for new-city-meaning = (second item)
               for new-state-meaning = (third item)
               for new-french-state-meaning = (find-french-name new-state-meaning "state")
               for new-lsfb = (list-of-predicates->string-of-predicates (replace-gloss-and-hamnosys (replace-gloss-and-hamnosys template-predicates old-state-gloss new-french-state-meaning new-state-meaning) old-city-gloss new-city-meaning new-city-meaning))
               do (loop for xml-line in (xmls::node-children new-xml)
                        do (when (string= (loop for attr in (xmls:node-attrs xml-line)
                                                do (when (string= (first attr) "id")
                                                     (return (second attr))))
                                          variant-id)
                             (insert-after (xmls:node-children xml-line)  3 (xmls:make-node :name "nl" :attrs `(("lang" "lsfb")("elan-ref" ,elan-ref)) :children `(,new-lsfb)))
                             (insert-after (xmls:node-children xml-line) 4 (xmls:make-node :name "nl" :attrs `(("lang" "fr")) :children `()))
                             (pushend `("type" ,variant-type) (xmls::node-attrs xml-line))
                             (return))))
         (loop for city in *cities* 
               for city-name = (cdr (assoc :name city))
               for state-name = (cdr (assoc :state city))
               for state-abbr = (loop for state in *states*
                                      do (when (string= (cdr (assoc :name state))
                                                        state-name)
                                           (return (cdr (assoc :abbreviation state)))))
               for new-french-state-meaning = (find-french-name state-name "state")
               for new-english = (string-replace (replace-english reference-example old-state-meaning state-name) old-city-meaning (capitalise city-name))
               for new-funql = (string-replace  (replace-funql reference-example old-city-meaning city-name) old-state-abbreviation state-abbr)
               for new-prolog = (string-replace  (replace-prolog reference-example old-city-meaning city-name) old-state-abbreviation state-abbr)
               for new-lsfb = (list-of-predicates->string-of-predicates (replace-gloss-and-hamnosys (replace-gloss-and-hamnosys template-predicates old-state-gloss new-french-state-meaning state-name) old-city-gloss city-name city-name))
               for new-id = *id-counter*
               do (unless (already-included-variant state-name city-name *18-0-87-overview* old-state-meaning old-city-meaning)
                    (pushend (xmls:make-node :name "example"
                                             :attrs `(("id" ,new-id)("type" ,variant-type))
                                             :children (list (xmls:make-node :name "nl"
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
                    (incf *id-counter*)))
         (with-open-file (out-stream new-xml-path :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8 :element-type 'cl:character)
           (xmls:write-xml new-xml out-stream :indent t))
         (length (xmls:node-children new-xml))))

;(extend-city-state "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-large.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/elan-files/18_0_87.eaf" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/geoquery-large-special-1.xml" *18-0-87-overview* "fs:minnesota" "fs:minneapolis" "minnesota" "minneapolis" "87" "18" "mn")

;-----------------------------------------------------------------------------------------------
; step 5: remove instances from the dataset file that are not part of the multilingual version
;-----------------------------------------------------------------------------------------------

(defun remove-non-multilingual-instances (path-to-original path-to-new-250 path-to-new-large)
  "removes all instances from the dataset that are not part of the multilingual version. It also creates the 250 version from the large version of the dataset."
  (let ((translated (read-xml path-to-original))
        (250-data (xmls:make-node :name "examples"
                             :children `()))
        (large (xmls:make-node :name "examples"
                             :children `())))
    (loop for example in (xmls:node-children translated)
          for id = (parse-integer (loop for attr in (xmls:node-attrs example)
                                    do (when (string= (first attr) "id")
                                         (return (second attr)))))
          for lsfb = (loop with output = nil
                       for line in (xmls::node-children example)
                       do (loop for attr in (xmls::node-attrs line)
                                do (when (string= (second attr) "lsfb")
                                     (setf output (first (xmls::node-children line)))
                                     (return)))
                          (when output (return output)))
          do (cond ((and (< id 880) lsfb)
                    (pushend example (xmls:node-children 250-data))
                    (pushend example (xmls:node-children large)))
                   (lsfb
                    (pushend example (xmls:node-children large)))))
    (with-open-file (out-stream path-to-new-250 :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8 :element-type 'cl:character)
           (xmls:write-xml 250-data out-stream :indent t))
    (with-open-file (out-stream path-to-new-large :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8 :element-type 'cl:character)
           (xmls:write-xml large out-stream :indent t))))
                    
                    
;(remove-non-multilingual-instances "/Users/liesbetdevos/Projects/geoquery-sign/generated-files/geoquery-large-original-translations.xml" "/Users/liesbetdevos/Projects/geoquery-sign/250-final.xml" "/Users/liesbetdevos/Projects/geoquery-sign/large-final.xml")


;--------------------------------------------;
; step 6: create json version of the dataset ;
;--------------------------------------------;


(defun xml->json (xml-path json-path)
  "transforms an xml dataset file into json format"
  (let ((xml-data (read-xml xml-path)))
    (with-open-file (out-stream json-path :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8 :element-type 'cl:character)
      (loop for example in (xmls:node-children xml-data)
            for id = nil
            for type = nil
            for english = nil
            for spanish = nil
            for turkish = nil
            for japanese = nil
            for french = nil
            for lsfb = nil
            for funql = nil
            for prolog = nil
            for elan-ref = nil
            do (loop for attr in (xmls:node-attrs example)
                     do (cond ((string= (first attr) "id")(setf id (second attr)))
                              ((string= (first attr) "type")(setf type (second attr)))))
            (loop for child in (xmls:node-children example)
                  do (cond ((and (string= (xmls:node-name child) "nl")
                                (string= (second (first (xmls:node-attrs child))) "en"))
                            (setf english (first (xmls:node-children child))))
                           ((and (string= (xmls:node-name child) "nl")
                                (string= (second (first (xmls:node-attrs child))) "es"))
                            (setf spanish (first (xmls:node-children child))))
                           ((and (string= (xmls:node-name child) "nl")
                                (string= (second (first (xmls:node-attrs child))) "tr"))
                            (setf turkish (first (xmls:node-children child))))
                           ((and (string= (xmls:node-name child) "nl")
                                (string= (second (first (xmls:node-attrs child))) "ja"))
                            (setf japanese (first (xmls:node-children child))))
                           ((and (string= (xmls:node-name child) "nl")
                                (string= (second (first (xmls:node-attrs child))) "fr"))
                            (setf french (first (xmls:node-children child))))
                           ((string= (xmls:node-name child) "nl")
                            (loop for attr in (xmls:node-attrs child)
                                       do (when (string= (first attr) "elan-ref")
                                                 (setf elan-ref (second attr)))
                                          (setf lsfb (xmls:node-children child))))
                           ((and (string= (xmls:node-name child) "mrl")
                                (string= (second (first (xmls:node-attrs child))) "geo-funql"))
                            (setf funql (first (xmls:node-children child))))
                           ((and (string= (xmls:node-name child) "mrl")
                                (string= (second (first (xmls:node-attrs child))) "geo-prolog"))
                            (setf prolog (first (xmls:node-children child))))))
            do (format out-stream "{\"id\":\"~a\",\"type\":\"~a\",\"elan-ref\":\"~a\",\"english\":\"~a\",\"spanish\":\"~a\",\"turkish\":\"~a\",\"japanese\":\"~a\",\"french\":\"~a\",\"lsfb\":\"~a\",\"geo-prolog\":\"~a\",\"geo-funql\":\"~a\"}~%" id type elan-ref english spanish turkish japanese french lsfb prolog funql)))))

;(xml->json "/Users/liesbetdevos/Projects/GeoQuery-LSFB/xml-files/geoquery-lsfb-250.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/json-files/geoquery-lsfb-250.jsonl")

;(xml->json "/Users/liesbetdevos/Projects/GeoQuery-LSFB/xml-files/geoquery-lsfb-4500.xml" "/Users/liesbetdevos/Projects/GeoQuery-LSFB/json-files/geoquery-lsfb-4500.jsonl")

;(read-jsonl-dataset "/Users/liesbetdevos/Projects/GeoQuery-LSFB/json-files/geoquery-lsfb-250.jsonl")
                                      
          
          
      
        
  
       
          
  



        