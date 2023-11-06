(in-package :muhai-cookingbot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                             ;;
;; This file contains an implementation of the VR primitives   ;;
;; used by the cooking bot.                                    ;;
;;                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; transfer-contents; lots of particles seem to be spilled!
;; request-to-mix; takes very long time?

;; make class 'dispenser', with subclasses sugar-bag, mayonnaise-jar, etc.
;; and slot 'dispenses', which contains the concept of sugar, mayonnaise, etc.


;; Defining the VR primitive inventory  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-irl-primitives vr-inventory :primitive-inventory *vr-primitives*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;                  HELP FUNCTIONS                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: A way in VR kitchen to search for 'sugar' and it returns the container with 'sugar' in it
;; TODO: Type names in the VR kitchen should be ontology with the yaml ontology so this mapping can be removed.
(defparameter *type-mapping*
  '(;; bags
    (butter-bag             . butter)
    (vanilla-extract-bag    . vanilla-extract)
    (almond-extract-bag     . almond-extract)
    (flour-bag              . all-purpose-flour)
    (almond-flour-bag       . almond-flour)
    ;; ingredients
    (peeled-red-onion        . red-onion)
    (cider-vinegar           . cider-vinegar-bottle)
    ;; appliances/tool/utensils
    (baking-sheet           . baking-paper)
    (kitchen-counter        . counter-top)
    (spoon                  . wooden-spoon)
    (cooking-knife          . knife)
    ;; misc
    (floor                  . vr-kitchen-floor)
    (ceiling                . vr-kitchen-ceiling)
    (abe                    . agent)
    (doughclump             . dough)))

(defparameter *property-mapping*
  '((peeled-red-onion (peeled . t))
    (peeled-potato (peeled . t))))
    
;; TODO: A way to know that if you want to portion ingredient X, you can find it in some object of type Y
(defparameter *source-mapping*
  '((sugar             . sugar-bag)
    (mayonnaise        . mayonnaise-jar)
    (cider-vinegar     . cider-vinegar-bottle)
    (white-sugar       . sugar-bag)
    (mozzarella        . mozzarella-bag)
    (grated-mozzarella . grated-mozzarella-bag)))

(defun map-type (type)
  (or (cdr (assoc type *type-mapping*))
      ;; only return <type> when there is a class with this name
      (when (find-class type nil) type)
      ;; as a backup, just return a kitchen-entity
      (progn (format t "~%Type ~a not found" type)
        'kitchen-entity)))

(defun map-properties (type)
  (cdr (assoc type *property-mapping*)))

(defun rmap-type (type)
  (or (car (rassoc type *type-mapping*))
      type))



(defun sym->string (s &key from-first)
  "Convert a keyword symbol name to a camelCase string."
  (lisp->camel-case (mkstr s) :from-first from-first))
  
(defun string->sym (s)
  "Convert a camelCase string to a keyword symbol name."
  (make-kw (camel-case->lisp s)))



(defun sym-name (o)
  "Converts a name used by the VR interface (a camelCased string)
   into the name of an IRL object (i.e. a keyword symbol)."
  (string->sym (gethash "name" o)))

(defun sym-type (o)
  "Converts the type record of a vr object description
   into the type of an object."
  (map-type (intern (upcase (camel-case->lisp (gethash "type" o))))))

(defun sym-properties (o)
  (map-properties (intern (upcase (camel-case->lisp (gethash "type" o))))))



(defun vr-name (o)
  "Converts the name of an IRL object (i.e. a keyword symbol)
   into a name used by the VR interface (a camelCased string)."
  (name o))

(defun vr-contents (o)
  "Retrieves a list of camelCased strings that are the vr-names
   of objects directly contained in an IRL container."
  (mapcar #'vr-name (contents o)))

(defun vr-type (o)
  "Converts the type of an object into a camelCased string."
  (sym->string (rmap-type (type-of o)) :from-first t))

(defmethod vr-shape ((o shape))
  "Converts an IRL shape object into a camelCased string."
  (sym->string (type-of o)))

(defmethod vr-pattern ((p pattern))
  "Converts an IRL pattern object into a camelCased string."
  (sym->string (type-of p)))

(defmethod vr-quantity ((q quantity))
  "Converts an IRL quantity object into a camelCased string."
  (value q))

(defmethod vr-unit ((u unit))
  "Converts an IRL unit object into a camelCased string."
  (sym->string (type-of u)))




(defmethod traverse-sym-kitchen ((kitchen-state kitchen-state) &key do-fn collect-fn)
  (loop for elem in (contents kitchen-state)
        if do-fn
        do (traverse-sym-kitchen elem :do-fn do-fn :collect-fn collect-fn)
        else
        append (remove nil (traverse-sym-kitchen elem :do-fn do-fn :collect-fn collect-fn))))
  
(defmethod traverse-sym-kitchen (node &key do-fn collect-fn)
  (if do-fn
    (progn (funcall do-fn node)
      (when (slot-exists-p node 'contents)
        (loop for child in (contents node)
              do (traverse-sym-kitchen child :do-fn do-fn :collect-fn collect-fn))))
    (cons (funcall collect-fn node)
          (when (slot-exists-p node 'contents)
            (loop for child in (contents node)
                  append (traverse-sym-kitchen child :do-fn do-fn :collect-fn collect-fn))))))



(defun sim-find-object-by-vr-name (vr-name kitchen-state)
  "Find an object by its vr-name in the IRL kitchen"
  (let ((found-entities
         (traverse-sym-kitchen kitchen-state
                               :collect-fn #'(lambda (entity)
                                               (when (string= (name entity) vr-name)
                                                 entity)))))
    (unless (null found-entities)
      (first found-entities))))

(defun find-kitchen-entity-with-content (object kitchen-state)
  "Find a kitchen entity that has <object> as its contents"
  (let ((found-entities
         (traverse-sym-kitchen kitchen-state
                               :collect-fn #'(lambda (entity)
                                               (when (and (slot-exists-p entity 'contents)
                                                          (member object (contents entity)))
                                                 entity)))))
    (unless (null found-entities)
      (first found-entities))))

(defun find-all-kitchen-entities-vr (type kitchen-state)
  (let ((found-entities
         (traverse-sym-kitchen kitchen-state
                               :collect-fn #'(lambda (entity)
                                               (when (subtypep (type-of entity) type)
                                                 entity)))))
    found-entities))

(defun find-kitchen-entity-vr (type kitchen-state &key unused)
  "Return an IRL object of the given type from the symbolic state (if one exists)."
  (let ((found-entities
         (traverse-sym-kitchen kitchen-state
                               :collect-fn #'(lambda (entity)
                                               (when (and (subtypep (type-of entity) type)
                                                          (if unused
                                                            (and (slot-exists-p entity 'used)
                                                                 (slot-exists-p entity 'contents)
                                                                 (null (used entity))
                                                                 (null (contents entity)))
                                                            T))
                                                 entity)))))
    (unless (null found-entities)
      (first found-entities))))

(defun find-unused-kitchen-entity-vr (type kitchen-state)
  "Return an IRL container of the given type from the symbolic state that is not currently used (if one exists)."
  (find-kitchen-entity-vr type kitchen-state :unused T))

(defun find-kitchen-cabinet-with-most-space (kitchen-state)
  "Find the kitchen cabinet that has the most space"
  (let ((kitchen-cabinets
         (traverse-sym-kitchen kitchen-state
                               :collect-fn #'(lambda (entity)
                                               (when (eql (type-of entity) 'kitchen-cabinet)
                                                 entity)))))
    (the-smallest
     #'(lambda (cabinet)
         (length (contents cabinet)))
     kitchen-cabinets)))

(defun find-source (concept kitchen-state)
  "Return an object that is able to generate objects of a given type during a portioning action."
  (let* ((source-type (cdr (assoc concept *source-mapping*))))
    (when source-type
      (find-kitchen-entity-vr source-type kitchen-state))))




(defun vr->sym-temperature (vr-name kitchen-state)
  "Retrieve the temperature value (in degrees C) of an object and convert it to an amount."
  (let* ((obj (sim-find-object-by-vr-name vr-name kitchen-state))
         (vr-data (simulation-data obj))
         (csv (gethash "customStateVariables" vr-data))
         (vr-temperature (gethash "temperature" csv)))
    (when (numberp vr-temperature)
      (make-instance 'amount
                     :quantity (make-instance 'quantity :value vr-temperature)
                     :unit (make-instance 'degrees-celsius)))))



(defun get-kitchen-to-send (sym-state sym-blackboard)
  (unless (equal (find-data sym-blackboard :current-state) sym-state)
    (symbolic-to-vr-kitchen sym-state)))

(defun update-current-state (sym-state sym-blackboard)
  (set-data sym-blackboard :current-state sym-state))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;               TO SYMBOLIC                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defun replace-collection-by-ingredient (collection)
  (let* ((raw-type-name
          (gethash "type" (simulation-data (first collection))))
         (clean-type-name
          (string-replace (string-replace raw-type-name "Chopped" "")
                          "Particle" ""))
         (clean-type
          (map-type (intern (upcase (camel-case->lisp clean-type-name)))))
         (ingredient
          (make-instance clean-type :elements collection)))
    (when (search "Chopped" raw-type-name)
      (setf (is-cut ingredient) (make-instance 'chopped)))
    (list ingredient)))

(defun handle-ingredient-collections (kitchen)
  (labels ((simulation-type (obj)
             (gethash "type" (simulation-data obj)))
           (simulation-name (obj)
             (gethash "name" (simulation-data obj)))
           (all-contents-same-vr-type-p (contents)
             (let ((vr-types (mapcar #'simulation-type contents)))
               (length= (remove-duplicates vr-types :test #'string=) 1)))
           (all-contents-collection-type-p (contents)
             (let ((vr-names (mapcar #'simulation-name contents)))
               (every #'(lambda (name) (search "_" name)) vr-names))))
    (traverse-sym-kitchen
     kitchen
     :do-fn
     #'(lambda (entity)
         (when (and (slot-exists-p entity 'contents)
                    (all-contents-same-vr-type-p (contents entity))
                    (all-contents-collection-type-p (contents entity)))
           (setf (contents entity)
                 (replace-collection-by-ingredient (contents entity))))))))
|#

(defun set-sym-object-slots (sym-object custom-state-variables)
  "When the <custom-state-variables> (hash table) of the VR simulator
   specify a slot that is present in the CLOS <object>,
   set the slot!"
  (loop for slot in (closer-mop:class-slots (class-of sym-object))
        for slot-name = (closer-mop:slot-definition-name slot)
        for slot-key-accessor = (car (closer-mop:slot-definition-initargs slot))
        for vr-key = (sym->string slot-key-accessor)
        when (gethash vr-key custom-state-variables)
        do (setf (slot-value sym-object slot-name)
                 (cond
                  ((eql slot-name 'is-cut)
                   (make-instance (intern (upcase (gethash vr-key custom-state-variables)))))
                  (t
                   (gethash vr-key custom-state-variables))))))

(defun vr-to-symbolic-kitchen (vr-kitchen)
  "Converts the world state formed used in AbeSim to a symbolic IRL kitchen"
  (labels ((make-sym-object (vr-object name)
             (let* ((type (sym-type vr-object))
                    (properties (sym-properties vr-object))
                    (csv (gethash "customStateVariables" vr-object))
                    (sym-object (make-instance type :name name :simulation-data vr-object)))
               (loop for (slot-name . value) in properties
                     do (setf (slot-value sym-object slot-name)
                              (if (or (eq value t) (eq value nil))
                                value
                                (make-instance value))))
               (set-sym-object-slots sym-object csv)
               sym-object)))
    (let* ((sym-kitchen (make-instance 'kitchen-state))
           (kconstraints-alist
            (loop for key in (alexandria:hash-table-keys vr-kitchen)
                  for vr-object = (gethash key vr-kitchen)
                  for name = (sym-name vr-object)
                  when (string= "kcon" (gethash "simtype" vr-object))
                  collect (cons name vr-object)))
           (ktrees-alist
            ;; make IRL objects
            (loop for key in (alexandria:hash-table-keys vr-kitchen)
                  for vr-object = (gethash key vr-kitchen)
                  for name = (gethash "name" vr-object)
                  when (string= "ktree" (gethash "simtype" vr-object))
                  collect (cons name (make-sym-object vr-object name)))))
      ;; make sure all IRL object is in the contents of another IRL object
      (loop for (nil . sym-object) in ktrees-alist
            for vr-data = (simulation-data sym-object)
            for at = (gethash "at" vr-data)
            for parent = (assoc at ktrees-alist :test #'string=)
            for container = (if parent (cdr parent) sym-kitchen)
            do (push sym-object (contents container)))
      ;; handle collections of the same ingredient (particles, chopped things, etc.)
      ; (handle-ingredient-collections sym-kitchen)
      ;; simply store the constraints
      (setf (constraints sym-kitchen) kconstraints-alist)
      sym-kitchen)))

(defun read-kitchen-state (vr-response &optional (keyname "kitchenStateOut"))
  (vr-to-symbolic-kitchen (gethash keyname vr-response)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;               TO VR                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-vr-object-slots (sym-object custom-state-variables)
  "Loop over the slots of an IRL object and store its values
   in the custom-state-variables hash table"
  (loop for slot in (closer-mop:class-slots (class-of sym-object))
        for slot-name = (closer-mop:slot-definition-name slot)
        for slot-key-accessor = (car (closer-mop:slot-definition-initargs slot))
        for slot-value = (slot-value sym-object slot-name)
        for vr-key = (sym->string slot-key-accessor)
        unless (member slot-name '(simulation-data contents))
        do (setf (gethash vr-key custom-state-variables)
                 ;; TO DO: depending on the slot-value
                 ;; make the appropriate VR object!
                 (typecase slot-value
                   (amount (let ((hash (make-hash-table :test #'string=)))
                             (setf (gethash "quantity" hash) (value (quantity slot-value)))
                             (setf (gethash "unit" hash) (vr-type (unit slot-value)))
                             hash))
                   (pattern (vr-pattern slot-value))
                   (symbol slot-value)
                   (string slot-value)
                   (number slot-value)
                   (t slot-value)))))

(defun symbolic-to-vr-kitchen (sym-obj &key (vr-kitchen nil) (parent nil))
  "Converts a symbolic IRL kitchen to the world state format used in AbeSim.
   Note that the top-level kitchen-state object will not appear in the conversion;
   this is fine, a kitchen-state object will be generated automatically when reading back to IRL."
  (labels ((add-to-vr-kitchen (sym-obj vr-kitchen simtype)
             (let* ((vr-data (simulation-data sym-obj))
                    (obj-vr-name (gethash "name" vr-data))
                    (dummy (setf (gethash obj-vr-name vr-kitchen) vr-data))
                    (vr-obj-record (gethash obj-vr-name vr-kitchen)))
               (setf (gethash "at" vr-obj-record) (if parent parent 'null))
               (setf (gethash "type" vr-obj-record) (vr-type sym-obj))
               (setf (gethash "simtype" vr-obj-record) simtype)
               (set-vr-object-slots sym-obj (gethash "customStateVariables" vr-obj-record))
               (symbolic-to-vr-kitchen sym-obj :vr-kitchen vr-kitchen :parent obj-vr-name))))
    (let* ((vr-kitchen (if vr-kitchen vr-kitchen (make-hash-table :test #'string=))))
      (cond ((slot-exists-p sym-obj 'contents)
             (loop for content-obj in (contents sym-obj)
                   if (and (slot-exists-p content-obj 'elements)
                           (elements content-obj))
                   do (loop for elem in (elements content-obj)
                            do (add-to-vr-kitchen elem vr-kitchen "ktree"))
                   else
                   do (add-to-vr-kitchen content-obj vr-kitchen "ktree")))
            ((slot-exists-p sym-obj 'constraints)
             (loop for (name . vr-constraint) in (constraints sym-obj)
                   do (setf (gethash name vr-kitchen) vr-constraint))))
      (unless parent
        vr-kitchen))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;                   PRIMITIVES                             ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;                                                          ;;
;;                 TO GET TIME                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive get-time ((time-output double-float))
  ((=> time-output)
   (bind (time-output 1.0 (request-to-get-time) 0.0)))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                   TO WAIT                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive wait ((time-to-wait double-float)
                    (time-unit string)
                    (kitchen-in kitchen-state)
                    (kitchen-out kitchen-state))
  ((time-to-wait time-unit kitchen-in
    => kitchen-out)
   (let* ((kitchen-to-send (symbolic-to-vr-kitchen kitchen-in))
          ;; TO DO: request-to-wait, wrong number of arguments
          (response (request-to-wait time-to-wait))
          (new-kitchen-state (read-kitchen-state response)))
     (bind (kitchen-out 1.0 new-kitchen-state 0.0))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;              TO LEAVE FOR TIME                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive leave-for-time ((container transferable-container)
                              (time-to-wait double-float)
                              (time-unit string)
                              (kitchen-in kitchen-state)
                              (kitchen-out kitchen-state))
  ((container time-to-wait time-unit kitchen-in 
    => kitchen-out)
   (let* ((kitchen-to-send (symbolic-to-vr-kitchen kitchen-in))
          (response (request-to-leave-for-time (vr-name container) time-to-wait time-unit kitchen-to-send))
          (new-kitchen-state (read-kitchen-state response)))
     (bind (kitchen-out 1.0 new-kitchen-state 0.0))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;           TO BRING TO TEMPERATURE                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive bring-to-temperature ((container transferable-container)
                                    (temperature double-float)
                                    (temperature-unit string)
                                    (kitchen-in kitchen-state)
                                    (kitchen-out kitchen-state))
  ((container temperature temperature-unit kitchen-in
    => kitchen-out)
   (let* ((kitchen-to-send (symbolic-to-vr-kitchen kitchen-in))
          (response (request-to-bring-to-temperature (vr-name container) temperature temperature-unit kitchen-to-send))
          (new-kitchen-state (read-kitchen-state response)))
     (bind (kitchen-out 1.0 new-kitchen-state 0.0))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;               TO PREHEAT OVEN                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive preheat-oven ((oven oven)
                            (temperature double-float)
                            (temperature-unit string)
                            (kitchen-in kitchen-state)
                            (kitchen-out kitchen-state))
  ((temperature temperature-unit kitchen-in oven
    => kitchen-out)
   (let* ((kitchen-to-send (symbolic-to-vr-kitchen kitchen-in))
          (response (request-to-preheat-oven (vr-name oven) temperature temperature-unit kitchen-to-send))
          (new-kitchen-state (read-kitchen-state response)))
     (bind (kitchen-out 1.0 new-kitchen-state 0.0))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                    TO GET                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive get-kitchen ((kitchen kitchen-state))
  ((=> kitchen)
   (let* ((response (request-get-kitchen "kitchenStateOut"))
          (new-kitchen-state (read-kitchen-state response)))
     (bind (kitchen 1.0 new-kitchen-state 0.0))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                    TO SET                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive set-kitchen ((kitchen-in kitchen-state)
                           (kitchen-out kitchen-state))
  ((kitchen-in
    =>
    kitchen-out)
   (let* ((kitchen-to-send (symbolic-to-vr-kitchen kitchen-in)))
     (request-set-kitchen kitchen-to-send)
     (bind (kitchen-out 1.0 kitchen-in 0.0))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO FETCH                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive fetch ((thing-fetched kitchen-entity)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (concept-to-fetch conceptualizable)
                     (quantity quantity)) ;;what if this is more than 1?
  ;; Case 1: Fetch object from somewhere in the kitchen and place it on the countertop
  ((kitchen-state-in
    concept-to-fetch
    quantity
    =>
    kitchen-state-out
    thing-fetched)
   (let* ((item-name
           (vr-name (find-kitchen-entity-vr (type-of concept-to-fetch) kitchen-state-in)))
          (vr-result
           (request-to-fetch item-name))
          (thing-fetched-available-at (request-to-get-time))
          (kitchen-state-available-at thing-fetched-available-at)
          (fetched-obj-vr-name (gethash "fetchedObject" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (fetched-object-instance (sim-find-object-by-vr-name fetched-obj-vr-name new-kitchen-state)))
     ;; TODO: Is a fetched thing on countertop used or not?
     (bind (thing-fetched 1.0 fetched-object-instance thing-fetched-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                 TO FETCH AND PROPORTION                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive fetch-and-proportion ((container-with-ingredient container)
                                    (kitchen-state-out kitchen-state)
                                    (kitchen-state-in kitchen-state)
                                    (target-container container)
                                    (ingredient-concept conceptualizable)
                                    (quantity quantity)
                                    (unit unit))

  ;; Takes a specified amount of an ingredient from somewhere in the kitchen and places it in a container
  ((kitchen-state-in
    ingredient-concept
    quantity
    unit
    =>
    kitchen-state-out
    container-with-ingredient
    target-container)

   (let* ((current-kitchen-time (kitchen-time kitchen-state-in))
          (unused-container
           (find-unused-kitchen-entity-vr 'medium-bowl kitchen-state-in))
          (vr-result-fetch
           (request-to-fetch (vr-name unused-container)))
          (ks-after-fetch (read-kitchen-state vr-result-fetch))
          ;; if ingredients do not have a source (like sugar has sugarBag)
          ;; simply place the ingredient in the unused container
          ;; (ignoring quantity and unit...)
          (source (find-source (type-of ingredient-concept) ks-after-fetch)))
     (if source
       (let* ((source-vr-name (vr-name source))
              (vr-result-portion
               (request-to-portion source-vr-name
                                   (vr-name unused-container)
                                   (value quantity)))
              (new-kitchen-state (read-kitchen-state vr-result-portion))
              (output-container-vr-name (gethash "outputContainer" vr-result-portion))
              (output-container-object (sim-find-object-by-vr-name output-container-vr-name new-kitchen-state))
              (container-available-at (request-to-get-time))
              (kitchen-state-available-at container-available-at))
         (setf (slot-value output-container-object 'used) t)
         (bind (target-container 1.0 unused-container current-kitchen-time)
               (container-with-ingredient 1.0 output-container-object container-available-at)
               (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
       (let* ((object-to-place
               (find-kitchen-entity-vr (type-of ingredient-concept) ks-after-fetch))
              (vr-result-place
               (request-to-place (vr-name object-to-place)
                                 (vr-name unused-container)))
              (new-kitchen-state (read-kitchen-state vr-result-place))
              (placed-object-vr-name (gethash "placedObject" vr-result-place))
              (placed-object
               (sim-find-object-by-vr-name placed-object-vr-name new-kitchen-state))
              (output-container-object
               (find-kitchen-entity-with-content placed-object new-kitchen-state))
              (container-available-at (request-to-get-time))
              (kitchen-state-available-at container-available-at))
         (setf (slot-value output-container-object 'used) t)
         (bind (target-container 1.0 unused-container current-kitchen-time)
               (container-with-ingredient 1.0 output-container-object container-available-at)
               (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                    TO TRANSFER                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive transfer-contents ((container-with-all-ingredients transferable-container)
                                 (container-with-rest transferable-container)
                                 (kitchen-state-out kitchen-state)
                                 (kitchen-state-in kitchen-state)
                                 (target-container transferable-container)
                                 (container-with-input-ingredients transferable-container)
                                 (quantity quantity)
                                 (unit unit))
  ;; Case in which the target container is not given in the input-kitchen-state and no quantity and unit are given
  ((kitchen-state-in
    container-with-input-ingredients
    =>
    target-container
    container-with-all-ingredients
    container-with-rest
    kitchen-state-out
    quantity
    unit)
   (let* ((total-amount nil)
          (new-kitchen-state nil)
          (unused-container (find-unused-kitchen-entity-vr 'medium-bowl kitchen-state-in))
          (container-with-input-ingredients-vr-name (vr-name container-with-input-ingredients))
          (vr-result (request-to-transfer container-with-input-ingredients-vr-name
                                          (vr-name unused-container)
                                          (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-all-ingredients-vr-name (gethash "containerWithAllIngredients" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (original-container (sim-find-object-by-vr-name container-with-input-ingredients-vr-name new-kitchen-state))
          (destination-container (sim-find-object-by-vr-name container-with-all-ingredients-vr-name new-kitchen-state)))
     (setf (slot-value destination-container 'used) t)
     (bind (target-container 0.0  destination-container nil)
           (container-with-all-ingredients 1.0 destination-container container-available-at)
           (container-with-rest 1.0 original-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (quantity 0.0 (make-instance 'quantity :value 100) nil)
           (unit 0.0 (make-instance 'unit) nil))))
  ;; Case in which the target container is given in the input-kitchen-state and no quantity and unit are given
  ((kitchen-state-in
    container-with-input-ingredients
    target-container
    =>
    container-with-all-ingredients
    container-with-rest
    kitchen-state-out
    quantity
    unit)
   (let* ((container-with-input-ingredients-vr-name (vr-name container-with-input-ingredients))
          (vr-result
           (request-to-transfer container-with-input-ingredients-vr-name
                                (vr-name target-container)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-rest-name (name container-with-input-ingredients))
          (container-with-all-ingredients-vr-name (gethash "containerWithAllIngredients" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (original-container (sim-find-object-by-vr-name container-with-input-ingredients-vr-name new-kitchen-state))
          (destination-container (sim-find-object-by-vr-name container-with-all-ingredients-vr-name new-kitchen-state)))
     (setf (slot-value destination-container 'used) t)
     (bind (container-with-all-ingredients 1.0 destination-container nil)
           (container-with-rest 1.0 original-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (quantity 0.0 (make-instance 'quantity :value 100) nil)
           (unit 0.0 (make-instance 'unit) nil))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;               TO PORTION AND ARRANGE                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive portion-and-arrange ((container-with-portions transferable-container)
                                   (kitchen-state-out kitchen-state)
                                   (kitchen-state-in kitchen-state)
                                   (input-container transferable-container)
                                   (destination transferable-container))
  ((kitchen-state-in
    input-container
    destination
    =>
    kitchen-state-out
    container-with-portions)
   (let* ((input-container-vr-name (vr-name input-container))
          (destination-vr-name (vr-name destination))
          (vr-result (request-to-portion-and-arrange input-container-vr-name
                                                     destination-vr-name
                                                     (symbolic-to-vr-kitchen kitchen-state-in)))
          (portions-available-at (request-to-get-time))
          (kitchen-state-available-at portions-available-at)
          (new-kitchen-state (read-kitchen-state vr-result))
          (container-with-portions-instance (sim-find-object-by-vr-name destination-vr-name new-kitchen-state)))
     (bind (container-with-portions 1.0 container-with-portions-instance portions-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                    TO MIX                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive mix ((container-with-mixture transferable-container)
                   (kitchen-state-out kitchen-state)
                   (kitchen-state-in kitchen-state)
                   (container-with-input-ingredients transferable-container)
                   (mixing-tool can-mix))
  ;;Case 1: Mixing tool not specified, use a whisk
  ((kitchen-state-in
    container-with-input-ingredients
    =>
    kitchen-state-out
    container-with-mixture
    mixing-tool)
   (let* ((tool-vr-name
           (vr-name (find-kitchen-entity-vr 'whisk kitchen-state-in)))
          (vr-result
           (request-to-mix (vr-name container-with-input-ingredients)
                           tool-vr-name))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-mixture-vr-name (gethash "containerWithMixture" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (target-whisk-instance (sim-find-object-by-vr-name tool-vr-name new-kitchen-state))
          (container-with-new-mixture (sim-find-object-by-vr-name container-with-mixture-vr-name new-kitchen-state)))
     (setf (slot-value container-with-new-mixture 'used) t)
     (bind (mixing-tool 0.0 target-whisk-instance nil)
           (container-with-mixture 1.0 container-with-new-mixture container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  ;;Case 2: Mixing tool specified
  ((kitchen-state-in
    container-with-input-ingredients
    mixing-tool
    =>
    kitchen-state-out
    container-with-mixture)
   (let* ((vr-result (request-to-mix (vr-name container-with-input-ingredients)
                                     (vr-name mixing-tool)
                                     (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-mixture-vr-name (gethash "containerWithMixture" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (container-with-new-mixture (sim-find-object-by-vr-name container-with-mixture-vr-name new-kitchen-state)))
     (setf (slot-value container-with-new-mixture 'used) t)
     (bind (container-with-mixture 1.0 container-with-new-mixture container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                    TO MINGLE                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive mingle ((container-with-mixture transferable-container)
                      (kitchen-state-out kitchen-state)
                      (kitchen-state-in kitchen-state)
                      (container-with-input-ingredients transferable-container)
                      (mingling-tool can-mingle))
  ;;Case 1: Mingling tool not specified, use a whisk
  ((kitchen-state-in
    container-with-input-ingredients
    =>
    kitchen-state-out
    container-with-mixture
    mingling-tool)
   (let* ((tool-vr-name (vr-name (find-kitchen-entity-vr 'whisk kitchen-state-in)))
          (vr-result
           (request-to-mingle (vr-name container-with-input-ingredients)
                              tool-vr-name))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-mixture-vr-name (gethash "containerWithMixture" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (target-whisk-instance (sim-find-object-by-vr-name tool-vr-name new-kitchen-state))
          (container-with-new-mixture (sim-find-object-by-vr-name container-with-mixture-vr-name new-kitchen-state)))
     (setf (slot-value container-with-new-mixture 'used) t)
     (bind (mingling-tool 0.0 target-whisk-instance nil)
           (container-with-mixture 1.0 container-with-new-mixture container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  ;;Case 2: Mingling tool specified
  ((kitchen-state-in
    container-with-input-ingredients
    mingling-tool
    =>
    kitchen-state-out
    container-with-mixture)
   (let* ((vr-result (request-to-mingle (vr-name container-with-input-ingredients)
                                        (vr-name mingling-tool)
                                        (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-mixture-vr-name (gethash "containerWithMixture" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (container-with-new-mixture (sim-find-object-by-vr-name container-with-mixture-vr-name new-kitchen-state)))
     (setf (slot-value container-with-new-mixture 'used) t)
     (bind (container-with-mixture 1.0 container-with-new-mixture container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                    TO BEAT                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive beat ((container-with-mixture transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (container-with-input-ingredients transferable-container)
                    (beating-tool can-beat))
  ;;Case 1: Mixing tool not specified, use a spoon
  ((kitchen-state-in
    container-with-input-ingredients
    =>
    kitchen-state-out
    container-with-mixture
    beating-tool)
   (let* ((tool-vr-name (vr-name (find-kitchen-entity-vr 'spoon kitchen-state-in)))
          (vr-result (request-to-beat (vr-name container-with-input-ingredients)
                                      tool-vr-name
                                      (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-mixture-vr-name (gethash "containerWithIngredientsBeaten" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (target-beating-tool-instance (sim-find-object-by-vr-name tool-vr-name new-kitchen-state))
          (container-with-new-mixture (sim-find-object-by-vr-name container-with-mixture-vr-name new-kitchen-state)))
     (setf (slot-value container-with-new-mixture 'used) t)
     (bind (beating-tool 0.0 target-beating-tool-instance nil)
           (container-with-mixture 1.0 container-with-new-mixture container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                    TO LINE                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprimitive line ((lined-thing lineable)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (thing-to-be-lined lineable)
                    (lining-material can-be-lined-with))

  ;; Case 1: baking paper to line with is not given
  ((kitchen-state-in
    thing-to-be-lined
    =>
    lining-material
    kitchen-state-out
    lined-thing)

   (let* ((baking-paper-vr-name (vr-name (find-kitchen-entity-vr 'baking-paper kitchen-state-in)))
          (vr-result (request-to-line (vr-name thing-to-be-lined)
                                      baking-paper-vr-name
                                      (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (lined-thing-vr-name (gethash "linedBakingTray" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (target-tray (sim-find-object-by-vr-name lined-thing-vr-name new-kitchen-state))
          (new-baking-paper (sim-find-object-by-vr-name baking-paper-vr-name new-kitchen-state))) ;; default: baking-paper
     ;; TODO: This can be set in the VR kitchen for consistency
     (setf (slot-value thing-to-be-lined 'used) t)
     (setf (slot-value thing-to-be-lined 'lined-with) new-baking-paper)
     (bind (lining-material 1.0 new-baking-paper kitchen-state-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (lined-thing 1.0 target-tray container-available-at))))
  ;; Case 2
  ((kitchen-state-in
    lining-material
    thing-to-be-lined
    =>
    kitchen-state-out
    lined-thing)
   (let* ((vr-result (request-to-line (vr-name thing-to-be-lined)
                                      (vr-name lining-material)
                                      (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (lined-thing-vr-name (gethash "linedBakingTray" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (target-thing-to-be-lined (sim-find-object-by-vr-name lined-thing-vr-name new-kitchen-state)))
     ;; TODO: This can be set in the VR kitchen for consistency
     (setf (slot-value target-thing-to-be-lined 'used) t)
     (setf (slot-value target-thing-to-be-lined 'lined-with) lining-material)
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (lined-thing 1.0 target-thing-to-be-lined container-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                    TO SHAPE                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive shape ((shaped-portions kitchen-entity)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (portion kitchen-entity)
                     (container kitchen-entity)
                     (shape shape))
  ((kitchen-state-in
    portion
    container
    shape
    =>
    shaped-portions
    kitchen-state-out)
   (let* ((container-vr-name (vr-name container))
          (vr-result (request-to-shape (vr-contents container)
                                       (vr-shape shape)
                                       (symbolic-to-vr-kitchen kitchen-state-in)))
          (shaped-portions-available-at (request-to-get-time))
          (kitchen-state-available-at shaped-portions-available-at)
          (new-kitchen-state (read-kitchen-state vr-result))
          ;; TODO we dont need a list of portion names they already all sit in the baking tray
          ;; (list-of-portions (loop for name in list-of-new-portion-names
          ;;                        collect (sim-find-object-by-name name new-kitchen-state)))
          ;;(new-portions (make-instance 'list-of-kitchen-entities :items list-of-portions)))
          ;; NOTE convert lisp to camel case because find object by name does lisp to camel to find in jsondata
          (container-with-new-portions (sim-find-object-by-vr-name container-vr-name new-kitchen-state)))
     (bind (shaped-portions 1.0 container-with-new-portions shaped-portions-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO BAKE                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive bake ((thing-baked transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (thing-to-bake transferable-container)
                    (oven-to-bake-in oven)
                    (time-to-bake-quantity quantity)
                    (time-to-bake-unit unit)
                    (target-temperature-quantity quantity)
                    (target-temperature-unit unit))
  ;; Case 1: Preheated oven is not given
  ((kitchen-state-in
    thing-to-bake
    time-to-bake-quantity
    time-to-bake-unit
    target-temperature-quantity
    target-temperature-unit
    =>
    kitchen-state-out
    thing-baked
    oven-to-bake-in)
   (let* ((oven-vr-name (vr-name (find-kitchen-entity-vr 'oven kitchen-state-in)))
          (countertop-vr-name (vr-name (find-kitchen-entity-vr 'countertop kitchen-state-in)))
          (vr-result (request-to-bake (vr-name thing-to-bake)  ;;(lisp-to-camel-case (symbol-name (slot-value thing-to-bake 'name)))
                                      oven-vr-name
                                      countertop-vr-name
                                      (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-baked-available-at (request-to-get-time))
          (kitchen-state-available-at thing-baked-available-at)
          (thing-baked-vr-name (gethash "thingBaked" vr-result))
          (destination-vr-name (gethash "outputDestinationContainer" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (thing-baked-instance (sim-find-object-by-vr-name thing-baked-vr-name new-kitchen-state)))
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (thing-baked 1.0 thing-baked-instance thing-baked-available-at)
           (oven-to-bake-in 0.0 (sim-find-object-by-vr-name oven-vr-name kitchen-state-in) nil))))
  ;; Case 2: Preheated oven is available, temperature not mentioned
  ((kitchen-state-in
    thing-to-bake
    time-to-bake-quantity
    time-to-bake-unit
    oven-to-bake-in
    =>
    kitchen-state-out
    thing-baked
    target-temperature-quantity
    target-temperature-unit)
   ;; baked things never actually enter the oven!
   (let* ((oven-vr-name (vr-name oven-to-bake-in))
          (countertop-vr-name (vr-name (find-kitchen-entity-vr 'countertop kitchen-state-in)))
          (vr-result (request-to-bake (vr-name thing-to-bake)
                                      oven-vr-name
                                      countertop-vr-name
                                      (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-baked-available-at (request-to-get-time))
          (kitchen-state-available-at thing-baked-available-at)
          (thing-baked-vr-name (gethash "thingBaked" vr-result))
          (destination-vr-name (gethash "outputDestinationContainer" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (oven-temperature (vr->sym-temperature oven-vr-name new-kitchen-state))
          (thing-baked-instance (sim-find-object-by-vr-name thing-baked-vr-name new-kitchen-state)))
     (bind (kitchen-state-out 1.0 kitchen-state-in kitchen-state-available-at)
           (thing-baked 1.0 thing-baked-instance thing-baked-available-at)
           (target-temperature-quantity 0.0 (quantity oven-temperature) nil)
           (target-temperature-unit 0.0 (unit oven-temperature) nil))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO FRY                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive fry ((thing-fried transferable-container)
                   (kitchen-state-out kitchen-state)
                   (kitchen-state-in kitchen-state)
                   (thing-to-fry transferable-container)
                   (stove-to-fry-on oven)
                   (time-to-fry-quantity quantity)
                   (time-to-fry-unit unit)
                   (target-temperature-quantity quantity)
                   (target-temperature-unit unit))
  ;; Case 1: Preheated oven is not given
  ((kitchen-state-in
    thing-to-fry
    time-to-fry-quantity
    time-to-fry-unit
    target-temperature-quantity
    target-temperature-unit
    =>
    kitchen-state-out
    thing-fried
    stove-to-fry-on)
   (let* ((stove-vr-name (vr-name (find-kitchen-entity-vr 'oven kitchen-state-in)))
          (countertop-vr-name (vr-name (find-kitchen-entity-vr 'countertop kitchen-state-in)))
          (vr-result (request-to-fry (vr-name thing-to-fry)
                                     stove-vr-name
                                     (vr-quantity target-temperature-quantity) ;; TODO: need to add unit here too
                                     (vr-quantity time-to-fry-quantity)
                                     (vr-unit time-to-fry-unit)
                                     (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-fried-available-at (request-to-get-time))
          (kitchen-state-available-at thing-fried-available-at)
          (thing-fried-vr-name (gethash "thingFried" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (thing-fried-instance (sim-find-object-by-vr-name thing-fried-vr-name new-kitchen-state)))
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (thing-fried 1.0 thing-fried-instance thing-fried-available-at)
           (stove-to-fry-on 0.0 (sim-find-object-by-vr-name stove-vr-name kitchen-state-in) nil))))
  ;; Case 2: Preheated oven is available, temperature not mentioned
  ((kitchen-state-in
    thing-to-fry
    time-to-fry-quantity
    time-to-fry-unit
    stove-to-fry-on
    =>
    kitchen-state-out
    thing-fried
    target-temperature-quantity
    target-temperature-unit)
   ;; baked things never actually enter the oven!
   (let* ((stove-vr-name (vr-name stove-to-fry-on))
          (countertop-vr-name (vr-name (find-kitchen-entity-vr 'countertop kitchen-state-in)))
          ;; TO DO: wrong number of arguments!!
          (vr-result (request-to-fry (vr-name thing-to-fry)
                                     stove-vr-name
                                     countertop-vr-name
                                     (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-fried-available-at (request-to-get-time))
          (kitchen-state-available-at thing-fried-available-at)
          (thing-fried-vr-name (gethash "thingFried" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (stove-temperature (vr->sym-temperature stove-vr-name kitchen-state-in))
          (thing-fried-instance (sim-find-object-by-vr-name thing-fried-vr-name new-kitchen-state)))
     (bind (kitchen-state-out 1.0 kitchen-state-in kitchen-state-available-at)
           (thing-fried 1.0 thing-fried-instance thing-fried-available-at)
           (target-temperature-quantity 0.0 (quantity stove-temperature) nil)
           (target-temperature-unit 0.0 (unit stove-temperature) nil))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO BOIL                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive boil ((thing-boiled transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (thing-to-boil transferable-container)
                    (stove-to-boil-on oven)
                    (time-to-boil-quantity quantity)
                    (time-to-boil-unit unit))
  ((kitchen-state-in
    thing-to-boil
    time-to-boil-quantity
    time-to-boil-unit
    =>
    kitchen-state-out
    thing-boiled
    stove-to-boil-on)
   (let* ((stove-vr-name (vr-name (find-kitchen-entity-vr 'oven kitchen-state-in)))
          (countertop-vr-name (vr-name (find-kitchen-entity-vr 'countertop kitchen-state-in)))
          (vr-result (request-to-boil (vr-name thing-to-boil)
                                      stove-vr-name
                                      100
                                      (vr-quantity time-to-boil-quantity)
                                      (vr-unit time-to-boil-unit)
                                      countertop-vr-name
                                      (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-boiled-available-at (request-to-get-time))
          (kitchen-state-available-at thing-boiled-available-at)
          (thing-boiled-vr-name (gethash "thingBoiled" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (thing-boiled-instance (sim-find-object-by-vr-name thing-boiled-vr-name new-kitchen-state)))
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (thing-boiled 1.0 thing-boiled-instance thing-boiled-available-at)
           (stove-to-boil-on 0.0 (sim-find-object-by-vr-name stove-vr-name kitchen-state-in) nil))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO MELT                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive melt ((container-with-output-ingredients transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (container-with-input-ingredients transferable-container)
                    (melting-tool oven))
  ((kitchen-state-in
    container-with-input-ingredients
    =>
    kitchen-state-out
    container-with-output-ingredients
    melting-tool)
   (let* ((stove-vr-name (vr-name (find-kitchen-entity-vr 'oven kitchen-state-in)))
          (countertop-vr-name (vr-name (find-kitchen-entity-vr 'countertop kitchen-state-in)))
          (vr-result (request-to-melt (vr-name container-with-input-ingredients)
                                      stove-vr-name
                                      (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-melted-available-at (request-to-get-time))
          (kitchen-state-available-at thing-melted-available-at)
          (container-with-output-ingredients-vr-name (gethash "containerWithMeltedIngredients" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (thing-melted-instance (sim-find-object-by-vr-name container-with-output-ingredients-vr-name new-kitchen-state)))
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (container-with-output-ingredients 1.0 thing-melted-instance thing-melted-available-at)
           (melting-tool 0.0 (sim-find-object-by-vr-name stove-vr-name kitchen-state-in) nil))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO WASH                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive wash ((thing-washed transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (thing-to-wash transferable-container)
                    (washing-tool sink))
  ((kitchen-state-in
    thing-to-wash
    =>
    kitchen-state-out
    thing-washed
    washing-tool)
   (let* ((sink-vr-name (vr-name (find-kitchen-entity-vr 'sink kitchen-state-in)))
          (countertop-vr-name (vr-name (find-kitchen-entity-vr 'countertop kitchen-state-in)))
          (vr-result (request-to-wash (vr-name thing-to-wash)
                                      sink-vr-name
                                      countertop-vr-name
                                      (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-washed-available-at (request-to-get-time))
          (kitchen-state-available-at thing-washed-available-at)
          (thing-washed-vr-name (gethash "thingWashed" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (thing-washed-instance (sim-find-object-by-vr-name thing-washed-vr-name new-kitchen-state)))
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (thing-washed 1.0 thing-washed-instance thing-washed-available-at)
           (washing-tool 0.0 (sim-find-object-by-vr-name sink-vr-name kitchen-state-in) nil))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO PLACE                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive place ((thing-placed fetchable)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (destination container)
                     (thing-to-place fetchable))
  ((kitchen-state-in
    thing-to-place
    destination
    =>
    kitchen-state-out
    thing-placed)
   (let* ((vr-result
           (request-to-place (vr-name (find-kitchen-entity-vr (type-of thing-to-place) kitchen-state-in))
                             (vr-name (find-kitchen-entity-vr (type-of destination) kitchen-state-in))
                             (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-placed-available-at (request-to-get-time))
          (kitchen-state-available-at thing-placed-available-at)
          (thing-placed-vr-name (gethash "placedObject" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (thing-placed-instance (sim-find-object-by-vr-name thing-placed-vr-name new-kitchen-state)))
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (thing-placed 1.0 thing-placed-instance thing-placed-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO COVER                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive cover ((thing-covered coverable)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (thing-to-cover coverable)
                     (covering can-cover))
  ((kitchen-state-in
    thing-to-cover
    covering
    =>
    kitchen-state-out
    thing-covered)
   (let* ((vr-result (request-to-cover (vr-name thing-to-cover)
                                       (vr-name covering)
                                       (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-covered-available-at (request-to-get-time))
          (kitchen-state-available-at thing-covered-available-at)
          (thing-covered-vr-name (gethash "coveredObject" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (thing-covered-instance (sim-find-object-by-vr-name thing-covered-vr-name new-kitchen-state)))
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (thing-covered 1.0 thing-covered-instance thing-covered-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                     TO UNCOVER                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive uncover ((thing-uncovered coverable)
                       (kitchen-state-out kitchen-state)
                       (kitchen-state-in kitchen-state)
                       (thing-to-uncover coverable))
  ((kitchen-state-in
    thing-to-uncover
    =>
    kitchen-state-out
    thing-uncovered)
   (let* ((vr-result (request-to-uncover (vr-name thing-to-uncover)
                                         (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-uncovered-available-at (request-to-get-time))
          (kitchen-state-available-at thing-uncovered-available-at)
          (thing-uncovered-vr-name (gethash "uncoveredObject" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (thing-uncovered-instance (sim-find-object-by-vr-name thing-uncovered-vr-name new-kitchen-state)))
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (thing-uncovered 1.0 thing-uncovered-instance thing-uncovered-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                    TO SPRINKLE                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprimitive sprinkle ((sprinkled-object transferable-container)
                        (kitchen-state-out kitchen-state)
                        (kitchen-state-in kitchen-state)
                        (object transferable-container)
                        (topping-container container))
  ((kitchen-state-in
    object
    topping-container
    =>
    kitchen-state-out
    sprinkled-object)
   (let* ((object-vr-name (vr-name object))
          (topping-vr-name (vr-name topping-container))
          (vr-result (request-to-sprinkle object-vr-name
                                          topping-vr-name
                                          (symbolic-to-vr-kitchen kitchen-state-in)))
          (sprinkled-object-available-at (request-to-get-time))
          (kitchen-state-available-at sprinkled-object-available-at)
          (sprinkled-object-vr-name (gethash "sprinkledObject" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (sprinkled-object-instance (sim-find-object-by-vr-name sprinkled-object-vr-name new-kitchen-state)))
     (bind (sprinkled-object 1.0 sprinkled-object-instance sprinkled-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                     TO FLOUR                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive flour ((floured-object transferable-container)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (object transferable-container)
                     (topping-container container))
  ((kitchen-state-in
    object
    topping-container
    =>
    kitchen-state-out
    floured-object)
   (let* ((object-vr-name (vr-name object))
          (topping-vr-name (vr-name topping-container))
          (vr-result (request-to-flour object-vr-name
                                       topping-vr-name
                                       (symbolic-to-vr-kitchen kitchen-state-in)))
          (floured-object-available-at (request-to-get-time))
          (kitchen-state-available-at floured-object-available-at)
          (floured-object-vr-name (gethash "flouredObject" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (floured-object-instance (sim-find-object-by-vr-name floured-object-vr-name new-kitchen-state)))
     (bind (floured-object 1.0 floured-object-instance floured-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                     TO GREASE                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive grease ((greased-object transferable-container)
                      (kitchen-state-out kitchen-state)
                      (kitchen-state-in kitchen-state)
                      (object transferable-container)
                      (topping-container container))
  ((kitchen-state-in
    object
    topping-container
    =>
    kitchen-state-out
    greased-object)
   (let* ((object-vr-name (vr-name object))
          (topping-vr-name (vr-name topping-container))
          (vr-result (request-to-grease object-vr-name
                                        topping-vr-name
                                        (symbolic-to-vr-kitchen kitchen-state-in)))
          (greased-object-available-at (request-to-get-time))
          (kitchen-state-available-at greased-object-available-at)
          (greased-object-vr-name (gethash "greasedContainer" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (greased-object-instance (sim-find-object-by-vr-name greased-object-vr-name new-kitchen-state)))
     (bind (greased-object 1.0 greased-object-instance greased-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO CUT                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *cut-mapping*
  '((broccoli . chopped-broccoli)
    (red-onion . chopped-red-onion)
    (bacon . chopped-bacon)))

(defprimitive cut ((cut-object transferable-container)
                   (kitchen-state-out kitchen-state)
                   (kitchen-state-in kitchen-state)
                   (container-with-cuttable transferable-container)
                   (cut-pattern cutting-pattern)
                   (cutting-tool can-cut)
                   (cutting-surface can-be-cut-on))
  ;; Case 1: cutting tool not given (use a knife), cut-pattern given, cutting-surface not given (use cutting board)
  ((kitchen-state-in
    container-with-cuttable
    cut-pattern
    =>
    cut-object
    kitchen-state-out
    cutting-tool
    cutting-surface)
   (let* ((knife-vr-name
           (vr-name (find-kitchen-entity-vr 'can-cut kitchen-state-in)))
          (cutting-board-vr-name
           (vr-name (find-kitchen-entity-vr 'can-be-cut-on kitchen-state-in)))
          (countertop-vr-name
           (vr-name (find-kitchen-entity-vr 'counter-top kitchen-state-in)))
          (cuttable-object
           (first (contents container-with-cuttable)))
          (vr-result-1
           (progn
             (request-to-place cutting-board-vr-name
                               countertop-vr-name)
             (request-to-place (vr-name cuttable-object)
                               cutting-board-vr-name)
             (request-to-cut (vr-name cuttable-object)
                             knife-vr-name
                             (vr-pattern cut-pattern))))
          (new-kitchen-state-1 (read-kitchen-state vr-result-1))
          (cut-objects
           (find-all-kitchen-entities-vr (rest (assoc (type-of cuttable-object) *cut-mapping*))
                                         new-kitchen-state-1))
          (cut-objects-vr-names (mapcar #'vr-name cut-objects))
          (kitchen-cabinet-vr-name
           (vr-name (find-kitchen-cabinet-with-most-space new-kitchen-state-1)))
          (vr-result-2
           (loop for name in cut-objects-vr-names
                 do (request-to-place name (vr-name container-with-cuttable))
                 finally
                   (return
                    (request-to-place (vr-name container-with-cuttable)
                                      kitchen-cabinet-vr-name))))
          (new-kitchen-state-2 (read-kitchen-state vr-result-2))
          (cut-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cutting-tool-instance
           (sim-find-object-by-vr-name knife-vr-name new-kitchen-state-2))
          (cut-portions-container
           (sim-find-object-by-vr-name (vr-name container-with-cuttable) new-kitchen-state-2))
          (cutting-location-2
           (sim-find-object-by-vr-name cutting-board-vr-name new-kitchen-state-2)))
     (loop for portion in (contents cut-portions-container)
           do (setf (slot-value portion 'is-cut)
                    (make-instance (type-of cut-pattern))))
     (setf (slot-value cut-portions-container 'used) t)
     (bind (cut-object 1.0 cut-portions-container cut-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state-2 kitchen-state-available-at)
           (cutting-tool 0.0 cutting-tool-instance cut-object-available-at)
           (cutting-surface 0.0 cutting-location-2 cut-object-available-at))))
  
  ;; Case 4: cutting tool given, cut-pattern given, cutting surface given
  #|((kitchen-state-in
    container-with-cuttable
    cut-pattern
    cutting-tool
    cutting-surface
    =>
    cut-object
    kitchen-state-out)
   (let* ((knife-vr-name (vr-name cutting-tool))
          (cutting-board-vr-name (vr-name cutting-surface))
          (countertop-vr-name (vr-name (find-kitchen-entity-vr 'countertop kitchen-state-in)))
          (vr-result (request-to-place cutting-board-vr-name
                                    countertop-vr-name
                                    (symbolic-to-vr-kitchen kitchen-state-in)))
          (vr-result (request-to-place (vr-name container-with-cuttable)
                                    cutting-board-vr-name
                                    nil)) ;; no need to reset state since performing actions immediately after one another
          (vr-result (request-to-cut (vr-name container-with-cuttable)
                                  knife-vr-name
                                  (vr-pattern cut-pattern)
                                  nil))
          (cut-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (new-kitchen-state (read-kitchen-state vr-result))
          (cutting-location (sim-find-object-by-vr-name cutting-board-vr-name new-kitchen-state))
          (cut-portions-vr-names (vr-contents cutting-location))
          (cut-portions-instances (mapcar (lambda (x) (sim-find-object-by-vr-name x new-kitchen-state)) cut-portions-vr-names)))
     (bind (cut-object 1.0 cut-portions-instances cut-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))|#
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO REFRIGERATE                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive refrigerate ((container-with-ingredients-at-temperature transferable-container)
                           (kitchen-state-out kitchen-state)
                           (kitchen-state-in kitchen-state)
                           (container-with-ingredients transferable-container)
                           (refrigerator fridge)
                           (cooling-quantity quantity)
                           (cooling-unit time-unit))
  ;; Case 1: refrigerator and cooling time (quantity and unit) are not given, use 0 as cooling time; this allows the command to finish once the item is in the refrigerator
  ((kitchen-state-in
    container-with-ingredients
    =>
    cooling-unit
    cooling-quantity
    refrigerator
    kitchen-state-out
    container-with-ingredients-at-temperature)
   (let* ((refrigerator-vr-name (vr-name (find-kitchen-entity-vr 'fridge kitchen-state-in)))
          (new-cooling-quantity (make-instance 'quantity :value 0))
          (new-cooling-unit (make-instance 'minute))
          (vr-result (request-to-refrigerate (vr-name container-with-ingredients)
                                             refrigerator-vr-name
                                             0
                                             "min"))
          (cooled-object-available-at (request-to-get-time))
          (kitchen-state-available-at cooled-object-available-at)
          (cooled-object-vr-name (gethash "containerWithIngredientsAtTemperature" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (cooled-object-instance (sim-find-object-by-vr-name cooled-object-vr-name new-kitchen-state))
          (new-fridge-instance (sim-find-object-by-vr-name refrigerator-vr-name new-kitchen-state)))
     (bind (container-with-ingredients-at-temperature 1.0 cooled-object-instance cooled-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (refrigerator 0.0 new-fridge-instance kitchen-state-available-at)
           (cooling-quantity 0.0 new-cooling-quantity nil)
           (cooling-unit 0.0 new-cooling-unit nil))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO PEEL                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defprimitive peel ((output-container-for-peeled transferable-container)
                    (container-for-peels transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (input-container-for-peeled transferable-container)
                    (peeling-tool can-peel))
  ((kitchen-state-in
    input-container-for-peeled
    =>
    peeling-tool
    container-for-peels
    output-container-for-peeled
    kitchen-state-out)
   (let* ((tool-vr-name
           (vr-name (find-kitchen-entity-vr 'can-peel kitchen-state-in)))
          (peeled-container-vr-name
           (vr-name input-container-for-peeled))
          (object-to-peel-vr-name
           (vr-name (first (contents input-container-for-peeled))))
          (container-for-peels-vr-name
           (vr-name (find-unused-kitchen-entity-vr 'medium-bowl kitchen-state-in)))
          (countertop-vr-name
           (vr-name (find-kitchen-entity-vr 'counter-top kitchen-state-in)))
          (kitchen-cabinet-vr-name
           (vr-name (find-kitchen-cabinet-with-most-space kitchen-state-in)))
          (vr-result
           (progn
             (request-to-place container-for-peels-vr-name
                               countertop-vr-name)
             (request-to-peel object-to-peel-vr-name
                              tool-vr-name
                              container-for-peels-vr-name
                              peeled-container-vr-name)
             (request-to-place container-for-peels-vr-name
                               kitchen-cabinet-vr-name)))
          (peeled-object-available-at (request-to-get-time))
          (kitchen-state-available-at peeled-object-available-at)
          (new-kitchen-state (read-kitchen-state vr-result))
          (output-container-for-peeled-instance
           (sim-find-object-by-vr-name peeled-container-vr-name new-kitchen-state))
          (container-for-peels-instance
           (sim-find-object-by-vr-name container-for-peels-vr-name new-kitchen-state))
          (peeling-tool-instance
           (sim-find-object-by-vr-name tool-vr-name new-kitchen-state)))
     (setf (slot-value output-container-for-peeled-instance 'used) t)
     (setf (slot-value container-for-peels-instance 'used) t)
     (bind (output-container-for-peeled 1.0 output-container-for-peeled-instance peeled-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (peeling-tool 0.0 peeling-tool-instance peeled-object-available-at)
           (container-for-peels 0.0 container-for-peels-instance peeled-object-available-at))))
  :primitive-inventory *vr-primitives*)
  
;;                                                          ;;
;;                      TO SEED                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive seed ((container-for-seeds transferable-container)
                    (output-container-for-seeded transferable-container)
                    (seeding-tool can-seed)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (input-container-for-seeded transferable-container)
                    (object seedable))
  ((kitchen-state-in object input-container-for-seeded => seeding-tool container-for-seeds output-container-for-seeded kitchen-state-out)
   (let* ((tool-vr-name (vr-name (find-kitchen-entity-vr 'can-seed kitchen-state-in)))
          (seeded-container-vr-name (vr-name input-container-for-seeded))
          (container-for-seeds-vr-name (vr-name (find-unused-kitchen-entity-vr 'medium-bowl kitchen-state-in)))
          (countertop-vr-name (vr-name (find-kitchen-entity-vr 'countertop kitchen-state-in)))
          (vr-result (request-to-place container-for-seeds-vr-name
                                    countertop-vr-name
                                    (symbolic-to-vr-kitchen kitchen-state-in)))
          (vr-result (request-to-seed (vr-name object)
                                   tool-vr-name
                                   container-for-seeds-vr-name
                                   seeded-container-vr-name
                                   nil))
          (seeded-object-available-at (request-to-get-time))
          (kitchen-state-available-at seeded-object-available-at)
          (new-kitchen-state (read-kitchen-state vr-result))
          (output-container-for-seeded-instance (sim-find-object-by-vr-name seeded-container-vr-name new-kitchen-state))
          (container-for-seeds-instance (sim-find-object-by-vr-name container-for-seeds-vr-name new-kitchen-state))
          (seeding-tool-instance (sim-find-object-by-vr-name tool-vr-name new-kitchen-state)))
     (bind (output-container-for-seeded 1.0 output-container-for-seeded-instance seeded-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (seeding-tool 0.0 seeding-tool-instance seeded-object-available-at)
           (container-for-seeds 0.0 container-for-seeds-instance seeded-object-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                   TO GET LOCATION                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive get-location ((location-out transferable-container)
                            (kitchen-state-out kitchen-state)
                            (kitchen-state-in kitchen-state)
                            (location-type conceptualizable))
  ((kitchen-state-in
    location-type
    =>
    kitchen-state-out
    location-out)
   (let* ((location-vr-type (vr-type location-type))
          (vr-result (request-to-get-location "location"
                                         location-vr-type
                                         (symbolic-to-vr-kitchen kitchen-state-in)))
          (location-available-at (request-to-get-time))
          (kitchen-state-available-at location-available-at)
          (location-vr-name (gethash "location" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (location-instance (sim-find-object-by-vr-name location-vr-name new-kitchen-state)))
     (bind (location-out 1.0 location-instance location-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                     TO FLATTEN                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive flatten ((flattened-object transferable-container)
                       (kitchen-state-out kitchen-state)
                       (kitchen-state-in kitchen-state)
                       (object transferable-container)
                       (flattening-tool can-flatten))
  ((kitchen-state-in
    object
    =>
    kitchen-state-out
    flattening-tool
    flattened-object)
   (let* ((object-vr-name (vr-name object))
          (tool-vr-name (vr-name (find-kitchen-entity-vr 'can-flatten kitchen-state-in)))
          (vr-results (mapcar (lambda (x)
                                (request-to-flatten (vr-name x)
                                         tool-vr-name
                                         (symbolic-to-vr-kitchen kitchen-state-in)))
                              (vr-contents object)))
          (vr-result (car vr-results))
          (flattened-object-available-at (request-to-get-time))
          (kitchen-state-available-at flattened-object-available-at)
          (flattened-object-vr-name (gethash "containerWithFlattenedItems" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (flattened-object-instance (sim-find-object-by-vr-name flattened-object-vr-name new-kitchen-state)))
     (bind (flattened-object 1.0 flattened-object-instance flattened-object-available-at)
           (flattening-tool 1.0 (sim-find-object-by-vr-name tool-vr-name new-kitchen-state) flattened-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO GRIND                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive grind ((ground-object transferable-container)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (object transferable-container)
                     (grinding-tool can-grind))
  ((kitchen-state-in
    object
    =>
    kitchen-state-out
    grinding-tool
    ground-object)
   (let* ((object-vr-name (vr-name object))
          (tool-vr-name (vr-name (find-kitchen-entity-vr 'can-grind kitchen-state-in)))
          (vr-result (request-to-grind object-vr-name
                                       tool-vr-name
                                       (symbolic-to-vr-kitchen kitchen-state-in)))
          (ground-object-available-at (request-to-get-time))
          (kitchen-state-available-at ground-object-available-at)
          (ground-object-vr-name (gethash "containerWithGroundIngredients" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (ground-object-instance (sim-find-object-by-vr-name ground-object-vr-name new-kitchen-state)))
     (bind (ground-object 1.0 ground-object-instance ground-object-available-at)
           (grinding-tool 1.0 (sim-find-object-by-vr-name tool-vr-name new-kitchen-state) ground-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                      TO MASH                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive mash ((mashed-object transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (object transferable-container)
                    (mashing-tool can-mash))
  ((kitchen-state-in
    object
    =>
    kitchen-state-out
    mashing-tool
    mashed-object)
   (let* ((object-vr-name (vr-name object))
          (tool-vr-name (vr-name (find-kitchen-entity-vr 'can-mash kitchen-state-in)))
          (vr-result (request-to-mash object-vr-name
                                      tool-vr-name
                                      (symbolic-to-vr-kitchen kitchen-state-in)))
          (mashed-object-available-at (request-to-get-time))
          (kitchen-state-available-at mashed-object-available-at)
          (mashed-object-vr-name (gethash "mashedIngredient" vr-result))
          (new-kitchen-state (read-kitchen-state vr-result))
          (mashed-object-instance (sim-find-object-by-vr-name mashed-object-vr-name new-kitchen-state)))
     (bind (mashed-object 1.0 mashed-object-instance mashed-object-available-at)
           (mashing-tool 1.0 (sim-find-object-by-vr-name tool-vr-name new-kitchen-state) mashed-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)


