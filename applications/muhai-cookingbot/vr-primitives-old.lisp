(in-package :muhai-cookingbot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                             ;;
;; This file contains an implementation of the VR primitives   ;;
;; used by the cooking bot.                                    ;;
;;                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Defining the VR primitive inventory  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-irl-primitives vr-inventory :primitive-inventory *vr-primitives*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;                  HELP FUNCTIONS                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {'ChoppedCookedBacon', 'ChoppedBroccoli', 'LargeBowl', 'Fridge', 'Bacon', 'Floor', 'Abe', 'SugarBag', 'Broccoli', 'PotatoPeel', 'MayonnaiseParticle', 'Potato', 'MediumBowl', 'CookedBacon', 'DressingParticle', 'MayonnaiseJar', 'FryingPan', 'CiderVinegar', 'CiderVinegarParticle', 'PotatoParticle', 'Oven', 'PeeledRedOnion', 'KitchenCounter', 'GratedMozzarellaBag', 'CookingKnife', 'Whisk', 'KitchenCabinet', 'CuttingBoard', 'PeeledPotato', 'Masher'}
;; {'PottedPlant', 'NorthWall', 'WestWall', 'Ceiling', 'KitchenCounter', 'MediumBowl', 'Chair', 'Fridge', 'Oven', 'KitchenSink', 'Table', 'VanillaExtractBag', 'KitchenCabinet', 'SugarBag', 'Abe', 'BakingTray', 'BakingSheet', 'SouthWall', 'TrashCan', 'FryingPan', 'Colander', 'Floor', 'ButterBag', 'CookingKnife', 'Whisk', 'EastWall', 'AlmondExtractBag', 'CookingPot', 'FreshCilantro'}


;; TODO: A way in VR kitchen to search for 'sugar' and it returns the container with 'sugar' in it
;; TODO: Type names in the VR kitchen should be ontology with the yaml ontology so this mapping can be removed.
(defvar *type-mapping* '(;; bags
                         (sugar-bag              . sugar)
                         (sugar-bag              . white-sugar)
                         (butter-bag             . butter)
                         (vanilla-extract-bag    . vanilla-extract)
                         (almond-extract-bag     . almond-extract)
                         (flour-bag              . all-purpose-flour)
                         (almond-flour-bag       . almond-flour)
                         ;; particles
                         (butterparticle         . butter-particle)
                         (sugarparticle          . sugar-particle)
                         (flourparticle          . flour-particle)
                         (almondflourparticle    . almond-flour-particle)
                         (vanillaextractparticle . vanilla-extract-particle)
                         (almondextractparticle  . almond-extract-particle)
                         ;; renamings
                         (baking-sheet           . baking-paper)
                         (kitchenstove           . oven)
                         (kitchen-counter        . counter-top)
                         (spoon                  . wooden-spoon)
                         (shaker                 . sugar-shaker)
                         (doughclump             . dough)
                         (powdered-white-sugar   . sugar)
                         ;; agent
                         (abe                    . agent)))


(defun map-type (type)
  (or (cdr (assoc type *type-mapping*))
      (find-class type nil)
      'kitchen-entity))


(defun rmap-type (type)
  (or (car (rassoc type *type-mapping*))
      (find-class type nil)))


(defun find-entity-by-name (name entity)
  (if (string= (name entity) name)
    entity
    (when (slot-exists-p entity 'contents)
      (loop for content in (contents entity)
            when (find-entity-by-name name content)
            return it))))


(defun find-empty-container-of-type (type entity)
  (if (and (slot-exists-p entity 'contents)
           (eq (contents entity) nil)
           (eq (type-of entity) type))
    entity
    (when (slot-exists-p entity 'contents)
      (loop for content in (contents entity)
            when (find-empty-container-of-type type content)
            return it))))


(defun hash-table->alist-rec (data)
  (if (hash-table-p data)
    (let ((alist (or (hash-table-alist data) 'empty-hash)))
      (if (eql alist 'empty-hash)
        'empty-hash
        (loop for (key . value) in alist
              if (hash-table-p value)
              collect (cons key (hash-table->alist-rec value))
              else collect (cons key value))))
    data))


(defun alist->hash-table-rec (data)
  (cond ((and data (alistp data))
         (alist-hash-table
          (loop for (key . value) in data
                collect (cons (lisp->camel-case (mkstr key) :from-first nil)
                              (alist->hash-table-rec value)))))
        ((eql data 'empty-hash)
         (make-hash-table))
        (t data)))


(defun keys-for-vr (table)
  ;; recursively traverse all hash tables
  ;; and fix the keys using
  ;; (lisp->camel-case (mkstr key) :from-first nil)
  )

(defun merge-hash-tables (table-1 table-2)
  (let ((merged-hash (copy-object table-1)))
    (loop for key being the hash-keys of table-2
          using (hash-value value)
          do (setf (gethash key merged-hash) value))
    merged-hash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;               TO SYMBOLIC                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-object-slots (object custom-state-variables list-of-slots-to-set)
  "When the <custom-state-variables> (hash table) of the VR simulator
   specify a slot that is present in the CLOS <object>,
   and that slot is part of <list-of-slots-to-set>,
   set the slot!"
  (loop for slot in (closer-mop:class-slots (class-of object))
        for slot-name = (closer-mop:slot-definition-name slot)
        for slot-key-accessor = (car (closer-mop:slot-definition-initargs slot))
        when (and custom-state-variables
                  (gethash slot-key-accessor custom-state-variables)
                  (member slot-name list-of-slots-to-set))
        do (setf (slot-value object slot-name)
                 (intern (gethash slot-key-accessor custom-state-variables)))))


(defun make-object (type simulation-data name)
  "Make an object of <type> with given <simulation-data>
   and <name>"
  (let* ((custom-state-variables
          (gethash :custom-state-variables simulation-data))
         (substance
          (when custom-state-variables
            (gethash :substance custom-state-variables)))
         (object-type
          (map-type (read-from-string (camel-case->lisp (or substance type)))))
         (object
          (make-instance object-type
                         :name name
                         :simulation-data simulation-data)))
    (set-object-slots object custom-state-variables '(persistent-id))
    object))


(defun vr-to-symbolic-kitchen (ks)
  (labels ((triple-name     (x) (first x))
           (triple-parent   (x) (second x))
           (triple-object   (x) (third x))
           (constraint?     (x) (string= "kcon" (gethash "simtype" x)))
           (object?         (x) (string= "ktree" (gethash "simtype" x))))
    (let* ((object-triples
            (loop for key being the hash-keys of ks
                    using (hash-value properties)
                  for type = (gethash "type" properties)
                  for parent = (gethash "at" properties)
                  for name = (gethash "name" properties)
                  for object = (when (and type (object? properties))
                                 (make-object type properties name))
                  when object collect (list name parent object)))
           (top-level-objects
            (loop for (name parent-name object) in object-triples
                  for parent-object =
                    (triple-object (find parent-name object-triples
                                         :key #'triple-name :test #'string=))
                  if (and parent-object (slot-exists-p parent-object 'contents))
                    do (push object (contents parent-object))
                  else collect object))
           (constraints-hash
            (loop with constraints-hash = (make-hash-table)
                  for key being the hash-keys of ks
                    using (hash-value properties)
                  when (constraint? properties)
                    do (setf (gethash key constraints-hash) properties)
                  finally (return constraints-hash))))
      (make-instance 'kitchen-state
                     :contents top-level-objects
                     :constraints constraints-hash))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;               TO VR                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slots-to-hash (object)
  (loop with hash-table = (make-hash-table)
        for slot in (closer-mop:class-slots (class-of object))
        for slot-name = (closer-mop:slot-definition-name slot)
        for slot-initarg = (car (closer-mop:slot-definition-initargs slot))
        for slot-value = (slot-value object slot-name)

        ;; TODO we will need to send properties too if the simulator accepts them
        unless (or (eq slot-name 'simulation-data)
                   (eq slot-name 'amount)
                   (eq slot-name 'quantity)
                   (eq slot-name 'arrangement)
                   (eq slot-name 'contents)
                   (eq slot-name 'temperature))
        do (setf (gethash slot-initarg hash-table) slot-value)
        finally (return hash-table)))


(defun symbolic-to-vr-object (object)
  (let* ((simulation-data (simulation-data object))
         (hash-slots (slots-to-hash object))
         (custom-state-variables
          (when simulation-data
            (gethash :custom-state-variables simulation-data))))
    (when custom-state-variables
      (setf (gethash :custom-state-variables simulation-data)
            (merge-hash-tables custom-state-variables hash-slots)))
    simulation-data))


(defun symbolic-to-vr-kitchen (kitchen)
  (labels ((has-contents (o) (slot-exists-p o 'contents))
           (collect-objects (root)
             (if (has-contents root)
               (cons root (mappend #'collect-objects (contents root)))
               (list root))))
    (let* ((all-objects (mappend #'collect-objects (contents kitchen)))
           (vr-kitchen (make-hash-table :test #'string=)))
      (loop for object in all-objects
            for key = (make-kw (camel-case->lisp (name object)))
            for value = (symbolic-to-vr-object object)
            when value
            do (setf (gethash key vr-kitchen) value))
      (merge-hash-tables vr-kitchen (constraints kitchen)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;                   PRIMITIVES                             ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;                                                          ;;
;;                    TO GET                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprimitive get-kitchen ((kitchen kitchen-state))
  ((=> kitchen)
   (let* ((kitchen-variable-name 'kitchen)
          (response-result (assqv :kitchen (list (request-get-kitchen kitchen-variable-name))))
          (new-kitchen-state (vr-to-symbolic-kitchen response-result))
          (new-kitchen-time (request-to-get-time)))
     (bind (kitchen 1.0 new-kitchen-state new-kitchen-time))))
  :primitive-inventory *vr-primitives*)


;;                                                          ;;
;;                    TO SET                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprimitive set-kitchen ((kitchen-in kitchen-state)
                           (kitchen-out kitchen-state))

  ((kitchen-in => kitchen-out)
   (let* ((kitchen-to-send (symbolic-to-vr-kitchen kitchen-in)))
     (request-set-kitchen kitchen-to-send)
     (bind (kitchen-out 1.0 kitchen-in 0.0))))
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

  ;; Takes a specified amount of an ingredient from somewhere in the kitchen and places it in
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
           ;; find an unused container of type medium-bowl
           (find-empty-container-of-type 'medium-bowl kitchen-state-in)) 
          (vr-fetch-result 
           ;; place the unused container on the countertop
           (request-to-fetch (slot-value unused-container 'name)
                             (symbolic-to-vr-kitchen kitchen-state-in)))
          (ks-after-fetch
           ;; parse the new kitchen state
           (vr-to-symbolic-kitchen (assqv :kitchen-state-out vr-fetch-result)))
          (ingredient-concept-type
           (rmap-type (type-of ingredient-concept)))
          (vr-portion-result
           ;; portion the ingredient in the unused container
           (request-to-portion ingredient-concept-type
                               (slot-value unused-container 'name)
                               (value quantity)
                               (symbolic-to-vr-kitchen ks-after-fetch)))
          (ks-after-portion
           ;; parse new kitchen state
           (vr-to-symbolic-kitchen (assqv :kitchen-state-out vr-portion-result)))
          (output-container-name
           (assqv :output-container vr-portion-result))
          (output-container
           (find-entity-by-name output-container-name ks-after-portion))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at))

     ;; TODO: This can be set in the VR kitchen for consistency
     (setf (slot-value output-container 'used) t)
     (bind (target-container 1.0 unused-container current-kitchen-time)
           (container-with-ingredient 1.0 output-container container-available-at)
           (kitchen-state-out 1.0 ks-after-portion kitchen-state-available-at))))
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

   (let* ((current-kitchen-time (kitchen-time kitchen-state-in))
          (unused-container
           (find-empty-container-of-type 'medium-bowl kitchen-state-in))
          (vr-transfer-result
           (request-to-transfer (name container-with-input-ingredients)
                                (name unused-container)
                                (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-rest-name
           (assqv :container-with-rest vr-transfer-result))
          (container-with-all-ingredients-name
           (assqv :container-with-all-ingredients vr-transfer-result))
          (vr-ks-after-transfer
           (assqv :kitchen-state-out vr-transfer-result))
          (ks-after-transfer
           (vr-to-symbolic-kitchen vr-ks-after-transfer))
          (original-container
           (find-entity-by-name container-with-rest-name ks-after-transfer))
          (resulting-contents
           (find-entity-by-name container-with-all-ingredients-name ks-after-transfer)))

     (setf (slot-value resulting-contents 'used) t)

     (bind (target-container 1.0 resulting-contents current-kitchen-time)
           (container-with-all-ingredients 1.0 resulting-contents container-available-at)
           (container-with-rest 1.0 original-container container-available-at)
           (kitchen-state-out 1.0 ks-after-transfer kitchen-state-available-at)
           (quantity 1.0 (make-instance 'quantity :value (value (quantity (amount container-with-input-ingredients)))) current-kitchen-time)
           (unit 1.0 (make-instance (type-of (unit (amount container-with-input-ingredients)))) current-kitchen-time))))


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

   (let* ((current-kitchen-time (kitchen-time kitchen-state-in))
          (vr-transfer-result
           (request-to-transfer (name container-with-input-ingredients)
                                (name target-container)
                                (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-rest-name (assqv :container-with-rest vr-transfer-result))
          (container-with-all-ingredients-name (assqv :container-with-all-ingredients vr-transfer-result))
          (vr-ks-after-transfer
           (assqv :kitchen-state-out vr-transfer-result))
          (ks-after-transfer
           (vr-to-symbolic-kitchen vr-ks-after-transfer))
          (original-container
           (find-entity-by-name container-with-rest-name ks-after-transfer))
          (resulting-contents
           (find-entity-by-name container-with-all-ingredients-name ks-after-transfer)))

     (setf (slot-value resulting-contents 'used) t)
     
     (bind (container-with-all-ingredients 1.0 resulting-contents container-available-at)
           (container-with-rest 1.0 original-container container-available-at)
           (kitchen-state-out 1.0 ks-after-transfer kitchen-state-available-at)
           (quantity 1.0 (make-instance 'quantity :value (value (quantity (amount container-with-input-ingredients)))) current-kitchen-time)
           (unit 1.0 (make-instance (type-of (unit (amount container-with-input-ingredients)))) current-kitchen-time))))
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

   (let* ((res (request-to-mix (slot-value container-with-input-ingredients 'name)
                               'whisk
                               (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-mixture-name (cdr (assoc :container-with-mixture res)))
          (new-kitchen-state (vr-to-symbolic-kitchen (cdr (assoc :kitchen-state-out res))))
          (target-whisk-instance (find-entity-by-name "whisk" new-kitchen-state))
          (container-with-new-mixture (find-entity-by-name container-with-mixture-name new-kitchen-state)))

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

   (let* ((res (request-to-mix (slot-value container-with-input-ingredients 'name)
                               (slot-value mixing-tool 'name)
                               (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-mixture-name (cdr (assoc :container-with-mixture res)))
          (new-kitchen-state (vr-to-symbolic-kitchen (cdr (assoc :kitchen-state-out res))))
          (container-with-new-mixture (find-entity-by-name container-with-mixture-name new-kitchen-state)))

     (setf (slot-value container-with-new-mixture 'used) t)

     (bind (container-with-mixture 1.0 container-with-input-ingredients-instance container-available-at)
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

   (let* ((res (request-to-beat (slot-value container-with-input-ingredients 'name) 'spoon (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-mixture-name (cdr (assoc :container-with-mixture res)))
          (new-kitchen-state (vr-to-symbolic-kitchen (cdr (assoc :kitchen-state-out res))))
          (target-beating-tool-instance (find-entity-by-name "spoon" new-kitchen-state))
          (container-with-new-mixture (find-entity-by-name container-with-mixture-name new-kitchen-state)))

     (setf (slot-value container-with-new-mixture 'used) t)

     (bind (beating-tool 0.0 target-beating-tool-instance nil)
       (container-with-mixture 1.0 container-with-new-mixture container-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
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

   (let* ((res-get-location (request-get-location 'item (rmap-type (type-of concept-to-fetch)) (symbolic-to-vr-kitchen kitchen-state-in)))
          (item-name (cdr (assoc :item res-get-location)))
          (res (request-to-fetch item-name (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-fetched-available-at (request-to-get-time))
          (kitchen-state-available-at thing-fetched-available-at)
          (fetched-obj-name (cdr (assoc :fetched-object res)))
          (fetched-kitchen-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen fetched-kitchen-alist))
          (fetched-object-instance (find-entity-by-name fetched-obj-name new-kitchen-state)))

     ;; TODO: Is a fetched thing on countertop used or not?

     (bind (thing-fetched 1.0 fetched-object-instance thing-fetched-available-at)
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

   (let* ((res (request-to-line (slot-value thing-to-be-lined 'name)
                                'baking-paper
                                (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (lined-thing-name (cdr (assoc :lined-baking-tray res)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (target-tray (find-entity-by-name lined-thing-name new-kitchen-state))
          (new-baking-paper (find-entity-by-name "baking-paper" new-kitchen-state))) ;; default: baking-paper

     ;; TODO: This can be set in the VR kitchen for consistency
     (setf (slot-value thing-to-be-lined 'used) t)
     (setf (slot-value thing-to-be-lined 'lined-with) new-baking-paper)

     (bind (lining-material 1.0 new-baking-paper kitchen-state-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
       (lined-thing 1.0 target-tray container-available-at))))


  ;; Case 1
  ((kitchen-state-in
    lining-material
    thing-to-be-lined
    =>
    kitchen-state-out
    lined-thing)

   (let* ((res (request-to-line (slot-value thing-to-be-lined 'name)
                                (slot-value lining-material 'name)
                                (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (lined-thing-name (cdr (assoc :lined-baking-tray res)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (target-thing-to-be-lined (find-entity-by-name lined-thing-name new-kitchen-state)))

     ;; TODO: This can be set in the VR kitchen for consistency
     (setf (slot-value target-tray 'used) t)
     (setf (slot-value target-tray 'lined-with) lining-material)

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

   (let* ((res (request-to-shape (slot-value portion 'name)
                                 (slot-value container 'name)
                                 (symbolic-to-vr-kitchen kitchen-state-in)))
          (shaped-portions-available-at (request-to-get-time))
          (kitchen-state-available-at shaped-portions-available-at)
          (list-of-new-portion-names (cdr (assoc :shaped-portions res)))
          (new-kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen new-kitchen-state-alist))
          ;; TODO we dont need a list of portion names they already all sit in the baking tray
          ;; (list-of-portions (loop for name in list-of-new-portion-names
          ;;                        collect (find-entity-by-name name new-kitchen-state)))
          ;;(new-portions (make-instance 'list-of-kitchen-entities :items list-of-portions)))
          ;; NOTE convert lisp to camel case because find object by name does lisp to camel to find in jsondata
          (container-camel-name (lisp->camel-case (symbol-name (slot-value container 'name))))
          (container-with-new-portions (find-entity-by-name container-camel-name new-kitchen-state)))

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

   ;; TODO maybe get oven name instead string "kitchenstove"

   (let* ((res (request-to-bake (lisp->camel-case (symbol-name (slot-value thing-to-bake 'name)))
                                "kitchenStove"
                                "counterTop"
                                (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-baked-available-at (request-to-get-time))
          (kitchen-state-available-at thing-baked-available-at)
          (thing-baked-name (cdr (assoc :thing-baked res)))
          (destination-name (cdr (assoc :output-destination-container res)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (thing-baked-instance (find-entity-by-name thing-baked-name new-kitchen-state)))

     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
       (thing-baked 1.0 thing-baked-instance thing-baked-available-at)
       (oven-to-bake-in 0.0 (find-entity-by-name "kitchenStove" kitchen-state-in) nil))))

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
   (let* ((res (request-to-bake (slot-value thing-to-bake 'name)
                                "kitchenStove"
                                "counterTop"
                                (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-baked-available-at (request-to-get-time))
          (kitchen-state-available-at thing-baked-available-at)
          (thing-baked-name (cdr (assoc :thing-baked res)))
          (destination-name (cdr (assoc :output-destination-container res)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (make-instance 'kitchen-state :contents (vr-to-symbolic-kitchen kitchen-state-alist)))
          (thing-baked-instance (find-entity-by-name thing-baked-name new-kitchen-state)))


     (bind (kitchen-state-out 1.0 kitchen-state-in kitchen-state-available-at)
       (thing-baked 1.0 thing-baked-instance thing-baked-available-at)
       (target-temperature-quantity 0.0 (quantity oven-temperature) nil)
       (target-temperature-unit 0.0 (unit oven-temperature) nil))))
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
    =>
    topping-container
    kitchen-state-out
    sprinkled-object)

   ;; TODO get sprinkler using get-location instead of just string
   (let* ((result (request-to-sprinkle (slot-value object 'name) "sugarShaker" (symbolic-to-vr-kitchen kitchen-state-in)))
          (sprinkled-object-available-at (request-to-get-time))
          (kitchen-state-available-at sprinkled-object-available-at)
          (sprinkled-object-name (cdr (assoc :sprinkled-object result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (sprinkled-object-instance (find-entity-by-name sprinkled-object-name new-kitchen-state))
          (sprinkler-instance (find-entity-by-name "sugarShaker" new-kitchen-state)))
     (bind (sprinkled-object 1.0 sprinkled-object-instance sprinkled-object-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
       (topping-container 1.0 sprinkler-instance nil))))
  :primitive-inventory *vr-primitives*)


;;                                                          ;;
;;                      TO CUT                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: What about cutting board in the VR kitchen?
(defprimitive cut ((cut-object transferable-container)
                   (kitchen-state-out kitchen-state)
                   (kitchen-state-in kitchen-state)
                   (object transferable-container)
                   (cut-pattern cutting-pattern)
                   (cutting-tool can-cut)
                   (cutting-surface can-be-cut-on))

  ;; Case 1: cutting tool not given (use a knife), cut-pattern given, cutting-surface not given
  ;; TODO: cutting-board
  ((kitchen-state-in object cut-pattern => cut-object kitchen-state-out cutting-tool cutting-surface)

   (let* ((result (request-to-cut (slot-value object 'name)
                                  'knife
                                  (slot-value cut-pattern 'name)
                                  (symbolic-to-vr-kitchen kitchen-state-in)))
          (cut-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cut-object-name (cdr (assoc :cut-object result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cut-object-instance (find-entity-by-name cut-object-name new-kitchen-state))
          (cutting-tool-instance (find-entity-by-name "knife" new-kitchen-state)))

     (bind (cut-object 1.0 new-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (cutting-tool 0.0 new-knife container-available-at)
           (cutting-surface 0.0 new-cutting-board container-available-at))))

  ;; Case 3: cutting tool not given (use a knife), cut-pattern given, cutting-surface given
  ;; TODO: cutting-board
  ((kitchen-state-in object cut-pattern cutting-surface => cut-object kitchen-state-out cutting-tool)

   (let* ((result (request-to-cut (slot-value object 'name)
                                  'knife
                                  (slot-value cut-pattern 'name)
                                  (symbolic-to-vr-kitchen kitchen-state-in)))
          (cut-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cut-object-name (cdr (assoc :cut-object result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cut-object-instance (find-entity-by-name cut-object-name new-kitchen-state))
          (cutting-tool-instance (find-entity-by-name "knife" new-kitchen-state)))

     (bind (cut-object 1.0 cut-object-instance cut-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (cutting-tool 1.0 cutting-tool-instance cut-object-available-at))))


  ;; Case 2: cutting tool given, cut-pattern given, cutting-surface not given
  ;; TODO: cutting-board
  ((kitchen-state-in object cut-pattern cutting-tool => cut-object kitchen-state-out cutting-surface)
   (let* ((result (request-to-cut (slot-value object 'name)
                                  (slot-value cutting-tool 'name)
                                  (slot-value cut-pattern 'name)
                                  (symbolic-to-vr-kitchen kitchen-state-in)))
          (cut-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cut-object-name (cdr (assoc :cut-object result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cut-object-instance (find-entity-by-name cut-object-name new-kitchen-state))
          (cutting-tool-instance (find-entity-by-name (slot-value cuttin-tool 'name) new-kitchen-state)))

     (bind (cut-object 1.0 cut-object-instance cut-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (cutting-surface 0.0 new-cutting-board container-available-at))))

  ;; Case 4: cutting tool given, cut-pattern given, cutting surface given
  ;; TODO: cutting-board
  ((kitchen-state-in object cut-pattern cutting-tool cutting-surface => cut-object kitchen-state-out)

   (let* ((result (request-to-cut (slot-value object 'name)
                                  (slot-value cutting-tool 'name)
                                  (slot-value cut-pattern 'name)
                                  (symbolic-to-vr-kitchen kitchen-state-in)))
          (cut-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cut-object-name (cdr (assoc :cut-object result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cut-object-instance (find-entity-by-name cut-object-name new-kitchen-state))
          (cutting-tool-instance (find-entity-by-name (slot-value cuttin-tool 'name) new-kitchen-state)))

     (bind (cut-object 1.0 cut-object-instance cut-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))

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

  ;; Case 1: refrigerator and cooling time (quantity and unit) are not given, use one hour as cooling time
  ((kitchen-state-in
    container-with-ingredients
    =>
    cooling-unit
    cooling-quantity
    refrigerator
    kitchen-state-out
    container-with-ingredients-at-temperature)

   (let* ((new-cooling-quantity (make-instance 'quantity :value 1))
          (new-cooling-unit (make-instance 'hour))
          (result (request-to-refrigerate (slot-value container-with-ingredients 'name)
                                          'refrigerator
                                          (value new-cooling-quantity)
                                          new-cooling-unit))
          (cooled-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cooled-object-name (cdr (assoc :container-with-ingredients-at-temperature result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cooled-object-instance (find-entity-by-name cooled-object-name new-kitchen-state))
          (new-fridge-instance (find-entity-by-name "fridge" new-kitchen-state)))



     (bind (container-with-ingredients-at-temperature 1.0 cooled-object-instance cooled-object-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
       (refrigerator 0.0 new-fridge-instance kitchen-state-available-at)
       (cooling-quantity 0.0 new-cooling-quantity nil)
       (cooling-unit 0.0 new-cooling-unit nil))))


  ;; Case 2: refrigerator is not given, cooling time (quantity and unit) is given
  ((kitchen-state-in
    container-with-ingredients
    cooling-quantity
    cooling-unit
    =>
    refrigerator
    kitchen-state-out
    container-with-ingredients-at-temperature)

   (let* ((result (request-to-refrigerate (slot-value container-with-ingredients 'name)
                                          'refrigerator
                                          (value cooling-quantity) ;; 1
                                          cooling-unit))           ;; 'hour
          (cooled-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cooled-object-name (cdr (assoc :container-with-ingredients-at-temperature result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cooled-object-instance (find-entity-by-name cooled-object-name new-kitchen-state))
          (new-fridge-instance (find-entity-by-name "fridge" new-kitchen-state)))

     (bind (refrigerator 0.0 new-fridge-instance kitchen-state-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
       (container-with-ingredients-at-temperature 1.0 cooled-object-instance cooled-object-available-at))))


  ;; Case 3: refrigerator, cooling time (quantity and unit) are all given
  ((kitchen-state-in
    container-with-ingredients
    refrigerator
    cooling-quantity
    cooling-unit
    =>
    kitchen-state-out
    container-with-ingredients-at-temperature)

   (let* ((result (request-to-refrigerate (slot-value container-with-ingredients 'name)
                                          (slot-value refrigerator 'name)
                                          (value cooling-quantity) ;; 1
                                          cooling-unit))           ;; 'hour
          (cooled-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cooled-object-name (cdr (assoc :container-with-ingredients-at-temperature result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cooled-object-instance (find-entity-by-name cooled-object-name new-kitchen-state))
          (new-fridge-instance (find-entity-by-name "fridge" new-kitchen-state)))

     (bind (container-with-ingredients-at-temperature 1.0 cooled-object-instance cooled-object-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))

  :primitive-inventory *vr-primitives*)
