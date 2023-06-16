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


;; TODO: A way in VR kitchen to search for 'sugar' and it returns the container with 'sugar' in it
;; TODO: Type names in the VR kitchen should be ontology with the yaml ontology so this mapping can be removed.
(defvar *type-mapping* '((mediumbowl             . medium-bowl)
                         (sugar-bag              . sugar)
                         (butter-bag             . butter)
                         (vanilla-extract-bag    . vanilla-extract)
                         (almond-extract-bag     . almond-extract)
                         (butterparticle         . butter-particle)
                         (sugarparticle          . sugar-particle)
                         (flourparticle          . flour-particle)
                         (almondflourparticle    . almond-flour-particle)
                         (vanillaextractparticle . vanilla-extract-particle)
                         (almondextractparticle  . almond-extract-particle)
                         (kitchencabinet         . kitchen-cabinet)
                         (countertop             . counter-top)
                         (bakingtray             . baking-tray)
                         (bakingsheet            . baking-paper)
                         (kitchenstove           . oven)
                         (spoon                  . wooden-spoon)
                         (shaker                 . sugar-shaker)
                         (sugar-bag              . white-sugar)
                         (floor                  . kitchen-floor)
                         (flour-bag              . all-purpose-flour)
                         (almond-flour-bag       . almond-flour)
                         (fridgedoor             . fridge-door)
                         (kitchenstovedoor       . kitchen-stove-door)
                         (freezerdoor            . freezer-door)
                         (doughclump             . dough)
                         (powdered-white-sugar   . sugar)))


(defun map-type (type)
  (or (cdr (assoc type *type-mapping*))
      type))


(defun rmap-type (type)
  (or (car (rassoc type *type-mapping*))
      type))


(defun sim-find-object-by-name (name root)
  (if (string= (lisp-to-camel-case (symbol-name (sim-identifier root))) name)
      root
      (when (slot-exists-p root 'contents)
        (loop for child in (contents root)
              when (sim-find-object-by-name name child)
                return it))))


(defun find-unused-kitchen-entity-vr (type kitchen-state)
  (labels ((traverse (type node)
             (if (and (slot-exists-p node 'contents)
                      (eq (contents node) nil)
                      (eq (type-of node) 'medium-bowl))
                 (progn node)
                 (when (slot-exists-p node 'contents)
                   (loop for child in (contents node)
                         when (traverse type child)
                           return it)))))

    (traverse type kitchen-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;               TO SYMBOLIC                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun set-object-slots (object alist list-of-slots-to-set)
  (loop for slot in (closer-mop:class-slots (class-of object))
        for slot-name = (closer-mop:slot-definition-name slot)
        for slot-key-accessor = (car (closer-mop:slot-definition-initargs slot))
        when (and (member slot-key-accessor alist :key #'car) (member slot-name list-of-slots-to-set))
          do (setf (slot-value object slot-name) (intern (cdr (assoc slot-key-accessor alist))))))

(defun make-object (type sim-arguments sim-identifier)
  (if type
      (let* ((custom-state-variables (cdr (assoc :custom-state-variables sim-arguments)))
             (substance (cdr (assoc :substance custom-state-variables)))
             (object (make-instance (map-type (read-from-string (camel-case-to-lisp (or substance type))))
                                    :sim-arguments sim-arguments
                                    :sim-identifier sim-identifier)))
        (set-object-slots object custom-state-variables '(persistent-id))
        object)))


(defun vr-to-symbolic-kitchen (lst)
  ;;NOTE constructs the class hierarchy from the kitchen simulation edge list json
  (labels ((triple-name     (x) (car x))
           (triple-parent   (x) (cadr x))
           (triple-object   (x) (caddr x))
           (triple-children (node edge-list)
             (loop for triple in edge-list
                   if (and (triple-parent triple) (eq (triple-name node) (triple-parent triple)))
                     collect triple))
           (dfs-traverse (visited edge-list node)
             (if (not (gethash (triple-name node) visited))
                 (progn (setf (gethash (triple-name node) visited) t)
                        (let ((children (triple-children node edge-list)))
                          (when children
                            (setf (slot-value (triple-object node) 'contents)
                                  (loop for child in children collect (dfs-traverse visited edge-list child))))
                          (triple-object node))))))

    ;; Find the correct keys and values in the JSON to construct the classes
    (let ((object-list (loop for (name . properties) in lst
                             for type            = (cdr (assoc :type (cdr (assoc :custom-state-variables properties))))
                             for parent-string   = (cdr (assoc :at properties))
                             for parent          = (if parent-string (intern (camel-case-to-lisp parent-string) :keyword))
                             for object-instance = (make-object type properties name)
                             when object-instance collect (list name parent object-instance))))

      (make-instance 'kitchen-state
                     :contents (loop for triple in object-list
                                     unless (triple-parent triple)
                                       collect (dfs-traverse (make-hash-table) object-list triple))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;               TO VR                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun slots-to-alist (object)
  (let ((alist-vr-properties (sim-arguments object))
        (alist-slots (iter-slots object)))
    (loop for (key . value) in alist-vr-properties
          if (eq key :custom-state-variables)
            collect (cons key (append value alist-slots))
          else
            collect (cons key value))))


(defun iter-slots (object)
  (loop for slot in (closer-mop:class-slots (class-of object))
        for slot-name    = (closer-mop:slot-definition-name slot)
        for slot-initarg = (car (closer-mop:slot-definition-initargs slot))
        for slot-value   = (slot-value object slot-name)
        unless (or (eq slot-name 'sim-arguments) (eq slot-name 'contents))
          collect (cons slot-initarg  slot-value)))


(defun symbolic-to-vr-kitchen (kitchen)
  (labels ((has-contents (o) (slot-exists-p o 'contents))
           (sim-my-traverse (root)
             (let ((curr-edges '()))
               (if (has-contents root)
                   (dolist (child  (contents root))
                     (setf curr-edges (append curr-edges (list (list root child)) (sim-my-traverse child)))))
               curr-edges))
           (sim-transform (edge-tuple)
             (let ((object (cadr edge-tuple)))
             `( ,(intern (string-upcase (sim-identifier object)) :keyword)
                .
                ,(slots-to-alist object)))))
    (map 'list #'sim-transform (sim-my-traverse kitchen))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;                   PRIMITIVES                             ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;                                                          ;;
;;                    TO GET                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprimitive get-kitchen ((kitchen kitchen-state))

  ((=>
    kitchen)

   (let* ((kitchen-variable-name 'kitchen)
          (response-result (cdr (assoc :kitchen (list (request-get-kitchen kitchen-variable-name)))))
          (new-kitchen-state (vr-to-symbolic-kitchen response-result)))

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

   (let* ((ingredient-concept-type (rmap-type (type-of ingredient-concept)))
          (unused-container (find-unused-kitchen-entity-vr 'medium-bowl kitchen-state-in))
          (vr-result (request-to-portion ingredient-concept-type
                                         (slot-value unused-container 'sim-identifier)
                                         (value quantity)
                                         (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (output-container-name (cdr (assoc :output-container vr-result)))
          (new-kitchen-state (vr-to-symbolic-kitchen (cdr (assoc :kitchen-state-out vr-result))))
          (output-container-object (sim-find-object-by-name output-container-name new-kitchen-state)))

     ;; TODO: This can be set in the VR kitchen for consistency
     (setf (slot-value output-container-object 'used) t)

     (bind (target-container 0.0 unused-container nil)
       (container-with-ingredient 1.0 output-container-object container-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
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
   ;;
   (let* ((total-amount nil)
          (new-kitchen-state nil)
          (unused-container (find-unused-kitchen-entity-vr 'medium-bowl kitchen-state-in))
          (res (request-to-transfer (sim-identifier container-with-input-ingredients)
                                    (sim-identifier unused-container)
                                    (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-rest-name (cdr (assoc :container-with-rest res)))
          (container-with-all-ingredients-name (cdr (assoc :container-with-all-ingredients res)))
          (new-kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen new-kitchen-state-alist))
          (original-container (sim-find-object-by-name container-with-rest-name new-kitchen-state))
          (resulting-contents (sim-find-object-by-name container-with-all-ingredients-name new-kitchen-state)))

     (setf (slot-value resulting-contents 'used) t)

     (bind (target-container 0.0  resulting-contents nil)
       (container-with-all-ingredients 1.0 resulting-contents container-available-at)
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
   (let* ((res (request-to-transfer (sim-identifier container-with-input-ingredients)
                                    (sim-identifier target-container)
                                    (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-rest-name (cdr (assoc :container-with-rest res)))
          (container-with-all-ingredients-name (cdr (assoc :container-with-all-ingredients res)))
          (new-kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen new-kitchen-state-alist))
          (original-container (sim-find-object-by-name container-with-rest-name new-kitchen-state))
          (resulting-contents (sim-find-object-by-name container-with-all-ingredients-name new-kitchen-state)))


     (setf (slot-value resulting-contents 'used) t)


     (bind (container-with-all-ingredients 1.0 resulting-contents nil)
       (container-with-rest 1.0 original-container container-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
       (quantity 0.0  (make-instance 'quantity :value 100) nil)
       (unit 0.0 (make-instance 'unit) nil))))
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

   (let* ((res (request-to-mix (slot-value container-with-input-ingredients 'sim-identifier)
                               'whisk
                               (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-mixture-name (cdr (assoc :container-with-mixture res)))
          (new-kitchen-state (vr-to-symbolic-kitchen (cdr (assoc :kitchen-state-out res))))
          (target-whisk-instance (sim-find-object-by-name "whisk" new-kitchen-state))
          (container-with-new-mixture (sim-find-object-by-name container-with-mixture-name new-kitchen-state)))

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

   (let* ((res (request-to-mix (slot-value container-with-input-ingredients 'sim-identifier)
                               (slot-value mixing-tool 'sim-identifier)
                               (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-mixture-name (cdr (assoc :container-with-mixture res)))
          (new-kitchen-state (vr-to-symbolic-kitchen (cdr (assoc :kitchen-state-out res))))
          (container-with-new-mixture (sim-find-object-by-name container-with-mixture-name new-kitchen-state)))

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

   (let* ((res (request-to-beat (slot-value container-with-input-ingredients 'sim-identifier) 'spoon (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (container-with-mixture-name (cdr (assoc :container-with-mixture res)))
          (new-kitchen-state (vr-to-symbolic-kitchen (cdr (assoc :kitchen-state-out res))))
          (target-beating-tool-instance (sim-find-object-by-name "spoon" new-kitchen-state))
          (container-with-new-mixture (sim-find-object-by-name container-with-mixture-name new-kitchen-state)))

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
          (fetched-object-instance (sim-find-object-by-name fetched-obj-name new-kitchen-state)))

     ;; TODO: Is a fetched thing on countertop used or not?

     (bind (thing-fetched 1.0 fetched-object-instance thing-fetched-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  :primitive-inventory *vr-primitives*)


;;                                                          ;;
;;                    TO LINE                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defprimitive line ((lined-baking-tray lineable)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (baking-tray lineable)
                    (baking-paper can-be-lined-with))

  ;; Case 1: baking paper to line with is not given
  ((kitchen-state-in
    thing-to-be-lined
    =>
    lining
    kitchen-state-out
    lined-thing)

   (let* ((res (request-to-line (slot-value baking-tray 'sim-identifier)
                                'baking-paper
                                (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (lined-baking-tray-name (cdr (assoc :lined-baking-tray res)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (target-tray (sim-find-object-by-name lined-baking-tray-name new-kitchen-state))
          (new-baking-paper (sim-find-object-by-name "baking-paper" new-kitchen-state)))

     ;; TODO: This can be set in the VR kitchen for consistency
     (setf (slot-value target-tray 'used) t)
     (setf (slot-value target-tray 'lined-with) new-baking-paper)

     (bind (lined-baking-tray 1.0 target-tray container-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
       (lined-thing 1.0 new-baking-paper kitchen-state-available-at))))


  ;; Case 1
  ((kitchen-state-in
    baking-tray
    baking-paper
    =>
    kitchen-state-out
    lined-baking-tray)

   (let* ((res (request-to-line (slot-value baking-tray 'sim-identifier)
                                (slot-value baking-paper 'sim-identifier)
                                (symbolic-to-vr-kitchen kitchen-state-in)))
          (container-available-at (request-to-get-time))
          (kitchen-state-available-at container-available-at)
          (lined-baking-tray-name (cdr (assoc :lined-baking-tray res)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (target-tray (sim-find-object-by-name lined-baking-tray-name new-kitchen-state)))

     ;; TODO: This can be set in the VR kitchen for consistency
     (setf (slot-value target-tray 'used) t)
     (setf (slot-value target-tray 'lined-with) baking-paper)

     (bind (lined-baking-tray 1.0 target-tray container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))

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

   (let* ((res (request-to-shape (slot-value portion 'sim-identifier)
                                 (slot-value container 'sim-identifier)
                                 (symbolic-to-vr-kitchen kitchen-state-in)))
          (shaped-portions-available-at (request-to-get-time))
          (kitchen-state-available-at shaped-portions-available-at)
          (list-of-new-portion-names (cdr (assoc :shaped-portions res)))
          (new-kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen new-kitchen-state-alist))
          ;; TODO we dont need a list of portion names they already all sit in the baking tray
          ;; (list-of-portions (loop for name in list-of-new-portion-names
          ;;                        collect (sim-find-object-by-name name new-kitchen-state)))
          ;;(new-portions (make-instance 'list-of-kitchen-entities :items list-of-portions)))
          ;; NOTE convert lisp to camel case because find object by name does lisp to camel to find in jsondata
          (container-camel-name (lisp-to-camel-case (symbol-name (slot-value container 'sim-identifier))))
          (container-with-new-portions (sim-find-object-by-name container-camel-name new-kitchen-state)))

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

   (let* ((res (request-to-bake (lisp-to-camel-case (symbol-name (slot-value thing-to-bake 'sim-identifier)))
                                "kitchenStove"
                                "counterTop"
                                (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-baked-available-at (request-to-get-time))
          (kitchen-state-available-at thing-baked-available-at)
          (thing-baked-name (cdr (assoc :thing-baked res)))
          (destination-name (cdr (assoc :output-destination-container res)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (thing-baked-instance (sim-find-object-by-name thing-baked-name new-kitchen-state)))

     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
       (thing-baked 1.0 thing-baked-instance thing-baked-available-at)
       (oven-to-bake-in 0.0 (sim-find-object-by-name "kitchenStove" kitchen-state-in) nil))))

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
   (let* ((res (request-to-bake (slot-value thing-to-bake 'sim-identifier)
                                "kitchenStove"
                                "counterTop"
                                (symbolic-to-vr-kitchen kitchen-state-in)))
          (thing-baked-available-at (request-to-get-time))
          (kitchen-state-available-at thing-baked-available-at)
          (thing-baked-name (cdr (assoc :thing-baked res)))
          (destination-name (cdr (assoc :output-destination-container res)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (make-instance 'kitchen-state :contents (vr-to-symbolic-kitchen kitchen-state-alist)))
          (thing-baked-instance (sim-find-object-by-name thing-baked-name new-kitchen-state)))


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
   (let* ((result (request-to-sprinkle (slot-value object 'sim-identifier) "sugarShaker" (symbolic-to-vr-kitchen kitchen-state-in)))
          (sprinkled-object-available-at (request-to-get-time))
          (kitchen-state-available-at sprinkled-object-available-at)
          (sprinkled-object-name (cdr (assoc :sprinkled-object result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (sprinkled-object-instance (sim-find-object-by-name sprinkled-object-name new-kitchen-state))
          (sprinkler-instance (sim-find-object-by-name "sugarShaker" new-kitchen-state)))
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

   (let* ((result (request-to-cut (slot-value object 'sim-identifier)
                                  'knife
                                  (slot-value cut-pattern 'sim-identifier)
                                  (symbolic-to-vr-kitchen kitchen-state-in)))
          (cut-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cut-object-name (cdr (assoc :cut-object result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cut-object-instance (sim-find-object-by-name cut-object-name new-kitchen-state))
          (cutting-tool-instance (sim-find-object-by-name "knife" new-kitchen-state)))

     (bind (cut-object 1.0 new-container container-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
       (cutting-tool 0.0 new-knife container-available-at)
       (cutting-surface 0.0 new-cutting-board container-available-at))))

  ;; Case 3: cutting tool not given (use a knife), cut-pattern given, cutting-surface given
  ;; TODO: cutting-board
  ((kitchen-state-in object cut-pattern cutting-surface => cut-object kitchen-state-out cutting-tool)

   (let* ((result (request-to-cut (slot-value object 'sim-identifier)
                                  'knife
                                  (slot-value cut-pattern 'sim-identifier)
                                  (symbolic-to-vr-kitchen kitchen-state-in)))
          (cut-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cut-object-name (cdr (assoc :cut-object result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cut-object-instance (sim-find-object-by-name cut-object-name new-kitchen-state))
          (cutting-tool-instance (sim-find-object-by-name "knife" new-kitchen-state)))

     (bind (cut-object 1.0 cut-object-instance cut-object-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
       (cutting-tool 1.0 cutting-tool-instance cut-object-available-at))))


  ;; Case 2: cutting tool given, cut-pattern given, cutting-surface not given
  ;; TODO: cutting-board
  ((kitchen-state-in object cut-pattern cutting-tool => cut-object kitchen-state-out cutting-surface)
   (let* ((result (request-to-cut (slot-value object 'sim-identifier)
                                  (slot-value cutting-tool 'sim-identifier)
                                  (slot-value cut-pattern 'sim-identifier)
                                  (symbolic-to-vr-kitchen kitchen-state-in)))
          (cut-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cut-object-name (cdr (assoc :cut-object result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cut-object-instance (sim-find-object-by-name cut-object-name new-kitchen-state))
          (cutting-tool-instance (sim-find-object-by-name (slot-value cuttin-tool 'sim-identifier) new-kitchen-state)))

     (bind (cut-object 1.0 cut-object-instance cut-object-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
       (cutting-surface 0.0 new-cutting-board container-available-at))))

  ;; Case 4: cutting tool given, cut-pattern given, cutting surface given
  ;; TODO: cutting-board
  ((kitchen-state-in object cut-pattern cutting-tool cutting-surface => cut-object kitchen-state-out)

   (let* ((result (request-to-cut (slot-value object 'sim-identifier)
                                  (slot-value cutting-tool 'sim-identifier)
                                  (slot-value cut-pattern 'sim-identifier)
                                  (symbolic-to-vr-kitchen kitchen-state-in)))
          (cut-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cut-object-name (cdr (assoc :cut-object result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cut-object-instance (sim-find-object-by-name cut-object-name new-kitchen-state))
          (cutting-tool-instance (sim-find-object-by-name (slot-value cuttin-tool 'sim-identifier) new-kitchen-state)))

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
          (result (request-to-refrigerate (slot-value container-with-ingredients 'sim-identifier)
                                          'refrigerator
                                          (value new-cooling-quantity)
                                          new-cooling-unit))
          (cooled-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cooled-object-name (cdr (assoc :container-with-ingredients-at-temperature result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cooled-object-instance (sim-find-object-by-name cooled-object-name new-kitchen-state))
          (new-fridge-instance (sim-find-object-by-name "fridge" new-kitchen-state)))



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

   (let* ((result (request-to-refrigerate (slot-value container-with-ingredients 'sim-identifier)
                                          'refrigerator
                                          (value cooling-quantity) ;; 1
                                          cooling-unit))           ;; 'hour
          (cooled-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cooled-object-name (cdr (assoc :container-with-ingredients-at-temperature result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cooled-object-instance (sim-find-object-by-name cooled-object-name new-kitchen-state))
          (new-fridge-instance (sim-find-object-by-name "fridge" new-kitchen-state)))

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

   (let* ((result (request-to-refrigerate (slot-value container-with-ingredients 'sim-identifier)
                                          (slot-value refrigerator 'sim-identifier)
                                          (value cooling-quantity) ;; 1
                                          cooling-unit))           ;; 'hour
          (cooled-object-available-at (request-to-get-time))
          (kitchen-state-available-at cut-object-available-at)
          (cooled-object-name (cdr (assoc :container-with-ingredients-at-temperature result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (cooled-object-instance (sim-find-object-by-name cooled-object-name new-kitchen-state))
          (new-fridge-instance (sim-find-object-by-name "fridge" new-kitchen-state)))

     (bind (container-with-ingredients-at-temperature 1.0 cooled-object-instance cooled-object-available-at)
       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))

  :primitive-inventory *vr-primitives*)
