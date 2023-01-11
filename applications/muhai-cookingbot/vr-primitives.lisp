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

;; MAPPING OF TYPES
;; some types have camelCase as type in the VR
;; some sugar vs sugarBag
;; floor can not be used as type
(defvar *type-mapping* '((mediumbowl . medium-bowl)
                         (sugar-bag . sugar)
                         (butter-bag . butter)
                         (butterparticle . butter-particle)
                         (sugarparticle . sugar-particle)
                         (kitchencabinet . kitchen-cabinet)
                         (countertop . counter-top)
                         (bakingtray . baking-tray)
                         (bakingsheet . baking-paper)
                         (kitchenstove . oven)
                         (shaker . white-sugar)
                         (floor . kitchen-floor)
                         (fridgedoor . fridge-door)
                         (kitchenstovedoor . kitchen-stove-door)
                         (freezerdoor . freezer-door)
                         (doughclump . dough)))

(defun map-type (type)
  (or (cdr (assoc type *type-mapping*))
      type))

(defun rmap-type (type)
  (or (car (rassoc type *type-mapping*))
      type))


(defun sim-find-object-by-name (name root)
  (if (string= (lisp-to-camel-case (symbol-name (slot-value root 'sim-identifier))) name)
      root
      (when (slot-exists-p root 'contents)
        (loop for child in (slot-value root 'contents)
              for found = (sim-find-object-by-name name child)
              when found
                return found))))


;;  TODO DEPRECATE AND USE REQUEST-GET-LOCATION
(defun find-unused-kitchen-entity-vr (type kitchen-state)
  (labels ((traverse (type node)
             (if (and (slot-exists-p node 'contents)
                      (eq (slot-value node 'contents) nil)
                      (eq (type-of node) 'medium-bowl))
                 (progn node)
                 (when (slot-exists-p node 'contents)
                   (loop for child in (slot-value node 'contents)
                         for found = (traverse type child)
                         when found
                           return found)))))

    (traverse type kitchen-state)))

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
          (response-result (cdr (assoc :kitchen (list (request-get-kitchen kitchen-variable-name)))))
          (new-kitchen-state (vr-to-symbolic-kitchen response-result)))


     (bind (kitchen 1.0 new-kitchen-state 0.0))))
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
  ((kitchen-state-in ingredient-concept quantity unit =>  kitchen-state-out container-with-ingredient target-container)
   (let* (;; Map 'butter' to 'butterBag'
          (ingredient-concept-type (rmap-type (type-of ingredient-concept)))
          ;; get available container
          (unused-container (find-unused-kitchen-entity-vr 'medium-bowl kitchen-state-in))
          ;; Retrieve it and portion
          (vr-result (request-to-portion ingredient-concept-type (sim-identifier unused-container) (value quantity)))
          ;; Get the name to find and bind the container later
          (output-container-name (cdr (assoc :output-container vr-result)))
          ;; Transform dict into objects
          (new-kitchen-state (vr-to-symbolic-kitchen (cdr (assoc :kitchen-state-out vr-result))))
          ;; find the container to bind later
          (output-container-object (sim-find-object-by-name output-container-name new-kitchen-state)))


     ;; set used
     (setf (slot-value output-container-object 'used) t)


     (bind (target-container 0.0 unused-container 0)
       (container-with-ingredient 1.0 output-container-object 0)
       (kitchen-state-out 1.0 new-kitchen-state 0))))
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
          (res (request-to-transfer (sim-identifier container-with-input-ingredients) (sim-identifier unused-container)))
          (container-with-rest-name (cdr (assoc :container-with-rest res)))
          (container-with-all-ingredients-name (cdr (assoc :container-with-all-ingredients res)))
          (new-kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen new-kitchen-state-alist))
          (original-container (sim-find-object-by-name container-with-rest-name new-kitchen-state))
          (resulting-contents (sim-find-object-by-name container-with-all-ingredients-name new-kitchen-state)))


     ;; set
     (setf (slot-value resulting-contents 'used) t)


     (bind (target-container 0.0  resulting-contents 0)
       (container-with-all-ingredients 1.0 resulting-contents 0)
       (container-with-rest 1.0 original-container 0)
       (kitchen-state-out 1.0 new-kitchen-state 0)
       (quantity 0.0 (make-instance 'quantity :value 100) 0)
       (unit 0.0 (make-instance 'unit) 0))))


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
   ;; TODO test
   ;; NOTE how do we specify the given target container in the input of the primitive?
   (let* ((res (request-to-transfer (sim-identifier container-with-input-ingredients) (sim-identifier target-container)))
          (container-with-rest-name (cdr (assoc :container-with-rest res)))
          (container-with-all-ingredients-name (cdr (assoc :container-with-all-ingredients res)))
          (new-kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen new-kitchen-state-alist))
          (original-container (sim-find-object-by-name container-with-rest-name new-kitchen-state))
          (resulting-contents (sim-find-object-by-name container-with-all-ingredients-name new-kitchen-state)))


     (setf (slot-value resulting-contents 'used) t)


     (bind (container-with-all-ingredients 1.0 resulting-contents 0)
       (container-with-rest 1.0 original-container 0)
       (kitchen-state-out 1.0 new-kitchen-state 0)
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
  ((kitchen-state-in container-with-input-ingredients => kitchen-state-out container-with-mixture mixing-tool)

   (defvar *saved-kithcen-state-1* kitchen-state-in)

   ;; TODO MIXING TOOl / WHISK ???
   (let* ((res (request-to-mix (slot-value container-with-input-ingredients 'sim-identifier) 'whisk))
          (container-with-mixture-name (cdr (assoc :container-with-mixture res)))
          (new-kitchen-state (vr-to-symbolic-kitchen (cdr (assoc :kitchen-state-out res))))
          (target-whisk-instance (sim-find-object-by-name "whisk" new-kitchen-state))
          (container-with-new-mixture (sim-find-object-by-name container-with-mixture-name new-kitchen-state)))

     (setf (slot-value container-with-new-mixture 'used) t)

     (bind (mixing-tool 0.0 target-whisk-instance)
       (container-with-mixture 1.0 container-with-new-mixture 0)
       (kitchen-state-out 1.0 new-kitchen-state 0))))
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
  ((kitchen-state-in concept-to-fetch quantity => kitchen-state-out thing-fetched)

   (let* ((res-get-location (request-get-location 'item (rmap-type (type-of concept-to-fetch))))
          (item-name (cdr (assoc :item res-get-location)))
          (res (request-to-fetch item-name))
          (fetched-obj-name (cdr (assoc :fetched-object res)))
          (fetched-kitchen-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen fetched-kitchen-alist))
          (fetched-object-instance (sim-find-object-by-name fetched-obj-name new-kitchen-state)))

     ;; TODO: Is a fetched thing on countertop used or not?

     (bind (thing-fetched 1.0 fetched-object-instance 0)
       (kitchen-state-out 1.0 new-kitchen-state 0))))
  :primitive-inventory *vr-primitives*)



;;                                                          ;;
;;                    TO LINE                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprimitive line ((lined-baking-tray lineable)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (baking-tray lineable)
                    (baking-paper can-be-lined-with))

  ;; Case 1
  ((kitchen-state-in
    baking-tray
    baking-paper
    =>
    kitchen-state-out
    lined-baking-tray)

   (let* ((tempvar (slot-value baking-tray 'sim-identifier))
          (tempvar2  (slot-value baking-paper 'sim-identifier))
          (res (request-to-line (slot-value baking-tray 'sim-identifier) (slot-value baking-paper 'sim-identifier)))
          (lined-baking-tray-name (cdr (assoc :lined-baking-tray res)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (target-tray (sim-find-object-by-name lined-baking-tray-name new-kitchen-state)))
     ;; find baking paper and bring it to the countertop

     (setf (slot-value target-tray 'used) t)
     (setf (slot-value target-tray 'lined-with) baking-paper)

     (bind (lined-baking-tray 1.0 target-tray 0)
       (kitchen-state-out 1.0 new-kitchen-state 0))))
  :primitive-inventory *vr-primitives*)




;;                                                          ;;
;;                    TO SHAPE                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprimitive shape ((shaped-portions kitchen-entity)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (portion kitchen-entity)
                     (baking-tray kitchen-entity)
                     (shape shape))

  ((kitchen-state-in
    portion
    baking-tray
    shape
    =>
    shaped-portions
    kitchen-state-out)

   (let* ((res (request-to-shape (slot-value portion 'sim-identifier) (slot-value baking-tray 'sim-identifier)))
          (list-of-new-portion-names (cdr (assoc :shaped-portions res)))
          (new-kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen new-kitchen-state-alist))
          ;; TODO we dont need a list of portion names they already all sit in the baking tray
          ;; (list-of-portions (loop for name in list-of-new-portion-names
          ;;                        collect (sim-find-object-by-name name new-kitchen-state)))
          ;;(new-portions (make-instance 'list-of-kitchen-entities :items list-of-portions)))
          ;; NOTE convert lisp to camel case because find object by name does lisp to camel to find in jsondata
          (baking-tray-camel-name (lisp-to-camel-case (symbol-name (slot-value baking-tray 'sim-identifier))))
          (tray-with-new-portions (sim-find-object-by-name baking-tray-camel-name new-kitchen-state)))

     (bind (shaped-portions 1.0 tray-with-new-portions 0)
       (kitchen-state-out 1.0 new-kitchen-state 0))))
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

   (let* (
          (res (request-to-bake (lisp-to-camel-case (symbol-name (slot-value thing-to-bake 'sim-identifier)))
                                "kitchenStove"
                                "counterTop"))
          (thing-baked-name (cdr (assoc :thing-baked res)))
          (destination-name (cdr (assoc :output-destination-container res)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (thing-baked-instance (sim-find-object-by-name thing-baked-name new-kitchen-state)))

     (bind (thing-baked 1.0 thing-baked-instance 0)
       (kitchen-state-out 1.0 new-kitchen-state 0)
       (oven-to-bake-in 0.0 (sim-find-object-by-name "kitchenStove" kitchen-state-in ) 0))))

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
                                "counterTop"))
          (thing-baked-name (cdr (assoc :thing-baked res)))
          (destination-name (cdr (assoc :output-destination-container res)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out res)))
          (new-kitchen-state (make-instance 'kitchen-state :contents (vr-to-symbolic-kitchen kitchen-state-alist)))
          (thing-baked-instance (sim-find-object-by-name thing-baked-name new-kitchen-state)))


     (bind (thing-baked 1.0 nil 0)
       (kitchen-state-out 1.0 kitchen-state-in 0)
       (target-temperature-quantity 0.0 (quantity oven-temperature) nil)
       (target-temperature-unit 0.0 (unit oven-temperature) nil))))
  :primitive-inventory *vr-primitives*)


;;                                                          ;;
;;                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprimitive get-baked-stuff ((shaped-portions kitchen-entity)
                               (kitchen-state-out kitchen-state))

  ((=>
    kitchen-state-out
    shaped-portions)

   (let* (
          (res (cdr (assoc :kitchen-state-out *kitchen-state-after-shape*)))
          (res-0 (request-set-kitchen res))
          (new-kitchen-state-alist  res)
          (new-kitchen-state (vr-to-symbolic-kitchen new-kitchen-state-alist))
          (tray-with-new-portions (sim-find-object-by-name "bakingTray1" new-kitchen-state)))

     (bind (shaped-portions 1.0 tray-with-new-portions 0)
       (kitchen-state-out 1.0 new-kitchen-state 0))))
   :primitive-inventory *vr-primitives*)

;;                                                          ;;
;;                    TO SPRINKLE                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprimitive sprinkle ((sprinkled-object transferable-container)
                        (kitchen-state-out kitchen-state)
                        (kitchen-state-in kitchen-state)
                        (object transferable-container)
                        (topping-container sugar))

  ((kitchen-state-in
    object
    =>
    topping-container
    kitchen-state-out
    sprinkled-object)

   ;; TODO get sprinkler using get-location instead of just string
   (let* ((result (request-to-sprinkle (slot-value object 'sim-identifier) "sugarShaker"))
          (sprinkled-object-name (cdr (assoc :sprinkled-object result)))
          (kitchen-state-alist (cdr (assoc :kitchen-state-out result)))
          (new-kitchen-state (vr-to-symbolic-kitchen kitchen-state-alist))
          (sprinkled-object-instance (sim-find-object-by-name sprinkled-object-name new-kitchen-state))
          (sprinkler-instance (sim-find-object-by-name "sugarShaker" new-kitchen-state)))
     (bind (sprinkled-object 1.0 sprinkled-object-instance 0)
       (kitchen-state-out 1.0 new-kitchen-state 0)
       (topping-container 1.0 sprinkler-instance 0))))
  :primitive-inventory *vr-primitives*)







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;               TO SYMBOLIC                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun vr-to-symbolic-kitchen (lst)
  ;;define helper functions
  (labels ((sim-name    (x) (car x))
           (sim-parent  (x) (cadr x))
           (sim-object  (x) (caddr x))
           ;; TODO find a way to determine whether or not object is used
           (make-object (type sim-arguments sim-identifier)
             (if type
                 (let ((object (make-instance (map-type (read-from-string (camel-case-to-lisp type)))
                                              :sim-arguments sim-arguments
                                              :sim-identifier sim-identifier)))
                   (loop for slot in (closer-mop:class-slots (class-of object))
                         for slot-name = (closer-mop:slot-definition-name slot)
                         do (pprint slot-name))
                   object)))

           (sim-children (node edge-list)
             (loop for child in edge-list
                   if (and (sim-parent child)
                           (eq (sim-name node) (sim-parent child)))
                     collect child))
           (sim-dfs (visited edge-list node)
             (if (not (gethash (sim-name node) visited))
                 (progn (setf (gethash (sim-name node) visited) t)
                        (let ((children (sim-children node edge-list)))
                          (when children
                            (setf (slot-value (sim-object node) 'contents) (loop for child in children

                                                                                 collect (sim-dfs visited edge-list child))))
                          (sim-object node))))))

    ;; Find the correct keys and values in the JSON to construct the classes
    (let ((object-list (loop for dictionary in lst
                             for name          = (sim-name dictionary)
                             for properties    = (cdr dictionary)
                             for type          = (cdr (assoc :type (cdr (assoc :custom-state-variables properties))))
                             for parent-string = (cdr (assoc :at properties))
                             for parent        = (if parent-string (intern (camel-case-to-lisp parent-string) :keyword))
                             for object-instance = (make-object type properties name)
                             ;; make the object list (from, to) and add object in triple to use as container
                             when object-instance collect (list name parent object-instance))))

      ;; dfs to nest the classes in each other coming from and edge-list structure
      (make-instance 'kitchen-state
                     :contents (loop for triplet in object-list
                                     unless (sim-parent triplet)
                                       collect (sim-dfs (make-hash-table) object-list triplet))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                          ;;
;;               TO VR                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symbolic-to-vr-kitchen (kitchen)
  ;; Helper and traversal functions
  (labels ((has-contents (x) (slot-exists-p x 'contents))

           ;; DFS through the objects and their contents
           (sim-my-traverse (root)
             (let ((curr-edges '()))
               (if (has-contents root)
                   (dolist (child  (contents root))
                     (setf curr-edges (append curr-edges (list (list root child)) (sim-my-traverse child)))))
               curr-edges))

           ;; Change all tuple of objects to alist for the json parser
           (sim-object-to-alist (l) (map 'list  #'sim-transform l))

           ;; Transform a tuple of object to an alist with {child: {at:parent}}
           ;;TODO maybe use persistent-id slot?
           ;;TODO the ":AT" key should be the `car` of the `edge-tuple` not from sim-argsuments
           (sim-transform (edge-tuple)
             `( ,(intern (string-upcase (slot-value (cadr edge-tuple) 'sim-identifier)) :keyword)
                .
                ,(slot-value (cadr edge-tuple) 'sim-arguments))))

    ;;Body; returns the (alist) edge-list for the vr kitchen to use
    (sim-object-to-alist (sim-my-traverse kitchen))))
