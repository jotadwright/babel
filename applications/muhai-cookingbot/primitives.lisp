(in-package :muhai-cookingbot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                             ;;
;; This file contains an implementation of the primitives      ;;
;; used by the cooking bot.                                    ;;
;;                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Defining the primitive inventory ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-irl-primitives muhai-cookingbot-inventory)


;; Primitives ;;
;;;;;;;;;;;;;;;;

(defprimitive get-kitchen ((kitchen kitchen-state))
  ((=> kitchen)
   (bind (kitchen 1.0 *initial-kitchen-state* 0.0))))



(defprimitive fetch-and-proportion ((container-with-ingredient container)
                                    (kitchen-state-out kitchen-state)
                                    (kitchen-state-in kitchen-state)
                                    (target-container container)
                                    (ingredient-concept conceptualizable)
                                    (quantity quantity)
                                    (unit unit))
  ;; Takes a specified amount of an ingredient from somewhere in the kitchen and places it in
  (( => kitchen-state-in ingredient-concept quantity unit container-with-ingredient kitchen-state-out target-container)

   (break "1")
   
   )
  ((kitchen-state-in =>  ingredient-concept quantity unit container-with-ingredient kitchen-state-out target-container)

   (break "2")

   )
  
  ((kitchen-state-in ingredient-concept quantity unit =>  kitchen-state-out container-with-ingredient target-container)

   (let ((new-kitchen-state (copy-object kitchen-state-in))
         (amount (make-instance 'amount :quantity quantity :unit unit))
         (new-kitchen-time (+ 30 (kitchen-time kitchen-state-in)))
         (container-available-at (+ 30 (kitchen-time kitchen-state-in)))
         (kitchen-state-available-at (+ 30 (kitchen-time kitchen-state-in)))
         (target-container-available-at nil))
     
     ;; 1) find target container and place it on the countertop
     (multiple-value-bind (target-container-instance-old-ks target-container-original-location)
         (find-unused-kitchen-entity 'medium-bowl kitchen-state-in)

       (let ((target-container-instance-new-ks
              (find-object-by-persistent-id target-container-instance-old-ks
                                 (funcall (type-of target-container-original-location) new-kitchen-state))))
       
         (change-kitchen-entity-location target-container-instance-new-ks
                                         (funcall (type-of target-container-original-location) new-kitchen-state)
                                         (counter-top new-kitchen-state))
       
       
       ;; 2) find ingredient and place it on the countertop
       (multiple-value-bind (ingredient-instance ingredient-original-location)
           (find-ingredient (type-of ingredient-concept) new-kitchen-state)

         (change-kitchen-entity-location ingredient-instance
                                         (funcall (type-of ingredient-original-location) new-kitchen-state)
                                         (counter-top new-kitchen-state))


         ;;3) weigh ingredient
         (multiple-value-bind (weighed-ingredient-container rest-ingredient-container)
             (weigh-ingredient ingredient-instance amount target-container-instance-new-ks)
           (change-kitchen-entity-location rest-ingredient-container (counter-top new-kitchen-state)
                                           (funcall (type-of ingredient-original-location)
                                                    new-kitchen-state))

           ;;4) set kitchen time
           (setf (kitchen-time new-kitchen-state) new-kitchen-time) ;;30 seconds for fetching and proportioning

           (bind (target-container 0.0 target-container-instance-old-ks target-container-available-at)
                 (container-with-ingredient 1.0 weighed-ingredient-container container-available-at)
                 (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))))


(defprimitive bring-up-to-temperature ((container-with-ingredients-at-temperature transferable-container)
                                       (kitchen-state-out kitchen-state)
                                       (kitchen-state-in kitchen-state)
                                       (container-with-ingredients transferable-container)
                                       (temperature-quantity quantity)
                                       (temperature-unit unit))
  
  ((kitchen-state-in container-with-ingredients temperature-quantity temperature-unit
                     => kitchen-state-out container-with-ingredients-at-temperature)
   
   (let* ((temperature (make-instance 'amount :quantity temperature-quantity :unit temperature-unit))
          (new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (new-kitchen-time (kitchen-time kitchen-state-in))
          (container-available-at (+ 600 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at (kitchen-time kitchen-state-in)))
     
     (change-temperature new-container temperature)

     (setf (kitchen-time new-kitchen-state) new-kitchen-time) 
                
     (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))


(defun change-temperature (container temperature)
  (loop for el in (contents container)
        do (setf (temperature el) temperature)))


(defprimitive beat ((container-with-ingredients-beaten transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (container-with-ingredients transferable-container)
                    (tool cooking-utensil))
  
  ((kitchen-state-in container-with-ingredients => kitchen-state-out container-with-ingredients-beaten tool)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (tool (find-unused-kitchen-entity 'whisk kitchen-state-in))
          (new-tool (find-object-by-persistent-id tool new-kitchen-state))
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (new-mixture (create-homogeneous-mixture-in-container new-container))
          (new-kitchen-time (+ 60 (max (kitchen-time kitchen-state-in)
                                       (available-at (find (id container-with-ingredients) binding-objects
                                                           :key #'(lambda (binding-object)
                                                                    (and (value binding-object)
                                                                         (id (value binding-object)))))))))
          (container-available-at new-kitchen-time)
          (kitchen-state-available-at new-kitchen-time))
   
     (setf (used new-tool) t)
     (setf (beaten new-mixture) t)
     (setf (contents new-container) (list new-mixture))
     
     (bind (container-with-ingredients-beaten 1.0 new-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (tool 0.0 tool nil)))))


(defprimitive transfer-contents ((container-with-all-ingredients transferable-container)
                                 (container-with-rest transferable-container)
                                 (kitchen-state-out kitchen-state)
                                 (kitchen-state-in kitchen-state)
                                 (target-container transferable-container)
                                 (container-with-input-ingredients transferable-container)
                                 (quantity quantity)
                                 (unit unit))

  
  ;; Case in which the target container is not given in the input-kitchen-state and no quantity and unit are given
  ((kitchen-state-in container-with-input-ingredients  
     => target-container container-with-all-ingredients container-with-rest kitchen-state-out quantity unit)

   (let ((new-kitchen-state (copy-object kitchen-state-in))
         (total-amount nil)
         (new-kitchen-time (+ 30 (kitchen-time kitchen-state-in)))
         (container-available-at (+ 30 (kitchen-time kitchen-state-in)))
         (kitchen-state-available-at (+ 30 (kitchen-time kitchen-state-in))))
   
   ;; 1) find target container and place it on the countertop
     (multiple-value-bind (target-container-in-kitchen-input-state target-container-original-location)
         (find-unused-kitchen-entity 'medium-bowl kitchen-state-in)

       (let ((target-container-instance
              (find-object-by-persistent-id target-container-in-kitchen-input-state
                                 (funcall (type-of target-container-original-location) new-kitchen-state)))
             (source-container-instance
              (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state)))) ;;to do: make recursive find function
       
         (change-kitchen-entity-location target-container-instance
                                         (funcall (type-of target-container-original-location) new-kitchen-state)
                                         (counter-top new-kitchen-state))

          ;; 2) add all contents from source container to target container
         (loop with container-amount = (make-instance 'amount)
               for ingredient in (contents source-container-instance)
               do (setf (value (quantity container-amount))
                        (+ (value (quantity container-amount)) (value (quantity (amount ingredient)))))
               (setf (contents target-container-instance) (cons ingredient (contents target-container-instance)))
               (setf (contents source-container-instance) (remove ingredient (contents source-container-instance) :test #'equalp))
               finally do
               (setf (used target-container-instance) t)
               (setf (unit container-amount) (unit (amount ingredient)))
               (setf total-amount container-amount))

         (setf (kitchen-time new-kitchen-state) new-kitchen-time)
         
     (bind (target-container 0.0 target-container-in-kitchen-input-state nil)
           (container-with-all-ingredients 1.0 target-container-instance container-available-at)
           (container-with-rest 1.0 source-container-instance container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (quantity 0.0 (quantity total-amount) nil)
           (unit 0.0 (unit total-amount) nil))))))

  ;; Case in which the target container is given in the input-kitchen-state and no quantity and unit are given
  ((kitchen-state-in container-with-input-ingredients target-container
     => container-with-all-ingredients container-with-rest kitchen-state-out quantity unit)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (target-container-instance
           (find-object-by-persistent-id target-container (counter-top new-kitchen-state)))
          (source-container-instance
           (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state)))
          (total-amount nil)
          (new-kitchen-time (+ 30 (kitchen-time kitchen-state-in)))
         (container-available-at (+ 30 (kitchen-time kitchen-state-in)))
         (kitchen-state-available-at (+ 30 (kitchen-time kitchen-state-in))))

          ;; 1) all contents from source container to target container
         (loop with container-amount = (make-instance 'amount)
               for ingredient in (contents source-container-instance)
               do (setf (value (quantity container-amount))
                        (+ (value (quantity container-amount)) (value (quantity (amount ingredient)))))
               (setf (contents target-container-instance) (cons ingredient (contents target-container-instance)))
               (setf (contents source-container-instance)
                     (remove ingredient (contents source-container-instance) :test #'equalp))
               finally do
               (setf (used target-container-instance) t)
               (setf (unit container-amount) (unit (amount ingredient)))
               (setf total-amount container-amount))

         (setf (kitchen-time new-kitchen-state) new-kitchen-time)
                  
     (bind (container-with-all-ingredients 1.0 target-container-instance container-available-at)
           (container-with-rest 1.0 source-container-instance container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (quantity 0.0 (quantity total-amount) nil)
           (unit 0.0 (unit total-amount) nil))))


  )



(defprimitive mix ((container-with-mixture transferable-container)
                   (kitchen-state-out kitchen-state)
                   (kitchen-state-in kitchen-state)
                   (container-with-input-ingredients transferable-container)
                   (mixing-tool cooking-utensil))
  
  ;;Case 1: Mixing tool not specified, use a whisk
  ((kitchen-state-in container-with-input-ingredients => kitchen-state-out container-with-mixture mixing-tool)
   
   (let ((new-kitchen-state (copy-object kitchen-state-in)))

     ;; 1) find whisk and bring it to the countertop
     (multiple-value-bind (target-whisk-in-kitchen-input-state target-whisk-original-location)
         (find-unused-kitchen-entity 'whisk kitchen-state-in)

       (let ((target-whisk-instance
              (find-object-by-persistent-id target-whisk-in-kitchen-input-state
                                            (funcall (type-of target-whisk-original-location) new-kitchen-state)))
             (container-with-input-ingredients-instance
              (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state))))
       
         (change-kitchen-entity-location target-whisk-instance ;;bring the whisk to the countertop
                                         (funcall (type-of target-whisk-original-location) new-kitchen-state)
                                         (counter-top new-kitchen-state))

          ;; 2) mix contents in container with ingredients
          (let ((mixture (create-homogeneous-mixture-in-container container-with-input-ingredients-instance)))
            
            (setf (used target-whisk-instance) t)
            (setf (mixed mixture) t)
            (setf (contents container-with-input-ingredients-instance) (list mixture)))
         
          (bind (mixing-tool 0.0 target-whisk-instance)
                (container-with-mixture 1.0 container-with-input-ingredients-instance)
                (kitchen-state-out 1.0 new-kitchen-state)))))))


(defprimitive portion-and-arrange ((portions list-of-kitchen-entities)
                                   (kitchen-state-out kitchen-state)
                                   (kitchen-state-in kitchen-state)
                                   (container-with-dough transferable-container)
                                   (quantity quantity)
                                   (unit unit)
                                   (arrangement-pattern arrangement-pattern)
                                   (destination container))
  
  ;; Case 1: Arrangement pattern and destination not specified, use evenly-spread and use countertop
  ((kitchen-state-in container-with-dough quantity unit
                     => portions kitchen-state-out arrangement-pattern destination)
   
   (let ((source-destination (counter-top kitchen-state-in))
         (new-kitchen-state (copy-object kitchen-state-in))
         (default-arrangement-pattern (make-instance 'evenly-spread)))


     ;; portion contents from container and put them on the counter top
     (let* ((container-with-dough-instance
              (find-object-by-persistent-id container-with-dough (counter-top new-kitchen-state)))
            (dough (first (contents container-with-dough-instance)))
            (value-to-transfer (value (quantity (amount dough))))
            (portion-amount (make-instance 'amount :quantity quantity :unit unit))
            (left-to-transfer (copy-object value-to-transfer))
            (countertop (counter-top new-kitchen-state))
            (portions (make-instance 'list-of-kitchen-entities)))
           
       (loop while (> left-to-transfer 0)
             for new-portion = (copy-object dough)
             do (push new-portion (items portions))
             if (> left-to-transfer (value (quantity portion-amount))) ;; not dealing with rest?
             do (setf (amount new-portion) portion-amount
                      (contents countertop) (cons new-portion (contents countertop))
                      left-to-transfer (- left-to-transfer (value (quantity portion-amount))))
             else do (setf (amount new-portion) (make-instance 'amount
                                                               :quantity (make-instance 'quantity
                                                                                        :value left-to-transfer)
                                                               :unit unit)
                           (contents countertop) (cons new-portion (contents countertop))
                           left-to-transfer 0)
             finally do
             (setf (contents container-with-dough-instance) nil)
             (setf (arrangement countertop) default-arrangement-pattern))

       (bind (portions 1.0 portions)
             (kitchen-state-out 1.0 new-kitchen-state)
             (arrangement-pattern 0.0 default-arrangement-pattern)
             (destination 0.0 source-destination))))))





(defprimitive shape ((shaped-portions list-of-kitchen-entities)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (portions list-of-kitchen-entities)
                     (shape shape))
  
  ((kitchen-state-in portions shape => shaped-portions kitchen-state-out)
   
    (let* ((new-kitchen-state (copy-object kitchen-state-in)))

      (loop for item in (items portions)
            do (setf (current-shape item) shape))

      (bind (shaped-portions 1.0 portions)
            (kitchen-state-out 1.0 new-kitchen-state)))))




(defprimitive line ((lined-baking-tray lineable)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (baking-tray lineable)
                    (baking-paper can-be-lined-with))

  ;; Case 1
  ((kitchen-state-in baking-tray baking-paper => kitchen-state-out lined-baking-tray)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (target-tray (find 'baking-tray (contents (counter-top new-kitchen-state))
                             :key #'(lambda (item) (class-name (class-of item))))))

     ;; 1) find tray and bring it to the countertop if it is not already there
     (unless target-tray

       (multiple-value-bind (target-tray-in-kitchen-input-state target-tray-original-location)
           (find-unused-kitchen-entity 'baking-tray kitchen-state-in)

         (let ((target-tray-instance
                (find-object-by-persistent-id target-tray-in-kitchen-input-state
                                              (funcall (type-of target-tray-original-location) new-kitchen-state))))

           (change-kitchen-entity-location target-tray-instance ;;bring the tray to the countertop
                                           (funcall (type-of target-tray-original-location) new-kitchen-state)
                                           (counter-top new-kitchen-state))
           (setf target-tray target-tray-instance))))

     ;; 2) find baking paper and bring it to the countertop
     (multiple-value-bind (target-paper-in-kitchen-input-state target-paper-original-location)
           (find-unused-kitchen-entity 'baking-paper kitchen-state-in)

         (let ((target-paper-instance
                (find-object-by-persistent-id target-paper-in-kitchen-input-state
                                              (funcall (type-of target-paper-original-location) new-kitchen-state))))

           (change-kitchen-entity-location target-paper-instance ;;bring the paper to the countertop
                                           (funcall (type-of target-paper-original-location) new-kitchen-state)
                                           (counter-top new-kitchen-state))

           (setf (lined-with target-tray) target-paper-instance) ;;do the lining
           (setf (is-lining target-paper-instance) t) 
           
           (setf (contents (counter-top new-kitchen-state)) ;;remove the paper from the countertop
                 (remove target-paper-instance (contents (counter-top new-kitchen-state))))
           
           (bind (lined-baking-tray 1.0 target-tray)
                 (kitchen-state-out 1.0 new-kitchen-state)))))))


(defprimitive transfer-items ((transferred container)
                              (kitchen-state-out kitchen-state)
                              (kitchen-state-in kitchen-state)
                              (items-to-transfer list-of-kitchen-entities)
                              (destination container))

  ;; Case 1 : transfer a number of items to a given destination
  ((kitchen-state-in items-to-transfer destination => kitchen-state-out transferred)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
         ; (new-items-to-transfer (find-object-by-persistent-id to-transfer new-kitchen-state))
          (new-destination (find-object-by-persistent-id destination new-kitchen-state)))
     
     (setf (used new-destination) t)
     (setf (contents new-destination) (items items-to-transfer))

     (bind (transferred 1.0 new-destination)
           (kitchen-state-out 1.0 new-kitchen-state)))))


(defprimitive bake ((thing-baked transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (thing-to-bake transferable-container)
                    (time-to-bake-quantity quantity)
                    (time-to-bake-unit unit)
                    (target-temperature-quantity quantity)
                    (target-temperature-unit unit))
  
  ((kitchen-state-in thing-to-bake time-to-bake-quantity time-to-bake-unit target-temperature-quantity target-temperature-unit
                     => kitchen-state-out thing-baked )
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-thing-to-bake (find-object-by-persistent-id thing-to-bake new-kitchen-state)))

     (loop for bakeable in (contents new-thing-to-bake)
           do (setf (temperature bakeable) (make-instance 'amount :quantity target-temperature-quantity :unit target-temperature-unit))
           ;;fast forward time to bake!!!!
           (setf (baked bakeable) t))
     
     (bind (thing-baked 1.0 new-thing-to-bake)
           (kitchen-state-out 1.0 new-kitchen-state)))))



(defprimitive sprinkle ((sprinkled-object transferable-container)
                        (kitchen-state-out kitchen-state)
                        (kitchen-state-in kitchen-state)
                        (object transferable-container)
                        (topping-container transferable-container))
  
  ((kitchen-state-in object topping-container
                        => kitchen-state-out sprinkled-object)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-input-container (find-object-by-persistent-id object (counter-top new-kitchen-state)))
          (new-topping-container (find-object-by-persistent-id topping-container (counter-top new-kitchen-state)))
          (topping (first (contents new-topping-container)))
          (total-topping-weight-in-grams (convert-to-g topping))
          (topping-weight-per-portion (make-instance 'amount
                                                     :quantity (make-instance 'quantity
                                                                              :value (/ (value (quantity (amount total-topping-weight-in-grams)))
                                                                                        (length (contents new-input-container))))
                                                     :unit 'g))) 

     (loop for portion in (contents new-input-container)
           for topping = (copy-object (first (contents new-topping-container)))
           do (setf (amount portion) topping-weight-per-portion)
           (setf (sprinkled-with portion) topping))
     
     (setf (contents new-topping-container) nil) 
     
     (bind (sprinkled-object 1.0 new-input-container)
           (kitchen-state-out 1.0 new-kitchen-state)))))


;;--------------------------------------------------------------------------
;; Helper functions
;;--------------------------------------------------------------------------


(defun change-kitchen-entity-location (kitchen-entity old-location new-location)
  "Adds the kitchen entity to the contents of the new-location and removes it from the contents of the old location "
  
  (setf (contents new-location)
        (cons kitchen-entity (contents new-location)))
  
  (setf (contents old-location)
        (remove kitchen-entity (contents old-location) :test #'equal)))



(defun weigh-ingredient (container-w-ingredient amount target-container)

  (let ((ingredient-copy (first (contents (copy-object container-w-ingredient)))))

    ;;TO DO: eerst alles omzetten naar gram??
    
    ;;weigh ingredient + separate rest
    (setf (value (quantity (amount ingredient-copy))) (value (quantity amount)))
    (setf (value (quantity (amount (first (contents container-w-ingredient)))))
          (- (value (quantity (amount (first (contents container-w-ingredient)))))
             (value (quantity amount))))

    ;;add weighed ingredient to contents of target-container
    (setf (contents target-container)
          (cons ingredient-copy (contents target-container)))
    (setf (used target-container) t)

    (values target-container container-w-ingredient)))


(defun find-unused-kitchen-entity (reusable-type place)
  (cond ((loop for el in (contents place)
               if (and (eql reusable-type (type-of el))
                       (not (used el)))
               do (return t))
         (loop for el in (contents place)
               if (and (eql reusable-type (type-of el))
                       (not (used el)))
               do (return (values el place))))
        (t
         (loop for el in (contents place)
               if (subtypep (type-of el) 'container)
               do (multiple-value-bind (found-entity found-place)
                      (find-unused-kitchen-entity reusable-type el)
                    (if found-entity
                      (return (values found-entity found-place))))))))

(defun find-ingredient (ingredient-type place &optional mother-place) ;;place can be bowl!!
  (cond ((loop for el in (contents place)
               if (or (eql ingredient-type (type-of el))
                      (member ingredient-type (mapcar #'class-name (clos::class-all-superclasses (class-of el)))))
               do (return t))
         (loop for el in (contents place)
               if (or (eql ingredient-type (type-of el))
                      (member ingredient-type (mapcar #'class-name (clos::class-all-superclasses (class-of el)))))
               do (return (values el place mother-place))))
        (t
         (loop for el in (contents place)
               if (subtypep (type-of el) 'container)
               do (multiple-value-bind (found-ingredient found-place found-mother-place)
                      (find-ingredient ingredient-type el place)
                    (cond ((and found-ingredient found-mother-place)
                           (return (values found-place found-mother-place)))
                          (found-ingredient
                           (return (values found-ingredient found-place)))))))))

               

;; (find-unused-kitchen-entity 'medium-bowl *initial-kitchen-state*)
;; (find-unused-kitchen-entity 'medium-bowl (kitchen-cabinet (or (eql type (type-of el))
;; (find-ingredient 'sugar (pantry *initial-kitchen-state*))
;; (find-ingredient 'butter *initial-kitchen-state*)
;; (find-ingredient 'almond-extract (pantry *initial-kitchen-state*))
;; (find-ingredient 'almond *initial-kitchen-state*)


;; find an object with the same id as the specified object inside the container
(defmethod find-object-by-persistent-id ((object kitchen-entity) (container container))
  (loop for item in (contents container)
        do (cond ((eq (persistent-id item) (persistent-id object))
                  (return item))
                 ((subtypep (type-of item) 'container)
                  (let* ((contents-current-item (contents item))
                         (found-item (if contents-current-item (find-object-by-persistent-id object item))))
                    (when found-item (return found-item)))))))

;; find an object with the same id as the specified object inside the entire kitchen state
(defmethod find-object-by-persistent-id ((object kitchen-entity) (kitchen-state kitchen-state))
  (let ((current-fridge (fridge kitchen-state))
        (current-freezer (freezer kitchen-state))
        (current-pantry (pantry kitchen-state))
        (current-kitchen-cabinet (kitchen-cabinet kitchen-state))
        (current-counter-top (counter-top kitchen-state))
        (current-oven (oven kitchen-state)))
    (cond ((eq (persistent-id object) (persistent-id current-fridge))
           current-fridge)
          ((eq (persistent-id object) (persistent-id current-freezer))
           current-freezer)
          ((eq (persistent-id object) (persistent-id current-pantry))
           current-pantry)
          ((eq (persistent-id object) (persistent-id current-kitchen-cabinet))
           current-kitchen-cabinet)
          ((eq (persistent-id object) (persistent-id current-counter-top))
           current-counter-top)
          ((eq (persistent-id object) (persistent-id current-oven))
           current-oven)
          (T
           (or (find-object-by-persistent-id object (counter-top kitchen-state))
               (find-object-by-persistent-id object current-fridge)
               (find-object-by-persistent-id object current-freezer)
               (find-object-by-persistent-id object current-pantry)
               (find-object-by-persistent-id object current-kitchen-cabinet)
               (find-object-by-persistent-id object current-oven))))))


(defun create-homogeneous-mixture-in-container (container)
  (let* ((total-value (loop for ingredient in (contents container)
                            for current-value = (value (quantity (amount (convert-to-g ingredient))))
                            sum current-value))
         (mixture (make-instance 'homogeneous-mixture :amount (make-instance 'amount
                                                                :unit (make-instance 'g)
                                                                :quantity (make-instance 'quantity :value total-value)))))
      (setf (contents container) (list mixture))
      (setf (mixed (first (contents container))) t)
      mixture))

(defun create-heterogeneous-mixture-in-container (container)
  (let* ((total-value (loop for ingredient in (contents container)
                            for current-value = (value (quantity (amount (convert-to-g ingredient))))
                            sum current-value))
         (mixture (make-instance 'heterogeneous-mixture :amount (make-instance 'amount
                                                                :unit (make-instance 'g)
                                                                :quantity (make-instance 'quantity :value total-value))
                                 :components (contents container))))
    (setf (contents container) mixture)
    mixture))


;; CONVERSION TABLE
;; create a conversion table for converting to g
;; the table will be hash-table with association lists as entries, e.g. for the value 'egg (('piece . 50)) could be found
(defun create-conversion-table-for-g ()
  (let ((conversion-table (make-hash-table)))
    (setf (gethash 'banana conversion-table)
	  (acons 'piece 118 '()))
    (setf (gethash 'cucumber conversion-table)
          (acons 'piece 250 '()))
    (setf (gethash 'egg conversion-table)
          (acons 'piece 50 '()))
    (setf (gethash 'jalapeno conversion-table)
          (acons 'piece 20 '()))
    (setf (gethash 'milk conversion-table)
	  (acons 'l 1032 '()))
    (setf (gethash 'onion conversion-table)
          (acons 'piece 100 '()))
    (setf (gethash 'red-onion conversion-table)
          (acons 'piece 50 '()))
    (setf (gethash 'shallot conversion-table)
          (acons 'piece 50 '()))
    (setf (gethash 'vanilla-extract conversion-table)
	  (acons 'l 879.16 '()))
    (setf (gethash 'water conversion-table)
	  (acons 'l 1000 '()))
    (setf (gethash 'whole-egg conversion-table)
	  (acons 'piece 50 '())) 
    (setf (gethash 'vegetable-oil conversion-table)
    (acons 'l 944 '()))
    conversion-table))

;; define conversion table as a global parameter
(defparameter *conversion-table-for-g* (create-conversion-table-for-g))

;; create a copy of the ingredient with g as its unit
(defmethod convert-to-g ((ingredient ingredient) &key &allow-other-keys)
  (let ((copied-ingredient (copy-object ingredient)))
    (when (not (eq (type-of (unit (amount copied-ingredient))) 'g))
      (let ((ingredient-type (type-of copied-ingredient))
            (source-unit-type (type-of (unit (amount copied-ingredient)))))
        (multiple-value-bind (conversion-rates found) (gethash ingredient-type *conversion-table-for-g*)
          (when (null found)
            (error "The ingredient ~S has no entry in the conversion table!" ingredient-type))
          (let* ((conversion-rate (assoc source-unit-type conversion-rates))
                 (converted-value (if (null conversion-rate)
                                    (error "The ingredient ~S has no entry in the conversion table for unit ~S!" ingredient-type source-unit-type)
                                    (* (value (quantity (amount copied-ingredient)))
                                       (rest conversion-rate)))))
            (setf (amount copied-ingredient)
                  (make-instance 'amount
                                 :unit (make-instance 'g)
                                 :quantity (make-instance 'quantity
                                                          :value converted-value)))))))
    copied-ingredient))



;;(evaluate-irl-program (instantiate-non-variables-in-irl-program `((get-kitchen ?kitchen-state-in)
                                       ;;                         (fetch-and-proportion ?container-with-ingredient
                                        ;;                                                ?kitchen-state-out
                                         ;;                                               ?kitchen-state-in
                                          ;;                                              ?target-container
                                           ;;                                             sugar
                                            ;;                                            2/3
                                             ;;                                           cup))) nil)


#|



(defprimitive to-get-oven ((available-oven oven) (kitchen kitchen-state))
  ((kitchen => available-oven)
   (bind (available-oven 1.0 (oven kitchen)))))

(defprimitive to-get-freezer ((available-freezer freezer) (kitchen kitchen-state))
  ((kitchen => available-freezer)
   (bind (available-freezer 1.0 (freezer kitchen)))))

(defprimitive to-get-fridge ((available-fridge fridge) (kitchen kitchen-state))
  ((kitchen => available-fridge)
   (bind (available-fridge 1.0 (fridge kitchen)))))

(defprimitive to-get-pantry ((available-pantry pantry) (kitchen kitchen-state))
  ((kitchen => available-pantry)
   (bind (available-pantry 1.0 (pantry kitchen)))))

(defprimitive to-define-quantity ((amount amount) (quantity quantity) (unit unit))
  ((quantity unit => amount)
   (bind (amount 1.0 (make-instance 'amount :unit unit :quantity quantity)))))

;; FETCH/TRANSFER
(defprimitive to-fetch ((fetched-object fetchable)
                        (kitchen-output-state kitchen-state)
                        (kitchen-input-state kitchen-state)
                        (object fetchable))
  ((kitchen-input-state object => fetched-object kitchen-output-state)
   (let ((object-location (cond ((find-fetchable-in-container object (fridge kitchen-input-state))
                                  'fridge)
                                 ((find-fetchable-in-container object (pantry kitchen-input-state))
                                  'pantry)
                                 ((find-fetchable-in-container object (freezer kitchen-input-state))
                                  'freezer)
                                 ((find-fetchable-in-container object (kitchen-cabinet kitchen-input-state))
                                  'kitchen-cabinet)
                                 ((find-fetchable-in-container object (oven kitchen-input-state))
                                  'oven))))

     (when object-location
       (let* ((new-kitchen-state (copy-object kitchen-input-state))
              (object-in-kitchen-input-state (find-fetchable-in-container object (funcall object-location kitchen-input-state)))
              (new-object (copy-object object-in-kitchen-input-state)))

         (setf (contents (counter-top new-kitchen-state))
               (cons new-object (contents (counter-top new-kitchen-state))))
         (setf (contents (funcall object-location new-kitchen-state))
               (copy-object (remove object-in-kitchen-input-state
                                    (contents (funcall object-location kitchen-input-state)))))

         (bind (fetched-object 1.0 new-object)
               (kitchen-output-state 1.0 new-kitchen-state)))))))

(defprimitive to-transfer ((outer-container container)
                           (inner-container transferable-container)
                           (kitchen-output-state kitchen-state)
                           (kitchen-input-state kitchen-state)
                           (input transferable-container)
                           (container container))
  ((kitchen-input-state input container => outer-container inner-container kitchen-output-state)
   (let* ((new-kitchen-state (copy-object kitchen-input-state))
          (new-counter-top (counter-top new-kitchen-state))
          (new-outer-container (find-object-by-id container new-kitchen-state))
          (new-inner-container (find-object-by-id input new-counter-top)))
     (setf (contents new-outer-container)
           (cons new-inner-container (contents new-outer-container)))
     (setf (contents new-counter-top) (remove new-inner-container
                                              (contents new-counter-top)))  
     (bind (outer-container 1.0 new-outer-container)
           (inner-container 1.0 new-inner-container)
           (kitchen-output-state 1.0 new-kitchen-state)))))




(defprimitive to-put-on-top ((output-container transferable-container)
                             (kitchen-output-state kitchen-state)
                             (kitchen-input-state kitchen-state)
                             (bottom-container transferable-container)
                             (top-container transferable-container)) ;Assumed to be 1 entity
              ((kitchen-input-state bottom-container top-container => output-container kitchen-output-state)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-counter-top (counter-top new-kitchen-state))
                      (new-output-container (find-object-by-id bottom-container new-counter-top))
                      (new-input-container (find-object-by-id top-container new-counter-top)))
		 (if (contents new-output-container)
		   (mapcar (lambda (ingredient) (setf (has-on-top ingredient) (contents new-input-container)))
			   (contents new-output-container))
		  ; (setf (has-on-top new-output-container) (contents new-input-container))
                   (progn
                     (use-container new-output-container)
		     (setf (contents new-output-container) (append (contents new-input-container) (contents new-output-container)))))
                 (setf (contents new-input-container) nil)
                 (bind (output-container 1.0 new-output-container)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

;; OVEN MANIPULATION
(defprimitive to-set-temperature ((heated-oven oven)
                                  (kitchen-output-state kitchen-state)
                                  (kitchen-input-state kitchen-state)
                                  (oven oven) ;; never actually used
                                  (new-temperature amount))
              ((kitchen-input-state oven new-temperature => kitchen-output-state heated-oven)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-oven (oven new-kitchen-state)))
                 (setf (temperature new-oven) new-temperature)
                 (bind (heated-oven 1.0 new-oven)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

;; CONTAINER MANIPULATION

(defprimitive to-line-with ((lined-baking-tray lineable)
                            (kitchen-output-state kitchen-state)
                            (kitchen-input-state kitchen-state)
                            (baking-tray lineable)
                            (baking-paper can-be-lined-with))
              ((kitchen-input-state baking-tray baking-paper => kitchen-output-state lined-baking-tray)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-counter-top (counter-top new-kitchen-state))
                      (new-baking-tray (find-object-by-id baking-tray new-counter-top))
                      (new-baking-paper (find-object-by-id baking-paper new-counter-top)))
                 (setf (is-lining new-baking-paper) T)           
                 (setf (lined-with new-baking-tray) new-baking-paper)
                 (setf (contents new-counter-top)
                       (remove new-baking-paper (contents new-counter-top)))    
                 (bind (lined-baking-tray 1.0 new-baking-tray)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

;; CONTAINER CONTENT MANIPULATION
(defprimitive to-bake ((baked-object transferable-container)
                       (kitchen-output-state kitchen-state)
                       (kitchen-input-state kitchen-state)
                       (object transferable-container)
                       (time-to-bake amount))
              ((kitchen-input-state object time-to-bake => kitchen-output-state baked-object)               
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-oven (oven new-kitchen-state))
                      (new-container (find-object-by-id object new-oven))
                      (bake (lambda (bakeable)
                              (setf (temperature bakeable) (copy-object (temperature new-oven)))
                              (setf (baked bakeable) T))))
                 (execute-for-all-ingredients new-container bake)       
                 (bind (baked-object 1.0 new-container)
                       (kitchen-output-state 1.0 new-kitchen-state)))))



(defprimitive to-brush ((brushed-object transferable-container)
                         (kitchen-output-state kitchen-state)
                         (kitchen-input-state kitchen-state)
                         (brushable-ingredient transferable-container)
                         (input-object transferable-container)
                         (brush can-brush))
              ((kitchen-input-state brushable-ingredient input-object brush => brushed-object kitchen-output-state)
              (let* ((new-kitchen-state (copy-object kitchen-input-state))
                     (new-counter-top (counter-top new-kitchen-state))
                     (new-brushable-ingredient (find-object-by-id  brushable-ingredient new-counter-top))
                     (new-object (find-object-by-id  input-object new-counter-top)))
                (mapcar (lambda (ingredient) (setf (is-brushed-with ingredient) T)) (contents new-brushable-ingredient))
                (setf (brushed-with new-object) (first (contents new-brushable-ingredient)))
                (use-container new-object) ;; TODO
                (setf (contents new-brushable-ingredient) nil)
                 (bind (brushed-object 1.0 new-object)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-cool ((output transferable-container)
                       (kitchen-output-state kitchen-state)
                       (kitchen-input-state kitchen-state)
                       (input transferable-container)
                       (amount-to-cool amount))
              ((kitchen-input-state input amount-to-cool => kitchen-output-state output)             
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-container (find-object-by-id input new-kitchen-state))
                      (new-temperature (if (eq (type-of (unit amount-to-cool)) 'degrees-celsius)
                                         (copy-object amount-to-cool)
                                         (make-instance 'amount ;; cool down to a fixed temperature in case of a time amount
                                                        :quantity (make-instance 'quantity :value 18)
                                                        :unit (make-instance 'degrees-celsius))))
                      (lower-temperature (lambda (has-temperature)
                                           (setf (temperature has-temperature) new-temperature))))
                 
                 (execute-for-all-contents new-container lower-temperature)              
                 (bind (output 1.0 new-container)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-crack ((container-with-whole-eggs transferable-container)
                        (kitchen-output-state kitchen-state)
                        (kitchen-input-state kitchen-state)
                        (container-with-shell-eggs transferable-container)
                        (container-for-whole-eggs transferable-container))
              ((kitchen-input-state container-with-shell-eggs container-for-whole-eggs => kitchen-output-state container-with-whole-eggs)         
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-counter-top (counter-top new-kitchen-state))
                      (new-container-with-shell-eggs (find-object-by-id container-with-shell-eggs new-counter-top))
                      (new-container-for-whole-eggs (find-object-by-id container-for-whole-eggs new-counter-top))
                      (whole-eggs (mapcar
                                   (lambda (shell-egg) (create-whole-egg shell-egg))
                                   (contents new-container-with-shell-eggs))))
                 (setf (contents new-container-for-whole-eggs) whole-eggs)
                 (use-container new-container-for-whole-eggs)
                 (setf (contents new-container-with-shell-eggs) nil) ;; all eggs are cracked and removed
                 (bind (container-with-whole-eggs 1.0 new-container-for-whole-eggs)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-dip ((dipped-object transferable-container)
                      (kitchen-output-state kitchen-state)
                      (kitchen-input-state kitchen-state)
                      (object transferable-container)
                      (dip-container transferable-container))
              ((kitchen-input-state object dip-container => kitchen-output-state dipped-object)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-counter-top (counter-top new-kitchen-state))
                      (new-input-container (find-object-by-id object new-counter-top))
                      (new-dip-container (find-object-by-id dip-container new-counter-top))
                      (new-dip (create-concept-for (first (contents new-dip-container)))) ;; assumed there is only one dip
                      (dip (lambda (dippable)
                             (setf (dipped-in dippable) new-dip))))
                 (execute-for-all-contents new-input-container dip)
                 (setf (contents new-dip-container) nil)    
                 (bind (dipped-object 1.0 new-input-container)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-drain ((drained-object transferable-container)
			(fluid transferable-container)
			(kitchen-output-state kitchen-state)
			(kitchen-input-state kitchen-state)
			(drainable-object transferable-container)
			(container-for-fluid transferable-container)
                        (container-for-solid transferable-container)
			(drain-tool can-drain))
	      ((kitchen-input-state drainable-object container-for-fluid container-for-solid drain-tool => kitchen-output-state fluid drained-object)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-counter-top (counter-top new-kitchen-state))
                      (new-container-for-fluid (find-object-by-id container-for-fluid new-counter-top))
                      (new-container-for-solid (find-object-by-id container-for-solid new-counter-top))
                      (new-drainable-object (find-object-by-id drainable-object new-counter-top))
                      (fluid-part (fluid-parts (car (contents drainable-object))))
                      (solid-part (solid-parts (car (contents drainable-object)))))
               ; (setf (contents new-container-for-fluid)
                 ;     (remove-if-not (lambda (ingredient) (typep ingredient 'fluid))
                   ;           (contents drainable-object)))
               ; (setf (contents new-container-for-solid)
                ;      (remove-if-not (lambda (ingredient) (not (typep ingredient 'fluid)))
                   ;           (contents drainable-object)))
                 (use-container new-container-for-fluid)
                 (use-container new-container-for-solid)
                ; (use-container new-container-for-fluid) TODO use drain-tool
                 (setf (contents new-container-for-fluid)
                       fluid-part)
                 (setf (contents new-container-for-solid)
                       solid-part)
                
                (setf (contents new-drainable-object) nil)
                (bind (drained-object 1.0 new-container-for-solid)
                      (fluid 1.0 new-container-for-fluid)
                      (kitchen-output-state 1.0 new-kitchen-state)))))              

(defprimitive to-mash ((mashed-ingredient transferable-container)
                       (kitchen-output-state kitchen-state)
                       (kitchen-input-state kitchen-state)
                       (input-ingredient transferable-container)
                       (mashing-tool can-mash))
              ((kitchen-input-state input-ingredient mashing-tool => mashed-ingredient kitchen-output-state)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-ingredient (copy-object input-ingredient))) 
                 (mapcar (lambda (ingredient) (setf (mashed ingredient) T)) (contents new-ingredient))      
                 (bind (mashed-ingredient 1.0 new-ingredient)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-melt ((melted-ingredient transferable-container)
                       (kitchen-output-state kitchen-state)
                       (kitchen-input-state kitchen-state)
                       (input-ingredient transferable-container))
              ((kitchen-input-state input-ingredient => melted-ingredient kitchen-output-state)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-ingredient (copy-object input-ingredient))) 
                 (mapcar (lambda (ingredient) (setf (melted ingredient) T)) (contents new-ingredient))      
                 (bind (melted-ingredient 1.0 new-ingredient)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-portion-and-arrange ((output-container transferable-container)
                                      (kitchen-output-state kitchen-state)
                                      (kitchen-input-state kitchen-state)
                                      (input-container transferable-container)
                                      (amount amount)
                                      (arrangement-pattern arrangement-pattern)
                                      (new-container container))
              ((kitchen-input-state input-container amount arrangement-pattern new-container => output-container kitchen-output-state)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-counter-top (counter-top new-kitchen-state))
                      (new-input-container (find-object-by-id input-container new-counter-top))
                      (new-output-container (find-object-by-id new-container new-counter-top))
                      (ingredient-to-portion (first (contents new-input-container)))
                      (absolute-amount-to-transfer (if (relative-amount-p amount)
                                                     (convert-relative-to-absolute amount (amount ingredient-to-portion))
                                                     (copy-object amount)))
                      (value-to-transfer (value (quantity absolute-amount-to-transfer)))
                      (left-to-transfer (value (quantity (amount ingredient-to-portion)))))
                 (loop while (> left-to-transfer 0)
                       do (let ((new-portion (copy-object ingredient-to-portion))
                                (portion-amount (copy-object absolute-amount-to-transfer)))
                            (when (< left-to-transfer value-to-transfer) ;; last portion could be smaller than the others (left-over portion)
                              (setf (value (quantity portion-amount)) left-to-transfer))
                            (setf (amount new-portion) portion-amount)
                            (setf (contents new-output-container) (cons new-portion (contents new-output-container)))
                            (setf left-to-transfer (- left-to-transfer value-to-transfer))))
                 (setf (contents new-input-container) nil) ;; everything has been transferred so should be empty
                 (use-container new-output-container)
                 (setf (arrangement new-output-container) arrangement-pattern)
                 (bind (output-container 1.0 new-output-container)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-shake ((output-container transferable-container)
                        (kitchen-output-state kitchen-state)
                        (kitchen-input-state kitchen-state)
                        (input-container transferable-container))
  ((kitchen-input-state input-container => output-container kitchen-output-state)
   (let* ((new-kitchen-state (copy-object kitchen-input-state))
          (new-counter-top (counter-top new-kitchen-state))
          (new-input-container (find-object-by-id input-container new-counter-top)))
     (if (and (typep new-input-container 'coverable-container)
              (cover new-input-container))
       (setf (shaken new-input-container) T))
     (bind (output-container 1.0 new-input-container)
           (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-shape ((output-container transferable-container)
                        (kitchen-output-state kitchen-state)
                        (kitchen-input-state kitchen-state)
                        (input-container transferable-container)
                        (object-shape shape))
              ((kitchen-input-state input-container object-shape => output-container kitchen-output-state)      
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-counter-top (counter-top new-kitchen-state))
                      (new-input-container (find-object-by-id input-container new-counter-top))
                      (new-shape (copy-object object-shape))
                      (modify-shape (lambda (shapeable)
                                      (setf (current-shape shapeable) new-shape))))
                 (execute-for-all-ingredients new-input-container modify-shape)   
                   (bind (output-container 1.0 new-input-container)
                         (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-sift ((sifted-object transferable-container)
                       (kitchen-output-state kitchen-state)
                       (kitchen-input-state kitchen-state)
                       (object transferable-container)
                       (sift can-sift))
              ((kitchen-input-state object sift => kitchen-output-state sifted-object)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-sift (find-object-by-id sift new-kitchen-state))
                      (new-counter-top (counter-top new-kitchen-state))
                      (new-container (find-object-by-id object new-counter-top))
                      (sift (lambda (siftable)
                              (setf (sifted siftable) T))))
                 (execute-for-all-contents new-container sift)
                 (use-cooking-utensil new-sift)
                 (bind (sifted-object 1.0 new-container)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-spread ((spread-object transferable-container)
                         (kitchen-output-state kitchen-state)
                         (kitchen-input-state kitchen-state)
                         (object-to-spread-upon transferable-container) ; Eg. toast, for now assumed to be 1 ingredient
			 (object-to-spread transferable-container) ; Eg. butter, for now assumed to be 1 ingredient
                         (can-spread-kitchen-tool can-spread))
              ((kitchen-input-state object-to-spread-upon object-to-spread can-spread-kitchen-tool => spread-object kitchen-output-state)
              (let* ((new-kitchen-state (copy-object kitchen-input-state))
                     (new-counter-top (counter-top new-kitchen-state))
                     (new-object-to-spread-upon (find-object-by-id  object-to-spread-upon new-counter-top))
		     (new-object-to-spread (find-object-by-id object-to-spread new-counter-top)))
		(mapcar (lambda (ingredient) (when (typep ingredient 'spreadable)
                                        	(setf (spread ingredient) T)))
                        	(contents new-object-to-spread))
		(if (contents new-object-to-spread-upon)
                  (let ((new-amount (/ (value (quantity (amount (car (contents new-object-to-spread))))) (length (contents new-object-to-spread-upon)))))
                    
                    (mapcar (lambda (ingredient)
                              (let ((portioned-object-to-spread (copy-object new-object-to-spread)))
                                (increment-id (car (contents portioned-object-to-spread)))
                                (setf (value (quantity (amount (car (contents portioned-object-to-spread))))) new-amount)
                                (setf (spread-with ingredient) (contents portioned-object-to-spread))))
                              (contents new-object-to-spread-upon)))
                  (progn
                    (use-container new-object-to-spread-upon)
                    (setf (contents new-object-to-spread-upon) (contents new-object-to-spread))))
		(setf (contents new-object-to-spread) nil)
                (bind (spread-object 1.0 new-object-to-spread-upon)
                      (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-sprinkle ((sprinkled-object transferable-container)
                           (kitchen-output-state kitchen-state)
                           (kitchen-input-state kitchen-state)
                           (object transferable-container)
                           (topping-container transferable-container))
              ((kitchen-input-state object topping-container => kitchen-output-state sprinkled-object)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-counter-top (counter-top new-kitchen-state))
                      (new-input-container (find-object-by-id object new-counter-top))
                      (new-topping-container (find-object-by-id topping-container new-counter-top))
                      (new-topping (convert-to-g (first (contents new-topping-container))))) ;; assumed there is only one topping
                 (setf (value (quantity (amount new-topping)))
                       (/ (value (quantity (amount new-topping)))
                          (length (contents new-input-container))))
                 (execute-for-all-contents new-input-container
                                           (lambda (can-be-sprinkled-on)
                                             (setf (sprinkled-with can-be-sprinkled-on)
                                                   (copy-object new-topping))))
                 (setf (contents new-topping-container) nil) ;; we can assume all the sprinkles are used up
                 (bind (sprinkled-object 1.0 new-input-container)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-flour ((floured-object transferable-container)
                        (kitchen-output-state kitchen-state)
                        (kitchen-input-state kitchen-state)
                        (object transferable-container)
                        (flour-container transferable-container))
              ((kitchen-input-state object flour-container => kitchen-output-state floured-object)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-counter-top (counter-top new-kitchen-state))
                      (new-input-container (find-object-by-id object new-counter-top))
                      (new-flour-container (find-object-by-id flour-container new-counter-top))
                      (new-flour (convert-to-g (first (contents new-flour-container)))))
                 #|(execute-for-all-contents new-input-container
                                           (lambda (can-be-sprinkled-on)
                                             (setf (sprinkled-with can-be-sprinkled-on)
                                                   (copy-object new-topping))))|#
                 (setf (contents new-flour-container) nil) 
                 (bind (floured-object 1.0 new-input-container)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

;; MIXING VARIANTS
(defprimitive to-beat ((mixture transferable-container)
                       (kitchen-output-state kitchen-state)
                       (kitchen-input-state kitchen-state)
                       (input transferable-container)
                       (tool cooking-utensil))
               ((kitchen-input-state input tool => kitchen-output-state mixture)
                (let* ((new-kitchen-state (copy-object kitchen-input-state))
                       (new-tool (find-object-by-id tool new-kitchen-state))
                       (new-counter-top (counter-top new-kitchen-state))
                       (new-container (find-object-by-id input new-counter-top))
                       (new-mixture (create-homogeneous-mixture-in-container new-container)))
                  (use-cooking-utensil new-tool)
                  (setf (beaten new-mixture) T)
                  (setf (contents new-container) (list new-mixture))
                  (bind (mixture 1.0 new-container)
                        (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-mix ((mixture transferable-container)
                      (kitchen-output-state kitchen-state)
                      (kitchen-input-state kitchen-state)
                      (input transferable-container)
                      (tool cooking-utensil))
              ((kitchen-input-state input tool => kitchen-output-state mixture)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-tool (find-object-by-id tool new-kitchen-state))
                      (new-counter-top (counter-top new-kitchen-state))
                      (new-container (find-object-by-id input new-counter-top))
                      (new-mixture (create-homogeneous-mixture-in-container new-container)))
                 (use-cooking-utensil new-tool)
                 (setf (mixed new-mixture) T)
                 (setf (contents new-container) (list new-mixture))
                 (bind (mixture 1.0 new-container)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-mingle ((mixture transferable-container)
                      (kitchen-output-state kitchen-state)
                      (kitchen-input-state kitchen-state)
                      (input transferable-container)
                      (tool cooking-utensil))
              ((kitchen-input-state input tool => kitchen-output-state mixture)
               (let* ((new-kitchen-state (copy-object kitchen-input-state))
                      (new-tool (find-object-by-id tool new-kitchen-state))
                      (new-counter-top (counter-top new-kitchen-state))
                      (new-container (find-object-by-id input new-counter-top))
                      (new-mixture (create-heterogeneous-mixture-in-container new-container)))
                 (use-cooking-utensil new-tool)
                 (setf (contents new-container) (list new-mixture))
                 (bind (mixture 1.0 new-container)
                       (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-peel ((peeled-object transferable-container)
                       (kitchen-output-state kitchen-state)
                       (kitchen-input-state kitchen-state)
                       (object transferable-container)
                       (peeling-tool can-peel))
  ((kitchen-input-state object peeling-tool => peeled-object kitchen-output-state)
   (let* ((new-kitchen-state (copy-object kitchen-input-state))
          (new-counter-top (counter-top new-kitchen-state))
          (new-tool (find-object-by-id peeling-tool new-kitchen-state))
          (new-container (find-object-by-id object new-counter-top)))
     (loop for item in (contents new-container) do (setf (peeled item) T))
     (use-cooking-utensil new-tool) 
     (bind (peeled-object 1.0 new-container)
           (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-seed ((seeded-object transferable-container)
                       (kitchen-output-state kitchen-state)
                       (kitchen-input-state kitchen-state)
                       (object transferable-container)
                       (seeding-tool can-seed))
  ((kitchen-input-state object seeding-tool => seeded-object kitchen-output-state)
   (let* ((new-kitchen-state (copy-object kitchen-input-state))
          (new-counter-top (counter-top new-kitchen-state))
          (new-tool (find-object-by-id seeding-tool new-kitchen-state))
          (new-container (find-object-by-id object new-counter-top)))
     (loop for item in (contents new-container) do (setf (seeded item) T))
     (use-cooking-utensil new-tool)
     (bind (seeded-object 1.0 new-container)
           (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-cut ((cut-object transferable-container)
                      (kitchen-output-state kitchen-state)
                      (kitchen-input-state kitchen-state)
                      (object transferable-container)
                      (cut-pattern cutting-pattern)
                      (cutting-tool can-cut))
  ((kitchen-input-state object cutting-tool cut-pattern => cut-object kitchen-output-state)
   (let* ((new-kitchen-state (copy-object kitchen-input-state))
          (new-counter-top (counter-top new-kitchen-state))
          (new-container (find-object-by-id object new-counter-top))
          (new-tool (find-object-by-id cutting-tool new-kitchen-state))
          (new-cutting-pattern (copy-object cut-pattern)))
     (loop for item in (contents new-container) do (setf (is-cut item) new-cutting-pattern))
     (use-cooking-utensil new-tool)
     (bind (cut-object 1.0 new-container)
           (kitchen-output-state 1.0 new-kitchen-state)))))

;; COVER AND UNCOVER
(defprimitive to-cover ((covered-object coverable-container)
                       (kitchen-output-state kitchen-state)
                       (kitchen-input-state kitchen-state)
                       (object coverable-container)
                       (cover can-cover))
  ((kitchen-input-state object cover => covered-object kitchen-output-state)
   (let* ((new-kitchen-state (copy-object kitchen-input-state))
          (new-counter-top (counter-top new-kitchen-state))
          (new-container (find-object-by-id object new-counter-top))
          (new-cover (find-object-by-id cover new-counter-top)))
       (setf (cover new-container) new-cover)
       (setf (contents new-counter-top) (remove new-cover
                                              (contents new-counter-top)))
       (setf (covered-container new-cover) T)
       (bind (covered-object 1.0 new-container)
           (kitchen-output-state 1.0 new-kitchen-state)))))

(defprimitive to-uncover ((uncovered-object coverable-container)
                          (kitchen-output-state kitchen-state)
                          (kitchen-input-state kitchen-state)
                          (covered-object coverable-container))
  ((kitchen-input-state covered-object => uncovered-object kitchen-output-state)
   (let* ((new-kitchen-state (copy-object kitchen-input-state))
          (new-counter-top (counter-top new-kitchen-state))
          (new-container (find-object-by-id covered-object new-counter-top))
          (new-cover (cover new-container)))
     
       (setf (cover new-container) nil)
       (setf (covered-container new-cover) nil)
       (use-cooking-utensil new-cover)
       (setf (contents new-counter-top ) (cons new-cover (contents new-counter-top)))
       (bind (uncovered-object 1.0 new-container)
             (kitchen-output-state 1.0 new-kitchen-state)))))


;; Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; CONVERSION TABLE
;; create a conversion table for converting to g
;; the table will be hash-table with association lists as entries, e.g. for the value 'egg (('piece . 50)) could be found
(defun create-conversion-table-for-g ()
  (let ((conversion-table (make-hash-table)))
    (setf (gethash 'banana conversion-table)
	  (acons 'piece 118 '()))
    (setf (gethash 'cucumber conversion-table)
          (acons 'piece 250 '()))
    (setf (gethash 'egg conversion-table)
          (acons 'piece 50 '()))
    (setf (gethash 'jalapeno conversion-table)
          (acons 'piece 20 '()))
    (setf (gethash 'milk conversion-table)
	  (acons 'l 1032 '()))
    (setf (gethash 'onion conversion-table)
          (acons 'piece 100 '()))
    (setf (gethash 'red-onion conversion-table)
          (acons 'piece 50 '()))
    (setf (gethash 'shallot conversion-table)
          (acons 'piece 50 '()))
    (setf (gethash 'vanilla-extract conversion-table)
	  (acons 'l 879.16 '()))
    (setf (gethash 'water conversion-table)
	  (acons 'l 1000 '()))
    (setf (gethash 'whole-egg conversion-table)
	  (acons 'piece 50 '())) 
    (setf (gethash 'vegetable-oil conversion-table)
    (acons 'l 944 '()))
    conversion-table))

;; define conversion table as a global parameter
(defparameter *conversion-table-for-g* (create-conversion-table-for-g))

;; create a copy of the ingredient with g as its unit
(defmethod convert-to-g ((ingredient ingredient) &key &allow-other-keys)
  (let ((copied-ingredient (copy-object ingredient)))
    (when (not (eq (type-of (unit (amount copied-ingredient))) 'g))
      (let ((ingredient-type (type-of copied-ingredient))
            (source-unit-type (type-of (unit (amount copied-ingredient)))))
        (multiple-value-bind (conversion-rates found) (gethash ingredient-type *conversion-table-for-g*)
          (when (null found)
            (error "The ingredient ~S has no entry in the conversion table!" ingredient-type))
          (let* ((conversion-rate (assoc source-unit-type conversion-rates))
                 (converted-value (if (null conversion-rate)
                                    (error "The ingredient ~S has no entry in the conversion table for unit ~S!" ingredient-type source-unit-type)
                                    (* (value (quantity (amount copied-ingredient)))
                                       (rest conversion-rate)))))
            (setf (amount copied-ingredient)
                  (make-instance 'amount
                                 :unit (make-instance 'g)
                                 :quantity (make-instance 'quantity
                                                          :value converted-value)))))))
    copied-ingredient))

;; CREATION
;; updates the container contents and returns a reference to the inner mixture
(defmethod create-homogeneous-mixture-in-container ((container transferable-container) &key &allow-other-keys)
  (let* ((total-value (loop for ingredient in (contents container)
                            for current-value = (value (quantity (amount (convert-to-g ingredient))))
                            sum current-value))
         (mixture (make-instance 'homogeneous-mixture :amount (make-instance 'amount
                                                                :unit (make-instance 'g)
                                                                :quantity (make-instance 'quantity :value total-value)))))
      (setf (contents container) (list mixture))
      (setf (mixed (first (contents container))) t)
      mixture))

(defmethod create-heterogeneous-mixture-in-container ((container transferable-container) &key &allow-other-keys)
  (let* ((total-value (loop for ingredient in (contents container)
                            for current-value = (value (quantity (amount (convert-to-g ingredient))))
                            sum current-value))
         (mixture (make-instance 'heterogeneous-mixture :amount (make-instance 'amount
                                                                :unit (make-instance 'g)
                                                                :quantity (make-instance 'quantity :value total-value))
                                 :components (contents container))))
    (setf (contents container) mixture)
    mixture))

(defmethod create-concept-for ((conceptualizable conceptualizable) &key &allow-other-keys)
  (create-concept-for (type-of conceptualizable)))

(defmethod create-concept-for ((concept-type symbol) &key &allow-other-keys)
  (let ((concept (make-instance concept-type :is-concept T)))
    (when (subtypep concept-type 'ingredient)
      (setf (amount concept) nil))
    concept))

(defmethod create-whole-egg ((shell-egg egg) &key &allow-other-keys)
  (let ((new-amount (copy-object (amount shell-egg))))
    (make-instance 'whole-egg :amount new-amount)))

;; SEARCH


(defmethod find-object-by-type ((object kitchen-entity) (container container))
  (loop for item in (contents container)
        when (eq (type-of item) (type-of object))
        return item))

;; method to find the fetchable inside a container
;; in case of an ingredient the container with the ingredient is fetched, not the ingredient itself
(defmethod find-fetchable-in-container((fetchable fetchable) (container container) &key &allow-other-keys)
  (if (concept-p fetchable)
    (cond ((subtypep (type-of fetchable) 'ingredient)
           (find-container-with-ingredient fetchable container))
          ((subtypep (type-of fetchable) 'container)
           (find-empty-container fetchable container))
          (T (find-object-by-type fetchable container)))
    (find-object-by-id fetchable container)))

;; method to find the first, empty container
(defmethod find-empty-container((inner-container container) (container container) &key &allow-other-keys)
  (loop for item in (contents container)
                 when (and (eq (type-of item) (type-of inner-container))
                           (eq (contents item) nil))
                 return item))    

;; helper method to find the first, innermost container with the specified ingredient
(defmethod find-container-with-ingredient((ingredient ingredient) (container container) &key &allow-other-keys)
  (loop for item in (contents container)
        do (let ((item (cond ((eq (type-of item) (type-of ingredient)) container)
                             ((subtypep (type-of item) 'container) (find-container-with-ingredient ingredient item)))))
            (when item (return item)))))

;; CONTAINER MANIPULATION
;; helper function that sets the container to used if it is reusable
(defmethod use-container ((container container) &key &allow-other-keys)
  (when (subtypep (type-of container) 'reusable)
    (setf (used container) T)))

;; CONTAINER CONTENT MANIPULATION
(defmethod execute-for-all-contents ((container container) (function-to-execute function) &key &allow-other-keys)
  (loop for item in (contents container)
        do (funcall function-to-execute item)))

(defmethod execute-for-all-ingredients ((container container) (function-to-execute function) &key &allow-other-keys)
  (loop for item in (contents container)
        do (cond ((subtypep (type-of item) 'ingredient)
                  (funcall function-to-execute item)) 
                 ((and (subtypep (type-of item) 'container) (contents item))
                  (execute-for-all-ingredients item function-to-execute)))))

;; TOOL MANIPULATION
;; helper function that sets the cooking-utensil to used if it is reusable
(defmethod use-cooking-utensil ((cooking-utensil cooking-utensil) &key &allow-other-keys)
  (when (subtypep (type-of cooking-utensil) 'reusable)
    (setf (used cooking-utensil) T)))

;; AMOUNT MANIPULATION
;; expects absolute amount
(defmethod add-amount-to-ingredient ((ingredient ingredient) (absolute-amount amount))
  (setf (value (quantity (amount ingredient))) (+ (value (quantity (amount ingredient)))
                                                  (value (quantity absolute-amount)))))

(defmethod remove-amount-from-ingredient ((ingredient ingredient) (absolute-amount amount))
  (let ((new-amount (copy-object absolute-amount)))
    (setf (value (quantity new-amount)) (- (value (quantity new-amount))))
    (add-amount-to-ingredient ingredient new-amount)))
  
(defmethod convert-relative-to-absolute ((relative-amount amount) (absolute-amount amount))
  (let ((new-amount (copy-object absolute-amount)))
    (setf (value (quantity new-amount)) (/ (* (value (quantity relative-amount)) (value (quantity absolute-amount))) 100))
    new-amount))

;; TESTING
(defmethod concept-p ((kitchen-entity kitchen-entity))
  (and (subtypep (type-of kitchen-entity) 'conceptualizable)
       (is-concept kitchen-entity)))

(defmethod relative-amount-p ((amount-to-check amount))
  (eq (type-of (unit amount-to-check)) 'percent))

;; ID MANIPULATION
;; only useful for testing purposes,
;; a more general approach for id incrementing should be added to copy-object
(defmethod increment-id ((object kitchen-entity))
  (let* ((object-type (type-of object))
         (new-id (utils:make-id object-type)))
    (setf (id object) new-id)))
|#
