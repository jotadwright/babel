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
  ((kitchen-state-in thing-to-bake time-to-bake-quantity time-to-bake-unit target-temperature-quantity target-temperature-unit
                     => kitchen-state-out thing-baked oven-to-bake-in)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (oven-to-be-heated (oven new-kitchen-state))
          (new-thing-to-bake (find-object-by-persistent-id thing-to-bake new-kitchen-state))
          (target-temperature (make-instance 'amount :quantity target-temperature-quantity :unit target-temperature-unit))
          (thing-baked-available-at (+ (max (kitchen-time kitchen-state-in)
                                            (available-at (find (id thing-to-bake) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                       (* (value time-to-bake-quantity) 60)))
          (kitchen-state-available-at (+ (max (kitchen-time kitchen-state-in)
                                            (available-at (find (id thing-to-bake) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                         30)))
                                       

     (setf (temperature oven-to-be-heated) target-temperature)

     ;; baked things never actually enter the oven!
     (loop for bakeable in (contents new-thing-to-bake)
           do (setf (temperature bakeable) target-temperature)
              (setf (baked bakeable) t))

     (setf (kitchen-time new-kitchen-state)  kitchen-state-available-at)
     
     (bind (thing-baked 1.0 new-thing-to-bake thing-baked-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (oven-to-bake-in 0.0 oven-to-be-heated thing-baked-available-at))))

  ;; Case 2: Preheated oven is available, temperature not mentioned
  ((kitchen-state-in thing-to-bake time-to-bake-quantity time-to-bake-unit oven-to-bake-in
                     => kitchen-state-out thing-baked target-temperature-quantity target-temperature-unit)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-oven-to-bake-in (find-object-by-persistent-id oven-to-bake-in new-kitchen-state))
          (new-thing-to-bake (find-object-by-persistent-id thing-to-bake new-kitchen-state))
          (oven-temperature (temperature new-oven-to-bake-in))
          (thing-baked-available-at (+ (max (kitchen-time kitchen-state-in)
                                            (available-at (find (id thing-to-bake) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object))))))
                                            (available-at (find (id oven-to-bake-in) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                       (* (value time-to-bake-quantity) 60)))
          (kitchen-state-available-at (+ (max (kitchen-time kitchen-state-in)
                                              (available-at (find (id thing-to-bake) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                         30)))
                                       

     ;; baked things never actually enter the oven!
     (loop for bakeable in (contents new-thing-to-bake)
           do (setf (temperature bakeable) oven-temperature)
              (setf (baked bakeable) t))

     (setf (kitchen-time new-kitchen-state)  kitchen-state-available-at)
     
     (bind (thing-baked 1.0 new-thing-to-bake thing-baked-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (target-temperature-quantity 0.0 (quantity oven-temperature) nil)
           (target-temperature-unit 0.0 (unit oven-temperature) nil))))
   )

(defprimitive beat ((container-with-ingredients-beaten transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (container-with-ingredients transferable-container)
                    (tool cooking-utensil))
  
  ((kitchen-state-in container-with-ingredients => kitchen-state-out container-with-ingredients-beaten tool)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (kitchen-state-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                                (available-at (find (id container-with-ingredients) binding-objects
                                                                     :key #'(lambda (binding-object)
                                                                              (and (value binding-object)
                                                                                   (id (value binding-object)))))))))
          (container-available-at kitchen-state-available-at))

     ;; 1) find tool and place it on the countertop
     (multiple-value-bind (target-tool-instance-old-ks target-tool-original-location)
         (find-unused-kitchen-entity 'whisk kitchen-state-in)

       (let ((target-tool-instance-new-ks
              (find-object-by-persistent-id target-tool-instance-old-ks
                                            (funcall (type-of target-tool-original-location) new-kitchen-state))))
       
         (change-kitchen-entity-location target-tool-instance-new-ks
                                         (funcall (type-of target-tool-original-location) new-kitchen-state)
                                         (counter-top new-kitchen-state))
         
         ;; 2) find container with integredients on countertop
         (let* ((new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
                (new-mixture (create-homogeneous-mixture-in-container new-container)))
          
           (setf (used target-tool-instance-new-ks) t)
           (setf (beaten new-mixture) t)
           (setf (contents new-container) (list new-mixture))

           (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
           (bind (container-with-ingredients-beaten 1.0 new-container container-available-at)
                 (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                 (tool 0.0 target-tool-instance-old-ks nil))))))))


(defprimitive bring-to-temperature ((container-with-ingredients-at-temperature transferable-container)
                                    (kitchen-state-out kitchen-state)
                                    (kitchen-state-in kitchen-state)
                                    (container-with-ingredients transferable-container)
                                    (temperature-quantity quantity)
                                    (temperature-unit unit))
  ;;to do add default temperature = room temperature
  ((kitchen-state-in container-with-ingredients temperature-quantity temperature-unit
                     => kitchen-state-out container-with-ingredients-at-temperature)
   
   (let* ((temperature (make-instance 'amount :quantity temperature-quantity :unit temperature-unit))
          (new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (container-available-at (+ 800 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at (kitchen-time kitchen-state-in)))
     
     (change-temperature new-container temperature)

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
                
     (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))



(defprimitive fetch-and-proportion ((container-with-ingredient container)
                                    (kitchen-state-out kitchen-state)
                                    (kitchen-state-in kitchen-state)
                                    (target-container container)
                                    (ingredient-concept conceptualizable)
                                    (quantity quantity)
                                    (unit unit))
  ;; Takes a specified amount of an ingredient from somewhere in the kitchen and places it in
  ((kitchen-state-in ingredient-concept quantity unit =>  kitchen-state-out container-with-ingredient target-container)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (amount (make-instance 'amount :quantity quantity :unit unit))
          (container-available-at (+ 30 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at container-available-at))
     
     ;; 1) find target container and place it on the countertop
     (multiple-value-bind (target-container-instance-old-ks target-container-original-location)
         (find-unused-kitchen-entity 'medium-bowl kitchen-state-in)

       (unless target-container-instance-old-ks
         (error "No more empty medium bowls found in kitchen state!!!"))

       (let ((target-container-instance-new-ks
              (find-object-by-persistent-id target-container-instance-old-ks
                                            (funcall (type-of target-container-original-location) new-kitchen-state))))

         (assert target-container-instance-new-ks)
         
         (change-kitchen-entity-location target-container-instance-new-ks
                                         (funcall (type-of target-container-original-location) new-kitchen-state)
                                         (counter-top new-kitchen-state))
       
       
         ;; 2) find ingredient and place it on the countertop
         (multiple-value-bind (ingredient-instance ingredient-original-location)
             (find-ingredient (type-of ingredient-concept) new-kitchen-state )

           (unless ingredient-instance
             (error (format nil "No more ~a found in current kitchen state!!!" (type-of ingredient-concept))))

           (change-kitchen-entity-location ingredient-instance
                                           (funcall (type-of ingredient-original-location) new-kitchen-state)
                                           (counter-top new-kitchen-state))


           ;;3) weigh ingredient
           (multiple-value-bind (weighed-ingredient-container rest-ingredient-container)
               (weigh-ingredient ingredient-instance amount target-container-instance-new-ks)
             
             ;;put the rest back 
             (when (and (contents rest-ingredient-container)
                        (not (typep ingredient-original-location 'counter-top)))
               (change-kitchen-entity-location rest-ingredient-container
                                               (counter-top new-kitchen-state)
                                               (funcall (type-of ingredient-original-location) new-kitchen-state)))

             ;;4) set kitchen time
             (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

             (bind (target-container 0.0 target-container-instance-old-ks nil)
                   (container-with-ingredient 1.0 weighed-ingredient-container container-available-at)
                   (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))))

(defprimitive flatten ((container-with-flattened-items list-of-kitchen-entities)
                       (kitchen-state-out kitchen-state)
                       (kitchen-state-in kitchen-state)
                       (portions list-of-kitchen-entities) ; E.g. dough or dough balls directly present on the countertop
                       (can-flatten-tool can-flatten))
  
  ;; Case 1: tool to flatten with not given, default is rolling pin
  ((portions kitchen-state-in => can-flatten-tool kitchen-state-out container-with-flattened-items)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-portions (find-kitchen-entities portions (counter-top new-kitchen-state))) 
          (portions-available-at (+ 90 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id portions) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at portions-available-at))

      ;; 1) find rolling pin and place it on the countertop
     (multiple-value-bind (original-can-flatten-tool can-flatten-tool-original-location)
         (find-unused-kitchen-entity 'rolling-pin kitchen-state-in)

       (unless original-can-flatten-tool
         (error "No more unused rolling pins found in kitchen!!!"))

       (let ((new-flatten-tool
              (find-object-by-persistent-id original-can-flatten-tool
                                            (funcall (type-of can-flatten-tool-original-location) new-kitchen-state))))
         
         (change-kitchen-entity-location new-flatten-tool
                                         (funcall (type-of can-flatten-tool-original-location) new-kitchen-state)
                                         (counter-top new-kitchen-state))
         
         (loop for item in (items new-portions)
               do (setf (flattened item) t))

         (setf (used new-flatten-tool) t)
         (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
         (bind (container-with-flattened-items 1.0 new-portions portions-available-at )
               (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
               (can-flatten-tool 0.0 new-flatten-tool nil))))))

  ;; Case 2: tool to flatten given
  ((portions kitchen-state-in can-flatten-tool
                          =>  kitchen-state-out container-with-flattened-items)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-portions (find-kitchen-entities portions (counter-top new-kitchen-state)))
          (new-flatten-tool (if (is-concept can-flatten-tool)
                              (retrieve-concept-instance-and-bring-to-countertop (type-of can-flatten-tool) new-kitchen-state)
                              (find-object-by-persistent-id can-flatten-tool new-kitchen-state)))
          (portions-available-at (+ 90 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id portions) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at portions-available-at))

     (loop for item in (items new-portions)
           do (setf (flattened item) t))

     (setf (used new-flatten-tool) t)
     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (bind (container-with-flattened-items 1.0 new-portions portions-available-at )
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))

  )

(defprimitive get-kitchen ((kitchen kitchen-state))
  ((=> kitchen)
   (bind (kitchen 1.0 *initial-kitchen-state* 0.0))))



(defprimitive mix ((container-with-mixture transferable-container)
                   (kitchen-state-out kitchen-state)
                   (kitchen-state-in kitchen-state)
                   (container-with-input-ingredients transferable-container)
                   (mixing-tool can-mix))
  
  ;;Case 1: Mixing tool not specified, use a whisk
  ((kitchen-state-in container-with-input-ingredients => kitchen-state-out container-with-mixture mixing-tool)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (container-available-at (+ 30 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-input-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                                   (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

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

          (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
         
          (bind (mixing-tool 0.0 target-whisk-instance)
                (container-with-mixture 1.0 container-with-input-ingredients-instance container-available-at)
                (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))

  ;;Case 2: Mixing tool specified
  ((kitchen-state-in container-with-input-ingredients mixing-tool => kitchen-state-out container-with-mixture )
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container-with-ingredients-to-mix (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state)))
          (new-mixing-tool (if (is-concept mixing-tool)
                              (retrieve-concept-instance-and-bring-to-countertop (type-of mixing-tool) new-kitchen-state)
                              (find-object-by-persistent-id mixing-tool new-kitchen-state)))
          (container-available-at (+ 30 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-input-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                                   (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

          ;; mix contents in container with ingredients
          (let ((mixture (create-homogeneous-mixture-in-container new-container-with-ingredients-to-mix)))
            
            (setf (used new-mixing-tool) t)
            (setf (mixed mixture) t)
            (setf (contents new-container-with-ingredients-to-mix) (list mixture)))

          (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
         
          (bind (container-with-mixture 1.0 new-container-with-ingredients-to-mix container-available-at)
                (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))
  


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

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (total-amount nil)
          (container-available-at (+ 20 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-input-ingredients) binding-objects
                                                                :key #'(lambda (binding-object)
                                                                         (and (value binding-object)
                                                                              (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))
   
     ;; 1) find target container and place it on the countertop
     (multiple-value-bind (target-container-in-kitchen-input-state target-container-original-location)
         (find-unused-kitchen-entity 'large-bowl kitchen-state-in)

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
               finally
               (setf (used target-container-instance) t)
               (setf (unit container-amount) (unit (amount ingredient)))
               (setf total-amount container-amount))

         (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
         
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
          (container-available-at (+ 20 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-input-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     ;; 1) all contents from source container to target container
     (loop with container-amount = (make-instance 'amount)
           for ingredient in (contents source-container-instance)
           do (setf (value (quantity container-amount))
                    (+ (value (quantity container-amount)) (value (quantity (amount ingredient)))))
           (setf (contents target-container-instance) (cons ingredient (contents target-container-instance)))
           (setf (contents source-container-instance)
                 (remove ingredient (contents source-container-instance) :test #'equalp))
           finally
           (setf (used target-container-instance) t)
           (setf (unit container-amount) (unit (amount ingredient)))
           (setf total-amount container-amount))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
                  
     (bind (container-with-all-ingredients 1.0 target-container-instance container-available-at)
           (container-with-rest 1.0 source-container-instance container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (quantity 0.0 (quantity total-amount) nil)
           (unit 0.0 (unit total-amount) nil)))))






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
   
   (let* ((source-destination (counter-top kitchen-state-in))
          (new-kitchen-state (copy-object kitchen-state-in))
          (default-arrangement-pattern (make-instance 'evenly-spread))
          (portions-available-at (+ 80 (max (kitchen-time kitchen-state-in)
                                            (available-at (find (id container-with-dough) binding-objects
                                                                :key #'(lambda (binding-object)
                                                                         (and (value binding-object)
                                                                              (id (value binding-object)))))))))
          (kitchen-state-available-at portions-available-at))


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
             finally 
             (setf (contents container-with-dough-instance) nil)
             (setf (arrangement countertop) default-arrangement-pattern))

       (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

       (bind (portions 1.0 portions portions-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
             (arrangement-pattern 0.0 default-arrangement-pattern)
             (destination 0.0 source-destination))))))

(defprimitive preheat-oven ((preheated-oven oven)
                            (kitchen-state-out kitchen-state)
                            (kitchen-state-in kitchen-state)
                            (quantity quantity)
                            (unit unit))
  
  ((kitchen-state-in quantity unit => preheated-oven kitchen-state-out)
   
    (let* ((new-kitchen-state (copy-object kitchen-state-in))
           (kitchen-state-available-at (+ 15  (kitchen-time kitchen-state-in)))
           (oven-available-at (+ 615 (kitchen-time kitchen-state-in)))
           (target-temperature (make-instance 'amount :quantity quantity :unit unit))
           (oven-in-new-kitchen-state (oven new-kitchen-state)))

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

      (setf (temperature oven-in-new-kitchen-state) target-temperature)

      (bind (preheated-oven 1.0 oven-in-new-kitchen-state oven-available-at)
            (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))



(defprimitive line ((lined-baking-tray lineable)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (baking-tray lineable)
                    (baking-paper can-be-lined-with))

  ;; Case 1
  ((kitchen-state-in baking-tray baking-paper => kitchen-state-out lined-baking-tray)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (target-tray (find 'baking-tray (contents (counter-top new-kitchen-state))
                             :key #'(lambda (item) (class-name (class-of item)))))
          (tray-available-at (+ 150 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at tray-available-at))

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

           (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
           
           (bind (lined-baking-tray 1.0 target-tray tray-available-at)
                 (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))


(defprimitive transfer-items ((transferred container)
                              (kitchen-state-out kitchen-state)
                              (kitchen-state-in kitchen-state)
                              (items-to-transfer list-of-kitchen-entities)
                              (destination container))

  ;; Case 1 : transfer a number of items to a given destination
  ((kitchen-state-in items-to-transfer destination => kitchen-state-out transferred)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-items-to-transfer (find-kitchen-entities items-to-transfer (counter-top new-kitchen-state)))
          (new-destination (if (is-concept destination)
                              (retrieve-concept-instance-and-bring-to-countertop (type-of destination) new-kitchen-state)
                              (find-object-by-persistent-id destination new-kitchen-state)))
          (container-available-at (+ 120 (max (kitchen-time kitchen-state-in)
                                              (available-at (find (id destination) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))
     
     (setf (used new-destination) t)
     (setf (contents new-destination) (items new-items-to-transfer))
     (setf (arrangement new-destination) 'side-to-side)
     (setf (contents (counter-top new-kitchen-state)) ;;delete items from countertop!
           (remove-if #'(lambda (el)
                          (find (persistent-id el) (items new-items-to-transfer) :test #'eql :key #'persistent-id))
                      (contents (counter-top new-kitchen-state))))
     
     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (bind (transferred 1.0 new-destination container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))

(defun find-kitchen-entities (list-of-kitchen-entities countertop)
  (let ((new-list-of-kitchen-entities (copy-object list-of-kitchen-entities)))
    (setf (items new-list-of-kitchen-entities) nil)
    (loop for item in (items list-of-kitchen-entities)
          for thing in (contents countertop)
          when (eq (persistent-id item) (persistent-id thing))
          do (push thing (items new-list-of-kitchen-entities))
          finally (return new-list-of-kitchen-entities))))

(defprimitive shape ((shaped-portions list-of-kitchen-entities)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (portions list-of-kitchen-entities)
                     (shape shape))
  
  ((kitchen-state-in portions shape => shaped-portions kitchen-state-out)
   
    (let* ((new-kitchen-state (copy-object kitchen-state-in))
           (new-portions (find-kitchen-entities portions (counter-top new-kitchen-state)))
           (portions-available-at (+ 85 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id portions) binding-objects
                                                                :key #'(lambda (binding-object)
                                                                         (and (value binding-object)
                                                                              (id (value binding-object)))))))))
           (kitchen-state-available-at portions-available-at))

      (loop for item in (items new-portions)
            do (setf (current-shape item) shape))

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

      (bind (shaped-portions 1.0 new-portions portions-available-at)
            (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))


(defprimitive sift ((container-with-sifted-contents transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (target-container transferable-container) 
                    (container-with-ingredients-to-be-sifted transferable-container)
                    (sifting-tool sift))
  
  ;; Case 1: target container not given, sift not given
  ((container-with-ingredients-to-be-sifted kitchen-state-in
                                         => target-container kitchen-state-out container-with-sifted-contents sifting-tool)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-source-container (find-object-by-persistent-id container-with-ingredients-to-be-sifted new-kitchen-state))
          (container-available-at (+ 90 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-ingredients-to-be-sifted) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     ;; 1) find target container and place it on the countertop
     (multiple-value-bind (target-container-in-kitchen-input-state target-container-original-location)
         (find-unused-kitchen-entity 'large-bowl kitchen-state-in)

       (let ((new-target-container
              (find-object-by-persistent-id target-container-in-kitchen-input-state
                                            (funcall (type-of target-container-original-location) new-kitchen-state)))) 
       
         (change-kitchen-entity-location new-target-container
                                         (funcall (type-of target-container-original-location) new-kitchen-state)
                                         (counter-top new-kitchen-state))

          ;; 2) find sift and place it on the countertop
          (multiple-value-bind (sift-in-kitchen-input-state sift-original-location)
              (find-unused-kitchen-entity 'sift kitchen-state-in)

            (let ((new-sift (find-object-by-persistent-id sift-in-kitchen-input-state
                                                          (funcall (type-of sift-original-location) new-kitchen-state))))
       
              (change-kitchen-entity-location new-sift
                                              (funcall (type-of sift-original-location) new-kitchen-state)
                                              (counter-top new-kitchen-state))

              ;; 3) transfer contents from source-container to empty target-container
              (setf (contents new-target-container) (contents new-source-container))
              (setf (used new-target-container) t)
              (setf (used new-sift) t)
         
              (loop for item in (contents new-target-container)
                    when (typep item 'siftable)
                      do (setf (sifted item) t))
         
              (setf (contents new-source-container) nil)

              (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
         
              (bind (container-with-sifted-contents 1.0 new-target-container container-available-at )
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (target-container 0.0 target-container-in-kitchen-input-state nil)
                    (sifting-tool 1.0 new-sift kitchen-state-available-at))))))))

  ;; Case 2: target container is given, sift is given
  ((container-with-ingredients-to-be-sifted target-container kitchen-state-in sifting-tool
                                            => kitchen-state-out container-with-sifted-contents)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-source-container (find-object-by-persistent-id container-with-ingredients-to-be-sifted new-kitchen-state))
          (new-target-container (find-object-by-persistent-id target-container new-kitchen-state))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-ingredients-to-be-sifted) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (assert new-source-container)
     (assert new-target-container)

     ;; transfer contents from source-container to target-container
     (setf (contents new-target-container) (append (contents new-target-container)
                                                   (contents new-source-container)))
     (setf (used new-target-container) t)
       
     (loop for item in (contents new-target-container)
           when (typep item 'siftable)
             do (setf (sifted item) t))
       
     (setf (contents new-source-container) nil)
       
     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
       
     (bind (container-with-sifted-contents 1.0 new-target-container container-available-at )
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))) 
  )

(defprimitive spread ((container-with-objects-that-have-been-spread transferable-container)
                      (kitchen-state-out kitchen-state)
                      (kitchen-state-in kitchen-state)
                      (object-to-be-spread transferable-container) ; E.g. toast on a plate
                      (container-with-spread transferable-container) ; E.g. butter in a bowl
                      (can-spread-kitchen-tool can-spread)) ;E.g. a butter knife
  
  ;;Case 1: spreading tool given, either its concept or an actual tool
  ((kitchen-state-in object-to-be-spread container-with-spread can-spread-kitchen-tool
                        => container-with-objects-that-have-been-spread kitchen-state-out)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container-with-things-spread (find-object-by-persistent-id object-to-be-spread new-kitchen-state))
          (number-of-spreadable-items (loop for item in (contents new-container-with-things-spread)
                                            counting (typep item 'spreadable) into spreadables
                                            finally (return spreadables)))
          (actual-spread-in-bowl (first (contents container-with-spread)))
          (quantity-per-item (/ (value (quantity (amount actual-spread-in-bowl)))
                                number-of-spreadable-items))
          (spread-unit (type-of (unit (amount actual-spread-in-bowl))))
          (type-of-spread (type-of (first (contents container-with-spread))))
          (new-spread-container (find-object-by-persistent-id container-with-spread new-kitchen-state))
          (new-spreading-tool (if (is-concept can-spread-kitchen-tool)
                                     (retrieve-concept-instance-and-bring-to-countertop can-spread-kitchen-tool new-kitchen-state)
                                     (find-object-by-persistent-id can-spread-kitchen-tool new-kitchen-state)))
          ;; time calculation of the spread object depends
          (container-available-at (+ 120 (max (kitchen-time kitchen-state-in)
                                              (available-at (find (id object-to-be-spread) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))
     
         
     (loop for item in (contents new-container-with-things-spread)
           for portioned-spread = (make-instance type-of-spread
                                                 :amount (make-instance 'amount
                                                                        :quantity (make-instance 'quantity
                                                                                                 :value quantity-per-item)
                                                                        :unit (make-instance spread-unit)))
           when (typep item 'spreadable)
           do (setf (spread item) t)
              (setf (spread-with item) portioned-spread))

     (setf (contents new-spread-container) nil)
     (setf (used new-spreading-tool) t)
     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (bind (container-with-objects-that-have-been-spread 1.0 new-container-with-things-spread container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))


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
                                                     :unit 'g))
          (sprinkled-object-available-at (+ (max (kitchen-time kitchen-state-in)
                                                 (available-at (find (id object) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                            50))
          (kitchen-state-available-at sprinkled-object-available-at))

     (loop for portion in (contents new-input-container)
           for topping = (copy-object (first (contents new-topping-container)))
           do (setf (amount portion) topping-weight-per-portion)
           (setf (sprinkled-with portion) topping))
     
     (setf (contents new-topping-container) nil)
     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (bind (sprinkled-object 1.0 new-input-container sprinkled-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))


;;--------------------------------------------------------------------------
;; Helper functions
;;--------------------------------------------------------------------------

(defun retrieve-concept-instance-and-bring-to-countertop (kitchen-concept kitchen-state)
  "Returns an instance of the given concept that has been put on the countertop."
  
  (multiple-value-bind (target-object-in-kitchen-input-state target-object-original-location)
      (find-unused-kitchen-entity kitchen-concept kitchen-state)

    (let ((concept-instance
           (find-object-by-persistent-id target-object-in-kitchen-input-state
                                         (funcall (type-of target-object-original-location) kitchen-state))))

      (change-kitchen-entity-location concept-instance ;;bring the retrieved instance to the countertop
                                      (funcall (type-of target-object-original-location) kitchen-state)
                                      (counter-top kitchen-state))

      concept-instance)))

(defun change-kitchen-entity-location (kitchen-entity old-location new-location)
  "Adds the kitchen entity to the contents of the new-location and removes it from the contents of the old location "
  
  (setf (contents new-location)
        (cons kitchen-entity (contents new-location)))
  
  (setf (contents old-location)
        (remove kitchen-entity (contents old-location) :test #'equal)))


(defun change-temperature (container temperature)
  (loop for el in (contents container)
        do (setf (temperature el) temperature)))


(defun weigh-ingredient (container-w-ingredient amount target-container)

  (let ((ingredient-copy (first (contents (copy-object container-w-ingredient)))))

    ;;TO DO: eerst alles omzetten naar gram??
    
    ;;weigh ingredient + separate rest    (setf (value (quantity (amount ingredient-copy))) (value (quantity amount)))
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
               unless (typep el 'counter-top)
               if (or (eql ingredient-type (type-of el))
                      (member ingredient-type (mapcar #'class-name (all-superclasses (class-of el)))))
               do (return t))
         (loop for el in (contents place)
               unless (typep el 'counter-top)
               if (or (eql ingredient-type (type-of el))
                      (member ingredient-type (mapcar #'class-name (all-superclasses (class-of el)))))
               do (return (values el place mother-place))))
        (t
         (loop for el in (contents place)
               unless (typep el 'counter-top)
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
(defmethod find-object-by-persistent-id ((object list-of-kitchen-entities) (container counter-top))
  (loop for item in (contents container)
        when (eq (persistent-id item) (persistent-id object))
          do (return item)))

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


#|

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


|#
