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

  ;; Case 3: Oven and temperature are given, oven is considered to not have been preheated
  ((kitchen-state-in thing-to-bake oven-to-bake-in time-to-bake-quantity time-to-bake-unit target-temperature-quantity target-temperature-unit
                     => kitchen-state-out thing-baked)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-oven-to-bake-in (find-object-by-persistent-id oven-to-bake-in new-kitchen-state))
          (new-thing-to-bake (find-object-by-persistent-id thing-to-bake new-kitchen-state))
          (target-temperature (make-instance 'amount :quantity target-temperature-quantity :unit target-temperature-unit))
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
                                       

     (setf (temperature new-oven-to-bake-in) target-temperature)

     ;; baked things never actually enter the oven!
     (loop for bakeable in (contents new-thing-to-bake)
           do (setf (temperature bakeable) target-temperature)
              (setf (baked bakeable) t))

     (setf (kitchen-time new-kitchen-state)  kitchen-state-available-at)
     
     (bind (thing-baked 1.0 new-thing-to-bake thing-baked-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))

(defprimitive beat ((container-with-ingredients-beaten transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (container-with-ingredients transferable-container)
                    (tool cooking-utensil))

  ;; Case 1: Fetch a new whisk for beating
  ((kitchen-state-in container-with-ingredients => kitchen-state-out container-with-ingredients-beaten tool)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (kitchen-state-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                                (available-at (find (id container-with-ingredients) binding-objects
                                                                     :key #'(lambda (binding-object)
                                                                              (and (value binding-object)
                                                                                   (id (value binding-object)))))))))
          (container-available-at kitchen-state-available-at))

     ;; 1) find tool and place it on the countertop
     ;; 2) find container with ingredients on countertop
     (let* ((target-tool-instance-new-ks (retrieve-concept-instance-and-bring-to-countertop 'whisk new-kitchen-state))
            (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
            (new-mixture (create-homogeneous-mixture-in-container new-container 'beaten)))

       (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
       
       (cond (target-tool-instance-new-ks

              (setf (used target-tool-instance-new-ks) t)
              (setf (contents new-container) (list new-mixture))

              (bind (container-with-ingredients-beaten 1.0 new-container container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (tool 0.0 target-tool-instance-new-ks nil)))
             (t
              (bind (container-with-ingredients-beaten 1.0 (make-instance 'failed-object) container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (tool 0.0 (make-instance 'failed-object) nil)))))))

  ;; Case 2: Reuse existing whisk for beating
  ((kitchen-state-in container-with-ingredients tool => kitchen-state-out container-with-ingredients-beaten )
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-tool (find-object-by-persistent-id tool (counter-top new-kitchen-state)))
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (new-mixture (create-homogeneous-mixture-in-container new-container 'beaten))
          (kitchen-state-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                                (available-at (find (id container-with-ingredients) binding-objects
                                                                     :key #'(lambda (binding-object)
                                                                              (and (value binding-object)
                                                                                   (id (value binding-object)))))))))
          (container-available-at kitchen-state-available-at))

     (setf (contents new-container) (list new-mixture))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (bind (container-with-ingredients-beaten 1.0 new-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))
  
(defprimitive bring-to-temperature ((container-with-ingredients-at-temperature transferable-container)
                                    (kitchen-state-out kitchen-state)
                                    (kitchen-state-in kitchen-state)
                                    (container-with-ingredients transferable-container)
                                    (temperature-quantity quantity)
                                    (temperature-unit unit))

  ; Case 1: temperature is given
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
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
  
  ; Case 2: temperature is not given, room temperature (i.e. kitchen-state temperature is used)
  ((kitchen-state-in container-with-ingredients
                     => kitchen-state-out container-with-ingredients-at-temperature temperature-quantity temperature-unit)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (container-available-at (+ 800 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at (kitchen-time kitchen-state-in)))

     ; change it to room temperature
     (change-temperature new-container (temperature new-kitchen-state))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
                
     (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (temperature-quantity 0.0 (quantity (temperature new-kitchen-state)) nil)
           (temperature-unit 0.0 (unit (temperature new-kitchen-state)) nil)))))

(defprimitive cool-for-time ((container-with-ingredients-at-temperature transferable-container)
                             (kitchen-state-out kitchen-state)
                             (kitchen-state-in kitchen-state)
                             (container-with-ingredients transferable-container)
                             (cooling-quantity quantity)
                             (cooling-unit time-unit))
  ((kitchen-state-in container-with-ingredients cooling-quantity cooling-unit
                     => kitchen-state-out container-with-ingredients-at-temperature)
   
   (let* ((cooling-time (make-instance 'amount :quantity cooling-quantity :unit cooling-unit))
          (new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (container-available-at (+ (kitchen-time kitchen-state-in)
                                     (if (eq (type-of cooling-unit) 'hour)
                                       (* 3600 (value cooling-quantity))
                                       (* 60 (value cooling-quantity)))))
          (kitchen-state-available-at (kitchen-time kitchen-state-in)))
     
     (change-temperature new-container cooling-time)

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
                
     (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))

(defprimitive cover ((covered-object coverable-container)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (object coverable-container)
                     (cover can-cover))

  ;;Case 1: cover not given, use the most appropriate cover
  ((kitchen-state-in object => covered-object kitchen-state-out cover)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id object new-kitchen-state))
          (container-available-at (+ 20 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id object) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at)
          (cover-type (cond ((subtypep (type-of object) 'medium-bowl) 'medium-bowl-lid)
                            ((subtypep (type-of object) 'large-bowl) 'large-bowl-lid)
                            ((subtypep (type-of object) 'jar) 'jar-lid)
                            (T 'plastic-wrap)))
          (new-cover (retrieve-concept-instance-and-bring-to-countertop cover-type new-kitchen-state)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
     
     (cond (new-cover
            (setf (covered-with new-container) new-cover)
            (setf (is-covering new-cover) T)
            (setf (used new-cover) T)
            (setf (contents (counter-top new-kitchen-state)) (remove new-cover (contents (counter-top new-kitchen-state))))

            (bind
             (covered-object 1.0 new-container container-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
             (cover 0.0 new-cover container-available-at)))
           (t
            (bind
             (covered-object 1.0 (make-instance 'failed-object) container-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
             (cover 0.0 (make-instance 'failed-object) container-available-at))))))        

  ;;Case 2: cover given
  ((kitchen-state-in object cover => covered-object kitchen-state-out)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id object new-kitchen-state))
          (new-cover (find-object-by-persistent-id cover new-kitchen-state))
          (container-available-at (+ 20 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id object) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
     
     (setf (covered-with new-container) new-cover)
     (setf (is-covering new-cover) T)
     (setf (used new-cover) T)
     (setf (contents (counter-top new-kitchen-state)) (remove new-cover (contents (counter-top new-kitchen-state))))
         
       (bind
        (covered-object 1.0 new-container container-available-at)
        (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))

(defprimitive cut ((cut-object transferable-container)
                   (kitchen-state-out kitchen-state)
                   (kitchen-state-in kitchen-state)
                   (object transferable-container)
                   (cut-pattern cutting-pattern)
                   (cutting-tool can-cut))
  
  ;;Case 1: cutting tool not given (use a knife), cut-pattern given
  ((kitchen-state-in object cut-pattern => cut-object kitchen-state-out cutting-tool)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id object new-kitchen-state))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id object) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
     
     ;; 1) find knife and place it on the countertop
     (let ((new-knife (retrieve-concept-instance-and-bring-to-countertop 'knife new-kitchen-state)))

        (cond ((not new-knife)
               (bind (cut-object 1.0 (make-instance 'failed-object) container-available-at)
                     (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                     (cutting-tool 0.0 (make-instance 'failed-object) container-available-at)))
              (t
               ;; 2) cut everything in the container according to the cutting pattern
               (loop for item in (contents new-container)
                     do (setf (is-cut item) cut-pattern))
               
               (setf (used new-knife) t)

               (bind (cut-object 1.0 new-container container-available-at)
                     (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                     (cutting-tool 0.0 new-knife container-available-at)))))))

  ;;Case 2: cutting tool given, cut-pattern given
  ((kitchen-state-in object cut-pattern cutting-tool => cut-object kitchen-state-out)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id object new-kitchen-state))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id object) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 

     ;; 1) find cutting tool
     (let ((new-cutting-tool (find-object-by-persistent-id cutting-tool (counter-top new-kitchen-state))))

       ;; 2) cut everything in the container according to the cutting pattern
       (loop for item in (contents new-container)
             do (setf (is-cut item) cut-pattern))
       (setf (used new-cutting-tool) t)

       (bind (cut-object 1.0 new-container container-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))

(defprimitive crack ((container-with-cracked-eggs transferable-container)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (eggs transferable-container) ;;eggs in bowl
                     (target-container transferable-container))
  
  ;; Case 1: target container given
  ((kitchen-state-in eggs target-container => kitchen-state-out container-with-cracked-eggs)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-eggs-with-shell (find-object-by-persistent-id eggs new-kitchen-state))
          (new-target-container (find-object-by-persistent-id target-container new-kitchen-state))
          (container-available-at (+ (kitchen-time kitchen-state-in)
                                     (* 5 (length (contents eggs)))))
          (kitchen-state-available-at container-available-at))

     (loop for egg in (contents new-eggs-with-shell)
           do (loop for i from 1 to (value (quantity (amount egg)))
                    for egg-amount = (make-instance 'amount :quantity (make-instance 'quantity :value 50) :unit (make-instance 'g))
                    for whole-egg = (make-instance 'whole-egg :amount egg-amount)
                    for egg-shell = (make-instance 'egg-shell :cracked t)
                    do (setf (contents new-target-container) (append (contents new-target-container) (list whole-egg)))
                       (setf (contents new-eggs-with-shell) (append (contents new-eggs-with-shell) (list egg-shell)))
                    finally (setf (contents new-eggs-with-shell)
                                  (remove-if #'(lambda (i) (typep i 'egg)) (contents new-eggs-with-shell)))))
     
     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
                
     (bind (container-with-cracked-eggs 1.0 new-target-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))

  ;; Case 2: target container not given, use a medium bowl
  ((kitchen-state-in eggs  => kitchen-state-out container-with-cracked-eggs target-container)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-eggs-with-shell (find-object-by-persistent-id eggs new-kitchen-state))
          (container-available-at (+ (kitchen-time kitchen-state-in)
                                     (* 5 (length (contents eggs)))))
          (kitchen-state-available-at container-available-at)
          ;; 1) find target container and place it on the countertop
          (new-target-container (retrieve-concept-instance-and-bring-to-countertop 'medium-bowl new-kitchen-state))
          (target-container-instance-old-ks (if new-target-container
                                              (find-object-by-persistent-id new-target-container kitchen-state-in)
                                              nil)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond (new-target-container
            (loop for egg in (contents new-eggs-with-shell)
                  do (loop for i from 1 to (value (quantity (amount egg)))
                           for egg-amount = (make-instance 'amount :quantity (make-instance 'quantity :value 50) :unit (make-instance 'g))
                           for whole-egg = (make-instance 'whole-egg :amount egg-amount)
                           for egg-shell = (make-instance 'egg-shell :cracked t)
                           do (setf (contents new-target-container) (append (contents new-target-container) (list whole-egg)))
                              (setf (contents new-eggs-with-shell) (append (contents new-eggs-with-shell) (list egg-shell)))
                           finally (setf (contents new-eggs-with-shell)
                                         (remove-if #'(lambda (i) (typep i 'egg)) (contents new-eggs-with-shell)))))

            (setf (used new-target-container) t)

            (bind (container-with-cracked-eggs 1.0 new-target-container container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (target-container 0.0 target-container-instance-old-ks nil)))
           (t
            (bind (container-with-cracked-eggs 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (target-container 0.0 (make-instance 'failed-object) nil)))))))


(defprimitive dip ((dipped-object t) ;;transferable container or list of kitchen entities
                   (kitchen-state-out kitchen-state)
                   (kitchen-state-in kitchen-state)
                   (object t) ;;transferable container or list of kitchen entities
                   (dip-container transferable-container))
  
  ((kitchen-state-in object dip-container
                        => kitchen-state-out dipped-object)

   (cond ((subtypep (type-of object) 'list-of-kitchen-entities)
          (let* ((new-kitchen-state (copy-object kitchen-state-in))
                 (new-items-to-dip (find-kitchen-entities object (counter-top new-kitchen-state)))
                 (new-dip-container (find-object-by-persistent-id dip-container (counter-top new-kitchen-state)))
                 (dip (first (contents new-dip-container)))
                 (total-dip-weight-in-grams (convert-to-g dip))
                 (dip-weight-per-portion (make-instance 'amount
                                                        :quantity (make-instance 'quantity
                                                                                 :value (/ (value (quantity (amount total-dip-weight-in-grams)))
                                                                                           (length (items new-items-to-dip))))
                                                        :unit (make-instance 'g)))
                 (dipped-object-available-at (+ (max (kitchen-time kitchen-state-in)
                                                     (available-at (find (id object) binding-objects
                                                                         :key #'(lambda (binding-object)
                                                                                  (and (value binding-object)
                                                                                       (id (value binding-object)))))))
                                                50))
                 (kitchen-state-available-at dipped-object-available-at))

                 (loop for portion in (items new-items-to-dip)
                       for dip = (copy-object (first (contents new-dip-container)))
                       do (setf (amount dip) dip-weight-per-portion)
                          (setf (dipped-in portion) dip))

                 (setf (contents new-dip-container) nil)
                 (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

                 (bind (dipped-object 1.0 new-items-to-dip dipped-object-available-at)
                       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
         ((subtypep (type-of object) 'transferable-container)
          (let* ((new-kitchen-state (copy-object kitchen-state-in))
                 (new-input-container (find-object-by-persistent-id object (counter-top new-kitchen-state)))
                 (new-dip-container (find-object-by-persistent-id dip-container (counter-top new-kitchen-state)))
                 (dip (first (contents new-dip-container)))
                 (total-dip-weight-in-grams (convert-to-g dip))
                 (dip-weight-per-portion (make-instance 'amount
                                                        :quantity (make-instance 'quantity
                                                                                 :value (/ (value (quantity (amount total-dip-weight-in-grams)))
                                                                                           (length (contents new-input-container))))
                                                        :unit (make-instance 'g)))
                 (dipped-object-available-at (+ (max (kitchen-time kitchen-state-in)
                                                     (available-at (find (id object) binding-objects
                                                                         :key #'(lambda (binding-object)
                                                                                  (and (value binding-object)
                                                                                       (id (value binding-object)))))))
                                                50))
                 (kitchen-state-available-at dipped-object-available-at))

                 (loop for portion in (contents new-input-container)
                       for dip = (copy-object (first (contents new-dip-container)))
                       do (setf (amount dip) dip-weight-per-portion)
                          (setf (dipped-in portion) dip))

                 (setf (contents new-dip-container) nil)
                 (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

                 (bind (dipped-object 1.0 new-input-container dipped-object-available-at)
                       (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

(defprimitive fetch ((thing-fetched kitchen-entity) ; if quantity > 1, this will be a list-of-kitchen-entities 
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (concept-to-fetch conceptualizable)
                     (quantity quantity)) ;;what if this is more than 1?
  
  ;; Case 1: Fetch object(s) from somewhere in the kitchen and place it on the countertop
  ((kitchen-state-in concept-to-fetch quantity =>  kitchen-state-out thing-fetched)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (thing-available-at (+ (* (value quantity) 30)
                                 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at thing-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
   
     (if (= (value quantity) 1)
       ;; find object and place it on the countertop
       (let ((target-concept-instance-new-ks (retrieve-concept-instance-and-bring-to-countertop (type-of concept-to-fetch) new-kitchen-state)))
         (if target-concept-instance-new-ks
           (bind (thing-fetched 1.0 target-concept-instance-new-ks thing-available-at)
                 (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))
           (bind (thing-fetched 1.0 (make-instance 'failed-object) thing-available-at)
                 (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))

       ;; find all objects and place them on the countertop (one by one)
       (let ((things-fetched (make-instance 'list-of-kitchen-entities))
             (has-failed nil))
         (loop for i from 1 to (value quantity)
               do
                 (let ((retrieved-object (retrieve-concept-instance-and-bring-to-countertop (type-of concept-to-fetch) new-kitchen-state (items things-fetched))))
                   (if (not retrieved-object)
                     (setf has-failed t)
                     (push retrieved-object (items things-fetched)))))

         (if has-failed
           (bind (thing-fetched 1.0 (make-instance 'failed-object) thing-available-at)
                 (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))
           (bind (thing-fetched 1.0 things-fetched thing-available-at)
                 (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))))

(defprimitive fetch-and-proportion ((container-with-ingredient container)
                                    (kitchen-state-out kitchen-state)
                                    (kitchen-state-in kitchen-state)
                                    (target-container container)
                                    (ingredient-concept conceptualizable)
                                    (quantity quantity)
                                    (unit unit))
  
  ;; Takes a specified amount of an ingredient from somewhere in the kitchen and places it in an empty medium bowl
  ((kitchen-state-in ingredient-concept quantity unit =>  kitchen-state-out container-with-ingredient target-container)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (amount (make-instance 'amount :quantity quantity :unit unit))
          (container-available-at (+ 30 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at container-available-at)
          ;; 1) find target container and place it on the countertop
          (target-container-instance-new-ks (retrieve-concept-instance-and-bring-to-countertop 'medium-bowl new-kitchen-state))
          (target-container-instance-old-ks (if target-container-instance-new-ks
                                              (find-object-by-persistent-id target-container-instance-new-ks kitchen-state-in)
                                              nil)))

     ;;  set kitchen time
     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     ;; 2) find ingredient and place it on the countertop
     (multiple-value-bind (ingredient-instance ingredient-original-location)
         (find-ingredient (type-of ingredient-concept) new-kitchen-state )

       (cond ((and ingredient-instance
                   target-container-instance-new-ks)
              (change-kitchen-entity-location ingredient-instance
                                              (funcall (type-of ingredient-original-location) new-kitchen-state)
                                              (counter-top new-kitchen-state))

              ;;3) weigh ingredient
              (multiple-value-bind (weighed-ingredient-container rest-ingredient-container)
                  (if (eq (type-of (unit (amount (first (contents ingredient-instance))))) 'piece)
                    (take-n-pieces ingredient-instance amount target-container-instance-new-ks)
                    (weigh-ingredient ingredient-instance amount target-container-instance-new-ks))

                (setf (used weighed-ingredient-container) t)

                ;;put the rest back
                (when (and (contents rest-ingredient-container)
                           (not (typep ingredient-original-location 'counter-top)))
                  (change-kitchen-entity-location rest-ingredient-container
                                                  (counter-top new-kitchen-state)
                                                  (funcall (type-of ingredient-original-location) new-kitchen-state)))

                (bind (target-container 0.0 target-container-instance-old-ks nil)
                      (container-with-ingredient 1.0 weighed-ingredient-container container-available-at)
                      (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
             (t
              (unless ingredient-instance
                (print (format nil "No more ~a found in current kitchen state!!!" (type-of ingredient-concept))))
              (bind (target-container 0.0 (make-instance 'failed-object) nil)
                    (container-with-ingredient 1.0 (make-instance 'failed-object) container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))))

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
          (kitchen-state-available-at portions-available-at)
          ;; 1) find rolling pin and place it on the countertop
          (new-flatten-tool (retrieve-concept-instance-and-bring-to-countertop 'rolling-pin new-kitchen-state)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (cond (new-flatten-tool
            (loop for item in (items new-portions)
                  do (setf (flattened item) t))

            (setf (used new-flatten-tool) t)
            
            (bind (container-with-flattened-items 1.0 new-portions portions-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (can-flatten-tool 0.0 new-flatten-tool nil)))
           (t
            (bind (container-with-flattened-items 1.0 (make-instance 'failed-object) portions-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (can-flatten-tool 0.0 (make-instance 'failed-object) nil))))))
            
   
  ;; Case 2: tool to flatten given
  ((portions kitchen-state-in can-flatten-tool
                          =>  kitchen-state-out container-with-flattened-items)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-portions (find-kitchen-entities portions (counter-top new-kitchen-state)))
          (new-flatten-tool (find-object-by-persistent-id can-flatten-tool new-kitchen-state))
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
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))

(defprimitive get-kitchen ((kitchen kitchen-state))
  ((=> kitchen)
   (bind (kitchen 1.0 *initial-kitchen-state* 0.0))))

(defprimitive grease ((greased-container transferable-container)
                      (kitchen-state-out kitchen-state)
                      (kitchen-state-in kitchen-state)
                      (container-to-grease transferable-container)
                      (ingredient-to-grease-with transferable-container))
                      ;(tool-for-greasing cooking-utensil))

  ;; Case 1: no ingredient to grease with given
  ((kitchen-state-in container-to-grease => kitchen-state-out ingredient-to-grease-with greased-container )
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-to-grease new-kitchen-state))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-to-grease) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     ;; 1) find ingredient to be used for greasing and bring it to the countertop
     (multiple-value-bind (original-butter butter-original-location)
         (find-ingredient 'butter kitchen-state-in)

       (let* ((new-butter-container
               (find-object-by-persistent-id original-butter
                                             (funcall (type-of butter-original-location) new-kitchen-state)))
              (butter-container-for-greasing (copy-object new-butter-container))
              (new-butter (first (contents new-butter-container)))
              (butter-for-greasing (first (contents butter-container-for-greasing)))
              (amount-for-greasing (make-instance 'amount :quantity (make-instance 'quantity :value 10)
                                                           :unit (make-instance 'g))))
       
         (change-kitchen-entity-location new-butter-container ;;bring the butter to the countertop
                                         (funcall (type-of butter-original-location) new-kitchen-state)
                                         (counter-top new-kitchen-state))

         ;; 2) apply the butter to the inner surface of the container
         
         (setf (amount butter-for-greasing) amount-for-greasing)
         (setf (value (quantity (amount new-butter))) (- (value (quantity (amount new-butter)))
                                                         (value (quantity amount-for-greasing))))
         (setf (spread butter-for-greasing) t)
         (setf (brushed-with new-container) butter-for-greasing)
         (setf (used new-container) t)

         ;; 3) put the butter back in the fridge
         (change-kitchen-entity-location new-butter-container 
                                         (counter-top new-kitchen-state)
                                         (funcall (type-of butter-original-location) new-kitchen-state))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (greased-container 1.0 new-container container-available-at)
           (ingredient-to-grease-with 0.0 butter-container-for-greasing nil))))))

   ;; Case 2: ingredient to grease with given
  ((kitchen-state-in container-to-grease ingredient-to-grease-with => kitchen-state-out greased-container)
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-to-grease new-kitchen-state))
          (new-grease-container (find-object-by-persistent-id ingredient-to-grease-with new-kitchen-state))
          (new-grease (first (contents new-grease-container)))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-to-grease) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

            
            ;; apply the grease to the inner surface of the container
            (setf (contents new-grease-container) nil)
            (setf (spread new-grease) t)
            (setf (brushed-with new-container) new-grease)
            (setf (used new-container) t)

            (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

            (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (greased-container 1.0 new-container container-available-at)))))

(defprimitive flour ((floured-container transferable-container)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (container-to-flour transferable-container)
                     (ingredient-to-flour-with transferable-container))

  ;; Case 1; no ingredient-to-flour-with given
  ((kitchen-state-in container-to-flour => kitchen-state-out ingredient-to-flour-with floured-container )
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-to-flour new-kitchen-state))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-to-flour) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     ;; 1) find ingredient to be used for flouring and bring it to the countertop
     (multiple-value-bind (original-flour flour-original-location)
         (find-ingredient 'all-purpose-flour kitchen-state-in)

       (let* ((new-flour-container
               (find-object-by-persistent-id original-flour
                                             (funcall (type-of flour-original-location) new-kitchen-state)))
              (flour-container-for-flouring (copy-object new-flour-container))
              (new-flour (first (contents new-flour-container)))
              (flour-for-flouring (first (contents flour-container-for-flouring)))
              (amount-for-flouring (make-instance 'amount :quantity (make-instance 'quantity :value 10)
                                                           :unit (make-instance 'g))))
       
         (change-kitchen-entity-location new-flour-container ;;bring the flour to the countertop
                                         (funcall (type-of flour-original-location) new-kitchen-state)
                                         (counter-top new-kitchen-state))

         ;; 2) apply the flour to the inner surface of the container
         
         (setf (amount flour-for-flouring) amount-for-flouring)
         (setf (value (quantity (amount new-flour))) (- (value (quantity (amount new-flour)))
                                                        (value (quantity amount-for-flouring))))
         (setf (sprinkled-with new-container) flour-for-flouring)
         (setf (used new-container) t)

         ;; 3) put the flour back in the fridge
         (change-kitchen-entity-location new-flour-container 
                                         (counter-top new-kitchen-state)
                                         (funcall (type-of flour-original-location) new-kitchen-state))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (floured-container 1.0 new-container container-available-at)
           (ingredient-to-flour-with 0.0 flour-container-for-flouring nil))))))

   ;; Case 2; ingredient-to-flour-with given
  ((kitchen-state-in container-to-flour ingredient-to-flour-with => kitchen-state-out floured-container )
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-to-flour new-kitchen-state))
          (new-flour-container (find-object-by-persistent-id ingredient-to-flour-with new-kitchen-state))
          (new-flour (first (contents new-flour-container)))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-to-flour) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

         ;; apply the flour to the inner surface of the container
         (setf (contents new-flour-container) nil)
         (setf (sprinkled-with new-container) new-flour)
         (setf (used new-container) t)

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (floured-container 1.0 new-container container-available-at)))))


(defprimitive line ((lined-baking-tray lineable)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (baking-tray lineable)
                    (baking-paper can-be-lined-with))

  ;; Case 1; baking paper to line with is not given
  ((kitchen-state-in baking-tray => baking-paper kitchen-state-out lined-baking-tray)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (target-tray (if (is-concept baking-tray)
                         (retrieve-concept-instance-and-bring-to-countertop (type-of baking-tray) new-kitchen-state)
                         (find-object-by-persistent-id baking-tray (counter-top new-kitchen-state))))
          (tray-available-at (+ 150 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at tray-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     ;; find baking paper and bring it to the countertop
     (let ((target-paper-instance (retrieve-concept-instance-and-bring-to-countertop 'baking-paper new-kitchen-state)))
       (cond ((and
               target-tray
               target-paper-instance)

              (setf (lined-with target-tray) target-paper-instance) ;;do the lining
              (setf (is-lining target-paper-instance) t)

              (setf (contents (counter-top new-kitchen-state)) ;;remove the paper from the countertop
                    (remove target-paper-instance (contents (counter-top new-kitchen-state))))              

              (bind (lined-baking-tray 1.0 target-tray tray-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (baking-paper 0.0 target-paper-instance nil)))
             (t
              (bind (lined-baking-tray 1.0 (make-instance 'failed-object) tray-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (baking-paper 0.0 (make-instance 'failed-object) nil)))))))
              

  ;; Case 2; baking paper to line with is given
  ((kitchen-state-in baking-tray baking-paper => kitchen-state-out lined-baking-tray)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (target-tray (if (is-concept baking-tray)
                         (retrieve-concept-instance-and-bring-to-countertop (type-of baking-tray) new-kitchen-state)
                         (find-object-by-persistent-id baking-tray (counter-top new-kitchen-state))))
          (target-paper-instance (if (is-concept baking-paper)
                                   (retrieve-concept-instance-and-bring-to-countertop (type-of baking-paper) new-kitchen-state)
                                   (find-object-by-persistent-id baking-paper new-kitchen-state)))
          (tray-available-at (+ 150 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at tray-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
  
     (cond ((and
             target-tray
             target-paper-instance)
                   
            (setf (lined-with target-tray) target-paper-instance) ;;do the lining
            (setf (is-lining target-paper-instance) t)

            (setf (contents (counter-top new-kitchen-state)) ;;remove the paper from the countertop
                  (remove target-paper-instance (contents (counter-top new-kitchen-state))))

            (bind (lined-baking-tray 1.0 target-tray tray-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind (lined-baking-tray 1.0 (make-instance 'failed-object) tray-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))


(defprimitive mash ((mashed-ingredient transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (input-ingredient transferable-container)
                    (mashing-tool can-mash))
  
  ;; Case 1: mashing tool not given
  ((kitchen-state-in input-ingredient  => mashed-ingredient kitchen-state-out mashing-tool)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-ingredient (copy-object input-ingredient))
          (mashing-tool (retrieve-concept-instance-and-bring-to-countertop 'fork new-kitchen-state))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in) 
                                             (available-at (find (id input-ingredient) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (cond (mashing-tool

            (loop for item in (contents new-ingredient)
                  do (setf (mashed item) t))

            (setf (used mashing-tool) t)
     
            (bind (mashed-ingredient 1.0 new-ingredient container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (mashing-tool 0.0 mashing-tool container-available-at)))
           (t
            (bind (mashed-ingredient 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (mashing-tool 0.0 (make-instance 'failed-object) container-available-at))))))
     
            
   ;; Case 2: mashing tool given
   ((kitchen-state-in input-ingredient mashing-tool => mashed-ingredient kitchen-state-out)

    (let* ((new-kitchen-state (copy-object kitchen-state-in))
           (new-ingredient (copy-object input-ingredient))
           (new-mashing-tool (find-object-by-persistent-id mashing-tool new-kitchen-state)) 
           (container-available-at (+ 60 (max (kitchen-time kitchen-state-in) 
                                              (available-at (find (id input-ingredient) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))))
           (kitchen-state-available-at container-available-at))
      
      (loop for item in (contents new-ingredient)
            do (setf (mashed item) t))

      (setf (used new-mashing-tool) t)

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
      (bind (mashed-ingredient 1.0 new-ingredient container-available-at)
            (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))

(defprimitive melt ((container-with-melted-ingredients transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (container-with-input-ingredients transferable-container)
                    (melting-tool has-temperature)) ; could be an oven, a pan on a stove or something else but it should be able to control its temperature

  ;; Case 1: melting tool not given, a microwave is used
  ((kitchen-state-in container-with-input-ingredients => kitchen-state-out container-with-melted-ingredients melting-tool)
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state)))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in) ;;duration of melting depends on manner
                                             (available-at (find (id container-with-input-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (loop for ingredient in (contents new-container)
           when (typep ingredient 'meltable)
           do (setf (melted ingredient) t))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (melting-tool 1.0 (microwave new-kitchen-state) container-available-at)
           (container-with-melted-ingredients 1.0 new-container container-available-at))))

    ;; Case 2: melting tool given
  ((kitchen-state-in container-with-input-ingredients melting-tool => kitchen-state-out container-with-melted-ingredients)
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state)))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in) ;;duration of melting depends on manner
                                             (available-at (find (id container-with-input-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (loop for ingredient in (contents new-container)
           when (typep ingredient 'meltable)
           do (setf (melted ingredient) t))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (container-with-melted-ingredients 1.0 new-container container-available-at)))))

(defprimitive mingle ((container-with-mixture transferable-container)
                      (kitchen-state-out kitchen-state)
                      (kitchen-state-in kitchen-state)
                      (container-with-input-ingredients transferable-container)
                      (mingling-tool can-mingle))
  
  ;;Case 1: Mingling tool not specified, use a wooden spoon
  ((kitchen-state-in container-with-input-ingredients => kitchen-state-out container-with-mixture mingling-tool)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (container-available-at (+ 30 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-input-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                                   (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     ;; 1) find wooden spoon and bring it to the countertop
       (let ((target-spoon-instance (retrieve-concept-instance-and-bring-to-countertop 'wooden-spoon new-kitchen-state))
             (container-with-input-ingredients-instance
              (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state))))

         (cond (target-spoon-instance
                ;; 2) mingle contents in container with ingredients
                (let ((mixture (create-heterogeneous-mixture-in-container container-with-input-ingredients-instance)))

                  (setf (used target-spoon-instance) t)
                  (setf (contents container-with-input-ingredients-instance) (list mixture))

                  (bind (mingling-tool 0.0 target-spoon-instance)
                        (container-with-mixture 1.0 container-with-input-ingredients-instance container-available-at)
                        (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
               (t
                (bind (mingling-tool 0.0 (make-instance 'failed-object))
                      (container-with-mixture 1.0 (make-instance 'failed-object) container-available-at)
                      (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

  ;;Case 2: Mingling tool specified
  ((kitchen-state-in container-with-input-ingredients mingling-tool => kitchen-state-out container-with-mixture )
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container-with-ingredients-to-mix (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state)))
          (new-mingling-tool (find-object-by-persistent-id mingling-tool new-kitchen-state))
          (container-available-at (+ 30 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-input-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

          ;; mingle contents in container with ingredients
          (let ((mixture (create-heterogeneous-mixture-in-container new-container-with-ingredients-to-mix)))
            
            (setf (used new-mingling-tool) t)
            (setf (contents new-container-with-ingredients-to-mix) (list mixture)))

          (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
         
          (bind (container-with-mixture 1.0 new-container-with-ingredients-to-mix container-available-at)
                (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))

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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     ;; 1) find whisk and bring it to the countertop
       (let ((target-whisk-instance (retrieve-concept-instance-and-bring-to-countertop 'whisk new-kitchen-state))
             (container-with-input-ingredients-instance
              (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state))))

         (cond (target-whisk-instance

                ;; 2) mix contents in container with ingredients
                (let ((mixture (create-homogeneous-mixture-in-container container-with-input-ingredients-instance)))
                  (setf (used target-whisk-instance) t)
                  (setf (mixed mixture) t)
                  (setf (contents container-with-input-ingredients-instance) (list mixture))

                  (bind (mixing-tool 0.0 target-whisk-instance)
                        (container-with-mixture 1.0 container-with-input-ingredients-instance container-available-at)
                        (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
               (t
                (bind (mixing-tool 0.0 (make-instance 'failed-object))
                      (container-with-mixture 1.0 (make-instance 'failed-object) container-available-at)
                      (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

  ;;Case 2: Mixing tool specified
  ((kitchen-state-in container-with-input-ingredients mixing-tool => kitchen-state-out container-with-mixture )
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container-with-ingredients-to-mix (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state)))
          (new-mixing-tool (find-object-by-persistent-id mixing-tool new-kitchen-state))
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


(defprimitive peel ((peeled-ingredient transferable-container)
                    (peel-of-ingredient transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (input-ingredient transferable-container)
                    (peeling-tool can-peel))
  
  ;; Case 1: peeling tool not given, use a knife
  ((kitchen-state-in input-ingredient  => peeled-ingredient peel-of-ingredient kitchen-state-out peeling-tool)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-ingredient (copy-object input-ingredient))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in) 
                                             (available-at (find (id input-ingredient) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at)
          ;; 1) find peeling tool
          (new-peeling-tool (retrieve-concept-instance-and-bring-to-countertop 'knife new-kitchen-state))
          ;; 2) find container for peels
          (new-peel-container (retrieve-concept-instance-and-bring-to-countertop 'medium-bowl new-kitchen-state)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (cond ((and
             new-peeling-tool
             new-peel-container)
            
            ;; 3) peel everything in the container
            (loop for item in (contents new-ingredient)
                  do (setf (peeled item) t)
                     (push (make-instance 'peel :peel-of item) (contents new-peel-container)))

            (setf (used new-peeling-tool) t)
            (setf (used new-peel-container) t)
     
            (bind (peeled-ingredient 1.0 new-ingredient container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (peel-of-ingredient 0.0 new-peel-container container-available-at)
                  (peeling-tool 0.0 new-peeling-tool container-available-at)))
           (t
            (bind (peeled-ingredient 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (peel-of-ingredient 0.0 (make-instance 'failed-object) container-available-at)
                  (peeling-tool 0.0 (make-instance 'failed-object) container-available-at))))))

  ;; Case 2: peeling tool given
  ((kitchen-state-in input-ingredient peeling-tool => peeled-ingredient peel-of-ingredient kitchen-state-out)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-ingredient (copy-object input-ingredient))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in) 
                                             (available-at (find (id input-ingredient) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at)
          ;; 1) find peeling tool
          (new-peeling-tool (find-object-by-persistent-id peeling-tool (counter-top new-kitchen-state)))
          ;; 2) find container for peels
          (new-peel-container (retrieve-concept-instance-and-bring-to-countertop 'medium-bowl new-kitchen-state)))

     ;; 3) peel everything in the container
     (loop for item in (contents new-ingredient)
           do (setf (peeled item) t)
              (push (make-instance 'peel :peel-of item) (contents new-peel-container)))
     
     (setf (used new-peeling-tool) t)
     (setf (used new-peel-container) t)
     
     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (bind (peeled-ingredient 1.0 new-ingredient container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (peel-of-ingredient 0.0 new-peel-container container-available-at)))))

(defprimitive portion-and-arrange ((portions t) ; can be a list-of-kitchen-entities or a transferable-container with portions
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

       ; convert the portion amount to grams
       (when (not (eq (type-of unit) 'g))
         (let ((conversion-ingredient (copy-object dough)))
           (setf (amount conversion-ingredient) portion-amount)
           (setf portion-amount (amount (convert-to-g conversion-ingredient)))))
       
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
             (destination 0.0 source-destination)))))

  ;; Case 2: Arrangement pattern specified but destination not specified, use countertop
  ((kitchen-state-in container-with-dough quantity unit arrangement-pattern
                     => portions kitchen-state-out destination)
   
   (let* ((source-destination (counter-top kitchen-state-in))
          (new-kitchen-state (copy-object kitchen-state-in))
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

       ; convert the portion amount to grams
       (when (not (eq (type-of unit) 'g))
         (let ((conversion-ingredient (copy-object dough)))
           (setf (amount conversion-ingredient) portion-amount)
           (setf portion-amount (amount (convert-to-g conversion-ingredient)))))
       
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
             (setf (arrangement countertop) arrangement-pattern)) 

       (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

       (bind (portions 1.0 portions portions-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
             (destination 0.0 source-destination)))))

  ;; Case 3: Destination container specified and arrangement pattern not specified
  ((kitchen-state-in container-with-dough destination quantity unit
                     => arrangement-pattern portions kitchen-state-out)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-destination (find-object-by-persistent-id destination new-kitchen-state))
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
            (countertop (counter-top new-kitchen-state)))

       ; convert the portion amount to grams
       (when (not (eq (type-of unit) 'g))
         (let ((conversion-ingredient (copy-object dough)))
           (setf (amount conversion-ingredient) portion-amount)
           (setf portion-amount (amount (convert-to-g conversion-ingredient)))))
       
       (loop while (> left-to-transfer 0)
             for new-portion = (copy-object dough)
             if (> left-to-transfer (value (quantity portion-amount))) ;; not dealing with rest?
             do (setf (amount new-portion) portion-amount
                      (contents new-destination) (cons new-portion (contents new-destination))
                      left-to-transfer (- left-to-transfer (value (quantity portion-amount))))
             else do (setf (amount new-portion) (make-instance 'amount
                                                               :quantity (make-instance 'quantity
                                                                                        :value left-to-transfer)
                                                               :unit unit)
                           (contents countertop) (cons new-portion (contents countertop))
                           left-to-transfer 0)
             finally 
             (setf (contents container-with-dough-instance) nil)
             (setf (arrangement new-destination) default-arrangement-pattern)) 

       (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

       (bind (portions 1.0 new-destination portions-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
             (arrangement-pattern 0.0 default-arrangement-pattern)))))
  
   ;; Case 4: Arrangement pattern specified and destination container specified
  ((kitchen-state-in container-with-dough destination quantity unit arrangement-pattern
                     => portions kitchen-state-out)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-destination (find-object-by-persistent-id destination new-kitchen-state))
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
            (countertop (counter-top new-kitchen-state)))

       ; convert the portion amount to grams
       (when (not (eq (type-of unit) 'g))
         (let ((conversion-ingredient (copy-object dough)))
           (setf (amount conversion-ingredient) portion-amount)
           (setf portion-amount (amount (convert-to-g conversion-ingredient)))))
       
       (loop while (> left-to-transfer 0)
             for new-portion = (copy-object dough)
             if (> left-to-transfer (value (quantity portion-amount))) ;; not dealing with rest?
             do (setf (amount new-portion) portion-amount
                      (contents new-destination) (cons new-portion (contents new-destination))
                      left-to-transfer (- left-to-transfer (value (quantity portion-amount))))
             else do (setf (amount new-portion) (make-instance 'amount
                                                               :quantity (make-instance 'quantity
                                                                                        :value left-to-transfer)
                                                               :unit unit)
                           (contents countertop) (cons new-portion (contents countertop))
                           left-to-transfer 0)
             finally 
             (setf (contents container-with-dough-instance) nil)
             (setf (arrangement new-destination) arrangement-pattern)) 

       (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

       (bind (portions 1.0 new-destination portions-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))
  

(defprimitive preheat-oven ((preheated-oven oven)
                            (kitchen-state-out kitchen-state)
                            (kitchen-state-in kitchen-state)
                            (oven oven)
                            (quantity quantity)
                            (unit unit))
  ; Case 1: oven not given
  ((kitchen-state-in quantity unit => preheated-oven kitchen-state-out oven)
   
    (let* ((new-kitchen-state (copy-object kitchen-state-in))
           (kitchen-state-available-at (+ 15  (kitchen-time kitchen-state-in)))
           (oven-available-at (+ 615 (kitchen-time kitchen-state-in)))
           (target-temperature (make-instance 'amount :quantity quantity :unit unit))
           (oven-in-new-kitchen-state (oven new-kitchen-state)))

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

      (setf (temperature oven-in-new-kitchen-state) target-temperature)

      (bind (preheated-oven 1.0 oven-in-new-kitchen-state oven-available-at)
            (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
            (oven 0.0 (oven kitchen-state-in) (kitchen-time kitchen-state-in)))))

  ; Case 2: oven given
  ((kitchen-state-in oven quantity unit => preheated-oven kitchen-state-out)
   
    (let* ((new-kitchen-state (copy-object kitchen-state-in))
           (new-oven (find-object-by-persistent-id oven new-kitchen-state))
           (kitchen-state-available-at (+ 15  (kitchen-time kitchen-state-in)))
           (oven-available-at (+ 615 (kitchen-time kitchen-state-in)))
           (target-temperature (make-instance 'amount :quantity quantity :unit unit))
           (oven-in-new-kitchen-state (oven new-kitchen-state)))

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

      (setf (temperature new-oven) target-temperature)

      (bind (preheated-oven 1.0 oven-in-new-kitchen-state oven-available-at)
            (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))
  

(defprimitive refrigerate ((container-with-ingredients-at-temperature transferable-container)
                           (kitchen-state-out kitchen-state)
                           (kitchen-state-in kitchen-state)
                           (container-with-ingredients transferable-container)
                           (refrigerator fridge)
                           (cooling-quantity quantity)
                           (cooling-unit time-unit))

  ;; Case 1: refrigerator and cooling time (quantity and unit) are not given, use one hour as cooling time
  ((kitchen-state-in container-with-ingredients
                     => cooling-unit cooling-quantity refrigerator kitchen-state-out container-with-ingredients-at-temperature)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-cooling-quantity (make-instance 'quantity :value 1))
          (new-cooling-unit (make-instance 'hour)) ; we refrigerate for 1 hour
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (container-available-at (+ (kitchen-time kitchen-state-in)
                                     (* (value new-cooling-quantity)
                                        (if (eq (type-of new-cooling-unit) 'hour)
                                          (* 3600 (value new-cooling-quantity))
                                          (* 60 (value new-cooling-quantity))))))
          (kitchen-state-available-at (kitchen-time kitchen-state-in)))

     (change-temperature new-container (temperature (fridge kitchen-state-in)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
                
     (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (refrigerator 0.0 (fridge kitchen-state-in) kitchen-state-available-at)
           (cooling-quantity 0.0 new-cooling-quantity nil)
           (cooling-unit 0.0 new-cooling-unit nil))))
  
  ;; Case 2: refrigerator is not given, cooling time (quantity and unit) is given
  ((kitchen-state-in container-with-ingredients cooling-quantity cooling-unit
                     => refrigerator kitchen-state-out container-with-ingredients-at-temperature)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (container-available-at (+ (kitchen-time kitchen-state-in)
                                     (* (value cooling-quantity)
                                        (if (eq (type-of cooling-unit) 'hour)
                                                   (* 3600 (value cooling-quantity))
                                                   (* 60 (value cooling-quantity))))))
          (kitchen-state-available-at (kitchen-time kitchen-state-in)))

     (change-temperature new-container (temperature (fridge kitchen-state-in)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
                
     (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (refrigerator 0.0 (fridge kitchen-state-in) kitchen-state-available-at))))

  ;; Case 3: refrigerator, cooling time (quantity and unit) are all given
  ((kitchen-state-in container-with-ingredients refrigerator cooling-quantity cooling-unit
                     => kitchen-state-out container-with-ingredients-at-temperature)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (new-refrigerator (find-object-by-persistent-id refrigerator new-kitchen-state))
          (container-available-at (+ (kitchen-time kitchen-state-in)
                                     (* (value cooling-quantity)
                                        (if (eq (type-of cooling-unit) 'hour)
                                                   (* 3600 (value cooling-quantity))
                                                   (* 60 (value cooling-quantity))))))
          (kitchen-state-available-at (kitchen-time kitchen-state-in)))

     (change-temperature new-container (temperature new-refrigerator))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
                
     (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))

(defprimitive seed ((seeded-ingredient transferable-container)
                    (seed-of-ingredient transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (input-ingredient transferable-container)
                    (seeding-tool can-seed))
  
  ;; Case 1: seeding tool not given, use a knife
  ((kitchen-state-in input-ingredient  => seeded-ingredient seed-of-ingredient kitchen-state-out seeding-tool)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-ingredient (copy-object input-ingredient))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in) 
                                             (available-at (find (id input-ingredient) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at)
          ;; 1) find seeding tool
          (new-seeding-tool (retrieve-concept-instance-and-bring-to-countertop 'knife new-kitchen-state))
          ;; 2) find container for seeds
          (new-seed-container (retrieve-concept-instance-and-bring-to-countertop 'medium-bowl new-kitchen-state)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (cond ((and
             new-seeding-tool
             new-seed-container)
            ;; 3) seed everything in the container
            (loop for item in (contents new-ingredient)
                  do (setf (seeded item) t)
                     (push (make-instance 'seed :seed-of item) (contents new-seed-container)))

            (setf (used new-seeding-tool) t)
            (setf (used new-seed-container) t)
     
            (bind (seeded-ingredient 1.0 new-ingredient container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (seed-of-ingredient 0.0 new-seed-container container-available-at)
                  (seeding-tool 0.0 new-seeding-tool container-available-at)))
           (t
            (bind (seeded-ingredient 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (seed-of-ingredient 0.0 (make-instance 'failed-object) container-available-at)
                  (seeding-tool 0.0 (make-instance 'failed-object) container-available-at))))))

  ;; Case 2: seeding tool given
  ((kitchen-state-in input-ingredient seeding-tool => seeded-ingredient seed-of-ingredient kitchen-state-out)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-ingredient (copy-object input-ingredient))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in) 
                                             (available-at (find (id input-ingredient) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at)
          ;; 1) find seeding tool
          (new-seeding-tool (find-object-by-persistent-id seeding-tool (counter-top new-kitchen-state)))
          ;; 2) find container for seeds
          (new-seed-container (retrieve-concept-instance-and-bring-to-countertop 'medium-bowl new-kitchen-state)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     ;; 3) seed everything in the container
     (loop for item in (contents new-ingredient)
           do (setf (seeded item) t)
              (push (make-instance 'seed :seed-of item) (contents new-seed-container)))

     (setf (used new-seeding-tool) t)
     (setf (used new-seed-container) t)
     
     (bind (seeded-ingredient 1.0 new-ingredient container-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
           (seed-of-ingredient 0.0 new-seed-container container-available-at)))))

(defprimitive shake ((container-with-mixture transferable-container)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (container-with-input-ingredients transferable-container))
  
  ((kitchen-state-in container-with-input-ingredients => kitchen-state-out container-with-mixture)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container-with-input-ingredients (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state)))
          (container-available-at (+ 30 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-input-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                                   (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (cond ((and (subtypep (type-of new-container-with-input-ingredients) 'coverable-container)
                 (covered-with new-container-with-input-ingredients))

            ;; "mix" contents in container with ingredients
            (let ((mixture (create-homogeneous-mixture-in-container new-container-with-input-ingredients 'shaken)))

              (setf (contents new-container-with-input-ingredients) (list mixture))

              (bind (container-with-mixture 1.0 new-container-with-input-ingredients container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
           (t
            (bind (container-with-mixture 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

(defprimitive shape ((shaped-portions t) ;;transferable container or list of kitchen entities
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (portions t) ;;transferable container or list of kitchen entities
                     (shape shape))
  
  ((kitchen-state-in portions shape => shaped-portions kitchen-state-out)

   (cond ((subtypep (type-of portions) 'list-of-kitchen-entities)
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
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
         
         ((subtypep (type-of portions) 'transferable-container)
          (let* ((new-kitchen-state (copy-object kitchen-state-in))
                 (new-portions (find-object-by-persistent-id portions (counter-top new-kitchen-state)))
                 (portions-available-at (+ 85 (max (kitchen-time kitchen-state-in)
                                                   (available-at (find (id portions) binding-objects
                                                                       :key #'(lambda (binding-object)
                                                                                (and (value binding-object)
                                                                                     (id (value binding-object)))))))))
                 (kitchen-state-available-at portions-available-at))

            (loop for item in (contents new-portions)
                  do (setf (current-shape item) shape))

            (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

            (bind (shaped-portions 1.0 new-portions portions-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     ;; 1) find target container and place it on the countertop
     ;; 2) find sift and place it on the countertop
     (let* ((new-target-container (retrieve-concept-instance-and-bring-to-countertop 'large-bowl new-kitchen-state))
            (target-container-in-kitchen-input-state (find-object-by-persistent-id new-target-container kitchen-state-in))
            (new-sift (retrieve-concept-instance-and-bring-to-countertop 'sift new-kitchen-state)))

       (cond ((and new-target-container new-sift)
              ;; 3) transfer contents from source-container to empty target-container
              (setf (contents new-target-container) (contents new-source-container))
              (setf (used new-target-container) t)
              (setf (used new-sift) t)

              (loop for item in (contents new-target-container)
                    when (typep item 'siftable)
                      do (setf (sifted item) t))
       
              (setf (contents new-source-container) nil)
              (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

              (bind (container-with-sifted-contents 1.0 new-target-container container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (target-container 0.0 target-container-in-kitchen-input-state nil)
                    (sifting-tool 1.0 new-sift kitchen-state-available-at)))
             (t
              (bind (container-with-sifted-contents 1.0 (make-instance 'failed-object) container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (target-container 0.0 target-container-in-kitchen-input-state nil)
                    (sifting-tool 1.0 (make-instance 'failed-object) kitchen-state-available-at)))))))

  ;; Case 2: target container given, sift not given
  ((container-with-ingredients-to-be-sifted kitchen-state-in target-container
                                         =>  kitchen-state-out container-with-sifted-contents sifting-tool)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-source-container (find-object-by-persistent-id container-with-ingredients-to-be-sifted new-kitchen-state))
          (new-target-container (find-object-by-persistent-id target-container new-kitchen-state))
          (container-available-at (+ 90 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-ingredients-to-be-sifted) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     ;; 1) find sift and place it on the countertop
     (let ((new-sift (retrieve-concept-instance-and-bring-to-countertop 'sift new-kitchen-state)))

       (cond (new-sift
              ;; 2) transfer contents from source-container to empty target-container
              (setf (contents new-target-container) (contents new-source-container))
              (setf (used new-target-container) t)
              (setf (used new-sift) t)

              (loop for item in (contents new-target-container)
                    when (typep item 'siftable)
                      do (setf (sifted item) t))

              (setf (contents new-source-container) nil)

              (bind (container-with-sifted-contents 1.0 new-target-container container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (sifting-tool 1.0 new-sift kitchen-state-available-at)))
             (t
              (bind (container-with-sifted-contents 1.0 (make-instance 'failed-object) container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (sifting-tool 1.0 (make-instance 'failed-object) kitchen-state-available-at)))))))

  ;; Case 3: target container is given, sift is given
  ((container-with-ingredients-to-be-sifted target-container kitchen-state-in sifting-tool
                                            => kitchen-state-out container-with-sifted-contents)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-sift (find-object-by-persistent-id sifting-tool new-kitchen-state))
          (new-source-container (find-object-by-persistent-id container-with-ingredients-to-be-sifted new-kitchen-state))
          (new-target-container (find-object-by-persistent-id target-container new-kitchen-state))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-ingredients-to-be-sifted) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     ;; transfer contents from source-container to target-container
     (setf (contents new-target-container) (append (contents new-target-container)
                                                   (contents new-source-container)))
     (setf (used new-target-container) t)
     (setf (used new-sift) t)
       
     (loop for item in (contents new-target-container)
           when (typep item 'siftable)
             do (setf (sifted item) t))
       
     (setf (contents new-source-container) nil)
       
     (bind (container-with-sifted-contents 1.0 new-target-container container-available-at )
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))

(defprimitive spread ((container-with-objects-that-have-been-spread transferable-container)
                      (kitchen-state-out kitchen-state)
                      (kitchen-state-in kitchen-state)
                      (object-to-be-spread transferable-container) ; E.g. toast on a plate
                      (container-with-spread transferable-container) ; E.g. butter in a bowl
                      (can-spread-kitchen-tool can-spread)) ;E.g. a butter knife
  
  ;;Case 1: spreading tool given
  ((kitchen-state-in object-to-be-spread container-with-spread can-spread-kitchen-tool
                        => container-with-objects-that-have-been-spread kitchen-state-out)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container-with-things-spread (find-object-by-persistent-id object-to-be-spread new-kitchen-state))
          (new-spread-container (find-object-by-persistent-id container-with-spread new-kitchen-state))
          (new-spreading-tool (find-object-by-persistent-id can-spread-kitchen-tool new-kitchen-state))
          ;; time calculation of the spread object depends
          (container-available-at (+ 120 (max (kitchen-time kitchen-state-in)
                                              (available-at (find (id object-to-be-spread) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (if (contents new-container-with-things-spread)

       (let* ((number-of-items-to-spread-upon (loop with count = 0
                                                    for item in (contents new-container-with-things-spread)
                                                    if  (typep item 'can-be-spread-upon)
                                                      do (incf count)
                                                    finally (return count)))
              (actual-spread-in-bowl (first (contents container-with-spread)))
              (quantity-per-item (float (/ (value (quantity (amount actual-spread-in-bowl)))
                                           number-of-items-to-spread-upon)))
              (spread-unit (type-of (unit (amount actual-spread-in-bowl)))))

         (loop for item in (contents new-container-with-things-spread)
               for portioned-spread = (copy-object actual-spread-in-bowl)
               do (setf (amount portioned-spread)  (make-instance 'amount
                                                                  :quantity (make-instance 'quantity
                                                                                           :value quantity-per-item)
                                                                  :unit (make-instance spread-unit)))
                                               
                  (setf (spread portioned-spread) t)
                  (setf (spread-with item) portioned-spread))

         (setf (contents new-spread-container) nil)
         (setf (used new-spreading-tool) t)
     
         (bind (container-with-objects-that-have-been-spread 1.0 new-container-with-things-spread container-available-at)
               (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))

       ; spread directly on the container itself

       (progn
         (setf (contents new-container-with-things-spread) (contents container-with-spread))
         
         (setf (contents new-spread-container) nil)
         (setf (used new-spreading-tool) t)
       
         (bind (container-with-objects-that-have-been-spread 1.0 new-container-with-things-spread container-available-at)
               (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))
  
  ;;Case 2: spreading tool not given => fall back on default, which is spatula
  ((kitchen-state-in object-to-be-spread container-with-spread 
                     => container-with-objects-that-have-been-spread kitchen-state-out can-spread-kitchen-tool)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container-with-things-spread (find-object-by-persistent-id object-to-be-spread new-kitchen-state))
          (new-spread-container (find-object-by-persistent-id container-with-spread new-kitchen-state))
          (spreading-tool (retrieve-concept-instance-and-bring-to-countertop 'spatula new-kitchen-state))
          (container-available-at (+ 20 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id object-to-be-spread) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond (spreading-tool
            (if (contents new-container-with-things-spread)
     
              (let* ((number-of-items-to-spread-upon (loop with count = 0
                                                           for item in (contents new-container-with-things-spread)
                                                           if  (typep item 'can-be-spread-upon)
                                                             do (incf count)
                                                           finally (return count)))
                     (actual-spread-in-bowl (first (contents container-with-spread)))
                     (quantity-per-item (float (/ (value (quantity (amount actual-spread-in-bowl)))
                                                  number-of-items-to-spread-upon)))
                     (spread-unit (type-of (unit (amount actual-spread-in-bowl)))))
     
                (loop for item in (contents new-container-with-things-spread)
                      for portioned-spread = (copy-object actual-spread-in-bowl)
                      do (setf (amount portioned-spread)  (make-instance 'amount
                                                                         :quantity (make-instance 'quantity
                                                                                                  :value quantity-per-item)
                                                                         :unit (make-instance spread-unit)))
                         (setf (spread portioned-spread) t)
                         (setf (spread-with item) portioned-spread))

                (setf (contents new-spread-container) nil)
                (setf (used spreading-tool) t)
                (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
       
                (bind (container-with-objects-that-have-been-spread 1.0 new-container-with-things-spread container-available-at)
                      (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                      (can-spread-kitchen-tool 0.0 spreading-tool container-available-at)))

       ; spread directly on the container itself
              (progn
                (setf (contents new-container-with-things-spread) (contents container-with-spread))
         
                (setf (contents new-spread-container) nil)
                (setf (used spreading-tool) t)
       
                (bind (container-with-objects-that-have-been-spread 1.0 new-container-with-things-spread container-available-at)
                      (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                      (can-spread-kitchen-tool 0.0 spreading-tool container-available-at)))))
           (t
            (bind (container-with-objects-that-have-been-spread 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (can-spread-kitchen-tool 0.0 (make-instance 'failed-object) container-available-at)))))))

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
                                                     :unit (make-instance 'g)))
          (sprinkled-object-available-at (+ (max (kitchen-time kitchen-state-in)
                                                 (available-at (find (id object) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                            50))
          (kitchen-state-available-at sprinkled-object-available-at))

     (loop for portion in (contents new-input-container)
           for topping = (copy-object (first (contents new-topping-container)))
           do (setf (amount topping) topping-weight-per-portion)
              (setf (sprinkled-with portion) topping))
     
     (setf (contents new-topping-container) nil)
     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (bind (sprinkled-object 1.0 new-input-container sprinkled-object-available-at)
           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))

(defprimitive transfer-contents ((container-with-all-ingredients transferable-container)
                                 (container-with-rest transferable-container)
                                 (kitchen-state-out kitchen-state)
                                 (kitchen-state-in kitchen-state)
                                 (target-container transferable-container)
                                 (container-with-input-ingredients transferable-container)
                                 (quantity quantity)
                                 (unit unit))
  
  ;; Case 1: Case in which the target container is not given in the input-kitchen-state and no quantity and unit are given
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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     ;; 1) find target container and place it on the countertop
     (let* ((target-container-instance
             (retrieve-concept-instance-and-bring-to-countertop 'large-bowl new-kitchen-state))
            (target-container-in-kitchen-input-state (if target-container-instance
                                                       (find-object-by-persistent-id target-container-instance kitchen-state-in)
                                                       nil))
            (source-container-instance
             (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state))))

       (cond ((and target-container-instance (contents source-container-instance))
              ;; 2) add all contents from source container to target container
              (loop with container-amount = (make-instance 'amount :quantity (make-instance 'quantity :value 0))
                    for ingredient in (contents source-container-instance)
                    do (setf (value (quantity container-amount))
                             (+ (value (quantity container-amount))
                                (value (quantity (amount ingredient)))))
                       (setf (contents target-container-instance) (cons ingredient (contents target-container-instance)))
                       (setf (contents source-container-instance) (remove ingredient (contents source-container-instance) :test #'equalp))
                    finally
                      (setf (used target-container-instance) t)
                      (setf (unit container-amount) (unit (amount ingredient)))
                      (setf total-amount container-amount))  

              (bind (target-container 0.0 target-container-in-kitchen-input-state nil)
                    (container-with-all-ingredients 1.0 target-container-instance container-available-at)
                    (container-with-rest 1.0 source-container-instance container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (quantity 0.0 (quantity total-amount) nil)
                    (unit 0.0 (unit total-amount) nil)))
             (t
              (bind (target-container 0.0 (make-instance 'failed-object) nil)
                    (container-with-all-ingredients 1.0 (make-instance 'failed-object) container-available-at)
                    (container-with-rest 1.0 (make-instance 'failed-object) container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (quantity 0.0 (quantity total-amount) nil)
                    (unit 0.0 (unit total-amount) nil)))))))

  ;; Case 2: Case in which the target container is given in the input-kitchen-state and no quantity and unit are given
  ((kitchen-state-in container-with-input-ingredients target-container
                     => container-with-all-ingredients container-with-rest kitchen-state-out quantity unit)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (target-container-instance (find-object-by-persistent-id target-container new-kitchen-state))
          (source-container-instance
           (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state)))
          (total-amount nil)
          (container-available-at (+ 20 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-input-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((contents source-container-instance)
     
            ;; 1) all contents from source container to target container
            (loop with container-amount = (make-instance 'amount :quantity (make-instance 'quantity :value 0))
                  for ingredient in (contents source-container-instance)
                  do (setf (value (quantity container-amount))
                           (+ (value (quantity container-amount))
                              (value (quantity (amount ingredient)))))
                     (setf (contents target-container-instance) (cons ingredient (contents target-container-instance)))
                     (setf (contents source-container-instance)
                           (remove ingredient (contents source-container-instance) :test #'equalp))
                  finally
                    (setf (used target-container-instance) t)
                    (setf (unit container-amount) (unit (amount ingredient)))
                    (setf total-amount container-amount))
                  
            (bind (container-with-all-ingredients 1.0 target-container-instance container-available-at)
                  (container-with-rest 1.0 source-container-instance container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (quantity 0.0 (quantity total-amount) nil)
                  (unit 0.0 (unit total-amount) nil)))
           (t
            (bind (container-with-all-ingredients 1.0 (make-instance 'failed-object) container-available-at)
                  (container-with-rest 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (quantity 0.0 (quantity total-amount) nil)
                  (unit 0.0 (unit total-amount) nil)))))))

(defprimitive transfer-items ((transferred container)
                              (kitchen-state-out kitchen-state)
                              (kitchen-state-in kitchen-state)
                              (items-to-transfer t) ;;transferable container or list of kitchen entities
                              (arrangement-pattern arrangement-pattern)
                              (destination container))

  ;; Case 1 : transfer a number of items to a given destination, no arrangement-pattern is given
  ((kitchen-state-in items-to-transfer destination => kitchen-state-out arrangement-pattern transferred)

   (cond ((subtypep (type-of items-to-transfer) 'list-of-kitchen-entities)
     ;; items are grouped as a list of kitchen entities, lying on the countertop
     (let* ((new-kitchen-state (copy-object kitchen-state-in))
            (new-items-to-transfer (find-kitchen-entities items-to-transfer (counter-top new-kitchen-state)))
            (default-arrangement (make-instance 'side-to-side))
            (new-destination (find-object-by-persistent-id destination new-kitchen-state))
            (container-available-at (+ 120 (max (kitchen-time kitchen-state-in)
                                                (available-at (find (id destination) binding-objects
                                                                    :key #'(lambda (binding-object)
                                                                             (and (value binding-object)
                                                                                  (id (value binding-object)))))))))
            (kitchen-state-available-at container-available-at))
     
       (setf (used new-destination) t)
       (setf (contents new-destination) (items new-items-to-transfer))
       (setf (arrangement new-destination) default-arrangement)
       (setf (contents (counter-top new-kitchen-state)) ;;delete items from countertop!
             (remove-if #'(lambda (el)
                            (find (persistent-id el) (items new-items-to-transfer) :test #'eql :key #'persistent-id))
                        (contents (counter-top new-kitchen-state))))
     
       (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
       (bind
        (arrangement-pattern 0.0 default-arrangement nil) 
        (transferred 1.0 new-destination container-available-at)
        (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
     
     ;; items are placed on a transferable container, such as a baking tray
     ((subtypep (type-of items-to-transfer) 'transferable-container) ;baking-tray or cookie sheet
      (let* ((new-kitchen-state (copy-object kitchen-state-in))
             (new-container (find-object-by-persistent-id items-to-transfer new-kitchen-state))
             (default-arrangement (make-instance 'side-to-side))
             (new-destination (find-object-by-persistent-id destination new-kitchen-state))
             (container-available-at (+ 120 (kitchen-time kitchen-state-in)))
             (kitchen-state-available-at container-available-at))
     
        (setf (used new-destination) t)
        (setf (contents new-destination) (contents new-container))
        (setf (contents new-container) nil)
        (setf (used new-destination) t)
        (setf (arrangement new-destination) default-arrangement)
     
        (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
        (bind
         (arrangement-pattern 0.0 default-arrangement nil) 
         (transferred 1.0 new-destination container-available-at)
         (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))

  ;; Case 2 : transfer a number of items to a given destination and place them in the given pattern
  ((kitchen-state-in items-to-transfer arrangement-pattern destination => kitchen-state-out transferred)

   (cond ((subtypep (type-of items-to-transfer) 'list-of-kitchen-entities)
     ;; items are grouped as a list of kitchen entities, lying on the countertop
     (let* ((new-kitchen-state (copy-object kitchen-state-in))
            (new-items-to-transfer (find-kitchen-entities items-to-transfer (counter-top new-kitchen-state)))
            (new-destination (find-object-by-persistent-id destination new-kitchen-state))
            (container-available-at (+ 120 (max (kitchen-time kitchen-state-in)
                                                (available-at (find (id destination) binding-objects
                                                                    :key #'(lambda (binding-object)
                                                                             (and (value binding-object)
                                                                                  (id (value binding-object)))))))))
            (kitchen-state-available-at container-available-at))
     
       (setf (used new-destination) t)
       (setf (contents new-destination) (items new-items-to-transfer))
       (setf (arrangement new-destination) arrangement-pattern)
       (setf (contents (counter-top new-kitchen-state)) ;;delete items from countertop!
             (remove-if #'(lambda (el)
                            (find (persistent-id el) (items new-items-to-transfer) :test #'eql :key #'persistent-id))
                        (contents (counter-top new-kitchen-state))))
     
       (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
       (bind (transferred 1.0 new-destination container-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
     
     ;; items are placed on a transferable container, such as a baking tray
     ((subtypep (type-of items-to-transfer) 'transferable-container) ;baking-tray or cookie sheet
      (let* ((new-kitchen-state (copy-object kitchen-state-in))
             (new-container (find-object-by-persistent-id items-to-transfer new-kitchen-state))
             (new-destination (find-object-by-persistent-id destination new-kitchen-state))
             (container-available-at (+ 120 (kitchen-time kitchen-state-in)))
             (kitchen-state-available-at container-available-at))
     
        (setf (used new-destination) t)
        (setf (contents new-destination) (contents new-container))
        (setf (contents new-container) nil)
        (setf (used new-destination) t)
        (setf (arrangement new-destination) arrangement-pattern)
     
        (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
        (bind
         (transferred 1.0 new-destination container-available-at)
         (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

(defprimitive uncover ((uncovered-object coverable-container)
                       (cover can-cover)
                       (kitchen-state-out kitchen-state)
                       (kitchen-state-in kitchen-state)
                       (object coverable-container))

  ((kitchen-state-in object => uncovered-object cover kitchen-state-out)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id object new-kitchen-state))
          (container-available-at (+ 20 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id object) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at)
          (new-cover (covered-with new-container)))

     (setf (covered-with new-container) nil)
     (setf (is-covering new-cover) nil)

     ; put the cover on the countertop
     (setf (contents (counter-top new-kitchen-state)) (cons new-cover (contents (counter-top new-kitchen-state))))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (bind
      (uncovered-object 1.0 new-container container-available-at)
      (cover 1.0 new-cover container-available-at)
      (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))

;;--------------------------------------------------------------------------
;; Helper functions
;;--------------------------------------------------------------------------

(defun retrieve-concept-instance-and-bring-to-countertop (kitchen-concept kitchen-state &optional excluded)
  "Returns an instance of the given concept that has been put on the countertop.
   Optionally some entities can be excluded from the search (used when fetching multiple items at once)."
  (multiple-value-bind (target-object-in-kitchen-input-state target-object-original-location)
      (find-unused-kitchen-entity kitchen-concept kitchen-state excluded)

    (cond (target-object-in-kitchen-input-state
           (let ((concept-instance
                  (find-object-by-persistent-id target-object-in-kitchen-input-state
                                                (funcall (type-of target-object-original-location) kitchen-state))))

             (change-kitchen-entity-location concept-instance ;;bring the retrieved instance to the countertop
                                             (funcall (type-of target-object-original-location) kitchen-state)
                                             (counter-top kitchen-state))
             concept-instance))
          (t
           (print (format nil "~a not found in current kitchen state!!" kitchen-concept))
           nil))))

(defun change-kitchen-entity-location (kitchen-entity old-location new-location)
  "Adds the kitchen entity to the contents of the new-location and removes it from the contents of the old location "
  
  (setf (contents new-location)
        (cons kitchen-entity (contents new-location)))
  
  (setf (contents old-location)
        (remove kitchen-entity (contents old-location) :test #'equal)))

(defun change-temperature (container new-amount)
  (if (subtypep (type-of (unit new-amount)) 'time-unit)
    (loop for el in (contents container)
          do (setf (temperature el) (compute-temperature el new-amount)))
    (loop for el in (contents container)
          do (setf (temperature el) new-amount))))

(defun compute-temperature (ingredient new-amount)
  "Compute the temperature for a given ingredient, based on a given time"
  ; only minutes are supported currently
  (make-instance 'amount
                 :quantity (make-instance 'quantity
                                          ; TOVERIFY RD: location-related formula?
                                          :value (max (- (value (quantity (temperature ingredient)))
                                                         (* (/ (value (quantity (temperature ingredient))) 45)
                                                            (value (quantity new-amount))))
                                                      18))
                 :unit (make-instance 'degrees-celsius)))
                        
(defun take-n-pieces (source-container target-amount target-container)
  (assert (= (length (contents source-container)) 1))
  
  (let* ((source-ingredient (first (contents source-container)))
         (new-amount-source (make-instance 'amount
                                          :unit (unit target-amount)
                                          :quantity (make-instance 'quantity
                                                                   :value (- (value (quantity (amount source-ingredient)))
                                                                             (value (quantity target-amount)))))))

    (multiple-value-bind (wholes parts) (floor (value (quantity target-amount)))
      (let* ((target-ingredients
              (loop for piece from 1 to wholes
                    collect (make-instance (type-of source-ingredient)
                                           :amount (make-instance 'amount
                                                                  :unit (make-instance 'piece)
                                                                  :quantity (make-instance 'quantity :value 1)))))
             (target-ingredients (if (= parts 0)
                                   target-ingredients
                                   (cons (make-instance (type-of source-ingredient)
                                                        :amount (make-instance 'amount
                                                                               :unit (make-instance 'piece)
                                                                               :quantity (make-instance 'quantity :value parts)))
                                         target-ingredients))))

        ;;adjust amounts of source ingredient
        (setf (amount source-ingredient) new-amount-source)

        ;;add all target ingredients to contents of target-container
        (setf (contents target-container)
              (append target-ingredients (contents target-container)))

        (values target-container source-container)))))

(defun weigh-ingredient (source-container target-amount target-container)

  (assert (= (length (contents source-container)) 1))
  
  (let* ((source-ingredient (first (contents source-container)))
         (new-amount-source (make-instance 'amount
                                          :unit (unit target-amount)
                                          :quantity (make-instance 'quantity
                                                                   :value (- (value (quantity (amount source-ingredient)))
                                                                             (value (quantity target-amount))))))
         (target-ingredient (copy-object source-ingredient)))

    ;;adjust amounts of target and source ingredients
    (setf (amount target-ingredient) target-amount)
    (setf (amount source-ingredient) new-amount-source)
    
    ;;add weighed ingredient to contents of target-container
    (setf (contents target-container)
          (cons target-ingredient (contents target-container)))
    
  (values target-container source-container)))

(defun find-unused-kitchen-entity (reusable-type place &optional excluded)
  "Recursively look for an unused kitchen entity of the given type in the given place.
   Optionally some entities can be excluded from the search (used when fetching multiple items at once)."
  (cond ((loop for el in (contents place)
               if (and (eql reusable-type (type-of el))
                       (not (used el))
                       (not (find el excluded)))
               do (return t)) ; first we check if an unused element of that type could be found in general (= condition part of cond)
         (loop for el in (contents place)
               if (and (eql reusable-type (type-of el))
                       (not (used el))
                       (not (find el excluded)))
               do (return (values el place)))) ; we go over elements again and this time we will actually return the found element and the place in which it is found (= execution part of cond)
        (t
         (loop for el in (contents place)
               if (subtypep (type-of el) 'container)
                 do (multiple-value-bind (found-entity found-place)
                        (find-unused-kitchen-entity reusable-type el excluded)
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

;; find all kitchen entities in the list that are on the countertop
(defun find-kitchen-entities (list-of-kitchen-entities countertop)
  (let ((new-list-of-kitchen-entities (copy-object list-of-kitchen-entities))
        (first-item (first (items list-of-kitchen-entities)))
        (countertop-start (contents countertop)))
    ; there could have been other items added to the countertop in the meantime
    (while (and countertop-start
                (not (eq (persistent-id first-item) (persistent-id (first countertop-start)))))
      (setf countertop-start (rest countertop-start)))
    (setf (items new-list-of-kitchen-entities) nil)
    (loop for item in (items list-of-kitchen-entities)
          for thing in countertop-start
          when (eq (persistent-id item) (persistent-id thing))
          do (push thing (items new-list-of-kitchen-entities))
          finally (return new-list-of-kitchen-entities))))

(defun create-homogeneous-mixture-in-container (container &optional (mixing-operation 'mixed))
  "Create a homogeneous mixture composed of the ingredients in the given container and mixed according to the given mixing-operation.
   The mixing-operation should be a valid slotname specifying a boolean slot that will be set to T."
  (let* ((total-value (loop for ingredient in (contents container)
                            for current-value = (value (quantity (amount (convert-to-g ingredient))))
                            sum current-value))
         (mixture (make-instance 'homogeneous-mixture :amount (make-instance 'amount
                                                                :unit (make-instance 'g)
                                                                :quantity (make-instance 'quantity :value total-value))
                                 :components (contents container))))

    ; modify the contents so they all contain percentages (to prevent recursion errors when portioning nested mixtures)
    (loop for ingredient in (components mixture)
          do (setf (amount ingredient)
                   (make-instance 'amount
                                  :quantity (make-instance 'quantity
                                                           :value (/ (value (quantity (amount (convert-to-g ingredient)))) total-value))
                                  :unit (make-instance 'percent))))
  
      (setf (contents container) (list mixture))
      (setf (slot-value (first (contents container)) mixing-operation) t)
      mixture))

(defun create-heterogeneous-mixture-in-container (container)
  "Create a homogeneous mixture composed of the ingredients in the given container and mixed according to the given mixing-operation.
   The mixing-operation should be a valid slotname specifying a boolean slot that will be set to T."
  (let* ((total-value (loop for ingredient in (contents container)
                            for current-value = (value (quantity (amount (convert-to-g ingredient))))
                            sum current-value))
         (mixture (make-instance 'heterogeneous-mixture :amount (make-instance 'amount
                                                                :unit (make-instance 'g)
                                                                :quantity (make-instance 'quantity :value total-value))
                                 :components (contents container))))
    ; modify the contents so they all contain percentages (to prevent recursion errors when portioning nested mixtures)
    (loop for ingredient in (components mixture)
          do (setf (amount ingredient)
                   (make-instance 'amount
                                  :quantity (make-instance 'quantity
                                                           :value (/ (value (quantity (amount (convert-to-g ingredient)))) total-value))
                                  :unit (make-instance 'percent))))
    
    (setf (contents container) (list mixture))
    mixture))


(defun create-conversion-table-for-g ()
  (let ((conversion-table (make-hash-table)))
    (setf (gethash 'almond-extract conversion-table)
	  (acons 'teaspoon 4 '()))
    (setf (gethash 'all-purpose-flour conversion-table)
          (acons 'teaspoon 3 '()))
    (setf (gethash 'baking-soda conversion-table)
	  (acons 'teaspoon 5 '()))
    (setf (gethash 'baking-powder conversion-table)
	  (acons 'teaspoon 4 '()))  
    (setf (gethash 'banana conversion-table)
	  (acons 'piece 118 '()))
    (setf (gethash 'butter conversion-table)
          (acons 'teaspoon 5 '()))
    (setf (gethash 'caster-sugar conversion-table)
	  (acons 'teaspoon 5 '()))
    (setf (gethash 'coarse-salt conversion-table)
          (acons 'teaspoon 5 '()))
    (setf (gethash 'cocoa-powder conversion-table)
          (acons 'teaspoon 4 '()))
    (setf (gethash 'corn-flakes conversion-table)
          (acons 'teaspoon 2 '()))
    (setf (gethash 'cucumber conversion-table)
          (acons 'piece 250 '()))
    (setf (gethash 'dried-dill-weed conversion-table)
          (acons 'teaspoon 5 '()))    
    (setf (gethash 'egg conversion-table)
          (acons 'piece 50 '()))
    (setf (gethash 'ground-allspice conversion-table)
	  (acons 'teaspoon 2 '()))
    (setf (gethash 'ground-black-pepper conversion-table)
	  (acons 'teaspoon 3 '()))
    (setf (gethash 'ground-cinnamon conversion-table)
	  (acons 'teaspoon 2.7 '()))
    (setf (gethash 'ground-cloves conversion-table)
	  (acons 'teaspoon 2.2 '()))
    (setf (gethash 'ground-cumin conversion-table)
          (acons 'teaspoon 2 '()))
    (setf (gethash 'ground-ginger conversion-table)
          (acons 'teaspoon 2 '()))
    (setf (gethash 'ground-nutmeg conversion-table)
	  (acons 'teaspoon 2.2 '()))
    (setf (gethash 'lime-juice conversion-table)
	  (acons 'teaspoon 5 '()))
    (setf (gethash 'jalapeno conversion-table)
          (acons 'piece 20 '()))
    (setf (gethash 'milk conversion-table)
	  (acons 'l 1032 '()))
    (setf (gethash 'olive-oil conversion-table)
          (acons 'teaspoon 4.5 '()))
    (setf (gethash 'onion conversion-table)
          (acons 'piece 100 '()))
    (setf (gethash 'red-onion conversion-table)
          (acons 'piece 50 '()))
    (setf (gethash 'red-pepper-flakes conversion-table)
          (acons 'teaspoon 0.5 '()))
    (setf (gethash 'salt conversion-table)
          (acons 'teaspoon 6 '()))
    (setf (gethash 'shallot conversion-table)
          (acons 'piece 50 '()))
    (setf (gethash 'vanilla-extract conversion-table)
	  (acons 'l 880 (acons 'teaspoon 4 '())))
    (setf (gethash 'vegetable-oil conversion-table)
          (acons 'l 944 '()))
    (setf (gethash 'water conversion-table)
	  (acons 'l 1000 (acons 'teaspoon 5 '()))) 
    (setf (gethash 'white-sugar conversion-table)
          (acons 'teaspoon 4.2 '()))
    (setf (gethash 'white-vinegar conversion-table)
	  (acons 'l 1085 '()))
    (setf (gethash 'whole-egg conversion-table)
	  (acons 'piece 50 '())) 
    conversion-table))

;; define conversion table as a global parameter
(defparameter *conversion-table-for-g* (create-conversion-table-for-g))

;; create a copy of the mixture with g as its unit
(defmethod convert-to-g ((mixture mixture) &key &allow-other-keys)
  (let ((converted-mixture (copy-object mixture)))
    (when (not (eq (type-of (unit (amount converted-mixture))) 'g))
      (let* ((copied-mixture (copy-object mixture))
             (source-unit-type (type-of (unit (amount copied-mixture))))
             (mixture-value (value (quantity (amount copied-mixture))))
             (total-g 0))
        (loop for comp in (components copied-mixture)
              do (setf (amount comp) (make-instance 'amount
                                                    :unit (make-instance source-unit-type)
                                                    :quantity (make-instance 'quantity
                                                                             :value (* (value (quantity (amount comp)))
                                                                                       mixture-value))))
                 (setf total-g (+ total-g (value (quantity (amount (convert-to-g comp)))))))
        
        (setf (amount converted-mixture)
              (make-instance 'amount
                             :unit (make-instance 'g)
                             :quantity (make-instance 'quantity :value total-g)))))
    converted-mixture))

;; create a copy of the ingredient with g as its unit
(defmethod convert-to-g ((ingredient ingredient) &key &allow-other-keys)
  (let ((copied-ingredient (copy-object ingredient)))
    (when (not (eq (type-of (unit (amount copied-ingredient))) 'g))
      (let ((ingredient-type (type-of copied-ingredient))
            (source-unit-type (type-of (unit (amount copied-ingredient)))))
        (when (eq source-unit-type 'ml)
          (setf source-unit-type 'l)
          (setf (value (quantity (amount copied-ingredient))) (/ (value (quantity (amount copied-ingredient))) 1000))
          (setf (unit (amount copied-ingredient)) (make-instance 'l)))
        (when (eq source-unit-type 'tablespoon)
          (setf source-unit-type 'teaspoon)
          (setf (value (quantity (amount copied-ingredient))) (* (value (quantity (amount copied-ingredient))) 3))
          (setf (unit (amount copied-ingredient)) (make-instance 'teaspoon)))      
        (multiple-value-bind (conversion-rates found) (gethash ingredient-type *conversion-table-for-g*)
          (unless found
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
