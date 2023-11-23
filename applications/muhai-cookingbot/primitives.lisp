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

     (setf (kitchen-time new-kitchen-state)  kitchen-state-available-at)

     (cond (new-thing-to-bake

            (setf (temperature oven-to-be-heated) target-temperature)

            ;; baked things never actually enter the oven!
            (loop for bakeable in (contents new-thing-to-bake)
                  do (setf (temperature bakeable) target-temperature)
                     (setf (baked bakeable) t))

            (bind (thing-baked 1.0 new-thing-to-bake thing-baked-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (oven-to-bake-in 0.0 oven-to-be-heated thing-baked-available-at)))
           (t
            (bind (thing-baked 1.0 (make-instance 'failed-object) thing-baked-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (oven-to-bake-in 0.0 oven-to-be-heated thing-baked-available-at))))))
     

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

     (setf (kitchen-time new-kitchen-state)  kitchen-state-available-at)

     (cond ((not (has-failed-objects thing-to-bake))
            
            ;; baked things never actually enter the oven!
            (loop for bakeable in (contents new-thing-to-bake)
                  do (setf (temperature bakeable) oven-temperature)
                     (setf (baked bakeable) t))
     
            (bind (thing-baked 1.0 new-thing-to-bake thing-baked-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (target-temperature-quantity 0.0 (quantity oven-temperature) nil)
                  (target-temperature-unit 0.0 (unit oven-temperature) nil)))
           (t
            (bind (thing-baked 1.0 (make-instance 'failed-object) thing-baked-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (target-temperature-quantity 0.0 (quantity oven-temperature) nil)
                  (target-temperature-unit 0.0 (unit oven-temperature) nil))))))
           
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

     (setf (kitchen-time new-kitchen-state)  kitchen-state-available-at)

     (cond ((not (has-failed-objects thing-to-bake))
     
            (setf (temperature new-oven-to-bake-in) target-temperature)

            ;; baked things never actually enter the oven!
            (loop for bakeable in (contents new-thing-to-bake)
                  do (setf (temperature bakeable) target-temperature)
                     (setf (baked bakeable) t))
     
            (bind (thing-baked 1.0 new-thing-to-bake thing-baked-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind (thing-baked 1.0 (make-instance 'failed-object) thing-baked-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

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
            (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state))))

       (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
       
       (cond ((and target-tool-instance-new-ks new-container)

              (let ((new-mixture (create-homogeneous-mixture-in-container new-container 'beaten)))
                (setf (used target-tool-instance-new-ks) t)
                (setf (contents new-container) (list new-mixture))

                (bind (container-with-ingredients-beaten 1.0 new-container container-available-at)
                      (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                      (tool 0.0 target-tool-instance-new-ks nil))))
             (t
              (bind (container-with-ingredients-beaten 1.0 (make-instance 'failed-object) container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (tool 0.0 (make-instance 'failed-object) nil)))))))

  ;; Case 2: Reuse existing whisk for beating
  ((kitchen-state-in container-with-ingredients tool => kitchen-state-out container-with-ingredients-beaten )
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-tool (find-object-by-persistent-id tool (counter-top new-kitchen-state)))
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (kitchen-state-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                                (available-at (find (id container-with-ingredients) binding-objects
                                                                     :key #'(lambda (binding-object)
                                                                              (and (value binding-object)
                                                                                   (id (value binding-object)))))))))
          (container-available-at kitchen-state-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((and new-tool new-container)
            (let ((new-mixture (create-homogeneous-mixture-in-container new-container 'beaten)))
              (setf (used new-tool) t)
              (setf (contents new-container) (list new-mixture))   
     
              (bind (container-with-ingredients-beaten 1.0 new-container container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
           (t
            (bind (container-with-ingredients-beaten 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

(defprimitive boil ((thing-boiled transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (thing-to-boil transferable-container)
                    (stove-to-boil-on stove)
                    (heating-mode stove-mode)
                    (time-to-boil-quantity quantity)
                    (time-to-boil-unit unit))

  ;; Case 1: Stove and heating mode are not given, but boiling time is given
  ((kitchen-state-in thing-to-boil time-to-boil-quantity time-to-boil-unit
                     => kitchen-state-out thing-boiled stove-to-boil-on heating-mode)
 
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (stove-to-be-heated (stove new-kitchen-state))
          (new-thing-to-boil (find-object-by-persistent-id thing-to-boil new-kitchen-state))
          (new-heating-mode (make-instance 'medium-heat))
          (thing-boiled-available-at (+ (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id thing-to-boil) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))
                                       (* (value time-to-boil-quantity)
                                          (if (eq time-to-boil-unit 'hour)
                                            3600
                                            60))))
          (kitchen-state-available-at (+ (max (kitchen-time kitchen-state-in)
                                            (available-at (find (id thing-to-boil) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                         30)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((and new-thing-to-boil stove-to-be-heated (subtypep (type-of new-thing-to-boil) 'heatable-container)) ; the container should support being put on a stove

            (let* ((all-contents (contents new-thing-to-boil))
                   (spices (remove-if-not #'(lambda (object) (subtypep (type-of object) 'spice)) all-contents))
                   (liquids (remove-if-not #'(lambda (object) (subtypep (type-of object) 'liquid)) all-contents))
                   (liquid-mixture (if (and (= (length spices) 0) (= (length liquids) 1))
                                     (first liquids)
                                     (create-homogeneous-mixture (append spices liquids) 'boiled)))
                   (main-content (set-difference all-contents (append spices liquids))))

              (setf (temperature stove-to-be-heated) (convert-to-temperature stove-to-be-heated new-heating-mode))

              ; we keep track of what the main content and liquids are boiled with
              (loop for boilable in main-content
                    do (setf (boiled boilable) t)
                       (setf (temperature boilable) (copy-object (temperature stove-to-be-heated)))
                       (setf (boiled-with boilable) liquid-mixture))

              (cond ((or (> (length spices) 0) (> (length liquids) 1))
                     (setf (is-liquid liquid-mixture) t)
                     (setf (temperature liquid-mixture) (copy-object (temperature stove-to-be-heated))))
                    (t
                     (setf (boiled (first liquids)) t)
                     (setf (temperature (first liquids)) (copy-object (temperature stove-to-be-heated)))))
              
              (setf (contents new-thing-to-boil) (append main-content (list liquid-mixture)))

              (bind (thing-boiled 1.0 new-thing-to-boil thing-boiled-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (stove-to-boil-on 0.0 stove-to-be-heated thing-boiled-available-at)
                    (heating-mode 0.0 new-heating-mode nil))))
           (t
            (bind (thing-boiled 1.0 (make-instance 'failed-object) thing-boiled-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (stove-to-boil-on 0.0 stove-to-be-heated thing-boiled-available-at)
                  (heating-mode 0.0 new-heating-mode nil))))))    

  ;; Case 2: Stove and time are given, but heating mode is not given
  ((kitchen-state-in thing-to-boil stove-to-boil-on time-to-boil-quantity time-to-boil-unit
                     => kitchen-state-out thing-boiled heating-mode)
 
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (stove-to-be-heated (find-object-by-persistent-id stove-to-boil-on new-kitchen-state))
          (new-thing-to-boil (find-object-by-persistent-id thing-to-boil new-kitchen-state))
          (new-heating-mode (make-instance 'medium-heat))
          (thing-boiled-available-at (+ (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id thing-to-boil) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))
                                        (* (value time-to-boil-quantity)
                                           (if (eq time-to-boil-unit 'hour)
                                             3600
                                            60))))
          (kitchen-state-available-at (+ (max (kitchen-time kitchen-state-in)
                                            (available-at (find (id thing-to-boil) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                         30)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((and new-thing-to-boil stove-to-be-heated)
            
            (let* ((all-contents (contents new-thing-to-boil))
                   (spices (remove-if-not #'(lambda (object) (subtypep (type-of object) 'spice)) all-contents))
                   (liquids (remove-if-not #'(lambda (object) (subtypep (type-of object) 'liquid)) all-contents))
                   (liquid-mixture (if (and (= (length spices) 0) (= (length liquids) 1))
                                     (first liquids)
                                     (create-homogeneous-mixture (append spices liquids) 'boiled)))
                   (main-content (set-difference all-contents (append spices liquids))))

              (setf (temperature stove-to-be-heated) (convert-to-temperature stove-to-be-heated new-heating-mode))

              ; we keep track of what the main content and liquids are boiled with
              (loop for boilable in main-content
                    do (setf (boiled boilable) t)
                       (setf (temperature boilable) (copy-object (temperature stove-to-be-heated)))
                       (setf (boiled-with boilable) liquid-mixture))

              (cond ((or (> (length spices) 0) (> (length liquids) 1))
                     (setf (is-liquid liquid-mixture) t)
                     (setf (temperature liquid-mixture) (copy-object (temperature stove-to-be-heated))))
                    (t
                     (setf (boiled (first liquids)) t)
                     (setf (temperature (first liquids)) (copy-object (temperature stove-to-be-heated)))))
              
              (setf (contents new-thing-to-boil) (append main-content (list liquid-mixture)))

              (bind (thing-boiled 1.0 new-thing-to-boil thing-boiled-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (heating-mode 0.0 new-heating-mode nil))))
           (t
            (bind (thing-boiled 1.0 (make-instance 'failed-object) thing-boiled-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (heating-mode 0.0 new-heating-mode nil))))))
           
  ;; Case 3: Stove is given, but time and heating mode are not given
  ((kitchen-state-in thing-to-boil stove-to-boil-on
                     => kitchen-state-out thing-boiled time-to-boil-quantity time-to-boil-unit heating-mode)
 
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (stove-to-be-heated (find-object-by-persistent-id stove-to-boil-on new-kitchen-state))
          (new-thing-to-boil (find-object-by-persistent-id thing-to-boil new-kitchen-state))
          (new-heating-mode (make-instance 'medium-heat))
          (new-time-to-boil-quantity (make-instance 'quantity :value 30)) ; fixed boiling time of 30 minutes
          (new-time-to-boil-unit (make-instance 'minute))
          (thing-boiled-available-at (+ (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id thing-to-boil) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))
                                       (* (value new-time-to-boil-quantity)
                                          (if (eq new-time-to-boil-unit 'hour)
                                            3600
                                            60))))
          (kitchen-state-available-at (+ (max (kitchen-time kitchen-state-in)
                                            (available-at (find (id thing-to-boil) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                         30)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((and new-thing-to-boil stove-to-be-heated)

            (let* ((all-contents (contents new-thing-to-boil))
                   (spices (remove-if-not #'(lambda (object) (subtypep (type-of object) 'spice)) all-contents))
                   (liquids (remove-if-not #'(lambda (object) (subtypep (type-of object) 'liquid)) all-contents))
                   (liquid-mixture (if (and (= (length spices) 0) (= (length liquids) 1))
                                     (first liquids)
                                     (create-homogeneous-mixture (append spices liquids) 'boiled)))
                   (main-content (set-difference all-contents (append spices liquids))))

              (setf (temperature stove-to-be-heated) (convert-to-temperature stove-to-be-heated new-heating-mode))

              ; we keep track of what the main content and liquids are boiled with
              (loop for boilable in main-content
                    do (setf (boiled boilable) t)
                       (setf (temperature boilable) (copy-object (temperature stove-to-be-heated)))
                       (setf (boiled-with boilable) liquid-mixture))

              (cond ((or (> (length spices) 0) (> (length liquids) 1))
                     (setf (is-liquid liquid-mixture) t)
                     (setf (temperature liquid-mixture) (copy-object (temperature stove-to-be-heated))))
                    (t
                     (setf (boiled (first liquids)) t)
                     (setf (temperature (first liquids)) (copy-object (temperature stove-to-be-heated)))))
              
              (setf (contents new-thing-to-boil) (append main-content (list liquid-mixture)))

              (bind (thing-boiled 1.0 new-thing-to-boil thing-boiled-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (heating-mode 0.0 new-heating-mode nil)
                    (time-to-boil-quantity 0.0 new-time-to-boil-quantity nil)
                    (time-to-boil-unit 0.0 new-time-to-boil-unit nil))))
           (t
            (bind (thing-boiled 1.0 (make-instance 'failed-object) thing-boiled-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (heating-mode 0.0 new-heating-mode nil)
                  (time-to-boil-quantity 0.0 new-time-to-boil-quantity nil)
                  (time-to-boil-unit 0.0 new-time-to-boil-unit nil))))))
  
  ;; Case 4: Stove, heating mode and time are not given
  ((kitchen-state-in thing-to-boil 
                     => kitchen-state-out thing-boiled stove-to-boil-on heating-mode time-to-boil-quantity time-to-boil-unit)
 
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (stove-to-be-heated (stove new-kitchen-state))
          (new-thing-to-boil (find-object-by-persistent-id thing-to-boil new-kitchen-state))
          (new-heating-mode (make-instance 'medium-heat))
          (new-time-to-boil-quantity (make-instance 'quantity :value 30)) ; fixed boiling time of 30 minutes
          (new-time-to-boil-unit (make-instance 'minute))
          (thing-boiled-available-at (+ (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id thing-to-boil) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))
                                       (* (value new-time-to-boil-quantity)
                                          (if (eq new-time-to-boil-unit 'hour)
                                            3600
                                            60))))
          (kitchen-state-available-at (+ (max (kitchen-time kitchen-state-in)
                                            (available-at (find (id thing-to-boil) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                         30)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((and new-thing-to-boil stove-to-be-heated)

            (let* ((all-contents (contents new-thing-to-boil))
                   (spices (remove-if-not #'(lambda (object) (subtypep (type-of object) 'spice)) all-contents))
                   (liquids (remove-if-not #'(lambda (object) (subtypep (type-of object) 'liquid)) all-contents))
                   (liquid-mixture (if (and (= (length spices) 0) (= (length liquids) 1))
                                     (first liquids)
                                     (create-homogeneous-mixture (append spices liquids) 'boiled)))
                   (main-content (set-difference all-contents (append spices liquids))))

              (setf (temperature stove-to-be-heated) (convert-to-temperature stove-to-be-heated new-heating-mode))

              ; we keep track of what the main content and liquids are boiled with
              (loop for boilable in main-content
                    do (setf (boiled boilable) t)
                       (setf (temperature boilable) (copy-object (temperature stove-to-be-heated)))
                       (setf (boiled-with boilable) liquid-mixture))

              (cond ((or (> (length spices) 0) (> (length liquids) 1))
                     (setf (is-liquid liquid-mixture) t)
                     (setf (temperature liquid-mixture) (copy-object (temperature stove-to-be-heated))))
                    (t
                     (setf (boiled (first liquids)) t)
                     (setf (temperature (first liquids)) (copy-object (temperature stove-to-be-heated)))))
              
              (setf (contents new-thing-to-boil) (append main-content (list liquid-mixture)))

              (bind (thing-boiled 1.0 new-thing-to-boil thing-boiled-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (stove-to-boil-on 0.0 stove-to-be-heated thing-boiled-available-at)
                    (heating-mode 0.0 new-heating-mode nil)
                    (time-to-boil-quantity 0.0 new-time-to-boil-quantity nil)
                    (time-to-boil-unit 0.0 new-time-to-boil-unit nil))))
           (t
              (bind (thing-boiled 1.0 (make-instance 'failed-object) thing-boiled-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (stove-to-boil-on 0.0 (make-instance 'failed-object) thing-boiled-available-at)
                    (heating-mode 0.0 new-heating-mode nil)
                    (time-to-boil-quantity 0.0 new-time-to-boil-quantity nil)
                    (time-to-boil-unit 0.0 new-time-to-boil-unit nil))))))
    
   ;; Case 5: Stove, heating mode and time are given
  ((kitchen-state-in thing-to-boil stove-to-boil-on heating-mode time-to-boil-quantity time-to-boil-unit
                     => kitchen-state-out thing-boiled)
 
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (stove-to-be-heated (find-object-by-persistent-id stove-to-boil-on new-kitchen-state))
          (new-thing-to-boil (find-object-by-persistent-id thing-to-boil new-kitchen-state))
          (thing-boiled-available-at (+ (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id thing-to-boil) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))
                                        (* (value time-to-boil-quantity)
                                           (if (eq time-to-boil-unit 'hour)
                                             3600
                                            60))))
          (kitchen-state-available-at (+ (max (kitchen-time kitchen-state-in)
                                            (available-at (find (id thing-to-boil) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                         30)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((and new-thing-to-boil stove-to-be-heated)
            
            (let* ((all-contents (contents new-thing-to-boil))
                   (spices (remove-if-not #'(lambda (object) (subtypep (type-of object) 'spice)) all-contents))
                   (liquids (remove-if-not #'(lambda (object) (subtypep (type-of object) 'liquid)) all-contents))
                   (liquid-mixture (if (and (= (length spices) 0) (= (length liquids) 1))
                                     (first liquids)
                                     (create-homogeneous-mixture (append spices liquids) 'boiled)))
                   (main-content (set-difference all-contents (append spices liquids))))

              (setf (temperature stove-to-be-heated) (convert-to-temperature stove-to-be-heated heating-mode))

              ; we keep track of what the main content and liquids are boiled with
              (loop for boilable in main-content
                    do (setf (boiled boilable) t)
                       (setf (temperature boilable) (copy-object (temperature stove-to-be-heated)))
                       (setf (boiled-with boilable) liquid-mixture))

              (cond ((or (> (length spices) 0) (> (length liquids) 1))
                     (setf (is-liquid liquid-mixture) t)
                     (setf (temperature liquid-mixture) (copy-object (temperature stove-to-be-heated))))
                    (t
                     (setf (boiled (first liquids)) t)
                     (setf (temperature (first liquids)) (copy-object (temperature stove-to-be-heated)))))
              
              (setf (contents new-thing-to-boil) (append main-content (list liquid-mixture)))

              (bind (thing-boiled 1.0 new-thing-to-boil thing-boiled-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
           (t
            (bind (thing-boiled 1.0 (make-instance 'failed-object) thing-boiled-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

  
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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond (new-container
     
            (change-temperature new-container temperature)
         
            (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind (container-with-ingredients-at-temperature 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))
  
  ; Case 2: temperature is not given, room temperature (i.e. kitchen-state temperature is used)
  ((kitchen-state-in container-with-ingredients
                     => kitchen-state-out container-with-ingredients-at-temperature temperature-quantity temperature-unit)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (container-available-at (+ 800 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at (kitchen-time kitchen-state-in)))

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 

      (cond (new-container       
             ; change it to room temperature
             (change-temperature new-container (temperature new-kitchen-state))
                
             (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
                   (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                   (temperature-quantity 0.0 (quantity (temperature new-kitchen-state)) nil)
                   (temperature-unit 0.0 (unit (temperature new-kitchen-state)) nil)))
            (t
             (bind (container-with-ingredients-at-temperature 1.0 (make-instance 'failed-object) container-available-at)
                   (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                   (temperature-quantity 0.0 (quantity (temperature new-kitchen-state)) nil)
                   (temperature-unit 0.0 (unit (temperature new-kitchen-state)) nil)))))))


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
     
     (cond ((and new-cover (not (has-failed-objects object)))
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

     (cond ((not (has-failed-objects object cover))
            (setf (covered-with new-container) new-cover)
            (setf (is-covering new-cover) T)
            (setf (used new-cover) T)
            (setf (contents (counter-top new-kitchen-state)) (remove new-cover (contents (counter-top new-kitchen-state))))
         
            (bind
             (covered-object 1.0 new-container container-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind
             (covered-object 1.0 (make-instance 'failed-object) container-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))
;
;
;
;
;
(defprimitive cut ((cut-object transferable-container)
                   (kitchen-state-out kitchen-state)
                   (kitchen-state-in kitchen-state)
                   (object transferable-container)
                   (cut-pattern cutting-pattern)
                   (cutting-tool can-cut)
                   (cutting-surface can-be-cut-on))

  ;;Case 1: cutting tool not given (use a knife), cut-pattern given, cutting-surface not given
  ((kitchen-state-in object cut-pattern => cut-object kitchen-state-out cutting-tool cutting-surface)
   
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
     (let ((new-knife (retrieve-concept-instance-and-bring-to-countertop 'knife new-kitchen-state))
           (new-cutting-board (retrieve-concept-instance-and-bring-to-countertop 'cutting-board new-kitchen-state)))

       (cond ((and new-knife new-container new-cutting-board)
              ;; 2) cut everything in the container according to the cutting pattern
              (loop for item in (contents new-container)
                    do (setf (is-cut item) cut-pattern))
               
              (setf (used new-knife) t)
              (setf (used new-cutting-board) t)

              (bind (cut-object 1.0 new-container container-available-at)
                (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                (cutting-tool 0.0 new-knife container-available-at)
                (cutting-surface 0.0 new-cutting-board container-available-at)))
             (t
              (bind (cut-object 1.0 (make-instance 'failed-object) container-available-at)
                (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                (cutting-tool 0.0 (make-instance 'failed-object) container-available-at)
                (cutting-surface 0.0 (make-instance 'failed-object) container-available-at)))))))


   ;; case 3: cutting tool not given (use a knife), cut-pattern given, cutting-surface given
((kitchen-state-in object cut-pattern cutting-surface => cut-object kitchen-state-out cutting-tool)

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
     (let ((new-knife (retrieve-concept-instance-and-bring-to-countertop 'knife new-kitchen-state))
           (reused-cutting-board (find-object-by-persistent-id cutting-surface (counter-top new-kitchen-state))))

       (cond ((and new-knife new-container reused-cutting-board)
              ;; 2) cut everything in the container according to the cutting pattern
              (loop for item in (contents new-container)
                    do (setf (is-cut item) cut-pattern))

              (setf (used new-knife) t)
              (setf (used reused-cutting-board) t)

              (bind (cut-object 1.0 new-container container-available-at)
                (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                (cutting-tool 0.0 new-knife container-available-at)))
             (t
              (bind (cut-object 1.0 (make-instance 'failed-object) container-available-at)
                (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                (cutting-tool 0.0 (make-instance 'failed-object) container-available-at)))))))

  ;;Case 2: cutting tool given, cut-pattern given, cutting-surface not given
  ((kitchen-state-in object cut-pattern cutting-tool => cut-object kitchen-state-out cutting-surface)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id object new-kitchen-state))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id object) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at)
          (new-cutting-board (retrieve-concept-instance-and-bring-to-countertop 'cutting-board new-kitchen-state)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 

     (cond ((and new-container new-cutting-board)
            ;; 1) find cutting tool
            (let ((new-cutting-tool (find-object-by-persistent-id cutting-tool (counter-top new-kitchen-state))))

              ;; 2) cut everything in the container according to the cutting pattern
              (loop for item in (contents new-container)
                    do (setf (is-cut item) cut-pattern))

              (setf (used new-cutting-tool) t)
              (setf (used new-cutting-board) t)

              (bind (cut-object 1.0 new-container container-available-at)
                (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                (cutting-surface 1.0 new-cutting-board container-available-at))))
           (t
            (bind (cut-object 1.0 (make-instance 'failed-object) container-available-at)
              (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
              (cutting-surface 1.0 (make-instance 'failed-object) container-available-at))))))


   ;; case 4: cutting tool given, cut-pattern given, cutting surface given
   ((kitchen-state-in object cut-pattern cutting-tool cutting-surface => cut-object kitchen-state-out)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id object new-kitchen-state))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id object) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at)
          (reused-cutting-board (find-object-by-persistent-id cutting-surface (counter-top new-kitchen-state))))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((and new-container reused-cutting-board)
            ;; 1) find cutting tool
            (let ((new-cutting-tool (find-object-by-persistent-id cutting-tool (counter-top new-kitchen-state))))

              ;; 2) cut everything in the container according to the cutting pattern
              (loop for item in (contents new-container)
                    do (setf (is-cut item) cut-pattern))

              (setf (used new-cutting-tool) t)
              (setf (used reused-cutting-board) t)

              (bind (cut-object 1.0 new-container container-available-at)
                (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
           (t
            (bind (cut-object 1.0 (make-instance 'failed-object) container-available-at)
              (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))



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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
     
     (cond ((not (has-failed-objects eggs target-container))       
            (loop for egg in (contents new-eggs-with-shell)
                  do (loop for i from 1 to (value (quantity (amount egg)))
                           for egg-amount = (make-instance 'amount :quantity (make-instance 'quantity :value 50) :unit (make-instance 'g))
                           for whole-egg = (make-instance 'whole-egg :amount egg-amount)
                           for egg-shell = (make-instance 'egg-shell :cracked t)
                           do (setf (contents new-target-container) (append (contents new-target-container) (list whole-egg)))
                              (setf (contents new-eggs-with-shell) (append (contents new-eggs-with-shell) (list egg-shell)))
                           finally (setf (contents new-eggs-with-shell)
                                         (remove-if #'(lambda (i) (typep i 'egg)) (contents new-eggs-with-shell)))))
                
            (bind (container-with-cracked-eggs 1.0 new-target-container container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind (container-with-cracked-eggs 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))          

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

     (cond ((and new-target-container
                 (not (has-failed-objects eggs)))   
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
                 (dipped-object-available-at (+ (max (kitchen-time kitchen-state-in)
                                                     (available-at (find (id object) binding-objects
                                                                         :key #'(lambda (binding-object)
                                                                                  (and (value binding-object)
                                                                                       (id (value binding-object)))))))
                                                50))
                 (kitchen-state-available-at dipped-object-available-at))

            (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

            (cond ((not (has-failed-objects object dip-container))
                   (let* ((dip (first (contents new-dip-container)))
                          (total-dip-weight-in-grams (convert-to-g dip))
                          (dip-weight-per-portion (make-instance 'amount
                                                                 :quantity (make-instance 'quantity
                                                                                          :value (/ (value (quantity (amount total-dip-weight-in-grams)))
                                                                                                    (length (items new-items-to-dip))))
                                                                 :unit (make-instance 'g))))
            
                     (loop for portion in (items new-items-to-dip)
                           for dip = (copy-object (first (contents new-dip-container)))
                           do (setf (amount dip) dip-weight-per-portion)
                              (setf (dipped-in portion) dip))

                     (setf (contents new-dip-container) nil)

                     (bind (dipped-object 1.0 new-items-to-dip dipped-object-available-at)
                           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
                  (t
                   (bind (dipped-object 1.0 (make-instance 'failed-object) dipped-object-available-at)
                         (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))
         
         ((subtypep (type-of object) 'transferable-container)
          (let* ((new-kitchen-state (copy-object kitchen-state-in))
                 (new-input-container (find-object-by-persistent-id object (counter-top new-kitchen-state)))
                 (new-dip-container (find-object-by-persistent-id dip-container (counter-top new-kitchen-state)))
                 (dipped-object-available-at (+ (max (kitchen-time kitchen-state-in)
                                                     (available-at (find (id object) binding-objects
                                                                         :key #'(lambda (binding-object)
                                                                                  (and (value binding-object)
                                                                                       (id (value binding-object)))))))
                                                50))
                 (kitchen-state-available-at dipped-object-available-at))

            (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
          
            (cond ((not (has-failed-objects object dip-container))     
                   (let* ((dip (first (contents new-dip-container)))
                          (total-dip-weight-in-grams (convert-to-g dip))
                          (dip-weight-per-portion (make-instance 'amount
                                                                 :quantity (make-instance 'quantity
                                                                                          :value (/ (value (quantity (amount total-dip-weight-in-grams)))
                                                                                                    (length (contents new-input-container))))
                                                                 :unit (make-instance 'g))))

                     (loop for portion in (contents new-input-container)
                           for dip = (copy-object (first (contents new-dip-container)))
                           do (setf (amount dip) dip-weight-per-portion)
                              (setf (dipped-in portion) dip))

                     (setf (contents new-dip-container) nil)
                     (bind (dipped-object 1.0 new-input-container dipped-object-available-at)
                           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
                  (t
                   (bind (dipped-object 1.0 (make-instance 'failed-object) dipped-object-available-at)
                         (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))))


(defprimitive drain ((drained-ingredient transferable-container)
                     (liquid-rests transferable-container)
                     (kitchen-state-out kitchen-state)
                     (kitchen-state-in kitchen-state)
                     (container-with-ingredients transferable-container)
                     (draining-tool can-drain))
  
  ;; Case 1: draining tool not given, use a colander
  ((kitchen-state-in container-with-ingredients  => drained-ingredient liquid-rests kitchen-state-out draining-tool)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in) 
                                             (available-at (find (id container-with-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at)
          ;; 1) find draining tool
          (new-draining-tool (retrieve-concept-instance-and-bring-to-countertop 'colander new-kitchen-state))
          ;; 2) find container for liquids
          (new-liquids-container (retrieve-concept-instance-and-bring-to-countertop 'medium-bowl new-kitchen-state)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (cond ((and
             new-draining-tool
             new-liquids-container
             new-container)
            
            ;; 3) drain everything in the container
            (let ((liquids '())
                  (solids '()))
              (loop for item in (contents new-container)
                    do (cond ((or (subtypep (type-of item) 'liquid)
                                  (and (subtypep (type-of item) 'liquefiable) (is-liquid item)))
                              (push item liquids))
                             ((subtypep (type-of item) 'heterogeneous-mixture)
                                ; check if the heterogeneous-mixture should be split back up into solids and liquids
                              (if (find-if #'(lambda (component)
                                               (or (subtypep (type-of component) 'liquid)
                                                   (and (subtypep (type-of component) 'liquefiable) (is-liquid component))))
                                           (components item))
                                (loop for component in (components item)
                                      do (setf (value (quantity (amount component)))
                                               (* (value (quantity (amount component))) ; mixture components are expressed in percent
                                                  (value (quantity (amount item)))))
                                         (setf (unit (amount component))
                                               (unit (amount item)))
                                         (cond ((or (subtypep (type-of component) 'liquid)
                                                    (and (subtypep (type-of component) 'liquefiable) (is-liquid component)))
                                                (push component liquids))
                                               (t
                                                (push component solids))))))
                             (t
                              (push item solids))))

              (setf (contents new-liquids-container) liquids)
              (setf (contents new-container) solids)

              (setf (used new-draining-tool) t)
              (setf (used new-liquids-container) t)
     
              (bind (drained-ingredient 1.0 new-container container-available-at)
                    (liquid-rests 1.0 new-liquids-container container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (draining-tool 0.0 new-draining-tool container-available-at))))
           (t
            (bind (drained-ingredient 1.0 (make-instance 'failed-object) container-available-at)
                  (liquid-rests 1.0  (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (draining-tool 0.0  (make-instance 'failed-object) container-available-at))))))

  ;; Case 2: draining tool given
    ((kitchen-state-in container-with-ingredients draining-tool => drained-ingredient liquid-rests kitchen-state-out)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-ingredients (counter-top new-kitchen-state)))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in) 
                                             (available-at (find (id container-with-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at)
          ;; 1) find draining tool
          (new-draining-tool (find-object-by-persistent-id draining-tool (counter-top new-kitchen-state)))
          ;; 2) find container for liquids
          (new-liquids-container (retrieve-concept-instance-and-bring-to-countertop 'medium-bowl new-kitchen-state)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (cond ((and
             new-draining-tool
             new-liquids-container
             new-container)
            
            ;; 3) drain everything in the container
            (let ((liquids '())
                  (solids '()))
              (loop for item in (contents new-container)
                    do (cond ((or (subtypep (type-of item) 'liquid)
                                  (and (subtypep (type-of item) 'liquefiable) (is-liquid item)))
                              (push item liquids))
                             ((subtypep (type-of item) 'heterogeneous-mixture)
                                ; check if the heterogeneous-mixture should be split back up into solids and liquids
                              (if (find-if #'(lambda (component)
                                               (or (subtypep (type-of component) 'liquid)
                                                   (and (subtypep (type-of component) 'liquefiable) (is-liquid component))))
                                           (components item))
                                (loop for component in (components item)
                                      do (setf (value (quantity (amount component)))
                                               (* (value (quantity (amount component))) ; mixture components are expressed in percent
                                                  (value (quantity (amount item)))))
                                         (setf (unit (amount component))
                                               (unit (amount item)))
                                         (cond ((or (subtypep (type-of component) 'liquid)
                                                    (and (subtypep (type-of component) 'liquefiable) (is-liquid component)))
                                                (push component liquids))
                                               (t
                                                (push component solids))))))
                             (t
                              (push item solids))))

              (setf (contents new-liquids-container) liquids)
              (setf (contents new-container) solids)

              (setf (used new-draining-tool) t)
              (setf (used new-liquids-container) t)
     
              (bind (drained-ingredient 1.0 new-container container-available-at)
                    (liquid-rests 1.0 new-liquids-container container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
           (t
            (bind (drained-ingredient 1.0 (make-instance 'failed-object) container-available-at)
                  (liquid-rests 1.0 (make-instance 'failed-object) container-available-at)
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

  ; Case 1: target container is not given, a medium-bowl is used
              
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
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

  ; Case 2: target container is given         
  ;; Takes a specified amount of an ingredient from somewhere in the kitchen and places it in the given container
  ((kitchen-state-in ingredient-concept target-container quantity unit =>  kitchen-state-out container-with-ingredient)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (amount (make-instance 'amount :quantity quantity :unit unit))
          (container-available-at (+ 30 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at container-available-at)
          ;; 1) find target container
          (target-container-instance-new-ks (find-object-by-persistent-id target-container new-kitchen-state)))

     ;;  set kitchen time
     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     ;; 2) find ingredient and place it on the countertop
     (multiple-value-bind (ingredient-instance ingredient-original-location)
         (find-ingredient (type-of ingredient-concept) new-kitchen-state )

       (cond ((and ingredient-instance
                   (not (has-failed-objects target-container)))
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

                (bind (container-with-ingredient 1.0 weighed-ingredient-container container-available-at)
                      (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
             (t
              (unless ingredient-instance
                (print (format nil "No more ~a found in current kitchen state!!!" (type-of ingredient-concept))))
              (bind (container-with-ingredient 1.0 (make-instance 'failed-object) container-available-at)
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
     
     (cond ((and new-flatten-tool (not (has-failed-objects portions)))
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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (cond ((not (has-failed-objects portions can-flatten-tool))
     
            (loop for item in (items new-portions)
                  do (setf (flattened item) t))

            (setf (used new-flatten-tool) t)
     
            (bind (container-with-flattened-items 1.0 new-portions portions-available-at )
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind (container-with-flattened-items 1.0 (make-instance 'failed-object) portions-available-at )
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

        ;; 1) find ingredient to be used for flouring and bring it to the countertop
            (multiple-value-bind (original-flour flour-original-location)
                (find-ingredient 'all-purpose-flour kitchen-state-in)

              (cond ((and new-container original-flour)

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
         
                       (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                             (floured-container 1.0 new-container container-available-at)
                             (ingredient-to-flour-with 0.0 flour-container-for-flouring nil))))  
                    (t
                     (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                           (floured-container 1.0 (make-instance 'failed-object) container-available-at)
                           (ingredient-to-flour-with 0.0 (make-instance 'failed-object) nil)))))))

  ;; Case 2; ingredient-to-flour-with given
  ((kitchen-state-in container-to-flour ingredient-to-flour-with => kitchen-state-out floured-container )
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-to-flour new-kitchen-state))
          (new-flour-container (find-object-by-persistent-id ingredient-to-flour-with new-kitchen-state))
          (new-flour (if new-flour-container
                       (first (contents new-flour-container))
                       nil))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-to-flour) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))
     
     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((and new-container new-flour)
             
            ;; apply the flour to the inner surface of the container
            (setf (contents new-flour-container) nil)
            (setf (sprinkled-with new-container) new-flour)
            (setf (used new-container) t)

     
            (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (floured-container 1.0 new-container container-available-at)))
           (t
            (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (floured-container 1.0 (make-instance 'failed-object) container-available-at)))))))

(defprimitive fry ((thing-fried transferable-container)
                   (kitchen-state-out kitchen-state)
                   (kitchen-state-in kitchen-state)
                   (thing-to-fry transferable-container)
                   (stove-to-fry-on stove)
                   (heating-mode stove-mode)
                   (time-to-fry-quantity quantity)
                   (time-to-fry-unit unit))

  ;; Case 1: Stove, heating mode and frying time are not given (defaults to 30 minutes on medium heat)
  ((kitchen-state-in thing-to-fry
                     => kitchen-state-out thing-fried stove-to-fry-on heating-mode time-to-fry-quantity time-to-fry-unit)
 
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (stove-to-be-heated (stove new-kitchen-state))
          (new-thing-to-fry (find-object-by-persistent-id thing-to-fry new-kitchen-state))
          (new-time-to-fry-quantity (make-instance 'quantity :value 30)) ; fixed frying time of 30 minutes
          (new-time-to-fry-unit (make-instance 'minute))
          (new-heating-mode (make-instance 'medium-heat)) ; default to medium heat setting
          (thing-fried-available-at (+ (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id thing-to-fry) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))
                                       (* (value new-time-to-fry-quantity)
                                          (if (eq new-time-to-fry-unit 'hour)
                                            3600
                                            60))))
          (kitchen-state-available-at (+ (max (kitchen-time kitchen-state-in)
                                            (available-at (find (id thing-to-fry) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                         30)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((and new-thing-to-fry stove-to-be-heated (subtypep (type-of new-thing-to-fry) 'heatable-container)) ; the container should support being put on a stove

            (setf (temperature stove-to-be-heated) (convert-to-temperature stove-to-be-heated new-heating-mode))

              ; fry all contents
            (loop for item in (contents new-thing-to-fry)
                  when (typep item 'fryable)
                    do
                      (setf (temperature item) (copy-object (temperature stove-to-be-heated)))
                      (setf (fried item) t))
              
            (bind (thing-fried 1.0 new-thing-to-fry thing-fried-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (stove-to-fry-on 0.0 stove-to-be-heated thing-fried-available-at)
                  (time-to-fry-quantity 0.0 new-time-to-fry-quantity nil)
                  (time-to-fry-unit 0.0 new-time-to-fry-unit nil)
                  (heating-mode 0.0 new-heating-mode nil)))   
           (t
            (bind (thing-fried 1.0 (make-instance 'failed-object) thing-fried-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (stove-to-fry-on 0.0 (make-instance 'failed-object) thing-fried-available-at)
                  (time-to-fry-quantity 0.0 new-time-to-fry-quantity nil)
                  (time-to-fry-unit 0.0 new-time-to-fry-unit nil)
                  (heating-mode 0.0 new-heating-mode nil))))))  

    ;; Case 2: Stove and frying time are not given, but heating-mode is given
  ((kitchen-state-in thing-to-fry heating-mode
                     => kitchen-state-out thing-fried stove-to-fry-on time-to-fry-quantity time-to-fry-unit)
 
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (stove-to-be-heated (stove new-kitchen-state))
          (new-thing-to-fry (find-object-by-persistent-id thing-to-fry new-kitchen-state))
          (new-time-to-fry-quantity (make-instance 'quantity :value 30)) ; fixed frying time of 30 minutes
          (new-time-to-fry-unit (make-instance 'minute))
          (thing-fried-available-at (+ (max (kitchen-time kitchen-state-in)
                                            (available-at (find (id thing-to-fry) binding-objects
                                                                :key #'(lambda (binding-object)
                                                                         (and (value binding-object)
                                                                              (id (value binding-object)))))))
                                       (* (value new-time-to-fry-quantity)
                                          (if (eq new-time-to-fry-unit 'hour)
                                            3600
                                            60))))
          (kitchen-state-available-at (+ (max (kitchen-time kitchen-state-in)
                                              (available-at (find (id thing-to-fry) binding-objects
                                                                  :key #'(lambda (binding-object)
                                                                           (and (value binding-object)
                                                                                (id (value binding-object)))))))
                                         30)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((and new-thing-to-fry stove-to-be-heated (subtypep (type-of new-thing-to-fry) 'heatable-container)) ; the container should support being put on a stove

            (setf (temperature stove-to-be-heated) (convert-to-temperature stove-to-be-heated heating-mode))

              ; fry all contents
            (loop for item in (contents new-thing-to-fry)
                  when (typep item 'fryable)
                    do
                      (setf (temperature item) (copy-object (temperature stove-to-be-heated)))
                      (setf (fried item) t))
              
            (bind (thing-fried 1.0 new-thing-to-fry thing-fried-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (stove-to-fry-on 0.0 stove-to-be-heated thing-fried-available-at)
                  (time-to-fry-quantity 0.0 new-time-to-fry-quantity nil)
                  (time-to-fry-unit 0.0 new-time-to-fry-unit nil)))
           (t
            (bind (thing-fried 1.0 (make-instance 'failed-object) thing-fried-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (stove-to-fry-on 0.0 (make-instance 'failed-object) thing-fried-available-at)
                  (time-to-fry-quantity 0.0 new-time-to-fry-quantity nil)
                  (time-to-fry-unit 0.0 new-time-to-fry-unit nil)))))))

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
  ((kitchen-state-in container-to-grease => kitchen-state-out ingredient-to-grease-with greased-container)
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-to-grease new-kitchen-state))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-to-grease) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((not (has-failed-objects container-to-grease))

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
     
                (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                      (greased-container 1.0 new-container container-available-at)
                      (ingredient-to-grease-with 0.0 butter-container-for-greasing nil)))))
           (t
            (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (greased-container 1.0 (make-instance 'failed-object) container-available-at)
                  (ingredient-to-grease-with 0.0 (make-instance 'failed-object) nil))))))

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

     (cond ((not (has-failed-objects ingredient-to-grease-with container-to-grease ingredient-to-grease-with))
            
            ;; apply the grease to the inner surface of the container
            (setf (contents new-grease-container) nil)
            (setf (spread new-grease) t)
            (setf (brushed-with new-container) new-grease)
            (setf (used new-container) t)

            (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

            (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (greased-container 1.0 new-container container-available-at)))
           (t
            (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (greased-container 1.0 (make-instance 'failed-object) container-available-at)))))))


(defprimitive grind ((container-with-ground-contents transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (container-with-ingredients-to-be-ground transferable-container)
                    (grinding-tool can-grind))
  
  ;; Case 1: grinding-tool not given so a food-processor is used
  ((container-with-ingredients-to-be-ground kitchen-state-in
                                            => kitchen-state-out container-with-ground-contents grinding-tool)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-ingredients-to-be-ground new-kitchen-state))
          ;; find grinding-tool and place it on the countertop
          (new-grinding-tool (retrieve-concept-instance-and-bring-to-countertop 'food-processor new-kitchen-state))
          (container-available-at (+ 90 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-ingredients-to-be-ground) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

       (cond ((and new-container new-grinding-tool)
              ;; 3) transfer contents from source-container to empty target-container
              
              (loop for item in (contents new-container)
                    when (typep item 'grindable)
                      do (setf (ground item) t))
       
              (setf (used new-grinding-tool) t)

              (bind (container-with-ground-contents 1.0 new-container container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (grinding-tool 1.0 new-grinding-tool kitchen-state-available-at)))
             (t
              (bind (container-with-ground-contents 1.0 (make-instance 'failed-object) container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (grinding-tool 1.0 (make-instance 'failed-object) kitchen-state-available-at))))))

  ;; Case 2: grinding-tool is given
  ((container-with-ingredients-to-be-ground kitchen-state-in grinding-tool
                                            => kitchen-state-out container-with-ground-contents)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-ingredients-to-be-ground new-kitchen-state))
          (new-grinding-tool (find-object-by-persistent-id grinding-tool new-kitchen-state))
          (container-available-at (+ 90 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-ingredients-to-be-ground) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

       (cond ((and new-container new-grinding-tool)
              
              (loop for item in (contents new-container)
                    when (typep item 'grindable)
                      do (setf (ground item) t))

              (setf (used new-grinding-tool) t)

              (bind (container-with-ground-contents 1.0 new-container container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
             (t
              (bind (container-with-ground-contents 1.0 (make-instance 'failed-object) container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

(defprimitive leave-for-time ((container-with-ingredients-at-temperature transferable-container)
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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
     
     (cond (new-container

            (change-temperature new-container cooling-time)
   
            (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind (container-with-ingredients-at-temperature 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

(defprimitive line ((lined-thing t) ;; something lineable or a list-of-kitchen-entities
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (thing-to-be-lined t) ;; something lineable or a list-of-kitchen-entities where the elements are lineable
                    (lining t)) ;; something can-be-lined-with or a bowl with an ingredient that can-be-lined-with

  ;; Case 1; baking paper to line with is not given
  ((kitchen-state-in thing-to-be-lined => lining kitchen-state-out lined-thing)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (target-available-at (+ 150 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at target-available-at))

     (cond
      ;; thing to be lined is something lineable
      ;; take baking paper as default lining
      ((subtypep (class-of thing-to-be-lined) 'lineable)
       
       (let ((target-lineable
              (if (is-concept thing-to-be-lined)
                (retrieve-concept-instance-and-bring-to-countertop (type-of thing-to-be-lined) new-kitchen-state)
                (find-object-by-persistent-id thing-to-be-lined (counter-top new-kitchen-state)))))
         
         (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
         
         ;; find baking paper and bring it to the countertop
         (let ((target-lining-instance (retrieve-concept-instance-and-bring-to-countertop 'baking-paper new-kitchen-state)))
           (cond ((and target-lineable target-lining-instance)
                  
                  (setf (lined-with target-lineable) target-lining-instance) ;;do the lining
                  (setf (is-lining target-lining-instance) t)
                  
                  (setf (used target-lineable) t)
                  (setf (used target-lining-instance) t)
                  
                  (setf (contents (counter-top new-kitchen-state)) ;;remove the paper from the countertop
                        (remove target-lining-instance (contents (counter-top new-kitchen-state))))              
                  
                  (bind (lined-thing 1.0 target-lineable target-available-at)
                        (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                        (lining 0.0 target-lining-instance nil)))
                 (t
                  (bind (lined-thing 1.0 (make-instance 'failed-object) target-available-at)
                        (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                        (lining 0.0 (make-instance 'failed-object) nil)))))))

      ;; thing to be lined is a list of kitchen entities where every element
      ;; is lineable
      ;; take baking paper as default lining for each element
      ((and (subtypep (class-of thing-to-be-lined) 'list-of-kitchen-entities)
            (loop for elem in (items thing-to-be-lined)
                  always (subtypep (class-of elem) 'lineable)))

       (let ((target-lineable (find-kitchen-entities thing-to-be-lined (counter-top new-kitchen-state))))
         (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
         (let ((target-lining-instances
                (loop repeat (length (items thing-to-be-lined))
                      collect (retrieve-concept-instance-and-bring-to-countertop 'baking-paper new-kitchen-state))))
           (cond ((and target-lineable target-lining-instances)
                  (loop for elem in (items target-lineable)
                        for lining in target-lining-instances
                        do (setf (lined-with elem) lining)
                           (setf (is-lining lining) t)
                           (setf (used elem) t)
                           (setf (used lining) t))
                  (bind (lined-thing 1.0 target-lineable target-available-at)
                        (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                        (lining 1.0 target-lining-instances nil)))
                 (t (bind (lined-thing 1.0 (make-instance 'failed-object) target-available-at)
                          (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                          (lining 0.0 (make-instance 'failed-object) nil)))))))

      (t (bind (lined-thing 1.0 (make-instance 'failed-object) target-available-at)
               (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
               (lining 0.0 (make-instance 'failed-object) nil))))))
              

  ;; Case 2; thing to be lined and lining are given
  ((kitchen-state-in thing-to-be-lined lining => kitchen-state-out lined-thing)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (target-available-at (+ 150 (kitchen-time kitchen-state-in)))
          (kitchen-state-available-at target-available-at))

   (cond
    ;; thing to be lined is something lineable
    ;; and lining is something that can be lined with
    ((and (subtypep (class-of thing-to-be-lined) 'lineable)
          (subtypep (class-of lining) 'can-be-lined-with))
   
     (let* ((target-lineable (if (is-concept thing-to-be-lined)
                               (retrieve-concept-instance-and-bring-to-countertop (type-of thing-to-be-lined) new-kitchen-state)
                               (find-object-by-persistent-id thing-to-be-lined (counter-top new-kitchen-state))))
            (target-lining-instance (if (is-concept lining)
                                      nil ; will be set later, dependent on the concept's class
                                      (find-object-by-persistent-id lining new-kitchen-state))))
          
       (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
       
       (when target-lineable
         (cond ((and (subtypep (type-of thing-to-be-lined) 'muffin-tins)
                     (eql (type-of lining) 'paper-baking-cups))
                (let ((remaining-tins (value (number-of-tins thing-to-be-lined)))
                      (retrieved-paper-baking-cups (make-instance 'paper-baking-cups))
                      (current-cups '()))

                  (loop do
                       (setf current-cups
                             (retrieve-concept-instance-and-bring-to-countertop (type-of lining) new-kitchen-state))

                       (loop while (and (> remaining-tins 0) (items current-cups))
                             do
                               (push (first (items current-cups)) (items retrieved-paper-baking-cups))
                               (setf (items current-cups) (rest (items current-cups)))
                               (setf remaining-tins (- remaining-tins 1)))

                       (unless (items current-cups)
                         (setf (contents (counter-top new-kitchen-state))
                               (remove current-cups (contents (counter-top new-kitchen-state)))))
                     
                     while (and (> remaining-tins 0)
                                current-cups))
                
                  (setf target-lining-instance retrieved-paper-baking-cups)))
               (t
                (setf target-lining-instance (retrieve-concept-instance-and-bring-to-countertop 'baking-paper new-kitchen-state)))))
  
       (cond ((and target-lineable target-lining-instance
                   (if (eql (type-of target-lining-instance) 'paper-baking-cups) (items target-lining-instance) t))
                   
              (setf (lined-with target-lineable) target-lining-instance) ;;do the lining
              (setf (is-lining target-lining-instance) t)

              (setf (used target-lineable) t)
              (setf (used target-lining-instance) t)

              (when (eql (type-of target-lining-instance) 'paper-baking-cups)
                (loop for cup in (items target-lining-instance)
                      do (setf (used cup) t)))

              (setf (contents (counter-top new-kitchen-state)) ;;remove the paper from the countertop
                    (remove target-lining-instance (contents (counter-top new-kitchen-state))))

              (bind (lined-thing 1.0 target-lineable target-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
             
             (t
              (bind (lined-thing 1.0 (make-instance 'failed-object) target-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))

    ;; thing to be lined is a list of kitchen entities where
    ;; each element is lineable
    ;; and lining is a transferable container with contents
    ;; that can be used for lining
    ((and (subtypep (class-of thing-to-be-lined) 'list-of-kitchen-entities)
          (subtypep (class-of lining) 'transferable-container)
          (loop for elem in (items thing-to-be-lined)
                always (subtypep (class-of elem) 'lineable))
          (loop for elem in (contents lining)
                always (subtypep (class-of elem) 'can-be-lined-with)))

     (let ((target-lineable (find-kitchen-entities thing-to-be-lined (counter-top new-kitchen-state)))
           (target-lining (find-object-by-persistent-id lining new-kitchen-state)))
       (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
       (loop for elem in (items target-lineable)
             for lining in (loop repeat (length (items target-lineable))
                                 collect (copy-object (first (contents target-lining))))
             do (setf (lined-with elem) lining)
                (setf (is-lining lining) t)
                (setf (used elem) t))
       (bind (lined-thing 1.0 target-lineable target-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
     
    (t
     (bind (lined-thing 1.0 (make-instance 'failed-object) target-available-at)
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
     
     (cond ((and mashing-tool (not (has-failed-objects input-ingredient)))

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

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
      
      (cond ((and mashing-tool (not (has-failed-objects input-ingredient mashing-tool)))
      
             (loop for item in (contents new-ingredient)
                   do (setf (mashed item) t))

             (setf (used new-mashing-tool) t)

             (bind (mashed-ingredient 1.0 new-ingredient container-available-at)
                   (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
            (t
             (bind (mashed-ingredient 1.0 (make-instance 'failed-object) container-available-at)
                   (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

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

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
      (cond ((not (has-failed-objects container-with-input-ingredients))
            
             (loop for ingredient in (contents new-container)
                   when (typep ingredient 'meltable)
                     do (setf (melted ingredient) t))
     
             (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                   (container-with-melted-ingredients 1.0 new-container container-available-at)
                   (melting-tool 0.0 (microwave new-kitchen-state) container-available-at)))
            (t
             (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                   (container-with-melted-ingredients 1.0 (make-instance 'failed-object) container-available-at)
                   (melting-tool 0.0 (microwave new-kitchen-state) container-available-at))))))

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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((not (has-failed-objects container-with-input-ingredients melting-tool))
            (loop for ingredient in (contents new-container)
                  when (typep ingredient 'meltable)
                    do (setf (melted ingredient) t))

            (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (container-with-melted-ingredients 1.0 new-container container-available-at)))
           (t
            (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (container-with-melted-ingredients 1.0 (make-instance 'failed-object) container-available-at)))))))

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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((and new-mingling-tool new-mingling-tool)

            ;; mingle contents in container with ingredients
            (let ((mixture (create-heterogeneous-mixture-in-container new-container-with-ingredients-to-mix)))
            
              (setf (used new-mingling-tool) t)
              (setf (contents new-container-with-ingredients-to-mix) (list mixture)))

            (bind (container-with-mixture 1.0 new-container-with-ingredients-to-mix container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind (container-with-mixture 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

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

         (cond ((and target-whisk-instance container-with-input-ingredients-instance)

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
  ((kitchen-state-in container-with-input-ingredients mixing-tool => kitchen-state-out container-with-mixture)
   
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container-with-ingredients-to-mix (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state)))
          (new-mixing-tool (find-object-by-persistent-id mixing-tool new-kitchen-state))
          (container-available-at (+ 30 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-input-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                                   (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (cond ((and new-mixing-tool new-container-with-ingredients-to-mix)
     
            ;; mix contents in container with ingredients
            (let ((mixture (create-homogeneous-mixture-in-container new-container-with-ingredients-to-mix)))
            
              (setf (used new-mixing-tool) t)
              (setf (mixed mixture) t)
              (setf (contents new-container-with-ingredients-to-mix) (list mixture)))

            (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
         
            (bind (container-with-mixture 1.0 new-container-with-ingredients-to-mix container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind (container-with-mixture 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))


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
             new-peel-container
             (not (has-failed-objects input-ingredient)))
            
            ;; 3) peel everything in the container
            (let ((peeled-ingredients '()))
              (loop for item in (contents new-ingredient)
                    do (cond ((and (subtypep (type-of item) 'egg) 
                                   (boiled item)) ; peeling a hard boiled egg is a special case of peeling
                              (push (make-instance 'hard-boiled-egg) peeled-ingredients)
                              (push (make-instance 'egg-shell :cracked t) (contents new-peel-container)))
                             (t
                              (setf (peeled item) t)
                              (push item peeled-ingredients)
                              (push (make-instance 'peel :peel-of item) (contents new-peel-container)))))
              (setf (contents new-ingredient) peeled-ingredients))

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

     (cond ((and
             new-peeling-tool
             new-peel-container
             (not (has-failed-objects input-ingredient)))
            
            ;; 3) peel everything in the container
            (let ((peeled-ingredients '()))
              (loop for item in (contents new-ingredient)
                    do (cond ((and (subtypep (type-of item) 'egg) 
                                   (boiled item)) ; peeling a hard boiled egg is a special case of peeling
                              (push (make-instance 'hard-boiled-egg) peeled-ingredients)
                              (push (make-instance 'egg-shell :cracked t) (contents new-peel-container)))
                             (t
                              (setf (peeled item) t)
                              (push item peeled-ingredients)
                              (push (make-instance 'peel :peel-of item) (contents new-peel-container)))))
              (setf (contents new-ingredient) peeled-ingredients))
     
            (setf (used new-peeling-tool) t)
            (setf (used new-peel-container) t)
     
            (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
            (bind (peeled-ingredient 1.0 new-ingredient container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (peel-of-ingredient 0.0 new-peel-container container-available-at)))
           (t
            (bind (peeled-ingredient 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (peel-of-ingredient 0.0 (make-instance 'failed-object) container-available-at)))))))

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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((not (has-failed-objects container-with-dough))
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
                                                                      :unit (make-instance 'g))
                                  (contents countertop) (cons new-portion (contents countertop))
                                  left-to-transfer 0)
                    finally 
                      (setf (contents container-with-dough-instance) nil)
                      (setf (arrangement countertop) default-arrangement-pattern)) 

              (bind (portions 1.0 portions portions-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (arrangement-pattern 0.0 default-arrangement-pattern)
                    (destination 0.0 source-destination))))
           (t
            (bind (portions 1.0 (make-instance 'failed-object) portions-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (arrangement-pattern 0.0 default-arrangement-pattern)
                  (destination 0.0 (make-instance 'failed-object)))))))

   ;; Case 2: Arrangement pattern specified but destination not specified, use countertop
   ((kitchen-state-in container-with-dough quantity unit arrangement-pattern
                      => portions kitchen-state-out destination) 
   
    (let* ((source-destination (counter-top kitchen-state-in))
           (new-kitchen-state (copy-object kitchen-state-in))
           (container-with-dough-instance (find-object-by-persistent-id container-with-dough (counter-top new-kitchen-state)))
           (portions-available-at (+ 80 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-dough) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
           (kitchen-state-available-at portions-available-at))

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

      (cond (container-with-dough-instance
             
             ;; portion contents from container and put them on the counter top
             (let* ((dough (first (contents container-with-dough-instance)))
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
                                                                       :unit (make-instance 'g))
                                   (contents countertop) (cons new-portion (contents countertop))
                                   left-to-transfer 0)
                     finally 
                       (setf (contents container-with-dough-instance) nil)
                       (setf (arrangement countertop) arrangement-pattern)) 

               (bind (portions 1.0 portions portions-available-at)
                     (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                     (destination 0.0 source-destination))))
            (t
             (bind (portions 1.0 (make-instance 'failed-object) portions-available-at)
                   (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                   (destination 0.0 (make-instance 'failed-object)))))))

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

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

      (cond ((not (has-failed-objects container-with-dough))
     
             ;; portion contents from container and put them on the counter top
             (let* ((container-with-dough-instance
                     (find-object-by-persistent-id container-with-dough (counter-top new-kitchen-state)))
                    (dough (first (contents container-with-dough-instance)))
                    (value-to-transfer (value (quantity (amount dough))))
                    (portion-amount (make-instance 'amount :quantity quantity :unit unit))
                    (left-to-transfer (copy-object value-to-transfer)))

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
                                                                       :unit (make-instance 'g))
                                   (contents new-destination) (cons new-portion (contents new-destination))
                                   left-to-transfer 0)
                     finally 
                       (setf (contents container-with-dough-instance) nil)
                       (setf (arrangement new-destination) default-arrangement-pattern)) 

               (bind (portions 1.0 new-destination portions-available-at)
                     (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                     (arrangement-pattern 0.0 default-arrangement-pattern))))
            (t
             (bind (portions 1.0 (make-instance 'failed-object) portions-available-at)
                   (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                   (arrangement-pattern 0.0 default-arrangement-pattern))))))
  
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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
     
     (cond ((not (has-failed-objects container-with-dough))

     ;; portion contents from container and put them on the counter top
     (let* ((container-with-dough-instance
              (find-object-by-persistent-id container-with-dough (counter-top new-kitchen-state)))
            (dough (first (contents container-with-dough-instance)))
            (value-to-transfer (value (quantity (amount dough))))
            (portion-amount (make-instance 'amount :quantity quantity :unit unit))
            (left-to-transfer (copy-object value-to-transfer)))

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
                                                               :unit (make-instance 'g))
                           (contents new-destination) (cons new-portion (contents new-destination))
                           left-to-transfer 0)
             finally 
               (setf (contents container-with-dough-instance) nil)
               (setf (arrangement new-destination) arrangement-pattern))    

       (bind (portions 1.0 new-destination portions-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
           (t
            (bind (portions 1.0 (make-instance 'failed-object) portions-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))


   ;; Case 5: Destination container specified but quantity, unit and arrangement pattern not specified
   ((kitchen-state-in container-with-dough destination  
                      => arrangement-pattern quantity unit portions kitchen-state-out)
   
    (let* ((new-kitchen-state (copy-object kitchen-state-in))
           (new-destination (find-object-by-persistent-id destination new-kitchen-state))
           (container-with-dough-instance (find-object-by-persistent-id container-with-dough (counter-top new-kitchen-state)))
           (default-arrangement-pattern (make-instance 'evenly-spread))
           (portions-available-at (+ 80 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-dough) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
           (kitchen-state-available-at portions-available-at))

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

      (cond ((and container-with-dough-instance
                  new-destination
                  (subtypep (type-of new-destination) 'muffin-tins)) ; only muffin-tins are supported in case of no quantity and unit being specified
     
             ;; portion contents from container and put them on the counter top
             (let* ((dough (first (contents container-with-dough-instance)))
                    (value-to-transfer (value (quantity (amount dough))))
                    (portion-amount (make-instance 'amount
                                                   :unit (copy-object (unit (amount dough)))
                                                   :quantity (make-instance 'quantity
                                                                            :value (/ value-to-transfer
                                                                                      (value (number-of-tins new-destination))))))
                    (number-of-tins-left (value (number-of-tins new-destination))))
       
               (loop while (> number-of-tins-left 0)
                     for new-portion = (copy-object dough)
                     do (setf (amount new-portion) portion-amount
                              (contents new-destination) (cons new-portion (contents new-destination))
                              number-of-tins-left (- number-of-tins-left 1))
                     finally 
                       (setf (contents container-with-dough-instance) nil)
                       (setf (arrangement new-destination) default-arrangement-pattern)) 

               (bind (portions 1.0 new-destination portions-available-at)
                     (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                     (arrangement-pattern 0.0 default-arrangement-pattern)
                     (quantity 0.0 (quantity portion-amount) nil)
                     (unit 0.0 (unit portion-amount) nil))))
            (t
             (bind (portions 1.0 (make-instance 'failed-object) portions-available-at)
                   (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                   (arrangement-pattern 0.0 default-arrangement-pattern)
                   (quantity 0.0 (make-instance 'failed-object) nil)
                   (unit 0.0 (make-instance 'failed-object) nil)))))))
  

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
          (target-temperature (make-instance 'amount :quantity quantity :unit unit)))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond (new-oven
      
            (setf (temperature new-oven) target-temperature)

            (bind (preheated-oven 1.0 new-oven oven-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind (preheated-oven 1.0 (make-instance 'failed-object) oven-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))
  

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

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
     
     (cond (new-container
     
            (change-temperature new-container (temperature (fridge kitchen-state-in)))    
                
            (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (refrigerator 0.0 (fridge kitchen-state-in) kitchen-state-available-at)
                  (cooling-quantity 0.0 new-cooling-quantity nil)
                  (cooling-unit 0.0 new-cooling-unit nil)))
           (t
            (bind (container-with-ingredients-at-temperature 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (refrigerator 0.0 (fridge kitchen-state-in) kitchen-state-available-at)
                  (cooling-quantity 0.0 new-cooling-quantity nil)
                  (cooling-unit 0.0 new-cooling-unit nil))))))
  
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

      (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 

      (cond (new-container
             (change-temperature new-container (temperature (fridge kitchen-state-in)))

             (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
                
             (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
                   (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                   (refrigerator 0.0 (fridge kitchen-state-in) kitchen-state-available-at)))
            (t
             (bind (container-with-ingredients-at-temperature 1.0 (make-instance 'failed-object) container-available-at)
                   (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                   (refrigerator 0.0 (fridge kitchen-state-in) kitchen-state-available-at))))))

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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
     
     (cond ((and new-container new-refrigerator)
     
            (change-temperature new-container (temperature new-refrigerator))
                
            (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind (container-with-ingredients-at-temperature 1.0 new-container container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

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
             new-seed-container
             (not (has-failed-objects input-ingredient)))
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

     (cond ((and
             new-seeding-tool
             new-seed-container
             (not (has-failed-objects input-ingredient)))
     
            ;; 3) seed everything in the container
            (loop for item in (contents new-ingredient)
                  do (setf (seeded item) t)
                     (push (make-instance 'seed :seed-of item) (contents new-seed-container)))

            (setf (used new-seeding-tool) t)
            (setf (used new-seed-container) t)
     
            (bind (seeded-ingredient 1.0 new-ingredient container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (seed-of-ingredient 0.0 new-seed-container container-available-at)))
           (t
            (bind (seeded-ingredient 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (seed-of-ingredient 0.0 (make-instance 'failed-object) container-available-at)))))))

(defprimitive separate-eggs ((container-with-egg-yolks transferable-container)
                             (container-with-egg-whites transferable-container) 
                             (kitchen-state-out kitchen-state)
                             (kitchen-state-in kitchen-state)
                             (eggs transferable-container) ;;eggs in bowl
                             (target-container-for-yolks transferable-container)
                             (target-container-for-whites transferable-container)
                             (egg-separator can-separate-eggs))
  
  ;; Case 1: target containers and egg separator given
  ((kitchen-state-in eggs target-container-for-yolks target-container-for-whites egg-separator => kitchen-state-out container-with-egg-yolks container-with-egg-whites)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-whole-eggs (find-object-by-persistent-id eggs new-kitchen-state))
          (new-white-container (find-object-by-persistent-id target-container-for-whites new-kitchen-state))
          (new-yolk-container (find-object-by-persistent-id target-container-for-yolks new-kitchen-state))
          (new-egg-separator (find-object-by-persistent-id egg-separator (counter-top new-kitchen-state)))
          (container-available-at (+ (kitchen-time kitchen-state-in)
                                     (* 5 (length (contents eggs)))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
     
     (cond ((and new-whole-eggs new-white-container new-yolk-container new-egg-separator)          
            (loop for egg in (contents new-whole-eggs)
                  for egg-yolk-amount = (make-instance 'amount
                                                       :quantity (make-instance 'quantity
                                                                                :value (* 0.36 (value (quantity (amount egg)))))
                                                       :unit (copy-object (unit (amount egg))))
                  for egg-white-amount = (make-instance 'amount
                                                        :quantity (make-instance 'quantity
                                                                                 :value (* 0.64 (value (quantity (amount egg)))))
                                                        :unit (copy-object (unit (amount egg)))) 
                  for egg-yolk = (make-instance 'egg-yolk :amount egg-yolk-amount)
                  for egg-white = (make-instance 'egg-white :amount egg-white-amount)
                  do (setf (contents new-yolk-container) (append (contents new-yolk-container) (list egg-yolk)))
                     (setf (contents new-white-container) (append (contents new-white-container) (list egg-white)))
                  finally (setf (contents new-whole-eggs)
                                (remove-if #'(lambda (i) (typep i 'whole-egg)) (contents new-whole-eggs))))

            (setf (used new-yolk-container) t)
            (setf (used new-white-container) t)
            (setf (used new-egg-separator) t)
                
            (bind (container-with-egg-yolks 1.0 new-yolk-container container-available-at)
                  (container-with-egg-whites 1.0 new-white-container container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind (container-with-egg-yolks 1.0 (make-instance 'failed-object) container-available-at)
                  (container-with-egg-whites 1.0 (make-instance 'failed-object) container-available-at)
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))
  
  ;; Case 2: target containers and egg separator not given
  ((kitchen-state-in eggs => kitchen-state-out container-with-egg-yolks container-with-egg-whites target-container-for-yolks target-container-for-whites egg-separator)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-whole-eggs (find-object-by-persistent-id eggs new-kitchen-state))
          (new-white-container (retrieve-concept-instance-and-bring-to-countertop 'medium-bowl new-kitchen-state))
          (white-target-container (if new-white-container
                                    (find-object-by-persistent-id new-white-container kitchen-state-in)
                                    nil))
          (new-yolk-container (retrieve-concept-instance-and-bring-to-countertop 'medium-bowl new-kitchen-state (list new-white-container)))
          (yolk-target-container (if new-yolk-container
                                   (find-object-by-persistent-id new-yolk-container kitchen-state-in)
                                   nil))
          (new-egg-separator (retrieve-concept-instance-and-bring-to-countertop 'egg-separator new-kitchen-state))
          (container-available-at (+ (kitchen-time kitchen-state-in)
                                     (* 5 (length (contents eggs)))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at) 
     
     (cond ((and new-whole-eggs new-white-container new-yolk-container new-egg-separator)          
            (loop for egg in (contents new-whole-eggs)
                  for egg-yolk-amount = (make-instance 'amount
                                                       :quantity (make-instance 'quantity
                                                                                :value (* 0.36 (value (quantity (amount egg)))))
                                                       :unit (copy-object (unit (amount egg))))
                  for egg-white-amount = (make-instance 'amount
                                                        :quantity (make-instance 'quantity
                                                                                 :value (* 0.64 (value (quantity (amount egg)))))
                                                        :unit (copy-object (unit (amount egg)))) 
                  for egg-yolk = (make-instance 'egg-yolk :amount egg-yolk-amount)
                  for egg-white = (make-instance 'egg-white :amount egg-white-amount)
                  do (setf (contents new-yolk-container) (append (contents new-yolk-container) (list egg-yolk)))
                     (setf (contents new-white-container) (append (contents new-white-container) (list egg-white)))
                  finally (setf (contents new-whole-eggs)
                                (remove-if #'(lambda (i) (typep i 'whole-egg)) (contents new-whole-eggs))))

            (setf (used new-yolk-container) t)
            (setf (used new-white-container) t)
            (setf (used new-egg-separator) t)
                
            (bind
             (container-with-egg-yolks 1.0 new-yolk-container container-available-at)
             (container-with-egg-whites 1.0 new-white-container container-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
             (target-container-for-yolks 0.0 yolk-target-container container-available-at)
             (target-container-for-whites 0.0 white-target-container container-available-at)
             (egg-separator 0.0 new-egg-separator container-available-at) ))
           (t
            (bind
             (container-with-egg-yolks 1.0 (make-instance 'failed-object) container-available-at)
             (container-with-egg-whites 1.0 (make-instance 'failed-object) container-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
             (target-container-for-yolks 0.0 (make-instance 'failed-object) container-available-at)
             (target-container-for-whites 0.0 (make-instance 'failed-object) container-available-at)
             (egg-separator 0.0 (make-instance 'failed-object) container-available-at)))))))

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
     
     (cond ((and
             new-container-with-input-ingredients
             (subtypep (type-of new-container-with-input-ingredients) 'coverable-container)
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

            (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

            (cond (new-portions 
                   (loop for item in (items new-portions)
                         do (setf (current-shape item) shape))

                   (bind (shaped-portions 1.0 new-portions portions-available-at)
                         (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
                  (t
                   (bind (shaped-portions 1.0 (make-instance 'failed-object) portions-available-at)
                         (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))              
         
         ((subtypep (type-of portions) 'transferable-container)
          (let* ((new-kitchen-state (copy-object kitchen-state-in))
                 (new-portions (find-object-by-persistent-id portions (counter-top new-kitchen-state)))
                 (portions-available-at (+ 85 (max (kitchen-time kitchen-state-in)
                                                   (available-at (find (id portions) binding-objects
                                                                       :key #'(lambda (binding-object)
                                                                                (and (value binding-object)
                                                                                     (id (value binding-object)))))))))
                 (kitchen-state-available-at portions-available-at))

            (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

            (cond (new-portions
                   (loop for item in (contents new-portions)
                         do (setf (current-shape item) shape))

                   (bind (shaped-portions 1.0 new-portions portions-available-at)
                         (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
                  (t
                   (bind (shaped-portions 1.0 (make-instance 'failed-object) portions-available-at)
                         (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))))

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
            (target-container-in-kitchen-input-state (if new-target-container
                                                       (find-object-by-persistent-id new-target-container kitchen-state-in)
                                                       nil))
            (new-sift (retrieve-concept-instance-and-bring-to-countertop 'sift new-kitchen-state)))

       (cond ((and new-source-container new-target-container new-sift)
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

       (cond ((and new-sift new-target-container new-source-container)
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

  ;; Case 3: target container not given, sift is given
  ((container-with-ingredients-to-be-sifted kitchen-state-in sifting-tool
                                            => target-container kitchen-state-out container-with-sifted-contents)

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
            (target-container-in-kitchen-input-state (if new-target-container
                                                       (find-object-by-persistent-id new-target-container kitchen-state-in)
                                                       nil))
            (new-sift (find-object-by-persistent-id sifting-tool new-kitchen-state)))

       (cond ((and new-source-container new-target-container new-sift)
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
                    (target-container 0.0 target-container-in-kitchen-input-state nil)))
             (t
              (bind (container-with-sifted-contents 1.0 (make-instance 'failed-object) container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                    (target-container 0.0 target-container-in-kitchen-input-state nil)))))))
  
  ;; Case 4: target container is given, sift is given
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

     (cond ((and new-sift new-target-container new-source-container)
     
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
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind (container-with-sifted-contents 1.0 (make-instance 'failed-object) container-available-at )
                  (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

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

     (cond ((and new-container-with-things-spread new-spread-container new-spreading-tool)
     
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
                      (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))
           (t
            (bind (container-with-objects-that-have-been-spread 1.0 (make-instance 'failed-object) container-available-at)
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

     (cond ((and new-container-with-things-spread new-spread-container spreading-tool)
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

(defprimitive sprinkle ((sprinkled-object t) ; transferable container or list-of-kitchen-entities
                        (kitchen-state-out kitchen-state)
                        (kitchen-state-in kitchen-state)
                        (object t) ; transferable container or list-of-kitchen-entities
                        (topping-container transferable-container))
  
  ((kitchen-state-in object topping-container
                     => kitchen-state-out sprinkled-object)

   (cond ((subtypep (type-of object) 'transferable-container)

          (let* ((new-kitchen-state (copy-object kitchen-state-in))
                 (new-input-container (find-object-by-persistent-id object (counter-top new-kitchen-state)))
                 (new-topping-container (find-object-by-persistent-id topping-container (counter-top new-kitchen-state)))
                 (sprinkled-object-available-at (+ (max (kitchen-time kitchen-state-in)
                                                        (available-at (find (id object) binding-objects
                                                                            :key #'(lambda (binding-object)
                                                                                     (and (value binding-object)
                                                                                          (id (value binding-object)))))))
                                                   50))
                 (kitchen-state-available-at sprinkled-object-available-at))

            (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
            
            (cond ((and new-input-container new-topping-container)
                   (let* ((topping (first (contents new-topping-container)))
                          (total-topping-weight-in-grams (convert-to-g topping))
                          (topping-weight-per-portion (make-instance 'amount
                                                                     :quantity (make-instance 'quantity
                                                                                              :value (/ (value (quantity (amount total-topping-weight-in-grams)))
                                                                                                        (length (contents new-input-container))))
                                                                     :unit (make-instance 'g))))

                     (loop for portion in (contents new-input-container)
                           for topping = (copy-object (first (contents new-topping-container)))
                           do (setf (amount topping) topping-weight-per-portion)
                              (setf (sprinkled-with portion) topping))
     
                     (setf (contents new-topping-container) nil)     
     
                     (bind (sprinkled-object 1.0 new-input-container sprinkled-object-available-at)
                           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
                  (t
                   (bind (sprinkled-object 1.0 (make-instance 'failed-object) sprinkled-object-available-at)
                         (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))
           
           ((subtypep (type-of object) 'list-of-kitchen-entities)

            (let* ((new-kitchen-state (copy-object kitchen-state-in))
                   (new-input-items (find-kitchen-entities object (counter-top new-kitchen-state)))
                   (new-topping-container (find-object-by-persistent-id topping-container (counter-top new-kitchen-state)))
                   (sprinkled-object-available-at (+ (max (kitchen-time kitchen-state-in)
                                                          (available-at (find (id object) binding-objects
                                                                              :key #'(lambda (binding-object)
                                                                                       (and (value binding-object)
                                                                                            (id (value binding-object)))))))
                                                     50))
                   (kitchen-state-available-at sprinkled-object-available-at))

              (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
            
              (cond ((and new-input-items new-topping-container)
                     (let* ((topping (first (contents new-topping-container)))
                          (total-topping-weight-in-grams (convert-to-g topping))
                          (topping-weight-per-portion (make-instance 'amount
                                                                     :quantity (make-instance 'quantity
                                                                                              :value (/ (value (quantity (amount total-topping-weight-in-grams)))
                                                                                                        (length (items new-input-items))))
                                                                     :unit (make-instance 'g))))

                     (loop for portion in (items new-input-items)
                           for topping = (copy-object (first (contents new-topping-container)))
                           do (setf (amount topping) topping-weight-per-portion)
                              (setf (sprinkled-with portion) topping))
     
                     (setf (contents new-topping-container) nil)     
     
                     (bind (sprinkled-object 1.0 new-input-items sprinkled-object-available-at)
                           (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
                  (t
                   (bind (sprinkled-object 1.0 (make-instance 'failed-object) sprinkled-object-available-at)
                         (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))))


(defprimitive top-with ((topped-object t) ; transferable container or list-of-kitchen-entities
                        (kitchen-state-out kitchen-state)
                        (kitchen-state-in kitchen-state)
                        (thing-to-be-topped t) ; transferable-container or list-of-kitchen-entities
                        (container-with-topping transferable-container)
                        (topping-quantity quantity)
                        (topping-unit unit))

  ;; case 1; topping-quantity and -unit are not given
  ;; determine quantity by dividing the container-with-topping
  ;; over the amount of items in thing-to-be-topped and take
  ;; the same unit as the container-with-topping
  ((kitchen-state-in thing-to-be-topped container-with-topping =>
                     topped-object kitchen-state-out topping-quantity topping-unit)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (topped-object-available-at
           (+ (max (kitchen-time kitchen-state-in)
                   (available-at (find (id thing-to-be-topped) binding-objects
                                       :key #'(lambda (binding-object)
                                                (and (value binding-object)
                                                     (id (value binding-object)))))))
              50))
          (kitchen-state-available-at topped-object-available-at))

     (if (or (and (subtypep (type-of thing-to-be-topped) 'transferable-container)
                  (loop for contents in (contents thing-to-be-topped)
                        always (subtypep (type-of contents) 'can-have-on-top)))
             (and (subtypep (type-of thing-to-be-topped) 'list-of-kitchen-entities)
                  (loop for elem in (items thing-to-be-topped)
                        always (subtypep (type-of elem) 'can-have-on-top))))
       
       (let ((new-thing-to-be-topped
              (typecase thing-to-be-topped
                (list-of-kitchen-entities (find-kitchen-entities thing-to-be-topped (counter-top new-kitchen-state)))
                (transferable-container (find-object-by-persistent-id thing-to-be-topped (counter-top new-kitchen-state)))))
             (new-topping-container (find-object-by-persistent-id container-with-topping (counter-top new-kitchen-state))))
         (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
         (cond ((and new-thing-to-be-topped new-topping-container)
                (let* ((topping (first (contents new-topping-container)))
                       (total-topping-weight-in-grams (convert-to-g topping))
                       (amount-of-topping-portions
                        (typecase new-thing-to-be-topped
                          (transferable-container (length (contents new-thing-to-be-topped)))
                          (list-of-kitchen-entities (length (items new-thing-to-be-topped)))))
                       (topping-weight-per-portion
                        (/ (value (quantity (amount total-topping-weight-in-grams)))
                           amount-of-topping-portions))
                       (topping-amount-per-portion
                        (make-instance 'amount
                                       :quantity (make-instance 'quantity :value topping-weight-per-portion)
                                       :unit (make-instance 'g))))
                  
                  (loop with portions
                          = (typecase new-thing-to-be-topped
                              (transferable-container (contents new-thing-to-be-topped))
                              (list-of-kitchen-entities (items new-thing-to-be-topped)))
                        for portion in portions
                        for topping = (copy-object (first (contents new-topping-container)))
                        do (setf (amount topping) topping-amount-per-portion)
                           (setf (has-on-top portion) topping))
                  
                  (setf (contents new-topping-container) nil)     
                  
                  (bind (topped-object 1.0 new-thing-to-be-topped topped-object-available-at)
                        (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                        (topping-quantity 1.0 (quantity topping-amount-per-portion))
                        (topping-unit 1.0 (unit topping-amount-per-portion)))))))
       
       (bind (topped-object 1.0 (make-instance 'failed-object) topped-object-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
             (topping-quantity 1.0 (make-instance 'failed-object) topped-object-available-at)
             (topping-unit 1.0 (make-instance 'failed-object) topped-object-available-at)))))
   
  ;; case 2; topping quantity and unit are given
  ((kitchen-state-in thing-to-be-topped container-with-topping topping-quantity topping-unit =>
                     topped-object kitchen-state-out)

   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (topped-object-available-at
           (+ (max (kitchen-time kitchen-state-in)
                   (available-at (find (id thing-to-be-topped) binding-objects
                                       :key #'(lambda (binding-object)
                                                (and (value binding-object)
                                                     (id (value binding-object)))))))
              50))
          (kitchen-state-available-at topped-object-available-at))
     
     (if (or (and (subtypep (type-of thing-to-be-topped) 'transferable-container)
                  (loop for contents in (contents thing-to-be-topped)
                        always (subtypep (type-of contents) 'can-have-on-top)))
             (and (subtypep (type-of thing-to-be-topped) 'list-of-kitchen-entities)
                  (loop for elem in (items thing-to-be-topped)
                        always (subtypep (type-of elem) 'can-have-on-top))))
       
       (let ((new-thing-to-be-topped
              (typecase thing-to-be-topped
                (transferable-container (find-object-by-persistent-id thing-to-be-topped (counter-top new-kitchen-state)))
                (list-of-kitchen-entities (find-kitchen-entities thing-to-be-topped (counter-top new-kitchen-state)))))
             (new-topping-container (find-object-by-persistent-id container-with-topping (counter-top new-kitchen-state))))
         (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)
         (cond ((and new-thing-to-be-topped new-topping-container)
                (let* ((topping (first (contents new-topping-container)))
                       (topping-value-to-transfer (value (quantity (amount topping))))
                       (topping-portion-amount (make-instance 'amount :quantity topping-quantity :unit topping-unit))
                       (topping-left-to-transfer (copy-object topping-value-to-transfer))
                       (topping-portions (make-instance 'list-of-kitchen-entities))
                       (portions
                        (typecase new-thing-to-be-topped
                          (transferable-container (contents new-thing-to-be-topped))
                          (list-of-kitchen-entities (items new-thing-to-be-topped)))))
                  
                  ; convert the portion amount to grams
                  (when (not (eq (type-of topping-unit) 'g))
                    (let ((conversion-ingredient (copy-object topping)))
                      (setf (amount conversion-ingredient) topping-portion-amount)
                      (setf topping-portion-amount (amount (convert-to-g conversion-ingredient)))))
                  
                  (loop while (> topping-left-to-transfer 0)
                        for portion in portions
                        for new-topping-portion = (copy-object topping)
                        do (push new-topping-portion (items topping-portions))
                           (setf (has-on-top portion) new-topping-portion)
                        if (> topping-left-to-transfer (value (quantity topping-portion-amount)))
                          do (setf (amount new-topping-portion) topping-portion-amount
                                   topping-left-to-transfer (- topping-left-to-transfer
                                                               (value (quantity topping-portion-amount))))
                        else
                          do (setf (amount new-topping-portion)
                                   (make-instance 'amount
                                                  :quantity (make-instance 'quantity :value topping-left-to-transfer)
                                                  :unit (make-instance 'g))
                                   topping-left-to-transfer 0)
                        finally 
                          (setf (contents new-topping-container) nil)) 
                  
                  (bind (topped-object 1.0 new-thing-to-be-topped topped-object-available-at)
                        (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))
               (t (bind (topped-object 1.0 (make-instance 'failed-object) topped-object-available-at)
                        (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))
       (bind (topped-object 1.0 (make-instance 'failed-object) topped-object-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))


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

       (cond ((and target-container-instance source-container-instance (contents source-container-instance))
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
                    (quantity 0.0 (make-instance 'failed-object) nil)
                    (unit 0.0 (make-instance 'failed-object) nil)))))))

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

     (cond ((and target-container-instance source-container-instance (contents source-container-instance))
     
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
                  (quantity 0.0 (make-instance 'failed-object) nil)
                  (unit 0.0 (make-instance 'failed-object) nil)))))))

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

       (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

       (cond ((and new-items-to-transfer new-destination)
     
              (setf (used new-destination) t)
              (setf (contents new-destination) (items new-items-to-transfer))
              (setf (arrangement new-destination) default-arrangement)
              (setf (contents (counter-top new-kitchen-state)) ;;delete items from countertop!
                    (remove-if #'(lambda (el)
                                   (find (persistent-id el) (items new-items-to-transfer) :test #'eql :key #'persistent-id))
                               (contents (counter-top new-kitchen-state))))
     
              (bind
               (arrangement-pattern 0.0 default-arrangement nil) 
               (transferred 1.0 new-destination container-available-at)
               (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
             (t
              (bind
               (transferred 1.0 (make-instance 'failed-object) container-available-at)
               (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
               (arrangement-pattern 0.0 default-arrangement nil))))))
     
     ;; items are placed on a transferable container, such as a baking tray
     ((subtypep (type-of items-to-transfer) 'transferable-container) ;baking-tray or cookie sheet
      (let* ((new-kitchen-state (copy-object kitchen-state-in))
             (new-container (find-object-by-persistent-id items-to-transfer new-kitchen-state))
             (default-arrangement (make-instance 'side-to-side))
             (new-destination (find-object-by-persistent-id destination new-kitchen-state))
             (container-available-at (+ 120 (kitchen-time kitchen-state-in)))
             (kitchen-state-available-at container-available-at))

        (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

        (cond ((and new-container new-destination)
    
               (setf (used new-destination) t)
               (setf (contents new-destination) (contents new-container))
               (setf (contents new-container) nil)
               (setf (used new-destination) t)
               (setf (arrangement new-destination) default-arrangement)

               (bind
                (transferred 1.0 new-destination container-available-at)
                (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                (arrangement-pattern 0.0 default-arrangement nil)))
              (t
               (bind
                (transferred 1.0 (make-instance 'failed-object) container-available-at)
                (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                (arrangement-pattern 0.0 default-arrangement nil))))))))

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

            (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

            (cond ((and new-items-to-transfer new-destination)     
                   (setf (used new-destination) t)
                   (setf (contents new-destination) (items new-items-to-transfer))
                   (setf (arrangement new-destination) arrangement-pattern)
                   (setf (contents (counter-top new-kitchen-state)) ;;delete items from countertop!
                         (remove-if #'(lambda (el)
                                        (find (persistent-id el) (items new-items-to-transfer) :test #'eql :key #'persistent-id))
                                    (contents (counter-top new-kitchen-state))))  
     
                   (bind (transferred 1.0 new-destination container-available-at)
                         (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
                  (t
                   (bind (transferred 1.0 (make-instance 'failed-object) container-available-at)
                         (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at))))))
     
         ;; items are placed on a transferable container, such as a baking tray
         ((subtypep (type-of items-to-transfer) 'transferable-container) ;baking-tray or cookie sheet
          (let* ((new-kitchen-state (copy-object kitchen-state-in))
                 (new-container (find-object-by-persistent-id items-to-transfer new-kitchen-state))
                 (new-destination (find-object-by-persistent-id destination new-kitchen-state))
                 (container-available-at (+ 120 (kitchen-time kitchen-state-in)))
                 (kitchen-state-available-at container-available-at))

            (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

            (cond ((and new-container new-destination)
               
                   (setf (used new-destination) t)
                   (setf (contents new-destination) (contents new-container))
                   (setf (contents new-container) nil)
                   (setf (used new-destination) t)
                   (setf (arrangement new-destination) arrangement-pattern)
       
                   (bind
                    (transferred 1.0 new-destination container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
                  (t
                   (bind
                    (transferred 1.0 (make-instance 'failed-object) container-available-at)
                    (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))))
  
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

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond ((and new-container new-cover)

            (setf (covered-with new-container) nil)
            (setf (is-covering new-cover) nil)

            ; put the cover on the countertop
            (setf (contents (counter-top new-kitchen-state)) (cons new-cover (contents (counter-top new-kitchen-state))))

            (bind
             (uncovered-object 1.0 new-container container-available-at)
             (cover 1.0 new-cover container-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))
           (t
            (bind
             (uncovered-object 1.0 (make-instance 'failed-object) container-available-at)
             (cover 1.0 (make-instance 'failed-object) container-available-at)
             (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)))))))

(defprimitive wash ((container-with-washed-ingredients transferable-container)
                    (kitchen-state-out kitchen-state)
                    (kitchen-state-in kitchen-state)
                    (container-with-input-ingredients transferable-container))

  ((kitchen-state-in container-with-input-ingredients => kitchen-state-out container-with-washed-ingredients)
   (let* ((new-kitchen-state (copy-object kitchen-state-in))
          (new-container (find-object-by-persistent-id container-with-input-ingredients (counter-top new-kitchen-state)))
          (container-available-at (+ 60 (max (kitchen-time kitchen-state-in)
                                             (available-at (find (id container-with-input-ingredients) binding-objects
                                                                 :key #'(lambda (binding-object)
                                                                          (and (value binding-object)
                                                                               (id (value binding-object)))))))))
          (kitchen-state-available-at container-available-at))

     (setf (kitchen-time new-kitchen-state) kitchen-state-available-at)

     (cond (new-container
            (loop for ingredient in (contents new-container)
                  when (typep ingredient 'washable)
                    do (setf (washed ingredient) t))

            (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (container-with-washed-ingredients 1.0 new-container container-available-at)))
           (t
            (bind (kitchen-state-out 1.0 new-kitchen-state kitchen-state-available-at)
                  (container-with-washed-ingredients 1.0 (make-instance 'failed-object) container-available-at)))))))

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
          when (subtypep (type-of el) 'has-temperature)
            do (setf (temperature el) (compute-temperature el new-amount)))
    (loop for el in (contents container)
          when (subtypep (type-of el) 'has-temperature)
            do (setf (temperature el) new-amount))))

(defun compute-temperature (ingredient new-amount)
  "Compute the temperature for a given ingredient, based on a given time"
  (if (temperature ingredient)
    (make-instance 'amount
                   :quantity (make-instance 'quantity
                                          ; TOVERIFY RD: location-related formula?
                                            :value (max (- (value (quantity (temperature ingredient)))
                                                           (* (/ (value (quantity (temperature ingredient))) 45)
                                                              (value (quantity new-amount))))
                                                        18))
                   :unit (make-instance 'degrees-celsius))
    nil))
                        
(defun take-n-pieces (source-container target-amount target-container)
  (assert (= (length (contents source-container)) 1))

  (if (subtypep (type-of (unit target-amount)) 'piece)
    
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

          (values target-container source-container))))
    
    (let* ((source-ingredient (first (contents source-container)))
           (source-ingredient-in-g (convert-to-g source-ingredient))
           (one-piece-in-g (/ (value (quantity (amount source-ingredient-in-g))) (value (quantity (amount source-ingredient)))) )    
           (target-ingredient (copy-object source-ingredient-in-g)))
        
      (setf (amount target-ingredient) target-amount)
      (setf target-ingredient (convert-to-g target-ingredient))

      (let* ((new-amount-source-in-g (make-instance 'amount
                                               :unit (unit target-amount)
                                               :quantity (make-instance 'quantity
                                                                        :value (- (value (quantity (amount source-ingredient-in-g)))
                                                                                  (value (quantity (amount target-ingredient)))))))
             (new-amount-source (make-instance 'amount
                                               :unit (make-instance 'piece)
                                               :quantity (make-instance 'quantity
                                                                        :value (/ (value (quantity new-amount-source-in-g))
                                                                                  one-piece-in-g)))))

        ;;adjust amounts of target and source ingredients
        (setf (amount target-ingredient) target-amount)
        (setf (amount source-ingredient) new-amount-source)

        ;;add weighed ingredient to contents of target-container
        (setf (contents target-container)
              (cons target-ingredient (contents target-container)))

            (values target-container source-container)))))

(defun weigh-ingredient (source-container target-amount target-container)
  
  (assert (= (length (contents source-container)) 1))

  (let ((source-ingredient (first (contents source-container))))
    (if (eq (type-of (unit (amount source-ingredient))) (type-of (unit target-amount)))
      (let* ((new-amount-source (make-instance 'amount
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
    
        (values target-container source-container))
      ; convert everything to g for correct computation of differences
      (let* ((source-ingredient-in-g (convert-to-g source-ingredient))
             (target-ingredient (copy-object source-ingredient-in-g)))
        
        (setf (amount target-ingredient) target-amount)
        (setf target-ingredient (convert-to-g target-ingredient))
        (let ((new-amount-source (make-instance 'amount
                                                :unit (make-instance 'g)
                                                :quantity (make-instance 'quantity
                                                                         :value (- (value (quantity (amount source-ingredient)))
                                                                                   (value (quantity (amount target-ingredient))))))))

        ;;adjust amounts of target and source ingredients
        (setf (amount target-ingredient) target-amount)
        (setf (amount source-ingredient) new-amount-source)
    
        ;;add weighed ingredient to contents of target-container
        (setf (contents target-container)
              (cons target-ingredient (contents target-container)))
    
        (values target-container source-container))))))

(defun find-unused-kitchen-entity (reusable-type place &optional excluded)
  "Recursively look for an unused kitchen entity of the given type in the given place.
   Optionally some entities can be excluded from the search (used when fetching multiple items at once)."
  (cond ((loop for el in (contents place)
               if (and (or (eql reusable-type (type-of el))
                           (member reusable-type (mapcar #'class-name (all-superclasses (class-of el)))))
                       (not (used el))
                       (not (find el excluded)))
                 do (return t)) ; first we check if an unused element of that type could be found in general (= condition part of cond)
         (loop for el in (contents place)
               if (and (or (eql reusable-type (type-of el))
                           (member reusable-type (mapcar #'class-name (all-superclasses (class-of el)))))
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

;; searching for a failed-object always fails
(defmethod find-object-by-persistent-id ((object failed-object) (container container))
  nil)

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

#|
;; find all kitchen entities in the list that are on the countertop
(defun find-kitchen-entities (list-of-kitchen-entities countertop)
  (if (subtypep (type-of list-of-kitchen-entities) 'failed-object)
    nil ; cannot find failed-objects
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
            finally (return new-list-of-kitchen-entities)))))
|#

(defun find-kitchen-entities (list-of-kitchen-entities countertop)
  "Find all kitchen entities in the list that are on the countertop"
  (unless (subtypep (type-of list-of-kitchen-entities) 'failed-object)
    (let ((new-list-of-kitchen-entities (copy-object list-of-kitchen-entities))
          (first-item (first (items list-of-kitchen-entities)))
          (unique-persistent-ids
           (remove-duplicates (mapcar #'persistent-id (items list-of-kitchen-entities))))
          (countertop-contents (contents countertop)))
      (setf (items new-list-of-kitchen-entities) nil)
      (cond ((= (length unique-persistent-ids) 1)
             (loop until (eq (persistent-id first-item) (persistent-id (first countertop-contents)))
                   do (setf countertop-contents (rest countertop-contents)))
             (loop for list-item in (items list-of-kitchen-entities)
                   for countertop-item in countertop-contents
                   when (eq (persistent-id list-item) (persistent-id countertop-item))
                     do (push list-item (items new-list-of-kitchen-entities))))
            ((= (length unique-persistent-ids) (length (items list-of-kitchen-entities)))
             (loop for list-item in (items list-of-kitchen-entities)
                   for countertop-item = (find (persistent-id list-item) countertop-contents
                                               :key #'persistent-id :test #'eq)
                   when countertop-item
                     do (push list-item (items new-list-of-kitchen-entities)))))
      new-list-of-kitchen-entities)))

(defun create-homogeneous-mixture (ingredients &optional (mixing-operation 'mixed))
  (let* ((total-value (loop for ingredient in ingredients
                            for current-value = (value (quantity (amount (convert-to-g ingredient))))
                            sum current-value))
         (mixture (make-instance 'homogeneous-mixture :amount (make-instance 'amount
                                                                             :unit (make-instance 'g)
                                                                             :quantity (make-instance 'quantity :value total-value))
                                 :components ingredients)))

    ; modify the ingredients so they all contain percentages (to prevent recursion errors when portioning nested mixtures)
    (loop for ingredient in (components mixture)
          do (setf (amount ingredient)
                   (make-instance 'amount
                                  :quantity (make-instance 'quantity
                                                           :value (/ (value (quantity (amount (convert-to-g ingredient)))) total-value))
                                  :unit (make-instance 'percent))))

    (setf (slot-value mixture mixing-operation) t)
    mixture))

(defun create-homogeneous-mixture-in-container (container &optional (mixing-operation 'mixed))
  "Create a homogeneous mixture composed of the ingredients in the given container and mixed according to the given mixing-operation.
   The mixing-operation should be a valid slotname specifying a boolean slot that will be set to T."
  (let ((mixture (create-homogeneous-mixture (contents container) mixing-operation)))
      (setf (contents container) (list mixture))
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
    (setf (gethash 'almond-flour conversion-table)
	  (acons 'teaspoon 2 '()))
    (setf (gethash 'apple conversion-table)
	  (acons 'piece 150 '()))
    (setf (gethash 'avocado conversion-table)
          (acons 'piece 215 '()))
    (setf (gethash 'baking-soda conversion-table)
	  (acons 'teaspoon 5 '()))
    (setf (gethash 'baking-powder conversion-table)
	  (acons 'teaspoon 4 '()))  
    (setf (gethash 'banana conversion-table)
	  (acons 'piece 118 '()))
    (setf (gethash 'black-olive conversion-table)
          (acons 'piece 3.5 '()))
    (setf (gethash 'broccoli conversion-table)
	  (acons 'piece 425 '()))
    (setf (gethash 'brown-sugar conversion-table)
          (acons 'teaspoon 4.2 '()))
    (setf (gethash 'butter conversion-table)
          (acons 'teaspoon 5 '()))
    (setf (gethash 'carrot conversion-table)
	  (acons 'piece 95 '()))
    (setf (gethash 'caster-sugar conversion-table)
	  (acons 'teaspoon 5 '()))
    (setf (gethash 'celery conversion-table)
	  (acons 'piece 53 '()))
    (setf (gethash 'celery-seed conversion-table)
          (acons 'teaspoon 2.2 '()))
    (setf (gethash 'cherry-tomato conversion-table)
	  (acons 'piece 17 '()))
    (setf (gethash 'cider-vinegar conversion-table)
	  (acons 'teaspoon 5 (acons 'l 950 '())))
    (setf (gethash 'coarse-salt conversion-table)
          (acons 'teaspoon 5 '()))
    (setf (gethash 'cocoa-powder conversion-table)
          (acons 'teaspoon 4 '()))
    (setf (gethash 'coconut-oil conversion-table)
	  (acons 'teaspoon 4.5 (acons 'l 921 '())))
    (setf (gethash 'corn-flakes conversion-table)
          (acons 'teaspoon 2 '()))
    (setf (gethash 'cucumber conversion-table)
          (acons 'piece 250 '()))
    (setf (gethash 'dried-dill-weed conversion-table)
          (acons 'teaspoon 5 '()))    
    (setf (gethash 'egg conversion-table)
          (acons 'piece 50 (acons 'teaspoon 5.1 '())))
    (setf (gethash 'extra-virgin-olive-oil conversion-table)
          (acons 'teaspoon 4.5 (acons 'l 920 '())))
    (setf (gethash 'fresh-cilantro conversion-table)
          (acons 'teaspoon 0.65 '()))
    (setf (gethash 'fresh-oregano conversion-table)
          (acons 'teaspoon 1.05 '()))
    (setf (gethash 'garlic conversion-table)
	  (acons 'piece 6 '()))
    (setf (gethash 'garlic-powder conversion-table)
	  (acons 'teaspoon 1.58 '()))
    (setf (gethash 'grated-horseradish conversion-table)
          (acons 'teaspoon 5 '()))
    (setf (gethash 'green-cabbage conversion-table)
          (acons 'piece 908 '()))
    (setf (gethash 'green-chili-pepper conversion-table)
          (acons 'piece 25 (acons 'teaspoon 5 '())))
    (setf (gethash 'green-onion conversion-table)
          (acons 'piece 17 '()))  
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
    (setf (gethash 'hard-boiled-egg conversion-table)
	  (acons 'piece 50 '()))
    (setf (gethash 'jalapeno conversion-table)
          (acons 'piece 20 '()))
    (setf (gethash 'mango conversion-table)
	  (acons 'piece 205 '()))
    (setf (gethash 'mayonnaise conversion-table)
	  (acons 'l 1010 '())) 
    (setf (gethash 'lemon-juice conversion-table)
	  (acons 'teaspoon 5 (acons 'l 1023 '())))
    (setf (gethash 'lime-juice conversion-table)
	  (acons 'teaspoon 5 (acons 'l 1023 '())))
    (setf (gethash 'milk conversion-table)
	  (acons 'l 1032 '()))
    (setf (gethash 'mustard-seed conversion-table)
          (acons 'teaspoon 2.13 '()))      
    (setf (gethash 'onion conversion-table)
          (acons 'piece 100 '()))
    (setf (gethash 'paprika-powder conversion-table)
          (acons 'teaspoon 2.3 '()))
    (setf (gethash 'potato conversion-table)
          (acons 'piece 210 '()))
    (setf (gethash 'radish conversion-table)
          (acons 'piece 20 '()))
    (setf (gethash 'raisin conversion-table)
          (acons 'teaspoon 3.3 (acons 'piece 1 '())))
    (setf (gethash 'red-bell-pepper conversion-table)
          (acons 'piece 300 '()))  
    (setf (gethash 'red-chili-pepper conversion-table)
          (acons 'piece 25 '()))  
    (setf (gethash 'red-onion conversion-table)
          (acons 'piece 50 '()))
    (setf (gethash 'red-pepper-flakes conversion-table)
          (acons 'teaspoon 0.5 '()))
    (setf (gethash 'red-wine-vinegar conversion-table)
          (acons 'teaspoon 5 (acons 'l 1010 '())))   
    (setf (gethash 'romaine-lettuce conversion-table)
	  (acons 'piece 300 '())) 
    (setf (gethash 'salt conversion-table)
          (acons 'teaspoon 6 '()))
    (setf (gethash 'shallot conversion-table)
          (acons 'piece 50 '()))
    (setf (gethash 'tabasco conversion-table)
          (acons 'l 1000 (acons 'teaspoon 4 '())))
    (setf (gethash 'tomato conversion-table)
          (acons 'piece 125 '()))    
    (setf (gethash 'trader-joes-cilantro-salad-dressing conversion-table)
	  (acons 'teaspoon 7 (acons 'l 1111 '())))
    (setf (gethash 'turmeric-powder conversion-table)
          (acons 'teaspoon 5 '()))      
    (setf (gethash 'vanilla conversion-table)
	  (acons 'teaspoon 4 '()))
    (setf (gethash 'vanilla-extract conversion-table)
	  (acons 'l 880 (acons 'teaspoon 4 '())))
    (setf (gethash 'vegetable-oil conversion-table)
          (acons 'l 944 (acons 'teaspoon 4.5 '())))
    (setf (gethash 'water conversion-table)
	  (acons 'l 1000 (acons 'teaspoon 5 '())))
    (setf (gethash 'white-bread-slice conversion-table)
	  (acons 'piece 38 '()))
    (setf (gethash 'white-sugar conversion-table)
          (acons 'teaspoon 4.2 '()))
    (setf (gethash 'white-vinegar conversion-table)
	  (acons 'l 1085 '()))
    (setf (gethash 'whole-egg conversion-table)
	  (acons 'piece 50 (acons 'teaspoon 5.5 '())))
    (setf (gethash 'yellow-mustard conversion-table)
	  (acons 'teaspoon 5 (acons 'l 1010 '())))
    (setf (gethash 'semisweet-chocolate-chips conversion-table)
          (acons 'teaspoon 3.33 '()))
    (setf (gethash 'cream-cheese conversion-table)
          (acons 'teaspoon 4.69 '()))
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
  (let ((converted-ingredient (copy-object ingredient)))
    (unless (eq (type-of (unit (amount converted-ingredient))) 'g)
      (let ((ingredient-type (type-of converted-ingredient))
            (source-unit-type (type-of (unit (amount converted-ingredient)))))
        (case source-unit-type
          (ml
           (setf source-unit-type 'l)
           (setf (value (quantity (amount converted-ingredient)))
                 (/ (value (quantity (amount converted-ingredient))) 1000))
           (setf (unit (amount converted-ingredient)) (make-instance 'l)))
          (tablespoon
           (setf source-unit-type 'teaspoon)
           (setf (value (quantity (amount converted-ingredient)))
                 (* (value (quantity (amount converted-ingredient))) 3))
           (setf (unit (amount converted-ingredient)) (make-instance 'teaspoon))))
        (multiple-value-bind (conversion-rates found) (gethash ingredient-type *conversion-table-for-g*)
          (unless found
            (error "The ingredient ~S has no entry in the conversion table!" ingredient-type))
          (let* ((conversion-rate (assoc source-unit-type conversion-rates))
                 (converted-value
                  (if (null conversion-rate)
                    (error "The ingredient ~S has no entry in the conversion table for unit ~S!"
                           ingredient-type source-unit-type)
                    (* (value (quantity (amount converted-ingredient)))
                       (rest conversion-rate)))))
            (setf (amount converted-ingredient)
                  (make-instance 'amount
                                 :unit (make-instance 'g)
                                 :quantity (make-instance 'quantity
                                                          :value converted-value)))))))
     converted-ingredient))
        

(defmethod convert-to-temperature ((stove stove) (stove-mode stove-mode))
  (cond ((eql (type-of stove-mode) 'low-heat)
         (make-instance 'amount
                        :unit (make-instance 'degrees-celsius)
                        :quantity (make-instance 'quantity :value 110)))
        ((eql (type-of stove-mode) 'medium-heat)
         (make-instance 'amount
                        :unit (make-instance 'degrees-celsius)
                        :quantity (make-instance 'quantity :value 150)))
        
        ((eql (type-of stove-mode) 'medium-high-heat)
         (make-instance 'amount
                        :unit (make-instance 'degrees-celsius)
                        :quantity (make-instance 'quantity :value 175)))
        
        ((eql (type-of stove-mode) 'high-heat)
         (make-instance 'amount
                        :unit (make-instance 'degrees-celsius)
                        :quantity (make-instance 'quantity :value 200)))))

(defun has-failed-objects (&rest objects)
  "Check if any of the objects is a failed objects"
  (find-if #'(lambda (object)
               (subtypep (type-of object) 'failed-object))
           objects))
