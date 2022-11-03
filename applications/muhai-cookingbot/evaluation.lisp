(ql:quickload :muhai-cookingbot)

(in-package :muhai-cookingbot)

;; Classes for simulation execution ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass solution ()
  ((recipe-id :type symbol :initarg :recipe-id :accessor recipe-id :initform nil)
   (meaning-network :type list :initarg :meaning-network :accessor meaning-network :initform '())
   (subgoals-percentage :accessor subgoals-percentage :initform '())
   (time-ratio :accessor time-ratio :initform '())
   (dish-score :accessor dish-score :initform '()))
  (:documentation "Class representing the solution of a recipe.")) 

(defclass simulation-environment ()
  ((recipe-id :type symbol :initarg :recipe-id :accessor recipe-id :initform nil)
   (kitchen-state :type kitchen-state :initarg :kitchen-state :accessor kitchen-state)
   (meaning-network :type list :initarg :meaning-network :accessor meaning-network :initform '())
   (solution-bindings :type list :accessor solution-bindings :initform '())
   (solution-node :type irl-program-processor-node :accessor solution-node))
  (:documentation "Class wrapping all information for setting up and evaluating an environment."))

(defmethod initialize-instance :after ((simulation-environment simulation-environment) &key)
  (when (not (null (meaning-network simulation-environment)))
    (let ((extended-mn (append-meaning-and-irl-bindings (meaning-network simulation-environment) nil)))
      (init-kitchen-state simulation-environment)
      (multiple-value-bind (bindings nodes) (evaluate-irl-program extended-mn nil)
        (setf (solution-bindings simulation-environment) bindings)
         ; we only expect there to be one solution
        (setf (solution-node simulation-environment) (first nodes))))))

(defun init-kitchen-state (simulation-environment)
  (setf *initial-kitchen-state* (kitchen-state simulation-environment)))

;; Simulation environments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *almond-crescent-cookies-environment*
  (make-instance 'simulation-environment
                 :recipe-id 'almond-crescent-cookies
                 :kitchen-state
                 (make-instance
                  'kitchen-state
                  :contents
                  (list (make-instance 'fridge
                                       :contents (list (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'butter
                                                                                                     :temperature
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'degrees-celsius)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 5))
                                                                                                     :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 500)))))))
                        (make-instance 'pantry
                                       :contents (list (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'white-sugar :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 1000)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'vanilla-extract :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 100)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'almond-extract :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 100)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'all-purpose-flour :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 1000)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'almond-flour :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 1000)))))
                                                       (make-instance 'medium-bowl
                                                                      :used T
                                                                      :contents (list (make-instance 'powdered-white-sugar :amount
                                                                                                     (make-instance 'amount
                                                                                                                    :unit (make-instance 'g)
                                                                                                                    :quantity (make-instance 'quantity
                                                                                                                                             :value 500)))))))
                        (make-instance 'kitchen-cabinet
                                       :contents (list
                                                  ;; bowls
                                                  (make-instance 'large-bowl) (make-instance 'large-bowl) (make-instance 'large-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)
                                                  (make-instance 'medium-bowl) (make-instance 'medium-bowl) (make-instance 'medium-bowl)

                                                  ;; tools
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)
                                                  (make-instance 'whisk) (make-instance 'whisk) (make-instance 'whisk)

                                                  ;; baking equipment
                                                  (make-instance 'baking-tray)
                                                  (make-instance 'baking-paper)))))
                 :meaning-network
                 (list '(get-kitchen ?kitchen-state-4)
                       '(bring-to-temperature ?ingredient-at-room-temperature-7 ?output-kitchen-state-47 ?kitchen-state-out-34 ?ingredient-out-6 18 degrees-celsius)
                       '(fetch-and-proportion ?ingredient-out-6 ?kitchen-state-out-34 ?kitchen-state-4 ?target-container-6 butter 226 g)
                       '(fetch-and-proportion ?ingredient-out-13 ?kitchen-state-out-77 ?output-kitchen-state-47 ?target-container-13 white-sugar 116 g)
                       '(fetch-and-proportion ?ingredient-out-20 ?kitchen-state-out-117 ?kitchen-state-out-77 ?target-container-20 vanilla-extract 4 g)
                       '(fetch-and-proportion ?ingredient-out-27 ?kitchen-state-out-158 ?kitchen-state-out-117 ?target-container-27 almond-extract 4 g)
                       '(fetch-and-proportion ?ingredient-out-33 ?kitchen-state-out-193 ?kitchen-state-out-158 ?target-container-33 all-purpose-flour 340 g)
                       '(fetch-and-proportion ?ingredient-out-39 ?kitchen-state-out-230 ?kitchen-state-out-193 ?target-container-39 almond-flour 112 g)
                       '(fetch-and-proportion ?ingredient-out-45 ?kitchen-state-out-267 ?kitchen-state-out-230 ?target-container-45 powdered-white-sugar 29 g)
                       '(beat ?output-container-298 ?output-kitchen-state-347 ?input-kitchen-state-298 ?input-container-249 ?tool-150)
                       '(transfer-contents ?input-container-249 ?rest-y-106 ?input-kitchen-state-298 ?output-kitchen-state-x-53 ?output-container-?x-53 ?ingredient-out-13 ?quantity-y-106 ?unit-y-106)
                       '(transfer-contents ?output-container-?x-53 ?rest-x-106 ?output-kitchen-state-x-53 ?kitchen-state-out-267 ?empty-container-53 ?ingredient-at-room-temperature-7 ?quantity-x-106 ?unit-x-106)
                       '(mix ?output-container-338 ?output-kitchen-state-394 ?input-kitchen-state-338 ?input-container-282 ?tool-169)
                       '(transfer-contents ?input-container-282 ?rest-y-208 ?input-kitchen-state-338 ?intermediate-ks-104 ?output-container-after-adding-x-104 ?ingredient-out-20 ?quantity-y-208 ?unit-y-208)
                       '(transfer-contents ?output-container-after-adding-x-104 ?rest-x-208 ?intermediate-ks-104 ?output-kitchen-state-347 ?output-container-298 ?ingredient-out-27 ?quantity-x-208 ?unit-x-208)
                       '(transfer-contents ?output-container-686 ?rest-y-229 ?output-kitchen-state-800 ?intermediate-ks-115 ?output-container-after-adding-x-115 ?ingredient-out-33 ?quantity-y-229 ?unit-y-229)
                       '(transfer-contents ?output-container-after-adding-x-115 ?rest-x-229 ?intermediate-ks-115 ?output-kitchen-state-394 ?output-container-338 ?ingredient-out-39 ?quantity-x-229 ?unit-x-229)
                       '(mix ?output-container-699 ?output-kitchen-state-815 ?output-kitchen-state-800 ?output-container-686 ?tool-349)
                       '(shape ?shaped-bakeables-262 ?kitchen-state-out-786 ?kitchen-state-with-portions-on-tray-130 ?portioned-dough-130 ball-shape)
                       '(portion-and-arrange ?portioned-dough-130 ?kitchen-state-with-portions-on-tray-130 ?output-kitchen-state-815 ?output-container-699 25 g ?pattern-130 ?lined-baking-tray-259)
                       '(shape ?shaped-bakeables-264 ?kitchen-state-out-792 ?kitchen-state-out-786 ?shaped-bakeables-262 crescent-shape)
                       '(transfer-items ?things-placed-136 ?kitchen-out-136 ?kitchen-state-out-805 ?shaped-bakeables-264 ?lined-baking-tray-269)
                       '(line ?lined-baking-tray-269 ?kitchen-state-out-805 ?kitchen-state-out-792 baking-tray baking-paper)
                       '(bake ?thing-baked-148 ?kitchen-state-out-887 ?kitchen-out-136 ?things-placed-136 ?oven-to-bake-in-148 15 minute 175 degrees-celsius)
                       '(sprinkle ?sprinkled-object-151 ?kitchen-state-out-901 ?kitchen-state-out-887 ?thing-baked-148 ?ingredient-out-45))))

; list of all available simulation environments
(defparameter *simulation-environments*
  (list *almond-crescent-cookies-environment*))

;; Functions for simulation execution ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-from-file (filepath)
  "Read all recipe solutions from the file at the given filepath. 
   Solutions are represented by a line containing the ID of a recipe, i.e., #recipe_ID, 
   followed by a sequence of lines each containing a primitive operation from the meaning network for the aforementiond recipe."
  (with-open-file (stream filepath)
    (let ((solutions '()))
      (loop for line = (read-line stream nil)
            while line
            do
              (cond ((not (find #\space line :test-not #'eql))
                     (print "Empty line is skipped"))
                    ((char= (char line 0) #\#)
                     ; first check the previous solution
                 ;    (when solutions
                 ;      (handler-case (irl::check-irl-program (meaning-network (first solutions)) nil *irl-primitives*)
                 ;        (error (e)
                 ;          (error "Invalid IRL program in solution ~S. Error was thrown: ~S" (recipe-id (first solutions)) e))))
                     ; we are starting a new solution
                     (push (make-instance 'solution :recipe-id (read-from-string (subseq line 1))) solutions))
                     ; we are adding a new primitive operation to the current solution's meaning network
                    ((char= (char line 0) #\()
                     (let ((current-solution (first solutions)))
                       (unless current-solution
                         (error "The file should start with a Recipe ID (#recipe-id)!"))
                       (setf (meaning-network current-solution) (nconc (meaning-network current-solution) (list (read-from-string line))))))
                    (t
                     (error "A line should either contain a recipe ID (#recipe-id) or a primitive operation (op ?a ?b), but ~S was found" line))))
;check-irl-program does not allow nupmbers, etc.
   ;   (irl::check-irl-program (meaning-network (first solutions)) nil *irl-primitives*)
 ;(when solutions
    ;   (handler-case (irl::check-irl-program (meaning-network (first solutions)) nil *irl-primitives*)
     ;     (error (e)
     ;       (error "Invalid IRL program in solution ~S. Error was thrown: ~a" (recipe-id (first solutions))))))

      ;TODO: check if the recipe contains a get-kitchen primitive?
      solutions)))

(defun verify-solutions-completeness (solutions &optional (sim-envs *simulation-environments*))
  "Check if the given solutions contain a solution for every recipe that has to be evaluated."
  (loop for sim-env in sim-envs
        when (not (find (recipe-id sim-env) solutions :key #'(lambda (sol) (recipe-id sol))))
          do (error "Recipe ~S is missing in the solutions" (recipe-id sim-env)))
  (loop for solution in solutions
        when (not (find (recipe-id solution) sim-envs :key #'(lambda (sim-env) (recipe-id sim-env))))
          do (error "Solution contains recipe ~S which is currently unsupported" (recipe-id solution))
        when (> (count (recipe-id solution) solutions :key #'(lambda (sol) (recipe-id sol))) 1)
          do (error "Duplicate entry found for recipe ~S" (recipe-id solution))))


; TODO RD: get node at maximum depth that was reached if no solution could be found?
; TODO RD: final value zou altijd een container met een ingredient in moeten zijn
; TODO RD: currently unused
(defun get-final-value (irl-node)
  (let* ((target-var (second (irl::primitive-under-evaluation irl-node)))
         (list-of-bindings (irl::bindings irl-node))
         (target-binding (find target-var list-of-bindings :key #'var)))
    (when target-binding
      (value target-binding))))

 ;; Equal ontology objects ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun similar-entities (entity-1 entity-2 &optional (ignore '(id persistent-id)))
  (equal-ontology-objects entity-1 entity-2 ignore))

(defun slot-names-to-compare (item &optional ignore)
  (let* ((classlots  (closer-mop:class-slots (class-of item)))
         (slotnames  (mapcar #'closer-mop:slot-definition-name classlots)))
    (remove-if #'(lambda (slotname) (member slotname ignore))
             slotnames)))

(defgeneric equal-ontology-objects (object-1 object-2 &optional ignore)
  (:documentation "Returns t if object-1 and object-2 are equal in ontological terms."))

(defmethod equal-ontology-objects ((object-1 symbol) (object-2 symbol) &optional ignore)
  (eql object-1 object-2))

(defmethod equal-ontology-objects ((object-1 number) (object-2 number) &optional ignore)
  (= object-1 object-2))

(defmethod equal-ontology-objects ((object-1 string) (object-2 string) &optional ignore)
  (equalp object-1 object-2))

(defmethod equal-ontology-objects ((object-1 list) (object-2 list) &optional ignore)
  (and (= (length object-1) (length object-2))
       (loop for el-1 in object-1
             for el-2 in object-2
             always (equal-ontology-objects el-1 el-2 ignore))))

(defmethod equal-ontology-objects (object-1 object-2 &optional ignore)
  (and (equal (class-of object-1) (class-of object-2))
       (let ((o1-slotnames (slot-names-to-compare object-1 ignore))
             (o2-slotnames (slot-names-to-compare object-2 ignore)))
       (loop for o1-slotname in o1-slotnames
             always (equal-ontology-objects
                     (slot-value object-1  o1-slotname)
                     (slot-value object-2  o1-slotname)
                     ignore)))))

;; Evaluate subgoals ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-predicate-name (prim-op)
  (first prim-op))

(defun get-output-binding (irl-node)
  (let ((target-var (second (irl::primitive-under-evaluation irl-node)))
        (list-of-bindings (irl::bindings irl-node)))
    (find target-var list-of-bindings :key #'var)))  

(defun get-output-value (irl-node)
  (let ((target-binding (get-output-binding irl-node)))
    (when target-binding
      (value target-binding))))

(defun get-node-sequence (irl-node)
  (let ((node irl-node)
        (node-seq '()))
    (loop do
         (push node node-seq)
         (setf node (parent node))
       while node)
    (rest node-seq)))
  
;; Condition goal test
(defun compute-subgoal-percentage (sol-node gold-node)
  (multiple-value-bind (goals-reached goals-failed) (evaluate-subgoals sol-node gold-node)
    (/ (length goals-reached) (+ (length goals-reached) (length goals-failed)))))

(defun get-outputs (irl-nodes &optional (ignore '(get-kitchen)))
  (let ((filtered-nodes (remove-if #'(lambda (node) (member (get-predicate-name (irl::primitive-under-evaluation node)) ignore)) irl-nodes)))
    (mapcar #'get-output-value filtered-nodes)))

(defun get-output-kitchen-state (irl-node)
  (let ((all-vars (irl::all-variables (list (irl::primitive-under-evaluation irl-node))))
        (list-of-bindings (irl::bindings irl-node)))
    (loop for var in all-vars
          for binding = (find var list-of-bindings :key #'var)
          if (eql (type-of (value binding)) 'kitchen-state)
            do (return (value binding)))))

(defun find-location (object place)
    (cond ((loop for el in (contents place)
                 if (eql (persistent-id object) (persistent-id el))
                   do (return t))
           (loop for el in (contents place)
                 if (eql (persistent-id object) (persistent-id el))
                   do (return (list place))))
          (t
           (loop for el in (contents place)
               if (subtypep (type-of el) 'container)
               do (let ((location (find-location object el)))
                    (when location
                      (return (append (list place) location))))))))

(defclass entity-with-location ()
  ((entity :initarg :entity :accessor entity)
   (location :initarg :location :accessor location))
  (:documentation "Class wrapping a kitchen entity with its location")) 

(defun get-output-entity-with-location (irl-node)
  (let ((output (get-output-value irl-node))
        (ks (get-output-kitchen-state irl-node)))
    (make-instance 'entity-with-location :entity output :location (find-location output ks))))

(defun similar-locations (location-1 location-2)
  (loop for place-1 in location-1
        for place-2 in location-2
        always (eql (type-of place-1) (type-of place-2))))

(defun evaluate-subgoals (sol-node gold-node)
  (let* ((gold-nodes (get-node-sequence gold-node))
         (filtered-gold-nodes (remove-if #'(lambda (node) (eql (get-predicate-name (irl::primitive-under-evaluation node)) 'get-kitchen)) gold-nodes))
         (sol-nodes (get-node-sequence sol-node))
         (filtered-sol-nodes (remove-if #'(lambda (node) (eql (get-predicate-name (irl::primitive-under-evaluation node)) 'get-kitchen)) sol-nodes))
         (gold-entities (mapcar #'get-output-entity-with-location filtered-gold-nodes))
         (sol-entities  (mapcar #'get-output-entity-with-location filtered-sol-nodes))
         (goals-reached '())
         (goals-failed '()))
    (loop for gold-entity in gold-entities
          for sol-entity = (find-if #'(lambda (sol-entity)
                                        (and (similar-entities (entity gold-entity) (entity sol-entity))
                                             (similar-locations (location gold-entity) (location sol-entity))))
                                        sol-entities)
          if sol-entity
            do
              (setf sol-entities (remove sol-entity sol-entities))
              (push gold-entity goals-reached)
          else do (push gold-entity goals-failed))
    (values goals-reached goals-failed)))

(defun evaluate (filepath &optional (sim-envs *simulation-environments*))
  (let ((solutions (read-from-file filepath)))
     ; check if the solutions file contains all the needed solutions
    (verify-solutions-completeness solutions sim-envs)
    (loop for current-solution in solutions
          for current-id = (recipe-id current-solution)
          for current-sim-env = (find current-id sim-envs :key #'(lambda (sim-env) (recipe-id sim-env)))
          for gold-mn = (meaning-network current-sim-env)
          for gold-bindings = (solution-bindings current-sim-env)
          for gold-node = (solution-node current-sim-env)
          do
            (init-kitchen-state current-sim-env)
            (let ((extended-mn (append-meaning-and-irl-bindings (meaning-network current-solution) nil)))
              (multiple-value-bind (sol-bindings sol-nodes) (evaluate-irl-program extended-mn nil)
                ; compute percentage of reached subgoals
                (setf (subgoals-percentage current-solution) (compute-subgoal-percentage (first sol-nodes) gold-node))
                (setf (time-ratio current-solution)
                      (/ (irl::available-at (get-output-binding (first sol-nodes)))
                         (irl::available-at (get-output-binding gold-node))))
                (setf (dish-score current-solution) (compute-ratio (compare-node-dishes (first sol-nodes) gold-node))))))
    solutions))


;; Approach with ingredient unfolding ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; same as a regular ingredient, except it also contains a pointer to the mixture it belongs to
; this pointer is not added to regular ingredients because of infinite loop issues in visualization
(defclass sim-ingredient ()
  ((ingredient :type ingredient :initarg :ingredient :accessor ingredient :initform nil)
   (part-of :initarg :part-of :accessor part-of :initform nil))
  (:documentation "For ingredients in simulation."))

(defclass similarity ()
  ((correct :type number :initarg :correct :accessor correct :initform 0)
   (total :type number :initarg :total :accessor total :initform 0))
  (:documentation "Class used to compute similarity of objects."))

(defmethod add-score ((similarity similarity) (score number) (possible-score number))
  (setf (correct similarity) (+ (correct similarity) score))
  (setf (total similarity) (+ (total similarity) possible-score)))

(defmethod add-similarity-to ((similarity similarity) (to-similarity similarity))
  (setf (correct to-similarity) (+ (correct similarity) (correct to-similarity)))
  (setf (total to-similarity) (+ (total similarity) (total to-similarity))))

(defmethod compute-ratio ((similarity similarity))
  (/ (correct similarity) (total similarity)))

(defmethod unfold-mixture ((mixture-to-unfold sim-ingredient))
  (let* ((inner-mixture (ingredient mixture-to-unfold))
         (comps (components inner-mixture))
         (unfolded-comps '()))
    (loop for comp in comps
          do
          (cond ((subtypep (type-of comp) 'mixture)
                 (let ((unfolded-sub-comps (unfold-mixture (make-instance 'sim-ingredient
                                                                          :ingredient comp
                                                                          :part-of mixture-to-unfold))))
                   (setf unfolded-comps (nconc unfolded-comps unfolded-sub-comps))))
                ((subtypep (type-of comp) 'ingredient)
                 (setf unfolded-comps (nconc unfolded-comps (list (make-instance 'sim-ingredient
                                                                                 :ingredient comp
                                                                                 :part-of mixture-to-unfold)))))
                (t (error "unsupported component of class ~a" (type-of comp)))))
    unfolded-comps))

(defmethod unfold-dish ((dish container))
  (let* ((dish-copy (copy-object dish))
         (items (contents dish-copy))
         (ref-item (copy-object (first items)))
         (unfolded-contents '()))
    ; if all items in the container are the same, then just consider it to be one big item since portioning is then just one missing step
    (when (loop for item in items
                always (similar-entities ref-item item '(id persistent-id amount)))
       (let ((total-value (loop for item in items
                                for current-value = (value (quantity (amount (convert-to-g item))))
                                sum current-value)))
         (setf (amount ref-item) (make-instance 'amount
                                                :unit (make-instance 'g)
                                                :quantity (make-instance 'quantity :value total-value)))
         (setf (contents dish-copy) (list ref-item))))
    (loop for item in (contents dish-copy)
          do (cond ((subtypep (type-of item) 'mixture)
                    (setf unfolded-contents (nconc unfolded-contents (unfold-mixture (make-instance 'sim-ingredient :ingredient item)))))
                   ((subtypep (type-of item) 'ingredient)
                    (setf unfolded-contents (nconc unfolded-contents (make-instace 'sim-ingredient :ingredient item))))))
    unfolded-contents))

(defmethod compare-node-dishes ((sol-dish irl::irl-program-processor-node) (gold-dish irl::irl-program-processor-node))
  (let* ((sol-value (get-output-value sol-dish))
         (sol-location (find-location sol-value (get-output-kitchen-state sol-dish)))
         (sol-dish-slots (slot-names-to-compare sol-value '(persistent-id id contents))) ; all slots except contents
         (gold-value (get-output-value gold-dish))
         (gold-location (find-location gold-value (get-output-kitchen-state gold-dish)))
         (gold-dish-slots (slot-names-to-compare gold-value '(persistent-id id contents))) ; all slots except contents
         (container-score (make-instance 'similarity))
         (contents-score (make-instance 'similarity)))
    ;; container specific scoring
    ; location of dish is worth a score of 1
    (if (similar-locations sol-location gold-location)
      (add-score container-score 1 1)
      (add-score container-score 0 1))
    ; type of container is worth a score of 1
    (if (eq (type-of sol-value) (type-of gold-value))
      (add-score container-score 1 1)
      (add-score container-score 0 1))
    ; each slot that is in common is worth a score of 1  
    (loop for slot in gold-dish-slots
          if (and (member slot sol-dish-slots)
                  (similar-entities (slot-value sol-value slot)
                                    (slot-value gold-value slot)))
            do (add-score container-score 1 1)
          else
            do (add-score container-score 0 1))
    ;; contents specific scoring
    ; number of portions in the dish is worth a score of 1
    ; (only 1 because right now an end dish will always be portions of the same mixture, so just one operation is missing)
    (if (= (length (contents sol-value)) (length (contents gold-value)))
      (add-score contents-score 1 1)
      (add-score contents-score 0 1))
    ; actual composition of the final contents
    (let ((unfolded-dish-sol (unfold-dish sol-value))
          (unfolded-dish-gold (unfold-dish gold-value))
          (missing-ingredients '()))
      (loop for unfolded-ing-gold in unfolded-dish-gold
            for matching-ings-sol = (find-all (type-of (ingredient unfolded-ing-gold)) unfolded-dish-sol :key #'(lambda (sim-ing) (type-of (ingredient sim-ing))))
            if matching-ings-sol
              do (let* ((sim-scores (loop for matching-ing-sol in matching-ings-sol
                                          collect (compare-dish-ingredient matching-ing-sol unfolded-ing-gold)))
                        (match-scores (mapcar #'compute-ratio sim-scores))
                        (max-score (apply #'max match-scores))
                        (max-position (position max-score match-scores))
                        (max-ing (nth max-position matching-ings-sol)))
                   (add-similarity-to (nth max-position sim-scores) contents-score)
                   (setf unfolded-dish-sol (remove max-ing unfolded-dish-sol)))
            else
              do (push unfolded-ing-gold missing-ingredients))
      (let ((missing-ratio (/ (- (length unfolded-dish-gold) (length missing-ingredients)) (length unfolded-dish-gold)))
            (extra-ratio (/ (+ (length unfolded-dish-sol) (length unfolded-dish-gold)) (length unfolded-dish-gold))))
        (setf (correct contents-score) (* (correct contents-score) missing-ratio))
        (setf (correct contents-score) (/ (correct contents-score) extra-ratio))))

    ; compute the final similarity for this dish
    (make-instance 'similarity
                   :correct (+ (* 0.95 (correct contents-score))
                               (* 0.05 (correct container-score)))
                   :total (+ (* 0.95 (total contents-score))
                             (* 0.05 (total container-score))))))

(defun get-mixture-hierarchy (sim-ingredient)
  (let ((mixture (part-of sim-ingredient))
        (mixtures '()))    
    (loop while mixture
            do
              (setf mixtures (nconc (list (ingredient mixture)) mixtures))
              (setf mixture (part-of mixture)))
    mixtures))

(defmethod compare-mixture ((sol-mixture mixture) (gold-mixture mixture))
  (let ((sol-dish-slots (slot-names-to-compare sol-mixture '(id persistent-id components)))
        (gold-dish-slots (slot-names-to-compare gold-mixture '(id persistent-id components)))
        (mixture-similarity (make-instance 'similarity)))    
    ; each slot that is in common is worth a score of 1  
    (loop for slot in gold-dish-slots
          if (and (member slot sol-dish-slots)
                  (similar-entities (slot-value sol-mixture slot)
                                    (slot-value gold-mixture slot)))
            do (add-score mixture-similarity 1 1)
          else
            do (add-score mixture-similarity 0 1))
    ; check if it is the same type of mixture (heterogeneous or homogeneous),
    ; this is very important so it has a big effect on the final score
    (unless (eq (type-of sol-mixture) (type-of gold-mixture))
      (setf (score mixture-similarity) (/ (score mixture-similarity 5))))
    mixture-similarity))

(defmethod compare-dish-ingredient ((sol-ingredient sim-ingredient) (gold-ingredient sim-ingredient))
  (let* ((sol-ing (ingredient sol-ingredient))
         (sol-dish-slots (slot-names-to-compare sol-ing '(id persistent-id)))
         (sol-mixtures (get-mixture-hierarchy sol-ingredient))
         (gold-ing (ingredient gold-ingredient))
         (gold-dish-slots (slot-names-to-compare gold-ing '(id persistent-id)))
         (gold-mixtures (get-mixture-hierarchy gold-ingredient))
         (ingredient-similarity (make-instance 'similarity))
         (hierarchy-similarity (make-instance 'similarity)))
    ; each slot that is in common is worth a score of 1  
    (loop for slot in gold-dish-slots
          if (and (member slot sol-dish-slots)
                  (similar-entities (slot-value sol-ing slot)
                                    (slot-value gold-ing slot)))
            do (add-score ingredient-similarity 1 1)
          else
            do (add-score ingredient-similarity 0 1))
    ; check mixture hierarchy composition
    (loop for gold-mixture in gold-mixtures
          for sol-mixture in sol-mixtures
          do
            (let ((mixture-similarity (compare-mixture sol-mixture gold-mixture)))
              (setf (correct hierarchy-similarity) (+ (correct hierarchy-similarity) (correct mixture-similarity)))
              (setf (total hierarchy-similarity) (+ (total hierarchy-similarity) (total mixture-similarity)))))
    (let* ((difference (abs (- (length gold-mixtures) (length sol-mixtures))))
           (ratio (/ (length gold-mixtures) (+ (length gold-mixtures) difference))))
      (setf (correct hierarchy-similarity) (* ratio (correct hierarchy-similarity))))

    ; compute the final similarity for this ingredient
    (make-instance 'similarity
                   :correct (+ (* 0.6 (correct ingredient-similarity))
                               (* 0.4 (correct hierarchy-similarity)))
                   :total (+ (* 0.6 (total ingredient-similarity))
                             (* 0.4 (total hierarchy-similarity))))))
                   


;(defparameter test (evaluate "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\test.lisp"))
;  (evaluate "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\test.lisp")

(defun print-results (solutions)
  (loop for solution in solutions
        do (print "SOLUTION:")
           (print (recipe-id solution))
           (print "sub percentage:")
           (print (subgoals-percentage solution))
           (print "dish score:")
           (print (dish-score solution))
           (print "time ratio:")
           (print (time-ratio solution))))


;test

;(defun testos (x) (print (time-ratio x)))
;(testos (first test))

;(print-results test)

(defun toppie (x)
  (print "ok"))

;(toppie test)

;; Tests ;;
;;;;;;;;;;;

;(defparameter test1 (evaluate "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\test.lisp"))