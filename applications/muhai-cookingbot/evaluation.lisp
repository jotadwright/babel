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
(defun get-final-value (irl-node)
  (let* ((target-var (second (irl::primitive-under-evaluation irl-node)))
         (list-of-bindings (irl::bindings irl-node))
         (target-binding (find target-var list-of-bindings :key #'var)))
    (when target-binding
      (value target-binding))))

(defun compare-final-values (sol sol-node sim-env)
  (let ((gold-node (solution-node sim-env))
        (gold-target-value (get-final-value (solution-node sim-env)))
        (sol-final-value (get-final-value sol-node)))
    (cond ((eq (type-of gold-target-value) (type-of sol-final-value))
           (compare-dish sol-final-value gold-target-value))
          (t
           (print "something else")))))

;; Comparing classes ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defclass similarity ()
  ((correct :type number :initarg :correct :accessor correct :initform 0)
   (total :type number :initarg :total :accessor total :initform 0))
  (:documentation "Class used to compute similarity of objects."))

(defmethod add-correct ((similarity similarity))
  (setf (correct similarity) (+ (correct similarity) 1))
  (setf (total similarity) (+ (total similarity) 1)))

(defmethod add-score ((similarity similarity) (score number) (possible-score number))
  (setf (correct similarity) (+ (correct similarity) score))
  (setf (total similarity) (+ (total similarity) possible-score)))

(defmethod add-wrong ((similarity similarity))
  (setf (total similarity) (+ (total similarity) 1)))

(defun slot-names-to-compare (item &optional ignore)
  (let* ((classlots  (closer-mop:class-slots (class-of item)))
         (slotnames  (mapcar #'closer-mop:slot-definition-name classlots)))
    (remove-if #'(lambda (slotname) (member slotname ignore))
             slotnames)))

; TODO: replace equal-ontology
(defmethod compare-dish ((sol-dish container) (gold-dish container))
  ; if it's a container we are mostly interested in the main ingredient on it, but we will also check the other attributes of this container
  (let* ((sol-dish-contents (contents sol-dish))
         (gold-dish-contents (contents gold-dish))
         (sol-container-slots (remove 'contents (slot-names-to-compare sol-dish)))
         (gold-container-slots (remove 'contents (slot-names-to-compare gold-dish)))
         (container-similarity (make-instance 'similarity))
         (contents-score 0))
    ; check how similar the container is (less important generally)
    (if (eq (class-of sol-dish) (class-of gold-dish))
      (add-correct container-similarity) ; same kind of container
      (add-wrong container-similarity))
    (loop for slot in gold-container-slots
          if (and (member slot sol-container-slots)
                  (similar-entities (slot-value sol-dish slot) (slot-value gold-dish slot)))
            do (add-correct container-similarity)
          else
            do (add-wrong container-similarity))
    ; check how similar the ingredients is (most important)
    (setf contents-score (compare-dish sol-dish-contents gold-dish-contents))
    (+ (* 0.1 (/ (correct container-similarity) (total container-similarity)))
       (* 0.9 contents-score))))

(defmethod compare-dish ((sol-dish mixture) (gold-dish mixture))
  (let* ((sol-dish-components (components sol-dish))
         (gold-dish-components (components gold-dish))
         (sol-dish-slots (remove 'components (slot-names-to-compare sol-dish)))
         (gold-dish-slots (remove 'components (slot-names-to-compare gold-dish)))
         (dish-similarity (make-instance 'similarity))
         (components-similarity '()))
  (if (eq (class-of sol-dish) (class-of gold-dish))
    (add-score dish-similarity 1 1)
    (add-score dish-similarity 0 1)) ; wrong kind of mixture
  (loop for slot in gold-dish-slots
          if (and (member slot sol-dish-slots)
                  (similar-entities (slot-value sol-dish slot) (slot-value gold-dish slot)))
            do (add-score dish-similarity 1 1)
          else
            do (add-score dish-similarity 0 1))
    ; check how similar the ingredients are (most important)
    (setf components-similarity (compare-dish sol-dish-components gold-dish-components))
    
    (+ (* 0.1 (/ (correct dish-similarity) (total dish-similarity)))
       (* 0.9 components-similarity))))

(defmethod compare-dish ((sol-dish ingredient) (gold-dish ingredient))
  (let* ((sol-dish-slots (slot-names-to-compare sol-dish))
         (gold-dish-slots (slot-names-to-compare gold-dish))
         (dish-similarity (make-instance 'similarity)))
  (cond ((eq (class-of sol-dish) (class-of gold-dish))
         (add-score dish-similarity 10 10)) ; same ingredient
        ; higher score if similar ingredient?
        (t
         (add-score dish-similarity 0 10))) ; other ingredient
  (loop for slot in gold-dish-slots
          if (and (member slot sol-dish-slots)
                  (similar-entities (slot-value sol-dish slot) (slot-value gold-dish slot)))
            do (add-score dish-similarity 1 1) ; each attribute counts as 1
          else
            do (add-score dish-similarity 0 1))
  (/ (correct dish-similarity) (total dish-similarity))))

(defmethod compare-dish ((sol-dish list) (gold-dish list))
  (let ((sol-copy (copy-list sol-dish))
        (sol-items-left (length sol-dish))
        (score 0))
    (loop for gold-item in gold-dish
          for gold-class = (class-of gold-item)
          for sol-item = (find gold-class sol-dish :key #'class-of)
          when sol-item
            do (setf sol-items-left (- sol-items-left 1))
               (setf sol-copy (remove sol-item sol-copy)) ; don't compare the same item twice
               (setf score (+ score (compare-dish sol-item gold-item))))
    (setf score (max (- score (/ sol-items-left (length sol-dish))) 0))
    (/ score (length gold-dish))))

;(defparameter test (evaluate "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\test.lisp"))

;test

 ;; Equal ontology objects ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun similar-entities (entity-1 entity-2 &optional (ignore '(id persistent-id)))
  (equal-ontology-objects entity-1 entity-2 ignore))

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

;; Approach with ingredient unfolding ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; same as a regular ingredient, except it also contains a pointer to the mixture it belongs to
; this pointer is not added to regular ingredients because of infinite loop issues in visualization
(defclass sim-ingredient ()
  ((ingredient :type ingredient :initarg :ingredient :accessor ingredient :initform nil)
   (part-of :initarg :part-of :accessor part-of :initform nil))
  (:documentation "For ingredients in simulation."))

(defmethod unfold-mixture ((mixture-to-unfold mixture))
  (let ((comps (components mixture-to-unfold))
        (unfolded-comps '()))
    (loop for comp in comps
          do
          (cond ((subtypep (type-of comp) 'mixture)
                 (let ((unfolded-sub-comps (unfold-mixture comp)))
                   (nconc unfolded-comps unfolded-sub-comps)))
                ((subtypep (type-of comp) 'ingredient)
                 (push (make-instance 'sim-ingredient
                                      :ingredient comp
                                      :part-of mixture-to-unfold ; kan ook een sim-ingredient zijn?
                       unfolded-comps))
                (t (error "unsupported component of class ~a" (type-of comp)))))
    unfolded-comps)))

(defmethod unfold-dish ((dish container))
  (let* ((items (contents dish))
         (ref-item (copy-object (first items))))
    ; if all items in the container are the same, then just consider it to be one big item (for easier comparison) since portioning is then just one missing step
    (when (loop for item in items
                always (similar-entities ref-item item '(id persistent-id amount)))
       (let ((total-value (loop for item in items
                                for current-value = (value (quantity (amount (convert-to-g item))))
                                sum current-value)))
         (setf (amount ref-item) (make-instance 'amount
                                                :unit (make-instance 'g)
                                                :quantity (make-instance 'quantity :value total-value)))))
    (if (subtypep (type-of ref-item) 'mixture)
      (unfold-mixture ref-item)
      (list (make-instance 'sim-ingredient :ingredient comp)))))

(defun compare-final-values2 (sol sol-node sim-env)
  (let ((gold-node (solution-node sim-env))
        (gold-target-value (get-final-value (solution-node sim-env)))
        (sol-final-value (get-final-value sol-node)))
    (cond ((eq (type-of gold-target-value) (type-of sol-final-value))
           (unfold-dish  gold-target-value))
          (t
           (print "something else")))))

(defun evaluate2 (filepath &optional (sim-envs *simulation-environments*))
  (let ((solutions (read-from-file filepath))
        (results '()))
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
                (push (compare-final-values2 current-solution (first sol-nodes) current-sim-env) results))))
    results))

(defun evaluate3 (filepath &optional (sim-envs *simulation-environments*))
  (let ((solutions (read-from-file filepath))
        (results '()))
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
                (push (evaluate-subgoals (first sol-nodes) gold-node) results))))
    results))

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

(defun check-location-and-similarity 

(defun evaluate-subgoals (sol-node gold-node)
  (let* ((gold-nodes (get-node-sequence gold-node))
         (filtered-gold-nodes (remove-if #'(lambda (node) (eql (get-predicate-name (irl::primitive-under-evaluation node)) 'get-kitchen)) gold-nodes))
         (gold-outputs (mapcar #'get-output-value filtered-gold-nodes))
         (gold-states (mapcar #'get-output-kitchen-state filtered-gold-nodes))
         (gold-location (mapcar #'find-location gold-outputs gold-states))

         (sol-nodes (get-node-sequence sol-node))
         (filtered-sol-nodes (remove-if #'(lambda (node) (eql (get-predicate-name (irl::primitive-under-evaluation node)) 'get-kitchen)) sol-nodes))
         (sol-outputs (mapcar #'get-output-value filtered-sol-nodes))
         (sol-states (mapcar #'get-output-kitchen-state filtered-sol-nodes))
         (sol-location (mapcar #'find-location sol-outputs sol-states))
         
         (goals-reached '())
         (goals-failed '()))
    (loop for gold-output in gold-outputs
          for gold-state in gold-states
          for gold-location in gold-locations
          for sol-output = (find-if #'(lambda (sol-output) (similar-entities gold-output sol-output)) sol-outputs)
          if sol-output
             do
              (setf sol-outputs (remove sol-output sol-outputs))
                (push gold-output goals-reached)
          else do (push gold-output goals-failed))
    (values goals-reached goals-failed)))

(defun evaluate (filepath &optional (sim-envs *simulation-environments*))
  (let ((solutions (read-from-file filepath))
        (results '()))
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
                         (irl::available-at (get-output-binding gold-node)))))))
            
               ; (push (compute-subgoal-percentage current-solution (first sol-nodes) current-sim-env) results))))
    solutions))

;(defparameter test (evaluate "C:\\Users\\robin\\Projects\\babel\\applications\\muhai-cookingbot\\test.lisp"))


(defun print-results (solutions)
  (loop for solution in solutions
        do (print "SOLUTION:")
           (print (recipe-id solution))
           (print "sub percentage:")
           (print (subgoals-percentage solution))
           (print "time ratio:")
           (print (time-ratio solution))))

;test

;(defun testos (x) (print (time-ratio x)))
;(testos (first test))

;(print-results test)
  