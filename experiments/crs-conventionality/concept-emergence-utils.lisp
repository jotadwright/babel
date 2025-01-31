(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                            ;;
;; !! Minor differences with :cle package, need to be generalised, this file is just for development purposes ;;
;;                                                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; FIRST RESTORE THE EXPERIMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf *restored-experiment*
        (cl-store:restore (babel-pathname :directory '("experiments"
                                                       "crs-conventionality")
                                          :name "demo-experiment"
                                          :type "store")))

;; CLASSES ;;
;;;;;;;;;;;;;


(defclass concept-emergence-experiment (crs-conventionality-experiment)
  ()
  (:documentation "Class for naming game experiment."))

(defmethod initialize-instance :after ((experiment concept-emergence-experiment) &key &allow-other-keys)
  "Creates the population and world of the experiment."
  ;; Set population
  (setf (population experiment)
        (make-instance 'concept-emergence-population :experiment experiment))
  ;; Set world
  (setf (world experiment)
        (make-instance 'concept-emergence-world :experiment experiment)))

(defclass concept-emergence-population (crs-conventionality-population)
  ()
  (:documentation "A population in the concept emergence game."))

(defmethod initialize-instance :after ((population concept-emergence-population) &key &allow-other-keys)
  "Creates the population of the experiment."
  ;; Set population
  (setf (agents population)
        (loop for i from 1 to (get-configuration (experiment population) :nr-of-agents-in-population)
              collect (make-instance 'concept-emergence-agent
                                     :id (intern (format nil "AGENT-~a" i))
                                     :population population))))

(defclass concept-emergence-agent (crs-conventionality-agent)
  ()
  (:documentation "An agent in the experiment"))



 
(defclass concept-emergence-entity (crs-conventionality-entity)
  ((features
    :documentation "The features of the object (a-list)."
    :type hash-table :accessor features :initarg :features)
   (description
    :documentation "Symbolic description of the original object."
    :type list :accessor description :initarg :description)))

(defclass concept-emergence-world (crs-conventionality-world)
  ()
  (:documentation "The concept emergence world."))


(defmethod initialize-instance :after ((world concept-emergence-world) &key &allow-other-keys)
  "Sets the entities of the world."
 (let ((entities (get-clevr-entities-from-files)))
   (setf (entities world)
         (random-elts entities (get-configuration (experiment world) :nr-of-entities-in-world)))))



(defmethod interact ((experiment concept-emergence-experiment)
                     (interaction interaction) &key)
  "Defines a single interaction/game in the naming game experiment."
  (let ((speaker (speaker interaction))
        (hearer (hearer interaction))
        (scene (scene interaction))
        (topic (topic interaction)))
    
    ;; The speaker conceptualizes the topic into a meaning representation
    (conceptualise-and-produce speaker scene topic)
    #|
    ;; 3) The speaker formulates the meaning
    (run-formulation speaker (get-data speaker :meaning))
    ;; --- The speaker passes on the formulated utterance to the hearer
    (set-data hearer :utterance (get-data speaker :utterance))
    ;(notify utterance-passed (get-data speaker :utterance))
    ;; 4) The hearer comprehends the utterance
    (run-comprehension hearer (get-data hearer :utterance))
    ;; 5) The hearer interprets the meaning in the scene
    (interpret hearer (get-data hearer :meaning) (get-data hearer :scene))
    ;; 6) Compare intended topic of speaker and interpreted topic of hearer
    ;;    and set communicated-succesfully for speaker, hearer and interaction
    (compare-topics speaker hearer interaction)
    ;; 7) If not succesful, then learning phase for hearer
    (unless (communicated-successfully hearer)
      (run-learning hearer (get-data speaker :topic) (get-data hearer :cipn)))
    ;; 8) Speaker and hearer update the scores of the constructions used and their competitors
    (align-agent speaker (get-configuration experiment :alignment))
    (align-agent hearer (get-configuration experiment :alignment))
    ;; Finishing interaction
    (finish-interaction experiment interaction)
    |#
    ))

;; GRAMMAR ;;
;;;;;;;;;;;;;

(defmethod make-initial-grammar ((agent concept-emergence-agent))
  "Initialises the grammar of a concept emergence agent. For now, this restores concepts from a previously learned experiment."
  (let* ((grammar-name (make-const "concept-grammar"))
         (cxns (loop for cxn in (concepts-to-fcg-cxns (get-concepts *restored-experiment*))
                     collect cxn)))
    (setf (grammar agent)
          (eval
           `(def-fcg-constructions ,grammar-name
                                   :hashed t
                                   :cxn-inventory ,grammar-name
                                   :feature-types ((meaning set)
                                                   (form set))
                                   :fcg-configurations (;; Rendering and de-rendering
                                                        (:de-render-mode . :de-render-raw)
                                                        (:render-mode . :render-raw)
                                                        (:create-initial-structure-mode . :topic-and-scene)
                                                        ;; Construction supplier and search
                                                        (:construction-inventory-processor-mode . :heuristic-search)
                                                        (:node-expansion-mode . :full-expansion)
                                                        (:cxn-supplier-mode . :hashed)
                                                        ;; for using heuristics
                                                        (:search-algorithm . :best-first)
                                                        (:heuristics :cxn-score)
                                                        (:heuristic-value-mode . :sum-heuristics-and-parent) 
                                                        ;; goal tests
                                                        (:parse-goal-tests :no-sequence-in-root)
                                                        (:production-goal-tests :no-meaning-in-root))
                                   ,@cxns)))))

;; ENTITIES AND FEATURE VECTORS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-clevr-entities-from-files ()
  (let* ((pathname (merge-pathnames (make-pathname :directory `(:relative "concept-emergence2" "clevr" "scenes" "train"))
                                    cl-user:*babel-corpora*))
         (files (sort (directory (make-pathname :directory (pathname-directory pathname) :name :wild :type "json")) #'string< :key #'namestring))
         
         (features (get-feature-vectors))
         (entities (loop for file in files
                         for json-as-alist = (cl-jonathan:decode-json-as-alist-from-source file)
                         for objects = (rest (assoc :objects json-as-alist))
                         append (loop for obj in objects
                                       collect (make-instance 'concept-emergence-entity
                                                              :features (filter-object (intern-alist (rest (assoc :attributes obj))) features)
                                                              :description (rest (assoc :description obj)))))))

    entities))

(defun get-feature-vectors ()
  (let* ((info-path (merge-pathnames (make-pathname :directory `(:relative "concept-emergence2" "-info")
                                                    :name "clevr"
                                                    :type "csv")
                                     cl-user:*babel-corpora*))
         (csv (read-csv info-path)))
    (loop for row in csv
          for channel = (intern (upcase (first row)))
          collect channel)))

;(get-feature-vectors)
;(setf *entities* (get-clevr-entities-from-files))

;; UTILS ;;
;;;;;;;;;;;

(defun intern-alist (alist)
  (mapcar #'(lambda (pair)
              (cons (intern (upcase (mkstr (car pair))) :crs-conventionality)
                    (cdr pair)))
          alist))

(defun filter-object (object feature-set)
  "Only keep the attributes that are in play."
  (loop with hash-table = (make-hash-table)
        for channel in feature-set
        do (setf (gethash channel hash-table) (assqv channel object))
        finally (return hash-table)))

(defun read-csv (fpath)
  "Reads a CSV file and returns a list of lists."
  (unless (probe-file fpath)
    (error "Could not find the file ~%~a" fpath))
  (with-open-file (stream fpath)
    (loop with skipped-header = nil
          for line = (read-line stream nil)
          while line
          for row = (split-sequence:split-sequence #\, line)
          if (not skipped-header)
            do (setf skipped-header t)
          else
            collect row)))

(defun parse-keyword (string)
  (intern (string-upcase (string-left-trim ":" string)) :keyword))

;; RESTORE EXPERIMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-concepts (experiment)
  (let ((fast-inventory (cle::fast-inventory (cle::lexicon (first (experiment-framework::agents experiment))))))
    (loop for h being the hash-keys of fast-inventory
          collect (gethash h fast-inventory))))


(defun concepts-to-fcg-cxns (concepts)
  (loop for concept in concepts
        for form = (cle::form concept)
        for meaning = (cle::meaning concept)
        for score = (cle::score concept)
        for cxn-name = (intern (upcase (format nil "~a-cxn" form)) :fcg)
        for unit-name = (intern (upcase (format nil "?~a-unit" form)) :fcg)
        collect `(def-fcg-cxn ,cxn-name
                              (<-
                               (,unit-name
                                (HASH topic ,meaning)
                                --
                                (HASH form (,form)))))))



;(first (concepts-to-fcg-cxns (get-concepts *restored-experiment*)))

