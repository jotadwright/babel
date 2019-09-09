(in-package :mwm)

;; ------------------
;; + Configurations +
;; ------------------
(define-configuration-default-value :dot-interval 100)

(defparameter *baseline-clevr-data-path* *clevr-data-path*)
(defparameter *baseline-simulated-data-sets* '("val"))
(defparameter *baseline-extracted-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "scenes" "val-ns-vqa"))
                   cl-user:*babel-corpora*))

(defparameter *cogent-clevr-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-CoGenT"))
                   cl-user:*babel-corpora*))
(defparameter *cogent-simulated-data-sets* '("valA"))
(defparameter *cogent-extracted-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR-CoGenT" "scenes" "valA-ns-vqa"))
                   cl-user:*babel-corpora*))

(defparameter *incremental-clevr-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-incremental"))
                   cl-user:*babel-corpora*))
(defparameter *incremental-simulated-data-sets* '("incr1"))
(defparameter *incremental-extracted-data-path*
  (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-incremental" "scenes" "incr1-ns-vqa"))
                   cl-user:*babel-corpora*))

; :baseline - :cogent - :incremental
(define-configuration-default-value :experiment-type :baseline)
; :simulated - :extracted
(define-configuration-default-value :data-type :simulated)
; data-sets for clevr-world class
(define-configuration-default-value :data-sets (list "val"))
; data-path for additional data
(define-configuration-default-value :data-path
   (merge-pathnames (make-pathname :directory '(:relative "CLEVR" "CLEVR-v1.0" "scenes" "val-ns-vqa"))
                    cl-user:*babel-corpora*))

(define-configuration-default-value :determine-interacting-agents-mode :tutor-speaks)
(define-configuration-default-value :initial-certainty 0.5)
(define-configuration-default-value :certainty-incf 0.1)
(define-configuration-default-value :certainty-decf -0.1)
(define-configuration-default-value :remove-on-lower-bound nil)
(define-configuration-default-value :category-representation :prototype)
(define-configuration-default-value :scale-world t)
(define-configuration-default-value :max-tutor-utterance-length 1)
(define-configuration-default-value :lexical-variation nil)
(define-configuration-default-value :export-lexicon-interval 500)
(define-configuration-default-value :learning-active t)
(define-configuration-default-value :switch-conditions-after-n-interactions nil)

;; --------------
;; + Experiment +
;; --------------
(defclass mwm-experiment (experiment)
  ()
  (:documentation "The experiment class"))

(defmethod initialize-instance :after ((experiment mwm-experiment) &key)
  "Create the population and load the scenes from file"
  (activate-monitor print-a-dot-for-each-interaction)
  ;; set the population
  (setf (population experiment)
        (list (make-tutor-agent experiment)
              (make-learner-agent experiment)))
  ;; set the clevr-data-path
  (setf *clevr-data-path*
        (case (get-configuration experiment :experiment-type)
          (:baseline *baseline-clevr-data-path*)
          (:cogent *cogent-clevr-data-path*)
          (:incremental *incremental-clevr-data-path*)))
  ;; set the world
  (setf (world experiment)
        (make-instance 'clevr-world :data-sets (get-configuration experiment :data-sets)))
  ;; set the data path
  (set-data experiment :data-path (get-configuration experiment :data-path)))

(defmethod learner ((experiment mwm-experiment))
  (find 'learner (population experiment) :key #'id))
(defmethod learner ((interaction interaction))
  (find 'learner (interacting-agents interaction) :key #'id))

(defmethod tutor ((experiment mwm-experiment))
  (find 'tutor (population experiment) :key #'id))
(defmethod tutor ((interaction interaction))
  (find 'tutor (interacting-agents interaction) :key #'id))

;; --------------
;; + Make Table +
;; --------------

(defparameter *words-for-categories*
  '((colors "blue" "green" "yellow" "red" "gray" "cyan" "purple" "brown")
    (shapes "cube" "sphere" "cylinder")
    (sizes "large" "small")
    (materials "rubber" "metal")))

(defun extract-from-lexicon (category agent)
  "Extract all words for the given category from the lexicon"
  (let ((words (rest (assoc category *words-for-categories*))))
    (loop for cxn in (constructions (grammar agent))
          when (member (attr-val cxn :form) words :test #'string=)
          collect cxn)))

(defun best-word (object cxns)
  "determine the best cxn for the object"
  (attr-val
   (the-biggest #'(lambda (cxn)
                    (weighted-similarity object (attr-val cxn :meaning)))
                cxns)
   :form))

(defun build-json-object (color shape size material xpos ypos)
  "create a JSON object from the symbols returned by the lexicon"
  `((:color . ,color) (:shape . ,shape) (:size . ,size)
    (:material . ,material) (:pixel--coords . ,(list xpos ypos))
    (:rotation . 0)))

(defmethod relation-holds-p ((obj-1 mwm-object) (obj-2 mwm-object)
                             (relation (eql :left)))
  "is obj-2 left of obj-1?"
  (< (get-attr-val obj-2 'xpos) (get-attr-val obj-1 'xpos)))
(defmethod relation-holds-p ((obj-1 mwm-object) (obj-2 mwm-object)
                             (relation (eql :right)))
  "is obj-2 right of obj-1?"
  (> (get-attr-val obj-2 'xpos) (get-attr-val obj-1 'xpos)))
(defmethod relation-holds-p ((obj-1 mwm-object) (obj-2 mwm-object)
                             (relation (eql :front)))
  "is obj-2 in front of obj-1?"
  (> (get-attr-val obj-2 'ypos) (get-attr-val obj-1 'ypos)))
(defmethod relation-holds-p ((obj-1 mwm-object) (obj-2 mwm-object)
                             (relation (eql :behind)))
  "is obj-2 behind obj-1?"
  (< (get-attr-val obj-2 'ypos) (get-attr-val obj-1 'ypos)))

(defun build-relationships (objects)
  "create the list of relationships from the x-y-pos of the objects"
  (loop with object = '((:dummy . 0))
        for relation in '(:left :right :front :behind)
        do (push `(,relation ,@(loop for obj-1 in objects
                                     collect (loop for obj-2 in (remove obj-1 objects)
                                                   for i = (position obj-2 objects)
                                                   when (relation-holds-p obj-1 obj-2 relation)
                                                   collect i)))
                 object)
        finally
        (return object)))

(defun build-scene (clevr-scene objects relationships)
  "create a complete scene, identical to the clevr dataset"
  `((:image--index . ,(index clevr-scene))
    (:objects . ,objects)
    (:relationships . ,relationships)
    (:image--filename . ,(mkstr (pathname-name (image clevr-scene))
                                "." (pathname-type (image clevr-scene))))
    (:split . "new")))

(defmethod make-table ((experiment mwm-experiment))
  "After playing a number of interactions,
   use the lexicon to build a table for each scene.
   To do this, we hand-coded which words belong to
   which categories"
  (let ((colors (extract-from-lexicon 'colors (learner experiment)))
        (shapes (extract-from-lexicon 'shapes (learner experiment)))
        (sizes (extract-from-lexicon 'sizes (learner experiment)))
        (materials (extract-from-lexicon 'materials (learner experiment)))
        (nr-of-scenes (length (scenes (world experiment))))
        (i 0))
    (labels ((scene->table (clevr-scene)
               (format t "Processing scene ~a/~a~%" i nr-of-scenes)
               (incf i)
               (let* ((context (clevr->continuous clevr-scene :directory (get-configuration experiment :data-path)))
                      (objects
                       (loop for object in (objects context)
                             collect (build-json-object (best-word object colors)
                                                        (best-word object shapes)
                                                        (best-word object sizes)
                                                        (best-word object materials)
                                                        (get-attr-val object 'xpos)
                                                        (get-attr-val object 'ypos))))
                      (relationships
                       (build-relationships (objects context)))
                      (scene
                       (build-scene clevr-scene objects relationships))
                      (output-path
                       (merge-pathnames
                        (make-pathname :directory '(:relative "scenes" "new")
                                       :name (format nil "CLEVR_new_~a"
                                                     (complete-digits
                                                      (mkstr (index clevr-scene))))
                                       :type "json")
                        *clevr-data-path*)))
                 (ensure-directories-exist output-path)
                 (with-open-file (stream output-path :direction :output)
                   (write-string (encode-json-alist-to-string scene)
                                 stream)))))
      (do-for-scenes (world experiment) #'scene->table))))
                
        

;; --------------------------------
;; + Determine interacting agents +
;; --------------------------------
(defmethod determine-interacting-agents ((experiment mwm-experiment)
                                         (interaction interaction)
                                         (mode (eql :tutor-speaks))
                                         &key &allow-other-keys)
  "The tutor is always the speaker"
  (let ((tutor (find 'tutor (population experiment) :key #'id))
        (learner (find 'learner (population experiment) :key #'id)))
    (setf (interacting-agents interaction)
          (list tutor learner))
    (setf (discourse-role tutor) 'speaker
          (discourse-role learner) 'hearer)
    (notify interacting-agents-determined experiment interaction)))

(defmethod determine-interacting-agents ((experiment mwm-experiment)
                                         (interaction interaction)
                                         (mode (eql :learner-speaks))
                                         &key &allow-other-keys)
  "The learner is always the speaker"
  (let ((tutor (find 'tutor (population experiment) :key #'id))
        (learner (find 'learner (population experiment) :key #'id)))
    (setf (interacting-agents interaction)
          (list tutor learner))
    (setf (discourse-role tutor) 'hearer
          (discourse-role learner) 'speaker)
    (notify interacting-agents-determined experiment interaction)))