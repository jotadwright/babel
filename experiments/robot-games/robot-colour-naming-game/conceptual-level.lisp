(in-package :gcng)

;; --------------------
;; + Color Categories +
;; --------------------

(defun category->irl-program (category var)
  `((filter-by-closest-color ?topic ?context ,var)
    (get-context ?context)
    (bind color-category ,var ,(id category))))

(defmethod distance ((object sensory-object) (category color-category))
  (euclidean (lab-color object) (value category)))

(defun find-best-category (object categories)
  (reduce #'(lambda (cat1 cat2)
              (if (< (distance object cat1)
                     (distance object cat2))
                     cat1 cat2))
          categories))

;; --------------
;; + Primitives +
;; --------------

;;; get-context
(defprimitive get-context ((context sensory-object-set))
  ((context =>)
   (equal-entity (get-data ontology 'context) context))
  ((=> context)
   (bind (context 1.0 (get-data ontology 'context)))))

;;; filter-by-closest-color
(defun filter-by-closest-color (object-set color-category all-color-categories)
  "for every object, compute the closest color category
   take out the objects of which this category equals color-category
   if multiple remain, take the one with the smallest distance"
  (let ((filtered-objects
         (loop for object in (entities object-set)
               for (closest-category . distance) = (loop with nearest = nil
                                                         with distance = nil
                                                         for color in all-color-categories
                                                         for d = (distance object color)
                                                         when (or (null nearest)
                                                                  (< d distance))
                                                         do (setf nearest color
                                                                  distance d)
                                                         finally
                                                         (return (cons nearest distance)))
               when (equal-entity closest-category color-category)
               collect (cons object distance))))
    (when filtered-objects
      (car (the-smallest #'cdr filtered-objects)))))

(defprimitive filter-by-closest-color ((closest-entity sensory-object)
                                       (source-set sensory-object-set)
                                       (color-category color-category))
  ;; case 1
  ((source-set color-category => closest-entity)
   (let ((closest-object (filter-by-closest-color source-set color-category (get-data ontology 'color-categories))))
     (when closest-object (bind (closest-entity 1.0 closest-object)))))
  ;; case 2
  ((source-set closest-entity => color-category)
   (let ((computed-category
          (find-if #'(lambda (color)
                       (equal-entity
                        closest-entity
                        (filter-by-closest-color source-set color (get-data ontology 'color-categories))))
                   (get-data ontology 'color-categories))))
     (when computed-category (bind (color-category 1.0 computed-category)))))
  ;; case 3
  ((source-set => closest-entity color-category)
   (loop for color in (get-data ontology 'color-categories)
         for computed-object = (filter-by-closest-color source-set color (get-data ontology 'color-categories))
         if computed-object
         do (bind (color-category 1.0 color) (closest-entity 1.0 computed-object))))
  ;; case 4
  ((source-set closest-entity color-category =>)
   (equal-entity closest-entity (filter-by-closest-color source-set color-category (get-data ontology 'color-categories)))))

;; -----------------------
;; + Create New Category +
;; -----------------------

(define-event new-category-created (category color-category))

(defmethod create-new-category ((agent grounded-color-naming-game-agent)
                                (topic sensory-object))
  "Create a new color category based on the topic"
  (let ((new-category (make-color-category topic)))
    (push-data (ontology agent) 'color-categories new-category)
    (notify new-category-created new-category)
    (unless (get-configuration agent :silent)
      (speak (robot agent) "I created a new color category"))
    new-category))

;; ---------------------
;; + Conceptualisation +
;; ---------------------

(defclass my-composer (single-topic-composer) ())

(defun make-default-composer (agent topic)
  (make-instance 'my-composer
                 :topic topic
                 :initial-chunk (make-instance 'chunk :id 'initial
                                               :target-var '(?topic . sensory-object)
                                               :open-vars '((?topic . sensory-object)))
                 :chunks (create-chunks-from-primitives (get-data (ontology agent) 'primitives))
                 :max-search-depth 4
                 :ontology (ontology agent)))

(define-event conceptualisation-started)
(define-event conceptualisation-failed)
(define-event conceptualisation-succeeded (category color-category)
  (irl-program list))

(defun get-applied-cat (agent composer-solution)
  "Find the applied color category from the composer solution"
  (let ((category-id (last-elt (first (bind-statements composer-solution)))))
    (find category-id (get-data (ontology agent) 'color-categories) :key #'id)))

(defmethod conceptualise ((agent grounded-color-naming-game-agent)
                          (topic sensory-object)
                          (context sensory-object-set))
  "Find a discriminating color category for the topic"
  (declare (ignorable context))
  (notify conceptualisation-started)
  (let* ((composer (make-default-composer agent topic))
         (solutions (get-next-solutions composer)))
    (if (and solutions (length= solutions 1))
      (let ((solution (first solutions)))
        (setf (irl-program agent) (append (irl-program (chunk solution))
                                            (bind-statements solution)))
        (setf (applied-category agent) (get-applied-cat agent solution))
        (notify conceptualisation-succeeded (applied-category agent) (irl-program agent))
        (unless (get-configuration agent :silent)
          (speak (robot agent) "I conceptualised the topic")))
      (progn (notify conceptualisation-failed)
        (unless (get-configuration agent :silent)
          (speak (robot agent) "I could not conceptualise the topic"))
        (create-new-category agent topic)
        (conceptualise agent topic context))))
  (applied-category agent))


;; ------------------
;; + Interpretation +
;; ------------------

(define-event interpretation-started (category color-category))
(define-event interpretation-succeeded (topic sensory-object))
(define-event interpretation-failed)

(defmethod interpret ((agent grounded-color-naming-game-agent)
                      (applied-cxn fcg-construction))
  "Interpret the utterance in the context"
  (when (applied-cxn agent)
    (let* ((bind-statement (find 'bind (irl-program agent) :key #'first))
           (color-category-id (last-elt bind-statement))
           (color-category (find color-category-id
                                 (get-data (ontology agent) 'color-categories)
                                 :key #'id))
           (solutions (evaluate-irl-program (irl-program agent) (ontology agent))))
      (notify interpretation-started color-category)
      (when (and solutions (length= solutions 1))
        (let* ((target-var (get-target-var (irl-program agent)))
               (solution (first solutions))
               (topic (value (find target-var solution :key #'var))))
          (unless (get-configuration agent :silent)
            (speak (robot agent) "I found a topic"))
          (setf (topic agent) topic
                (applied-category agent) color-category))))
    (if (topic agent)
      (notify interpretation-succeeded (topic agent))
      (notify interpretation-failed)))
  (topic agent))