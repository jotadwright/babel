(in-package :mwm)

(export '(concept))

(defclass concept ()
  ((form
    :documentation "the word of the concept"
    :accessor form :initarg :form :type string)
   (meaning
    :documentation "a list of prototypes"
    :accessor meaning :initarg :meaning :type list))
  (:documentation "a concept, or lexical item"))

(defun make-concept (form attribute-proto-cons initial-certainty)
  (make-instance 'concept
                 :form form
                 :meaning (loop for (attribute . proto-value) in attribute-proto-cons
                                collect (make-prototype attribute proto-value
                                                        initial-certainty))))


(defmethod copy-object-content ((source entity) (destination entity))
  (setf (id destination) (make-id (get-base-name (mkstr (id source))))))


(defmethod copy-object-content ((source concept) (destination concept))
  (setf (form destination) (form source))
  (setf (meaning destination)
        (loop for p in (meaning source)
              collect (my-copy-object p :concept destination))))


(defclass prototype (entity)
  ((attribute
    :documentation "the name of the attribute"
    :accessor attribute :initarg :attribute :type symbol)
   (value
    :documentation "the prototypical value"
    :accessor value :initarg :value :type number)
   (certainty
    :documentation "the certainty of the attribute"
    :accessor certainty :initarg :certainty :type number)
   (nr-samples
    :accessor nr-samples :initarg :nr-samples :initform 0)
   (M2
    :accessor M2 :initarg :M2 :initform nil)
   (concept
    :documentation "a pointer back to the entire concept"
    :accessor concept :initarg :concept :initform nil))
  (:documentation "Prototype category"))

(defmethod copy-object-content ((source prototype) (destination prototype))
  (setf (attribute destination) (attribute source)
        (value destination) (value source)
        (certainty destination) (certainty source)
        (M2 destination) (M2 source)
        (nr-samples destination) (nr-samples source)
        (concept destination) (concept source)))


(defmethod my-copy-object ((p prototype) &key concept)
  (make-instance 'prototype
                 :id (make-id (get-base-name (mkstr (id p))))
                 :attribute (attribute p) :value (value p)
                 :certainty (certainty p) :nr-samples (nr-samples p)
                 :M2 (M2 p) :concept concept))


(defun make-prototype (attribute prototypical-value initial-certainty)
  (make-instance 'prototype
                 :attribute attribute
                 :value prototypical-value
                 :certainty initial-certainty
                 :nr-samples 1
                 :M2 0.05))


(defgeneric update-prototype (prototype object)
  (:documentation "Update the category based on the object"))

(defmethod update-prototype ((prototype prototype)
                            (object mwm-object))
  ;; take the object pointed to by the tutor
  ;; and estimate the mean and variance of the category
  (incf (nr-samples prototype))
  (let* ((exemplar (get-attr-val object (attribute prototype)))
         (delta-1 (- exemplar (value prototype)))
         (new-prototypical-value (+ (value prototype) (/ delta-1 (nr-samples prototype))))
         (delta-2 (- exemplar new-prototypical-value))
         (new-M2 (+ (M2 prototype) (* delta-1 delta-2))))
    (setf (value prototype) new-prototypical-value
          (M2 prototype) new-M2)
    prototype))


(defgeneric weighted-similarity (object concept)
  (:documentation "Compute the weighted similarity between an object and a concept"))

(defmethod weighted-similarity ((object mwm-object) (concept concept))
  (loop for prototype in (meaning concept)
        for similarity = (similarity object prototype)
        collect (* (certainty prototype) similarity) into weighted-similarities
        finally (return (average weighted-similarities))))

(defmethod weighted-similarity ((object mwm-object) (concept mwm-evaluation::concept-entity))
  (loop for prototype in (meaning concept)
        for similarity = (similarity object prototype)
        collect (* (mwm::certainty prototype) similarity) into weighted-similarities
        finally (return (average weighted-similarities))))


(defgeneric similarity (object prototype)
  (:documentation "Similarity on the level of a single prototype"))

(defmethod similarity ((object mwm-object) (prototype prototype))
  (let* ((max-z-score 2)
         (exemplar (get-attr-val object (attribute prototype)))
         (stdev (sqrt (/ (M2 prototype) (nr-samples prototype))))
         (z-score (abs (/ (- exemplar (value prototype)) stdev))))
    (max (/ (+ (- z-score) max-z-score) max-z-score) -1)))


(define-event attribute-removed (concept concept) (attribute symbol))
(define-event concept-removed (concept concept))

(defun adjust-certainty (agent concept attribute delta
                               &key (upper-bound 1.0)
                               (lower-bound 0.0)
                               (remove-on-lower-bound t))
  (let ((prototype (find attribute (meaning concept) :key #'attribute)))
    ;; update the certainty
    (setf (certainty prototype)
          (+ (certainty prototype) delta))
    ;; check the boundaries
    (when (> (certainty prototype) upper-bound)
      (setf (certainty prototype) upper-bound))
    (when (<= (certainty prototype) lower-bound)
      (if remove-on-lower-bound
        (progn (notify attribute-removed concept attribute)
          (setf (meaning concept)
                (remove prototype (meaning concept))))
        (setf (certainty prototype) lower-bound)))
    ;; check if the concept is empty
    (when (null (meaning concept))
      (notify concept-removed concept)
      (setf (lexicon agent)
            (remove concept (lexicon agent))))))



;;;; concept -> s-dot
(defgeneric concept->s-dot (concept &key highlight-green highlight-red)
  (:documentation "Display a concept using s-dot"))

(defmethod concept->s-dot ((concept concept) &key highlight-green highlight-red)
  (let ((g '(((s-dot::ranksep "0.3")
              (s-dot::nodesep "0.5")
              (s-dot::margin "0")
              (s-dot::rankdir "LR"))
             s-dot::graph)))
    ;; the form
    (push
     `(s-dot::record  
       ((s-dot::style "solid")
        (s-dot::fontsize "9.5")
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id ,(downcase (mkdotstr (form concept))))
                     (s-dot::label ,(downcase (mkdotstr (form concept))))
                     (s-dot::fontcolor "#AA0000"))))
     g)
    ;; the meaning
    (loop for prototype in (meaning concept)
          for record = (prototype->s-dot prototype
                                         :green (member (attribute prototype) highlight-green)
                                         :red (member (attribute prototype) highlight-red))
          when (> (certainty prototype) 0.0)
          do (push record g))
    ;; the edges
    (loop for prototype in (meaning concept)
          when (> (certainty prototype) 0.0)
          do (push
              `(s-dot::edge
                ((s-dot::from ,(downcase (mkdotstr (form concept))))
                 (s-dot::to ,(mkdotstr (downcase (attribute prototype))))
                 (s-dot::label ,(format nil "~,2f" (certainty prototype)))
                 (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                       #-(or :win32 :windows) "Arial")
                 (s-dot::fontsize "8.5")
                 (s-dot::arrowsize "0.5")
                 (s-dot::style ,(if (= (certainty prototype) 1.0) "solid" "dashed"))))
              g))
    ;; return
    (reverse g)))


(defgeneric prototype->s-dot (prototype &key green red)
  (:documentation "Display a prototype using s-dot"))

(defmethod prototype->s-dot ((prototype prototype) &key green red)
  (let* ((stdev (sqrt (/ (M2 prototype) (nr-samples prototype))))
         (lower-bound (- (value prototype) (* 2 stdev)))
         (upper-bound (+ (value prototype) (* 2 stdev)))
         (record-properties
          (cond (green '((s-dot::style "filled")
                         (s-dot::fillcolor "#AAFFAA")))
                (red '((s-dot::style "filled")
                       (s-dot::fillcolor "#AA0000")
                       (s-dot::fontcolor "#FFFFFF")))
                (t (if (= (certainty prototype) 1.0)
                     '((s-dot::style "solid"))
                     '((s-dot::style "dashed")))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(downcase (mkdotstr (attribute prototype))))
                    (s-dot::label ,(format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkdotstr (attribute prototype)))
                                           (value prototype) lower-bound upper-bound)))))))




