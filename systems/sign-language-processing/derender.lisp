(in-package :slp)

(defun read-elan (pathname)
  "Reads in elan-file at pathname and returns it as an xmls-object"
  (with-open-file (stream pathname :external-format :utf-8 :element-type 'cl:character) 
    (xmls::parse stream)))

(defparameter *dominant-hand*
  'RH)

(defclass elan-interval ()
  ((elan-id
    :documentation "The elan-id to refer to this elan-interval"
    :initarg :elan-id
    :accessor elan-id
    :initform nil
    :type symbol)
   (fcg-id
    :documentation "The fcg-id to refer to this elan-interval"
    :initarg :fcg-id
    :accessor fcg-id
    :initform nil
    :type symbol)
   (value
    :documentation "The value for the elan-interval"
    :initarg :value
    :accessor value
    :initform nil
    :type symbol)
  (begin
    :documentation "The begin of the elan-interval"
    :initarg :begin
    :accessor begin
    :initform nil
    :type cons)
  (end
    :documentation "The end of the elan-interval"
    :initarg :end
    :accessor end
    :initform nil
    :type cons)
  (interval-type
    :documentation "The type of the elan-interval"
    :initarg :interval-type
    :accessor interval-type
    :initform nil
    :type symbol)
  (modification
   :documentation "The modification of the elan-interval"
   :initarg :modification
   :accessor modification
   :initform nil
   :type symbol)
  (handshape
   :documentation "The handshape of the elan-interval"
   :initarg :handshape
   :accessor handshape
   :initform nil
   :type symbol)
  (orientation
   :documentation "The orientation of the elan-interval"
   :initarg :orientation
   :accessor orientation
   :initform nil
   :type symbol)
  (movement
   :documentation "The movement of the elan-interval"
   :initarg :movement
   :accessor movement
   :initform nil
   :type symbol)
  (location
   :documentation "The location of the elan-interval"
   :initarg :location
   :accessor location
   :initform nil
   :type symbol)))

(defun retrieve-time-points (xmls)
  "Retrieves the time order constraint nodes from the root xmls node"
  (loop with time-points = (make-hash-table)
        for child in (xmls:node-children xmls)
        do (when (string= (xmls:node-name child) "TIME_ORDER")
             (loop for time-point in (xmls:node-children child)
                   for elan-id = (intern (upcase (second (second (xmls:node-attrs time-point)))) :slp)
                   for value = (parse-integer (second (first (xmls:node-attrs time-point))))
                   do (setf (gethash elan-id time-points) value))
             (return time-points))))

(defun retrieve-intervals (xmls time-points &key (type 'left-hand-articulation))
  "uses the time-points extracted from the xml structure to create intervals for each annotation that can be used to easily search the value using an interval's begin or end point."
  (loop with intervals = '()
        with annotations = (xmls:node-children xmls)
        for node in annotations
        for annotation = (first (xmls:node-children node))
        for time-reference-1 = (intern (upcase (second (second (xmls:node-attrs annotation)))) :slp)
        for time-reference-2 = (intern (upcase (second (first (xmls:node-attrs annotation)))) :slp)
        for value = (intern (upcase (string-replace (first (xmls:node-children (first (xmls:node-children annotation)))) ":" "\:")) :slp)
        do (pushend (make-instance 'elan-interval
                                   :begin (cons time-reference-1 (gethash time-reference-1 time-points))
                                   :end (cons time-reference-2 (gethash time-reference-2 time-points)) 
                                   :elan-id (loop for attribute in (xmls:node-attrs annotation)
                                                  when (string= (first attribute) "ANNOTATION_ID")
                                                    do (return (intern (upcase (second attribute)) :slp)))
                                   :fcg-id (make-const (remove #\\ (format nil "~a" value)))
                                   :interval-type type
                                   :value value) intervals)
        finally (return intervals)))

(defun find-by-elan-id (intervals elan-id)
  "find the interval in a list of intervals that has the specified elan-id"
  (loop for interval in intervals
        when (eql (elan-id interval) elan-id)
          do (return interval)))

(defun retrieve-dominant-meets (manuals)
  "makes meets predicates for all dominant-hand manuals"
  (let ((relations '()))
    (loop with previous-manual = (first manuals)
          for manual in (rest manuals)
          do (pushend `(meets ,(fcg-id previous-manual) ,(fcg-id manual)) relations)
             (setf previous-manual manual))
    relations))

(defun measure-overlap (interval-1 interval-2)
  "measures whether two intervals overlap"
  (max 0 (- (min (cdr (end interval-1)) (cdr (end interval-2))) (max (cdr (begin interval-1)) (cdr (begin interval-2))))))

(defun equal-relaxed (time-1 time-2 error-margin)
  "compares whether the two time points are the same using a error-margin to define"
  (<= (max (- time-1 time-2) (- time-2 time-1)) error-margin))
  

(defun retrieve-alignments (reference-intervals intervals)
  "Makes a set of temporal alignments between articulations on the dominant and non-dominant hand"
  (let ((relations '()))
    (loop for interval in intervals
          do (loop for reference-interval in reference-intervals
                   do (when (> (measure-overlap interval reference-interval) 0)
                        (cond ((and (equal-relaxed (cdr (begin reference-interval)) (cdr (begin interval)) 100)
                                    (equal-relaxed (cdr (end reference-interval)) (cdr (end interval)) 100))
                               (pushend `(coincides-relation ,(fcg-id reference-interval),(fcg-id interval) equals) relations))
                              ((equal-relaxed (cdr (begin reference-interval)) (cdr (begin interval)) 100)
                               (pushend `(coincides-relation ,(fcg-id reference-interval) ,(fcg-id interval) starts) relations))
                              ((equal-relaxed (cdr (end reference-interval)) (cdr (end interval)) 100)
                               (pushend `(coincides-relation ,(fcg-id reference-interval) ,(fcg-id interval) finishes) relations))
                              ((and (> (cdr (begin reference-interval))(cdr (begin interval)))(< (cdr (end reference-interval)) (cdr (end interval))))
                               (pushend `(coincides-relation ,(fcg-id reference-interval) ,(fcg-id interval) during) relations)))
                        )
                      ;(when (equal-relaxed (cdr (end reference-interval)) (cdr (begin interval)) 100)
                        ;(pushend `(meets ,(fcg-id reference-interval),(fcg-id interval)) relations))
          ))
            relations))

(defun make-predicates (intervals)
  "makes a set of predicates from a set of intervals"
  (loop with predicates = '()
        for interval in intervals
        do (pushend `(,(interval-type interval) ,(fcg-id interval) ,(value interval)) predicates)
           (when (modification interval) (pushend `(modification ,(fcg-id interval) ,(modification interval)) predicates))
           (when (location interval) (pushend `(location ,(fcg-id interval) ,(location interval)) predicates))
           (when (handshape interval) (pushend `(handshape ,(fcg-id interval) ,(handshape interval)) predicates))
           (when (orientation interval) (pushend `(orientation ,(fcg-id interval) ,(orientation interval)) predicates))
           (when (movement interval) (pushend `(movement ,(fcg-id interval) ,(movement interval)) predicates))
        finally (return predicates)))


(defun add-modification (modification-tier manual-intervals)
  "adds modifications from the modification tier to the right manual interval"
  (loop with annotations = (xmls:node-children modification-tier)
        for annotation in annotations
        for annotation-ref = (intern (upcase (second (second (xmls:node-attrs (first (xmls:node-children annotation)))))) :slp)
        for annotation-value = (intern (upcase (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children annotation))))))) :slp)
        for ref-interval = (find-by-elan-id manual-intervals annotation-ref)
        do (setf (modification ref-interval) annotation-value)))

(defun add-handshape (handshape-tier manual-intervals)
  "adds handshape from the handshape-tier to the right manual interval"
  (loop with annotations = (xmls:node-children handshape-tier)
        for annotation in annotations
        for annotation-ref = (intern (upcase (second (second (xmls:node-attrs (first (xmls:node-children annotation)))))) :slp)
        for annotation-value = (intern (upcase (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children annotation))))))) :slp)
        for ref-interval = (find-by-elan-id manual-intervals annotation-ref)
        do (setf (handshape ref-interval) annotation-value)))

(defun add-orientation (orientation-tier manual-intervals)
  "adds orientation from the orientation-tier to the right manual interval"
  (loop with annotations = (xmls:node-children orientation-tier)
        for annotation in annotations
        for annotation-ref = (intern (upcase (second (second (xmls:node-attrs (first (xmls:node-children annotation)))))) :slp)
        for annotation-value = (intern (upcase (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children annotation))))))) :slp)
        for ref-interval = (find-by-elan-id manual-intervals annotation-ref)
        do (setf (orientation ref-interval) annotation-value)))

(defun add-location (location-tier manual-intervals)
  "adds location from the location-tier to the right manual-interval"
  (loop with annotations = (xmls:node-children location-tier)
        for annotation in annotations
        for annotation-ref = (intern (upcase (second (second (xmls:node-attrs (first (xmls:node-children annotation)))))) :slp)
        for annotation-value = (intern (upcase (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children annotation))))))):slp)
        for ref-interval = (find-by-elan-id manual-intervals annotation-ref)
        do (setf (location ref-interval) annotation-value)))

(defun add-movement (movement-tier manual-intervals)
  "adds movement from the movement-tier to the right manual-interval"
  (loop with annotations = (xmls:node-children movement-tier)
        for annotation in annotations
        for annotation-ref = (intern (upcase (second (second (xmls:node-attrs (first (xmls:node-children annotation)))))) :slp)
        for annotation-value = (intern (upcase (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children annotation))))))) :slp)
        for ref-interval = (find-by-elan-id manual-intervals annotation-ref)
        do (setf (movement ref-interval) annotation-value)))
  
(defun dominant-hand-tier? (tier-name)
  (let ((dominant-hand *dominant-hand*)
        (tier-hand (intern (upcase (first (split-sequence::split-sequence #\- (string tier-name)))) :slp)))
    (eql dominant-hand tier-hand)))

(defun type-from-tier-id (tier-id)
  (intern (upcase (first (last (split-sequence::split-sequence #\- (string tier-id))))):slp))

(defun xmls->predicates (xmls)
  "transforms an xmls structure to a set of predicates"
  (let* ((time-points (retrieve-time-points xmls))
        (dominant-intervals '())
        (non-dominant-intervals '())
        (alignments '())
        (dominant-hand-articulation (if (eql *dominant-hand* 'RH)
                                      'right-hand-articulation
                                      'left-hand-articulation))
        (non-dominant-hand-articulation (if (eql 'right-hand-articulation dominant-hand-articulation)
                                          'left-hand-articulation
                                          'right-hand-articulation)))
    (loop for child in (xmls:node-children xmls)
          for tier-id = (loop for attribute in (xmls:node-attrs child)
                              when (string= (first attribute) "TIER_ID")
                                do (return (intern (upcase (string-replace (second attribute) " " "-")) :slp)))
          do (when (member tier-id (list 'LH-SEGMENTATION 'RH-SEGMENTATION 'LH-LOCATION 'LH-HANDSHAPE 'LH-ORIENTATION 'LH-MODIFICATION 'LH-MOVEMENT 'RH-LOCATION 'RH-HANDSHAPE 'RH-ORIENTATION 'RH-MODIFICATION 'RH-MOVEMENT))
               (cond
                 ((and (dominant-hand-tier? tier-id) (eql (type-from-tier-id tier-id) 'segmentation))
                  (setf dominant-intervals (retrieve-intervals child time-points :type dominant-hand-articulation)))
                 ((and (NOT (dominant-hand-tier? tier-id)) (eql (type-from-tier-id tier-id) 'segmentation))
                  (setf non-dominant-intervals (retrieve-intervals child time-points :type non-dominant-hand-articulation)))
                 ((and (dominant-hand-tier? tier-id) (eql(type-from-tier-id tier-id) 'location))
                  (add-location child dominant-intervals))
                 ((and (dominant-hand-tier? tier-id) (eql (type-from-tier-id tier-id) 'handshape))
                  (add-handshape child dominant-intervals))
                 ((and (dominant-hand-tier? tier-id) (eql (type-from-tier-id tier-id) 'orientation))
                  (add-orientation child dominant-intervals))
                 ((and (dominant-hand-tier? tier-id) (eql (type-from-tier-id tier-id) 'movement))
                  (add-movement child dominant-intervals))
                 ((and (dominant-hand-tier? tier-id) (eql (type-from-tier-id tier-id) 'modification))
                  (add-modification child dominant-intervals))
                 ((and (NOT (dominant-hand-tier? tier-id)) (eql (type-from-tier-id tier-id) 'location))
                  (add-location child non-dominant-intervals))
                 ((and (NOT (dominant-hand-tier? tier-id)) (eql (type-from-tier-id tier-id) 'handshape))
                  (add-handshape child non-dominant-intervals))
                 ((and (NOT (dominant-hand-tier? tier-id)) (eql (type-from-tier-id tier-id) 'orientation))
                  (add-orientation child non-dominant-intervals))
                 ((and (NOT (dominant-hand-tier? tier-id)) (eql (type-from-tier-id tier-id) 'movement))
                  (add-movement child non-dominant-intervals))
                 ((and (NOT (dominant-hand-tier? tier-id)) (eql (type-from-tier-id tier-id) 'modification))
                  (add-modification child non-dominant-intervals)))))
    (setf alignments (append (retrieve-dominant-meets dominant-intervals)
                             (retrieve-alignments dominant-intervals non-dominant-intervals)))
    (append (make-predicates dominant-intervals)
            (make-predicates non-dominant-intervals)
            alignments)))


(defmethod de-render ((predicates list) (mode (eql :set-of-predicates)) &key &allow-other-keys)
  "De-renders a set of predicates into a transient structure containing a collection of form predicates"
    (make-instance 'coupled-feature-structure 
                   :left-pole `((root (form ,predicates)))
                   :right-pole `((root))))

(define-event-handler (trace-fcg parse-started)
  (let* ((visualisation (represent-signs utterance)))
    (add-element `((h3) ,(format nil "Comprehending the following signed utterance:")))
    (add-element visualisation)))

