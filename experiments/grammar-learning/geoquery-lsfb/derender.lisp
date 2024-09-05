(in-package :geoquery-lsfb)

(defun read-xml (pathname)
  "Reads in elan-file at pathname and returns it as an xmls-object"
  (with-open-file (stream pathname :external-format :utf-8 :element-type 'cl:character) 
    (xmls::parse stream)))

(defparameter *dominant-hand*
  'RH)

;(concatenate 'string hex-string "&#x" hex ";")

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
  (hamnosys
   :documentation "The hamnosys of the elan-interval"
   :initarg :hamnosys
   :accessor hamnosys
   :initform nil
   :type string)))

(defun retrieve-time-points (xmls)
  "Retrieves the time order constraint nodes from the root xmls node"
  (loop with time-points = (make-hash-table)
        for child in (xmls:node-children xmls)
        do (when (string= (xmls:node-name child) "TIME_ORDER")
             (loop for time-point in (xmls:node-children child)
                   for elan-id = (read-from-string (upcase (second (second (xmls:node-attrs time-point)))))
                   for value = (parse-integer (second (first (xmls:node-attrs time-point))))
                   do (setf (gethash elan-id time-points) value))
             (return time-points))))

(defun dominant-hand-tier? (tier-name)
  (let ((dominant-hand *dominant-hand*)
        (tier-hand (read-from-string (upcase (first (split-sequence::split-sequence #\- (string tier-name)))))))
    (eql dominant-hand tier-hand)))

(defun type-from-tier-id (tier-id)
  (read-from-string (upcase (first (last (split-sequence::split-sequence #\- (string tier-id)))))))

(defun retrieve-intervals (xmls time-points &key (type 'left-hand-articulation))
  "uses the time-points extracted from the xml structure to create intervals for each annotation that can be used to easily search the value using an interval's begin or end point."
  (loop with intervals = '()
        with annotations = (xmls:node-children xmls)
        for node in annotations
        for annotation = (first (xmls:node-children node))
        for time-reference-1 = (read-from-string (upcase (second (second (xmls:node-attrs annotation)))))
        for time-reference-2 = (read-from-string (upcase (second (first (xmls:node-attrs annotation)))))
        for value = (read-from-string (upcase (string-replace (first (xmls:node-children (first (xmls:node-children annotation)))) ":" "\\:")))
        do (pushend (make-instance 'elan-interval
                                   :begin (cons time-reference-1 (gethash time-reference-1 time-points))
                                   :end (cons time-reference-2 (gethash time-reference-2 time-points)) 
                                   :elan-id (loop for attribute in (xmls:node-attrs annotation)
                                                  when (string= (first attribute) "ANNOTATION_ID")
                                                    do (return (read-from-string (upcase (second attribute)))))
                                   :fcg-id (make-const (remove #\\ (format nil "~a" value)))
                                   :interval-type type
                                   :value value) intervals)
        finally (return intervals)))     

(defun add-hamnosys (hamnosys-tier manual-intervals)
  "adds hamnosys from the hamnosys tier to the right manual interval"
  (loop with annotations = (xmls:node-children hamnosys-tier)
        for annotation in annotations
        for annotation-ref = (read-from-string (upcase (second (first (xmls:node-attrs (first (xmls:node-children annotation)))))))
        for annotation-value = (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children annotation))))))
        for ref-interval = (find-by-elan-id manual-intervals annotation-ref)
        do (setf (hamnosys ref-interval) annotation-value)))

(defun find-by-elan-id (intervals elan-id)
  "find the interval in a list of intervals that has the specified elan-id"
  (loop for interval in intervals
        when (eql (elan-id interval) elan-id)
          do (return interval)))

(defun retrieve-dominant-adjacent (manuals)
  "makes adjacent predicates for all dominant-hand manuals"
  (let ((relations '()))
    (loop with previous-manual = (first manuals)
          for manual in (rest manuals)
          do (when (equal-relaxed (cdr (begin manual)) (cdr (end previous-manual)) 100)
               (pushend `(adjacent ,(fcg-id previous-manual) ,(fcg-id manual)) relations))
               (setf previous-manual manual))
    relations))

(defun measure-overlap (interval-1 interval-2)
  "measures whether two intervals overlap"
  (max 0 (- (min (cdr (end interval-1)) (cdr (end interval-2))) (max (cdr (begin interval-1)) (cdr (begin interval-2))))))

(defun equal-relaxed (time-1 time-2 error-margin)
  "compares whether the two time points are the same using a error-margin to define"
  (<= (max (- time-1 time-2) (- time-2 time-1)) error-margin))

(defun find-preceding-dominant-interval (interval reference-intervals)
  (loop for reference-interval in reference-intervals
        do (when (equal-relaxed (cdr (end reference-interval)) (cdr (begin interval)) 100)
             (return reference-interval))))

(defun find-following-dominant-interval (interval reference-intervals)
  (loop for reference-interval in reference-intervals
        do (when (equal-relaxed (cdr (begin reference-interval)) (cdr (end interval)) 100)
             (return reference-interval))))

(defun retrieve-alignments (reference-intervals intervals)
  "Makes a set of temporal alignments between articulations on the dominant and non-dominant hand"
  (let ((relations '()))
    (loop for interval in intervals
          for preceding-dominant-interval = nil
          for following-dominant-interval = nil
          do (loop for reference-interval in reference-intervals
                   do (when (> (measure-overlap interval reference-interval) 0)
                        (cond ((member (char-name (char (hamnosys reference-interval) 0)) (list "U+E0E9" "U+E0E8") :test #'string=)
                               (setf (interval-type reference-interval) 'two-hand-articulation)
                               (setf (interval-type interval) nil))
                              ((and (equal-relaxed (cdr (begin reference-interval)) (cdr (begin interval)) 100)
                                    (equal-relaxed (cdr (end reference-interval)) (cdr (end interval)) 100))
                               (pushend `(start-coincides ,(fcg-id reference-interval),(fcg-id interval)) relations)
                               (pushend `(end-coincides ,(fcg-id reference-interval),(fcg-id interval)) relations))
                              ((equal-relaxed (cdr (begin reference-interval)) (cdr (begin interval)) 100)
                               (pushend `(start-coincides ,(fcg-id reference-interval) ,(fcg-id interval)) relations))
                              ((equal-relaxed (cdr (end reference-interval)) (cdr (end interval)) 100)
                               (pushend `(end-coincides ,(fcg-id reference-interval) ,(fcg-id interval)) relations))
                              ((and (> (cdr (begin reference-interval))(cdr (begin interval)))(< (cdr (end reference-interval)) (cdr (end interval))))
                               (pushend `(during ,(fcg-id reference-interval) ,(fcg-id interval)) relations))
                              ))
                      
          )
             (when (interval-type interval)
               (setf preceding-dominant-interval (find-preceding-dominant-interval interval reference-intervals))
               (setf following-dominant-interval (find-following-dominant-interval interval reference-intervals))
               (when preceding-dominant-interval
                 (pushend `(adjacent ,(fcg-id preceding-dominant-interval) ,(fcg-id interval)) relations))
               (when following-dominant-interval
                 (pushend `(adjacent ,(fcg-id interval) ,(fcg-id following-dominant-interval)) relations))))
            relations))

(defun make-predicates (intervals)
  "makes a set of predicates from a set of intervals"
  (loop with predicates = '()
        for interval in intervals
        do (when (interval-type interval)
             (pushend `(,(interval-type interval) ,(fcg-id interval) ,(hamnosys interval)) predicates))
        finally (return predicates)))


(defun xmls->hamnosyspredicates (xmls)
  "transforms an xmls structure to a set of predicates that represent signs using hamnosys-strings"
  (let* ((time-points (retrieve-time-points xmls))
         (dominant-intervals '())
         (non-dominant-intervals '())
         (alignments '()))
    (loop for child in (xmls:node-children xmls)
          for tier-id = (loop for attribute in (xmls:node-attrs child)
                              when (string= (first attribute) "TIER_ID")
                                do (return (read-from-string (upcase (string-replace (second attribute) " " "-")))))
          do (when (member tier-id (list 'LH-SEGMENTATION 'RH-SEGMENTATION 'LH-HAMNOSYS 'RH-HAMNOSYS))
               (cond
                 ((eql tier-id 'RH-SEGMENTATION)
                  (setf dominant-intervals (retrieve-intervals child time-points :type 'right-hand-articulation)))
                 ((eql tier-id 'LH-SEGMENTATION)
                  (setf non-dominant-intervals (retrieve-intervals child time-points :type 'left-hand-articulation)))
                 ((eql tier-id 'RH-HAMNOSYS)
                  (add-hamnosys child dominant-intervals))
                 ((eql tier-id 'LH-HAMNOSYS)
                  (add-hamnosys child non-dominant-intervals)))))
    (setf alignments (append (retrieve-dominant-adjacent dominant-intervals)
                             (retrieve-alignments dominant-intervals non-dominant-intervals)))
    (append (make-predicates dominant-intervals)
            (make-predicates non-dominant-intervals)
            alignments)))

(defmethod de-render ((predicates list) (mode (eql :set-of-predicates)) &key &allow-other-keys)
  "De-renders a set of predicates into a transient structure containing a collection of form predicates"
    (make-instance 'coupled-feature-structure 
                   :left-pole `((root (form ,predicates)))
                   :right-pole `((root))))

;(define-event-handler (trace-fcg parse-started)
  ;(let* ((visualisation (represent-signs utterance)))
    ;(add-element `((h3) ,(format nil "Comprehending the following signed utterance:")))
    ;(add-element visualisation)))

