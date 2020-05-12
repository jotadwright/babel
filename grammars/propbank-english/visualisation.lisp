(in-package :propbank-english)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                   ;;
;; Extracting and visualising frame representations. ;;
;;                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;
;; Representation ;;
;;;;;;;;;;;;;;;;;;;;

;; Frame-set ;;
;;;;;;;;;;;;;;;

(defclass frame-set (entity)
  ((frames :type list
           :accessor frames
           :initarg :frames
           :initform nil
           :documentation "The list of frames."))
  (:documentation "Class for representing a set of frames."))

(defmethod print-object ((frame-set frame-set) (stream t))
  (format stream "<frame-set: {~{~a~^, ~}}>" (frames frame-set )))


;; Frame ;;
;;;;;;;;;;;

(defclass frame (entity)
  ((frame-name :type symbol
               :accessor frame-name
               :initarg :frame-name
               :initform nil
               :documentation "The name of the frame.")
   (frame-evoking-element :type frame-evoking-element
                          :accessor frame-evoking-element
                          :initarg :frame-evoking-element
                          :initform nil
                          :documentation "The frame evoking element object.")
   (frame-elements :type list
                   :accessor frame-elements
                   :initarg :frame-elements
                   :initform nil
                   :documentation "A list of frame element objects."))
  (:documentation "Class for representing frames."))

(defmethod print-object ((frame frame) (stream t))
  (format stream "<frame: ~a>" (frame-name frame)))

;; Frame evoking element ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass frame-evoking-element ()
  ((fel-string :type string
               :accessor fel-string
               :initarg :fel-string
               :initform nil
               :documentation "The string of the frame-evoking element.")
   (index :type number
            :accessor index
            :initarg :index
            :initform nil
            :documentation "The index of the frame-evoking-element in the utterance.")
   (lemma :type symbol
          :accessor lemma
          :initarg :lemma
          :initform nil
          :documentation "The lemma of de frame-evoking-element."))
  (:documentation "Class for representing frames."))


(defmethod print-object ((frame-evoking-element frame-evoking-element) (stream t))
  (format stream "<frame-evoking-element: ~a>" (fel-string frame-evoking-element)))

;; Frame element ;;
;;;;;;;;;;;;;;;;;;;

(defclass frame-element ()
  ((fe-name :type symbol
                       :accessor fe-name
                       :initarg :fe-name
                       :initform nil
                       :documentation "The name of the frame element (e.g. cognizer).")
   (fe-role :type symbol
               :accessor fe-role
               :initarg :fe-role
               :initform nil
               :documentation "The role name of the frame element (e.g. arg0).")
   (fe-string :type string
              :accessor fe-string
              :initarg :fe-string
              :initform nil
              :documentation "The string.")
   (indices :accessor indices
            :initarg :indices
            :initform nil
            :documentation "A list of indices."))
  (:documentation "Class for representing frame elements."))

(defmethod print-object ((frame-element frame-element) (stream t))
  (format stream "<frame-element: ~a>" (fe-name frame-element)))

;;;;;;;;;;;;;;;;;;;;
;; Extraction     ;;
;;;;;;;;;;;;;;;;;;;;

(defun extract-frames (transient-structure)
  (loop with unit-list = (left-pole-structure transient-structure)
        for unit in unit-list
        when (find '(frame-evoking +) (unit-body unit) :test #'equalp)
        collect (make-instance 'frame
                               :frame-name (find-frame-name unit)
                               :frame-evoking-element (find-frame-evoking-element unit)
                               :frame-elements (find-frame-elements unit unit-list))
        into frames
        finally
        return (make-instance 'frame-set :frames frames)))
        
(defun find-frame-name (unit)
  (let ((meaning (find 'meaning (unit-body unit) :key #'feature-name)))
    (loop for predicate in (feature-value meaning)
          when (equalp (first predicate) 'frame)
          return (second predicate))))

(defun find-frame-evoking-element (unit)
  (make-instance 'frame-evoking-element
                 :fel-string (second (find 'string (unit-body unit) :key #'feature-name))
                 :index (first (second (find 'span (unit-body unit) :key #'feature-name)))
                 :lemma (second (find 'lemma (unit-body unit) :key #'feature-name))))

(defun find-frame-elements (unit unit-list)
  (let ((fe-predicates (loop for predicate in (feature-value (find 'meaning (unit-body unit) :key #'feature-name))
                             when (equalp (first predicate) 'frame-element)
                             collect predicate)))
    (loop for predicate in fe-predicates
          for fe-consituent-unit = (find (fourth predicate) unit-list :key #'feature-name)
          collect
          (make-instance 'frame-element
                 :fe-name (second predicate)
                 :fe-role (loop for arg in (feature-value (find 'args (unit-body unit) :key #'feature-name))
                             when (equalp (second arg) (fourth predicate))
                             return (first arg))
                 :fe-string (second (find 'string (unit-body fe-consituent-unit) :key #'feature-name))
                 :indices (loop for i
                                from (first (second (find 'span (unit-body fe-consituent-unit) :key #'feature-name)))
                                to (- (second (second (find 'span (unit-body fe-consituent-unit) :key #'feature-name))) 1)
                                collect i)))))
