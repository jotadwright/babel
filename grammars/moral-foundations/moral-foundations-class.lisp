
(in-package :moral-foundations)

;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;; Moral Foundations Theory
;;;;    -> https://moralfoundations.org
;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;;; ---------------------------------------------------------------------------------------------------------
;;;; Class Definition
;;;; ---------------------------------------------------------------------------------------------------------

(defclass moral-foundation ()
  ((name 
    :type symbol :initform nil
    :initarg :name
    :accessor name
    :documentation "Name of the moral foundation.")
   (antonym
    :type symbol :initform nil
    :initarg :antonym
    :accessor antonym
    :documentation "Opposite of the moral foundation.")
   (virtues 
    :type list :initform nil
    :initarg :virtues
    :accessor virtues
    :documentation "List of virtues associated with a moral foundation.")
   (vices
    :type list :initform nil
    :initarg :vices
    :accessor vices
    :documentation "List of vices associated with violation of a moral foundation.")
   (mappings
    :type list :initform nil
    :initarg :mappings
    :accessor mappings
    :documentation "Associate a list of mappings for unification.")
   (description
    :type string :initform "A moral foundation."
    :initarg :description
    :accessor description
    :documentation "Describe the moral foundation in this slot."))
  (:documentation "Class that represents common slots in moral foundations."))

;;;; ---------------------------------------------------------------------------------------------------------
;;;; Initialization of the five original foundations
;;;; ---------------------------------------------------------------------------------------------------------

(setf *moral-foundations*
      (list ;;;; Five original foundations:
            ;;;; ====================================================
            ;;;; CARE
            ;;;; ----------------------------------------------------
            (make-instance 'moral-foundation
                           :name 'care
                           :antonym 'harm
                           :description "This foundation is related to our long evolution as mammals with attachment systems and an ability to feel (and dislike) the pain of others. It underlies the virtues of kindness, gentleness, and nurturance.")

            ;;;; FAIRNESS
            ;;;; ----------------------------------------------------
            (make-instance 'moral-foundation
                           :name 'fairness
                           :antonym 'unfairness
                           :description "This foundation is related to the evolutionary process of reciprocal altruism. It underlies the virtues of justice and rights.")

            ;;;; LOYALTY
            ;;;; ----------------------------------------------------
            (make-instance 'moral-foundation
                           :name 'loyalty
                           :antonym 'disloyalty
                           :description "This foundation is related to our long history as tribal creatures able to form shifting coalitions. It is active anytime people feel that it's 'one for all and all for one.' It underlies the virtues of patriotism and self-sacrifice for the group.")

            ;;;; AUTHORITY
            ;;;; ----------------------------------------------------
            (make-instance 'moral-foundation
                           :name 'authority
                           :antonym 'anarchy
                           :description "This foundation was shaped by our long primate history of hierarchical social interactions. It underlies virtues of leadership and followership, including deference to prestigious authority figures and respect for traditions.")

            ;;;; PURITY
            ;;;; ----------------------------------------------------
            (make-instance 'moral-foundation
                           :name 'sanctity
                           :antonym 'impurity
                           :description "This foundation was shaped by the psychology of disgust and contamination. It underlies notions of striving to live in an elevated, less carnal, more noble, and more 'natural' way (often present in religious narratives). This foundation underlies the widespread idea that the body is a temple that can be desecrated by immoral activities and contaminants (an idea not unique to religious traditions). It underlies the virtues of self-discipline, self-improvement, naturalness, and spirituality.")

            ;;;; MORALITY-GENERAL
            ;;;; ----------------------------------------------------
            (make-instance 'moral-foundation
                           :name 'morality-general
                           :antonym 'immorality-general
                           :description "This is a 'rest' category for everything that did not fit the others.")))

;; You can always describe a moral foundation as follows:
;; (describe (first *moral-foundations*))

;; Or using this helper function:
(defun describe-moral-foundation (name &optional only-documentation)
  (let ((moral-foundation (find name *moral-foundations* :key #'name)))
    (cond
     ((and moral-foundation only-documentation)
      (description moral-foundation))
     (moral-foundation (describe moral-foundation))
     (t
      (warn (format nil "Moral foundation ~a not found. Please select from: ~a" name (mapcar #'name *moral-foundations*)))))))
;; (describe-moral-foundation 'care)
;; (describe-moral-foundation 'care t)
;; (describe-moral-foundation 'cares)