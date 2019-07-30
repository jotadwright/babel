;;;; /categories.lisp

(in-package :roos)

;; --------------
;; + Categories +
;; --------------

(defclass category (entity)
  ((prototype :accessor prototype :initarg :prototype :initform nil
              :documentation "The prototypical value for the category"))
  (:documentation "A general category"))

(defgeneric distance (category object)
  (:documentation "Distance measure between a category and an object"))

(defgeneric category->predicate (category var)
  (:documentation "Create an irl network (predicate) from the category"))

(defgeneric predicate->category (predicate)
  (:documentation "Get the category from an irl predicate"))

(defmethod predicate->category (predicate)
  (second predicate))


;; + XPOS Category +

(defclass xpos-category (category) ()
  (:documentation "Categories for the xpos feature"))

(defun make-xpos-category (prototype)
  (make-instance 'xpos-category
                 :prototype prototype))

(defmethod category->predicate ((cat xpos-category) var)
  `(:xpos ,(id cat) ,var))

(defmethod distance ((cat xpos-category) (obj sensory-object))
  (abs (- (prototype cat)
          (get-object-feature obj :xpos))))

(defmethod distance ((obj sensory-object) (cat xpos-category))
  (abs (- (prototype cat)
          (get-object-feature obj :xpos))))

(defmethod copy-object ((cat xpos-category))
  (make-instance 'xpos-category
                 :id (id cat)
                 :prototype (prototype cat)))

(defmethod print-object ((cat xpos-category) stream)
  (format stream "<xpos-category ~a>" (id cat)))

;; + YPOS Category +

(defclass ypos-category (category) ()
  (:documentation "Categories for the ypos feature"))

(defun make-ypos-category (prototype)
  (make-instance 'ypos-category
                 :prototype prototype))

(defmethod category->predicate ((cat ypos-category) var)
  `(:ypos ,(id cat) ,var))

(defmethod distance ((cat ypos-category) (obj sensory-object))
  (abs (- (prototype cat)
          (get-object-feature obj :ypos))))

(defmethod distance ((obj sensory-object) (cat ypos-category))
  (abs (- (prototype cat)
          (get-object-feature obj :ypos))))

(defmethod copy-object ((cat ypos-category))
  (make-instance 'ypos-category
                 :id (id cat)
                 :prototype (prototype cat)))

(defmethod print-object ((cat ypos-category) stream)
  (format stream "<ypos-category ~a>" (id cat)))

;; + Color Category +

(defclass color-category (category) ()
  (:documentation "Categories for the color feature"))

(defun make-color-category (prototype)
  (make-instance 'color-category
                 :prototype prototype))

(defmethod category->predicate ((cat color-category) var)
  `(:color ,(id cat) ,var))

(defmethod distance ((cat color-category) (obj sensory-object))
  (euclidean (prototype cat)
             (get-object-feature obj :color)))

(defmethod distance ((obj sensory-object) (cat color-category))
  (euclidean (prototype cat)
             (get-object-feature obj :color)))

(defmethod copy-object ((cat color-category))
  (make-instance 'color-category
                 :id (id cat)
                 :prototype (prototype cat)))

(defmethod print-object ((cat color-category) stream)
  (format stream "<color-category ~a>" (id cat)))

;; + Area Category +

(defclass area-category (category) ()
  (:documentation "Categories for the area feature"))

(defun make-area-category (prototype)
  (make-instance 'area-category
                 :prototype prototype))

(defmethod category->predicate ((cat area-category) var)
  `(:area ,(id cat) ,var))

(defmethod distance ((cat area-category) (obj sensory-object))
  (abs (- (prototype cat)
          (get-object-feature obj :area))))

(defmethod distance ((obj sensory-object) (cat area-category))
  (abs (- (prototype cat)
          (get-object-feature obj :area))))

(defmethod copy-object ((cat area-category))
  (make-instance 'area-category
                 :id (id cat)
                 :prototype (prototype cat)))

(defmethod print-object ((cat area-category) stream)
  (format stream "<area-category ~a>" (id cat)))
  