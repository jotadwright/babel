(in-package :cle)

;; ---------------------
;; + Lexicon inventory +
;; ---------------------

(defclass lexicon ()
  ((fast-inventory
    :documentation "Fast-access constructions."
    :type list :accessor fast-inventory :initform nil)
   (slow-inventory
    :documentation "Slow-access constructions."
    :type list :accessor slow-inventory :initform nil)
   (trash-inventory
    :documentation "Trashed constructions."
    :type list :accessor trash-inventory :initform nil)
   (configuration
    :documentation "Configuration of the lexicon"
    :type object :accessor configuration :initform nil)))

(defmethod initialize-instance :after ((lexicon lexicon) &key (configuration configuration))
  (setf (configuration lexicon) configuration))

(defmethod update-lexicon-inventory ((lexicon lexicon) (cxn cxn))
  ;; Settings:
  ;; If you want to trash things:
  ;;     trash-threshold: POS
  ;; If you want to use a slow inventory:
  ;;     slow-threshold: POS
  (cond ((<= (score cxn) (get-configuration (configuration lexicon) :trash-threshold))
         ;; assumes that score lower-bound is never negative (after update)
         (progn
           (push cxn (trash-inventory lexicon))
           (setf (slow-inventory lexicon) (remove cxn (slow-inventory lexicon)))
           (setf (fast-inventory lexicon) (remove cxn (fast-inventory lexicon)))))
        ((>= (score cxn) (get-configuration (configuration lexicon) :slow-threshold))
         (progn
           (push cxn (fast-inventory lexicon))
           (setf (slow-inventory lexicon) (remove cxn (slow-inventory lexicon)))
           (setf (trash-inventory lexicon) (remove cxn (trash-inventory lexicon)))))
        (t
         (progn
           (push cxn (slow-inventory lexicon))
           (setf (fast-inventory lexicon) (remove cxn (fast-inventory lexicon)))
           (setf (trash-inventory lexicon) (remove cxn (trash-inventory lexicon)))))))

(defmethod find-form-in-lexicon ((lexicon lexicon) (form string))
  "Waterfall search through the inventories."
  (let ((fast (find-in-inventory (fast-inventory lexicon) form)))
    (if fast
      fast
      (let ((slow (find-in-inventory (slow-inventory lexicon) form)))
        (if slow
          slow
          (let ((trash (find-in-inventory (trash-inventory lexicon) form)))
            trash))))))

(defmethod find-in-inventory ((inventory list) (form string))
  (find form inventory :key #'form :test #'string=))

(defmethod lexicon-size ((lexicon lexicon))
  (+ (length (fast-inventory lexicon)) (length (slow-inventory lexicon))))

(defmethod lexicon-trash-size ((lexicon lexicon))
  (length (lexicon trash-inventory)))

(defmethod get-inventory ((lexicon lexicon) key &key (sorted nil))
  (let ((inventory (case key
                     (:fast (fast-inventory lexicon))
                     (:slow (slow-inventory lexicon))
                     (:trash (trash-inventory lexicon)))))
    (if sorted
      (sort inventory #'(lambda (x y) (> (score x) (score y))))
      inventory)))
