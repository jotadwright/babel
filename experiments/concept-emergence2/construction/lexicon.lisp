(in-package :cle)

;; ---------------------
;; + Lexicon inventory +
;; ---------------------

(defclass lexicon ()
  ((fast-inventory
    :documentation "Fast-access constructions."
    :type hash-table :accessor fast-inventory :initform (make-hash-table :test 'equal))
   (slow-inventory
    :documentation "Slow-access constructions."
    :type hash-table :accessor slow-inventory :initform (make-hash-table :test 'equal))
   (trash-inventory
    :documentation "Trashed constructions."
    :type hash-table :accessor trash-inventory :initform (make-hash-table :test 'equal))
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
         (setf (gethash (form cxn) (trash-inventory lexicon)) cxn)
         (remhash (form cxn) (fast-inventory lexicon))
         (remhash (form cxn) (slow-inventory lexicon)))
        ((>= (score cxn) (get-configuration (configuration lexicon) :slow-threshold))
         (setf (gethash (form cxn) (fast-inventory lexicon)) cxn)
         (remhash (form cxn) (slow-inventory lexicon))
         (remhash (form cxn) (trash-inventory lexicon)))
        (t
         (setf (gethash (form cxn) (slow-inventory lexicon)) cxn)
         (remhash (form cxn) (fast-inventory lexicon))
         (remhash (form cxn) (trash-inventory lexicon)))))

(defmethod find-form-in-lexicon ((lexicon lexicon) (form string))
  "Waterfall search through the inventories."
  (loop for inventory-name in (list :fast :slow :trash)
        for inventory = (get-inventory lexicon inventory-name)
        do (let ((cxn (gethash form inventory)))
           (if cxn (return cxn)))))

(defmethod lexicon-size ((lexicon lexicon))
  (+ (hash-table-count (fast-inventory lexicon))
     (hash-table-count (slow-inventory lexicon))))

(defmethod get-inventory ((lexicon lexicon) key &key (sorted nil))
  (let ((inventory (case key
                     (:fast (fast-inventory lexicon))
                     (:slow (slow-inventory lexicon))
                     (:trash (trash-inventory lexicon)))))
    (if sorted
      (sort inventory #'(lambda (x y) (> (score x) (score y))))
      inventory)))
