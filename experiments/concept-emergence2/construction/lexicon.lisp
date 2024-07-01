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
         (remhash (form cxn) (get-inventory lexicon :fast))
         (remhash (form cxn) (get-inventory lexicon :slow)))
        ((>= (score cxn) (get-configuration (configuration lexicon) :slow-threshold))
         (setf (gethash (form cxn) (fast-inventory lexicon)) cxn)
         (remhash (form cxn) (get-inventory lexicon :slow))
         (remhash (form cxn) (get-inventory lexicon :trash)))
        (t
         (setf (gethash (form cxn) (slow-inventory lexicon)) cxn)
         (remhash (form cxn) (get-inventory lexicon :fast))
         (remhash (form cxn) (get-inventory lexicon :trash)))))

(defmethod find-form-in-lexicon ((lexicon lexicon) (form string))
  "Waterfall search through the inventories."
  (loop for inventory-name in (list :fast :slow :trash)
        for inventory = (get-inventory lexicon inventory-name)
        do (let ((cxn (gethash form inventory)))
           (if cxn (return cxn)))))

(defmethod lexicon-size ((lexicon lexicon))
  (+ (hash-table-count (get-inventory lexicon :fast))
     (hash-table-count (get-inventory lexicon :slow))))

(defmethod get-inventory ((lexicon lexicon) key)
  (let ((inventory (case key
                     (:fast (fast-inventory lexicon))
                     (:slow (slow-inventory lexicon))
                     (:trash (trash-inventory lexicon)))))
    inventory))
