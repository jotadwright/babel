(in-package :cle)

;; ---------------------
;; + Lexicon inventory +
;; ---------------------

(defclass lexicon ()
  ((fast-inventory
    :documentation "Fast-access constructions."
    :type hash-table :accessor fast-inventory :initform (make-hash-table :test 'equal))
   (trash-inventory
    :documentation "Trashed constructions."
    :type hash-table :accessor trash-inventory :initform (make-hash-table :test 'equal))
   (unassigned-inventory
    :documentation "Unassigned cluster concepts"
    :type hash-table :accessor unassigned-inventory :initform (make-hash-table :test 'equal))
   (configuration
    :documentation "Configuration of the lexicon"
    :type object :accessor configuration :initform nil)))

(defmethod initialize-instance :after ((lexicon lexicon) &key (configuration configuration))
  (setf (configuration lexicon) configuration))

(defmethod update-lexicon-inventory ((lexicon lexicon) (cxn cxn))
  (when (stringp (form cxn))
    (cond ((<= (score cxn) 0.0) ;; TODO 0.0 should be configurable in :trash-treshold
           ;; assumes that score lower-bound is never negative (after update)
           (setf (gethash (form cxn) (get-inventory lexicon :trash)) cxn)
           (remhash (form cxn) (get-inventory lexicon :fast)))
          (t
           (setf (gethash (form cxn) (get-inventory lexicon :fast)) cxn)
           (remhash (form cxn) (get-inventory lexicon :trash))))))

(defmethod find-form-in-lexicon ((lexicon lexicon) (form string))
  "Waterfall search through the inventories."
  (loop for inventory-name in (list :fast :trash)
        for inventory = (get-inventory lexicon inventory-name)
        do (let ((cxn (gethash form inventory)))
             (if cxn (return cxn)))))

(defmethod lexicon-size ((lexicon lexicon))
  (+
   (hash-table-count (get-inventory lexicon :fast))
   (hash-table-count (get-inventory lexicon :trash))))

(defmethod get-inventory ((lexicon lexicon) key)
  (let ((inventory (case key
                     (:fast (fast-inventory lexicon))
                     (:trash (trash-inventory lexicon))
                     (:unassigned (unassigned-inventory lexicon)))))
    inventory))
