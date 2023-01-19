(in-package :fcg)

(export '(cxn-supplier-hashed-and-scored-routine-cxn-set-only
          cxn-supplier-hashed-routine-set-only))


(defclass cxn-supplier-hashed-routine-set-only ()
  ()
  (:documentation "Construction supplier that returns all constructions except incompatible hashed ones."))

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :hashed-routine-only)))
  "Creates an instance of the cxn-supplier."
  (make-instance 'cxn-supplier-hashed-routine-set-only))

(defmethod next-cxn ((cxn-supplier cxn-supplier-hashed-routine-set-only) (node cip-node))
  "Returns all constructions that satisfy the hash of the node."
  (all-constructions-of-label-hashed node :routine))






(defun constructions-for-application-hashed-and-scored-routine-cxn-set-only (node)
  "computes all constructions that could be applied for this node
   plus nil hashed constructions"
  (let ((constructions
         ;; get all constructions compatible
         ;; with the hashes of the node
         ;; append nil hashed constructions
         (remove-duplicates
          (append
           (loop
            for hash in (hash node (get-configuration node :hash-mode))
            append (gethash hash (constructions-hash-table (construction-inventory node))))
           (gethash nil (constructions-hash-table (construction-inventory node)))) :key #'name)))
    ;; shuffle if requested
    (when (get-configuration node :shuffle-cxns-before-application)
      (setq constructions 
            (shuffle constructions)))
    ;; filter the cxns for a specific cxn set here using (attr-val cxn :label)
    (setq constructions
          (loop for cxn in constructions
                when (equal (attr-val cxn :label) 'routine)
                collect cxn))
    
    ;; sort 
    (setq constructions
          (sort constructions #'> :key #'(lambda (cxn) (attr-val cxn :score))))
    ;; return constructions
    constructions))

(defclass cxn-supplier-hashed-and-scored-routine-cxn-set-only ()
  ((remaining-constructions
    :type list :initarg :remaining-constructions
    :accessor remaining-constructions
    :documentation "A list of constructions that are still to try")))

(defmethod create-cxn-supplier ((node cip-node)
                                (mode (eql :hashed-and-scored-routine-cxn-set-only)))
  (make-instance
   'cxn-supplier-hashed-and-scored-routine-cxn-set-only
   :remaining-constructions (constructions-for-application-hashed-and-scored-routine-cxn-set-only node)))

(defmethod next-cxn ((cxn-supplier cxn-supplier-hashed-and-scored-routine-cxn-set-only)
                     (node cip-node))
  (pop (remaining-constructions cxn-supplier)))


(export '(cxn-supplier-hashed-and-scored-meta-layer-cxn-set-only))

(defun constructions-for-application-hashed-and-scored-meta-layer-cxn-set-only (node)
  "computes all constructions that could be applied for this node
   plus nil hashed constructions"
  (let ((constructions
         ;; get all constructions compatible
         ;; with the hashes of the node
         ;; append nil hashed constructions
         (remove-duplicates
          (append
           (loop
            for hash in (hash node (get-configuration node :hash-mode))
            append (gethash hash (constructions-hash-table (construction-inventory node))))
           (gethash nil (constructions-hash-table (construction-inventory node)))) :key #'name)))
    ;; shuffle if requested
    (when (get-configuration node :shuffle-cxns-before-application)
      (setq constructions 
            (shuffle constructions)))
    ;; filter the cxns for a specific cxn set here using (attr-val cxn :label)
    (setq constructions
          (loop for cxn in constructions
                when (equal (attr-val cxn :label) 'meta-only)
                collect cxn))
    
    ;; sort 
    (setq constructions
          (sort constructions #'> :key #'(lambda (cxn) (attr-val cxn :score))))
    ;; return constructions
    constructions))

(defclass cxn-supplier-hashed-and-scored-meta-layer-cxn-set-only ()
  ((remaining-constructions
    :type list :initarg :remaining-constructions
    :accessor remaining-constructions
    :documentation "A list of constructions that are still to try")))

(defmethod create-cxn-supplier ((node cip-node)
                                (mode (eql :hashed-and-scored-meta-layer-cxn-set-only)))
  (make-instance
   'cxn-supplier-hashed-and-scored-meta-layer-cxn-set-only
   :remaining-constructions (constructions-for-application-hashed-and-scored-meta-layer-cxn-set-only node)))

(defmethod next-cxn ((cxn-supplier cxn-supplier-hashed-and-scored-meta-layer-cxn-set-only)
                     (node cip-node))
  (pop (remaining-constructions cxn-supplier)))