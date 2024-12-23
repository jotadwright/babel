(in-package :au-lib)

;;;;
;;;; classes
;;;; --------

(defclass swap ()
  ((phi-s
    :accessor phi-s :type list :initform nil :initarg :phi-s
    :documentation "the part of phi that is removed")
   (phi-c
    :accessor phi-c :type list :initform nil :initarg :phi-c
    :documentation "the part of comp that is inserted")
   (phi-s-rank
    :accessor phi-s-rank :type number :initarg :phi-s-rank
    :documentation "the rank of phi-s")
   (phi-c-rank
    :accessor phi-c-rank :type number :initarg :phi-c-rank
    :documentation "the rank of phi-c")
   (phi-s-gain
    :accessor phi-s-gain :type number :initarg :phi-s-gain
    :documentation "the gain of phi-s, i.e. bindings lost")
   (phi-c-gain
    :accessor phi-c-gain :type number :initarg :phi-c-gain
    :documentation "the gain of phi-c, i.e. bindings added")
   (num-phi-s-candidates
    :accessor num-phi-s-candidates :type number :initarg :num-phi-s-candidates
    :documentation "the number of candidates for phi-s")
   (num-phi-c-candidates
    :accessor num-phi-c-candidates :type number :initarg :num-phi-c-candidates
    :documentation "the number of candidates for phi-c")
   (k :accessor k :type number :initarg :k
      :documentation "the size of the swap")))

(defclass generalisation-state ()
  ((phi
    :accessor phi :type list :initform nil :initarg :phi
    :documentation "the current generalsation")
   (phi-delta-i
    :accessor phi-delta-i :type number :initform 0 :initarg :phi-delta-i
    :documentation "the quality of the current generalisation (Delta_I)")
   (phi-bindings
    :accessor phi-bindings :type list :initform nil :initarg :phi-bindings
    :documentation "a counter of the bindings in phi")
   (gen
    :reader get-gen :type list :initform nil :initarg :gen
    :documentation "all possible pairings of the input G1 and G2")
   (comp
    :reader get-comp :type list :initform nil :initarg :comp
    :documentation "part of gen that is compable with phi")
   (A-pair
    :accessor A-pair :type list :initform nil :initarg :A-pair
    :documentation "the pair that was added to get to this depth")
   (swaps
    :accessor swaps :type list :initform nil :initarg :swaps
    :documentation "a list of swaps that have been performed")
   (delta-i-storage
    :accessor delta-i-storage :type hash-table :initarg :delta-i-storage
    :documentation "storage with pre-computed delta-i values")
   (singleton-storage
    :accessor singleton-storage :type hash-table :initarg :singleton-storage
    :documentation "storage with pre-computed singleton bindings")
   (phi-s-storage
    :accessor phi-s-storage :type hash-table :initarg :phi-s-storage
    :documentation "storage with phi-s candidates")
   (parent
    :accessor parent :initform nil :initarg :parent
    :documentation "the parent state")
   (configuration
    :accessor configuration :initform nil :initarg :configuration
    :documentation "the configuration for the state (keyword arguments)"))
  (:documentation "the state of a generalisation"))

;;;
;;; 2-swap-equal
;;; ------------

(defun 2-swap-equal (x y)
  ;; more general: permutation-of? 
  (if (and (> (length x) 1) (> (length y) 1))
    (or (equal x y) (equal (reverse x) y))
    (equal x y)))

;;;;
;;;; Update bindings(phi)
;;;; --------------------

(defun make-counter (set-of-bindings)
  (loop with counter = nil
        for binding in set-of-bindings
        for entry = (assoc binding counter :test #'equal)
        if entry do (incf (cdr entry))
        else do (push (cons binding 1) counter)
        finally (return counter)))

(defun assqv (item alist &optional default &key (test #'equal) (key #'identity))
  (let ((entry (assoc item alist :test test :key key)))
    (if entry (cdr entry) default)))

(defun bindings-extension (bindings A-pair)
  (let ((A-pair-bindings (make-counter (third A-pair))))
    (loop for binding in (union (mapcar #'car bindings)
                                (mapcar #'car A-pair-bindings)
                                :test #'equal)
          for count = (+ (assqv binding bindings 0)
                         (assqv binding A-pair-bindings 0))
          when (plusp count)
            collect (cons binding count))))

(defun bindings-removal (bindings phi-s)
  (let ((phi-s-bindings (make-counter (mappend #'third phi-s))))
    (loop for binding in (mapcar #'car bindings)
          for count = (- (assqv binding bindings 0)
                         (assqv binding phi-s-bindings 0))
          when (plusp count)
            collect (cons binding count))))

(defun bindings-swap (bindings phi-s phi-c)
  (let ((phi-s-bindings (make-counter (mappend #'third phi-s)))
        (phi-c-bindings (make-counter (mappend #'third phi-c))))
    (loop for binding in (union (mapcar #'car bindings)
                                (mapcar #'car phi-c-bindings)
                                :test #'equal)
          for count = (+ (- (assqv binding bindings 0)
                            (assqv binding phi-s-bindings 0))
                         (assqv binding phi-c-bindings 0))
          when (plusp count)
            collect (cons binding count))))

(defun get-singleton-bindings (state)
  (loop for (binding . count) in (phi-bindings state)
        when (= count 1) collect binding))

;;;;
;;;; Pre-compute Delta_I values
;;;; --------------------------

(defgeneric %pre-compute-delta-i (gen k)
  (:documentation "pre-compute Delta_I values according to dimensionality k."))

(defmethod %pre-compute-delta-i (gen (k (eql 1)))
  (let ((table (make-hash-table :test #'equal)))
    (dolist (pair gen)
      (setf (gethash pair table)
            (delta-intern (list pair))))
    table))

(defmethod %pre-compute-delta-i (gen (k (eql 2)))
  ;; optimization - only store half of the matrix
  (let ((matrix (make-hash-table :test #'equal)))
    (loop for (A-pair . gen-rest) on gen
          when gen-rest
            do (loop with row = (make-hash-table :test #'equal)
                     for B-pair in gen-rest
                     do (setf (gethash B-pair row)
                              (delta-intern (list A-pair B-pair)))
                     finally (setf (gethash A-pair matrix) row)))
    matrix))

(defun pre-compute-delta-i (gen k)
  "Pre-compute delta-i values for all dimensions up to k."
  (let ((storage (make-hash-table :test #'=)))
    (loop for i from 1 to k
          do (setf (gethash i storage)
                   (%pre-compute-delta-i gen i)))
    storage))

(defgeneric get-delta-i (storage entry k)
  (:documentation "Get the Delta_I value of entry from storage."))

(defmethod get-delta-i ((storage hash-table) entry (k (eql 1)))
  (let ((k-storage (gethash k storage)))
    (gethash (first entry) k-storage)))

(defmethod get-delta-i ((storage hash-table) entry (k (eql 2)))
  (let ((k-storage (gethash k storage)))
    (get-pairwise-delta-i k-storage (first entry) (second entry))))

(defun get-pairwise-delta-i (storage A-pair B-pair)
  "Returns the Delta_I((A-pair B-pair)).
   Equivalent to Delta_I((B-pair A-pair))."
  (let ((A-row (gethash A-pair storage))
        (B-row (gethash B-pair storage)))
    (cond ((and A-row B-row)
           (multiple-value-bind (delta-i present-p) (gethash B-pair A-row)
             (if present-p
               delta-i
               (gethash A-pair B-row))))
          (A-row
           (gethash B-pair A-row))
          (B-row
           (gethash A-pair B-row)))))


;;;;
;;;; Pre-compute singleton bindings
;;;; ------------------------------

(defgeneric %pre-compute-singletons (gen k)
  (:documentation "pre-compute singleton bindings according to dimensionality k."))

(defmethod %pre-compute-singletons (gen (k (eql 1)))
  (let ((table (make-hash-table :test #'equal)))
    (loop for A-pair in gen
          do (setf (gethash A-pair table)
                   (singleton-bindings (third a-pair))))
    table))

(defun singleton-bindings-in-pairs (set-of-pairs)
  (singleton-bindings (mappend #'third set-of-pairs)))

(defmethod %pre-compute-singletons (gen (k (eql 2)))
  (let ((matrix (make-hash-table :test #'equal)))
    (loop for (A-pair . gen-rest) on gen
          when gen-rest
            do (loop with row = (make-hash-table :test #'equal)
                     for B-pair in gen-rest
                     do (setf (gethash B-pair row)
                              (singleton-bindings-in-pairs (list A-pair B-pair)))
                     finally (setf (gethash A-pair matrix) row)))
    matrix))

(defun pre-compute-singletons (gen k)
  "Pre-compute singleton bindings for all dimensions up to k."
  (let ((storage (make-hash-table :test #'=)))
    (loop for i from 1 to k
          do (setf (gethash i storage)
                   (%pre-compute-singletons gen i)))
    storage))

(defgeneric get-singletons (storage entry k)
  (:documentation "get singleton bindings from storage"))

(defmethod get-singletons ((storage hash-table) entry (k (eql 1)))
  (let ((k-storage (gethash k storage)))
    (gethash (first entry) k-storage)))

(defmethod get-singletons ((storage hash-table) entry (k (eql 2)))
  (let ((k-storage (gethash k storage)))
    (get-pairwise-singletons k-storage (first entry) (second entry))))

(defun get-pairwise-singletons (matrix A-pair B-pair)
  (let ((A-row (gethash A-pair matrix))
        (B-row (gethash B-pair matrix)))
    (cond ((and A-row B-row)
           (multiple-value-bind (singletons present-p) (gethash B-pair A-row)
             (if present-p
               singletons
               (gethash A-pair B-row))))
          (A-row
           (gethash B-pair A-row))
          (B-row
           (gethash A-pair B-row)))))


;;;;
;;;; Storage for phi_s candidates
;;;; ----------------------------

;; for every phi-s candidate, store
;; (i) its gain,
;; (ii) phi minus that candidate,
;; (iii) bindings of phi minus that candidate

(defun initialise-phi-s-candidates (k)
  (let ((storage (make-hash-table :test #'=)))
    (loop for i from 1 to k
          do (setf (gethash i storage)
                   (make-hash-table :test #'equal)))
    storage))

(defgeneric %extend-phi-s-candidates (updated-phi updated-bindings state A-pair A-pair-gain storage k)
  (:documentation "extend phi-s candidates for given k"))

(defmethod %extend-phi-s-candidates (updated-phi updated-bindings (state generalisation-state) A-pair A-pair-gain (storage hash-table) (k (eql 1)))
  "extend phi-s candidates for 1-swaps"
  ;; update the gain for all existing phi-s candidates
  (loop for candidate-phi-s in (hash-table-keys storage)
        for phi-minus-phi-s = (remove candidate-phi-s updated-phi :test #'equal)
        for phi-minus-phi-s-bindings = (bindings-removal updated-bindings (list candidate-phi-s))
        for gain = (phi-s-candidate-gain (list candidate-phi-s) phi-minus-phi-s phi-minus-phi-s-bindings state k)
        do (setf (gethash candidate-phi-s storage)
                 (list gain phi-minus-phi-s phi-minus-phi-s-bindings)))
  ;; add the A-pair as a new phi-s candidate
  (setf (gethash A-pair storage)
        (list A-pair-gain (phi state) (phi-bindings state))))

(defmethod %extend-phi-s-candidates (updated-phi updated-bindings (state generalisation-state) A-pair A-pair-gain (storage hash-table) (k (eql 2)))
  "extend phi-s candidates for 2-swaps"
  ;; update the gain for all existing phi-s candidates
  (loop for candidate-phi-s in (hash-table-keys storage)
        for phi-minus-phi-s = (set-difference updated-phi candidate-phi-s :test #'2-swap-equal)
        for phi-minus-phi-s-bindings = (bindings-removal updated-bindings candidate-phi-s)
        for gain = (phi-s-candidate-gain candidate-phi-s phi-minus-phi-s phi-minus-phi-s-bindings state k)
        do (setf (gethash candidate-phi-s storage)
                 (list gain phi-minus-phi-s phi-minus-phi-s-bindings)))
  ;; add the A-pair in combination with any other pair as new candidates
  (loop for other-pair in (phi state)
        for candidate-phi-s = (list A-pair other-pair)
        for phi-minus-phi-s = (set-difference updated-phi candidate-phi-s :test #'2-swap-equal)
        for phi-minus-phi-s-bindings = (bindings-removal updated-bindings candidate-phi-s)
        for gain = (phi-s-candidate-gain candidate-phi-s phi-minus-phi-s phi-minus-phi-s-bindings state k)
        do (setf (gethash candidate-phi-s storage)
                 (list gain phi-minus-phi-s phi-minus-phi-s-bindings))))
  

(defun extend-phi-s-candidates (updated-phi updated-bindings state A-pair A-pair-gain)
  ;; extend all phi-s candidates for k=1 and k=2 now that A-pair is added
  ;; for k=1, this means A-pair itself
  ;; for k=2, this means A-pair + B-pair for every B-pair in phi
  (let ((storage (phi-s-storage state)))
    (loop for k from 1 to 2
          for k-storage = (gethash k storage)
          do (%extend-phi-s-candidates updated-phi updated-bindings state A-pair A-pair-gain k-storage k))
    storage))


(defgeneric %update-phi-s-candidates (updated-phi updated-bindings state swap swap-size storage k)
  (:documentation "update phi-s candidates for given k"))

(defmethod %update-phi-s-candidates (updated-phi updated-bindings (state generalisation-state) (swap swap) (swap-size (eql 1)) (storage hash-table) (k (eql 1)))
  ;; how do 1-swap candidates change after a 1-swap?
  ;; - remove the entry for (phi-s swap)
  ;; - add the entry for (phi-c swap)
  ;; - recompute all other gains
  (remhash (first (phi-s swap)) storage)
  (loop for candidate-phi-s in (cons (first (phi-c swap)) (hash-table-keys storage))
        for phi-minus-phi-s = (remove candidate-phi-s updated-phi :test #'equal)
        for phi-minus-phi-s-bindings = (bindings-removal updated-bindings (list candidate-phi-s))
        for gain = (phi-s-candidate-gain (list candidate-phi-s) phi-minus-phi-s phi-minus-phi-s-bindings state k)
        do (setf (gethash candidate-phi-s storage)
                 (list gain phi-minus-phi-s phi-minus-phi-s-bindings))))

(defmethod %update-phi-s-candidates (updated-phi updated-bindings (state generalisation-state) (swap swap) (swap-size (eql 2)) (storage hash-table) (k (eql 1)))
  ;; how do 1-swap candidates change after a 2-swap?
  ;; - remove the entries that occur in (phi-s swap)
  ;; - add the entries that occur in (phi-c swap)
  ;; - recompute all other gains
  (loop for pair in (phi-s swap) do (remhash pair storage))
  (loop for candidate-phi-s in (append (phi-c swap) (hash-table-keys storage))
        for phi-minus-phi-s = (remove candidate-phi-s updated-phi :test #'equal)
        for phi-minus-phi-s-bindings = (bindings-removal updated-bindings (list candidate-phi-s))
        for gain = (phi-s-candidate-gain (list candidate-phi-s) phi-minus-phi-s phi-minus-phi-s-bindings state k)
        do (setf (gethash candidate-phi-s storage)
                 (list gain phi-minus-phi-s phi-minus-phi-s-bindings))))
          

(defmethod %update-phi-s-candidates (updated-phi updated-bindings (state generalisation-state) (swap swap) (swap-size (eql 1)) (storage hash-table) (k (eql 2)))
  ;; how do 2-swap candidates change after a 1-swap?
  ;; - remove the entries that have (phi-s swap) in them
  ;; - add entries in combination with (phi-c swap)
  ;; - recompute all other gains
  (loop for candidate-phi-s in (hash-table-keys storage)
        when (member (first (phi-s swap)) candidate-phi-s :test #'equal)
          do (remhash candidate-phi-s storage))
  (loop with new-candidates = (loop for pair in updated-phi
                                    unless (equal pair (first (phi-c swap)))
                                      collect (list pair (first (phi-c swap))))
        for candidate-phi-s in (append new-candidates (hash-table-keys storage))
        for phi-minus-phi-s = (set-difference updated-phi candidate-phi-s :test #'2-swap-equal)
        for phi-minus-phi-s-bindings = (bindings-removal updated-bindings candidate-phi-s)
        for gain = (phi-s-candidate-gain candidate-phi-s phi-minus-phi-s phi-minus-phi-s-bindings state k)
        do (setf (gethash candidate-phi-s storage)
                 (list gain phi-minus-phi-s phi-minus-phi-s-bindings))))
          

(defmethod %update-phi-s-candidates (updated-phi updated-bindings (state generalisation-state) (swap swap) (swap-size (eql 2)) (storage hash-table) (k (eql 2)))
  ;; how do 2-swap candidates change after a 2-swap?
  ;; - remove the entries that have an overlap with (phi-s swap) + phi-s itself
  ;; - add entries in combination with (phi-c swap) + phi-c itself
  ;; - recompute all other gains
  (loop for candidate-phi-s in (hash-table-keys storage)
        when (intersection (phi-s swap) candidate-phi-s :test #'2-swap-equal)
          do (remhash candidate-phi-s storage))
  (loop with new-candidates = (loop for pair in updated-phi
                                    unless (member pair (phi-c swap) :test #'equal)
                                      append (list (list pair (first (phi-c swap)))
                                                   (list pair (second (phi-c swap)))))
        for candidate-phi-s in (append new-candidates (hash-table-keys storage))
        for phi-minus-phi-s = (set-difference updated-phi candidate-phi-s :test #'2-swap-equal)
        for phi-minus-phi-s-bindings = (bindings-removal updated-bindings candidate-phi-s)
        for gain = (phi-s-candidate-gain candidate-phi-s phi-minus-phi-s phi-minus-phi-s-bindings state k)
        do (setf (gethash candidate-phi-s storage)
                 (list gain phi-minus-phi-s phi-minus-phi-s-bindings))))
  

(defun update-phi-s-candidates (updated-phi updated-bindings state swap)
  ;; update all phi-s candidates for k=1 and k=2 according to the given swap
  ;; for k=1; if phi-s was part of the swap, remove it + add the gain for (each item in) phi-c
  ;; for k=2: if phi-s and swap have an intersection, change it + add the gain for phi-c
  ;; both for k=1 and k=2, this means to recompute the gain for every phi-s
  (let ((storage (phi-s-storage state))
        (swap-size (k swap)))
    (loop for k from 1 to 2
          for k-storage = (gethash k storage)
          do (%update-phi-s-candidates updated-phi updated-bindings state swap swap-size k-storage k))
    storage))

;;;;
;;;; Delta_E with singletons given
;;;; -----------------------------


(defun delta-extern-w-singletons (source-set-singletons target-bindings)
  "Count the single occurrence of all bindings with multiplicity 1
   in source-set-of-pairs that have at least an occurrence in
   target-set-of-pairs."
  (loop for binding in source-set-singletons
        when (member binding target-bindings :test #'equal)
        count binding))
;;(length (intersection source-set-singletons target-bindings :test #'equal)))


;;;;
;;;; Update Delta_I(phi)
;;;; -------------------

(defun update-phi-delta-i-extension (phi phi-delta-i delta-i-storage A-pair)
  "Compute Delta_I(phi) when A-pair is added to phi.
   Delta_I(phi U A-pair) = Delta_I(phi) + Delta_I(A-pair) + Delta_E(phi, A-pair) + Delta_E(A-pair, phi)"
  ;; simplify/avoid by using gains
  (+ phi-delta-i
     (get-delta-i delta-i-storage (list A-pair) 1)
     (delta-extern phi (list A-pair))
     (delta-extern (list A-pair) phi)))

(defun update-phi-delta-i-swap (state swap)
  "Compute the new Delta_I(phi) when phi-s is swapped for phi-c.
   Delta_I(phi \ phi-s U phi-c)
   = Delta_I(phi) - (Delta_I(phi-s) + Delta_E(phi\phi-s, phi-s) + Delta_E(phi-s, phi\phi-s))
     + Delta_I(phi-c) + Delta_E(phi\phi-s, phi-c) + Delta_E(phi-c, phi\phi-s)"
  ;; simplify/avoid by using gains
  (with-slots (phi phi-delta-i delta-i-storage) state
    (with-slots (phi-s phi-c k) swap
      (let ((phi-minus-phi-s (set-difference phi phi-s :test #'2-swap-equal))
            (phi-s-delta-i (get-delta-i delta-i-storage phi-s k))
            (phi-c-delta-i (get-delta-i delta-i-storage phi-c k)))
        (+ phi-delta-i
           (- (+ phi-s-delta-i
                 (delta-extern phi-minus-phi-s phi-s)
                 (delta-extern phi-s phi-minus-phi-s)))
           phi-c-delta-i
           (delta-extern phi-minus-phi-s phi-c)
           (delta-extern phi-c phi-minus-phi-s))))))

;;;;
;;;; Swap condition
;;;; --------------

(defun improves-quality-p (phi-s phi-c phi-minus-phi-s phi-delta-i delta-i-storage k)
  "Return t if swapping phi-s with phi-c in phi
   will yield a generalisation of better quality"
  ;; simplify/avoid by using gains
  (let ((phi-s-delta-i (get-delta-i delta-i-storage phi-s k))
        (phi-c-delta-i (get-delta-i delta-i-storage phi-c k)))
    (> (+ phi-delta-i
          (- (+ phi-s-delta-i
                (delta-extern phi-minus-phi-s phi-s)
                (delta-extern phi-s phi-minus-phi-s)))
          phi-c-delta-i
          (delta-extern phi-minus-phi-s phi-c)
          (delta-extern phi-c phi-minus-phi-s))
       phi-delta-i)))

;;;;
;;;; Compute quality for swap candidates
;;;; -----------------------------------

(defun phi-s-candidate-gain (candidate phi-minus-phi-s phi-minus-phi-s-bindings state k)
  (let ((mode (getf (configuration state) :phi-s-selection-mode)))
    (candidate-gain candidate state phi-minus-phi-s phi-minus-phi-s-bindings k mode)))

(defun phi-c-candidate-gain (candidate phi-minus-phi-s phi-minus-phi-s-bindings state k)
  (let ((mode (getf (configuration state) :phi-c-selection-mode)))
    (candidate-gain candidate state phi-minus-phi-s phi-minus-phi-s-bindings k mode)))


(defgeneric candidate-gain (candidate state phi-minus-phi-s phi-minus-phi-s-bindings k mode)
  (:documentation "Compute the quality for a candidate according to the mode"))

(defmethod candidate-gain (candidate state phi-minus-phi-s phi-minus-phi-s-bindings k (mode (eql :random)))
  (declare (ignore candidate state phi-minus-phi-s phi-minus-phi-s-bindings))
  (random 1.0))

(defmethod candidate-gain (candidate state phi-minus-phi-s phi-minus-phi-s-bindings k (mode (eql :gain)))
  (let ((candidate-delta-i (get-delta-i (delta-i-storage state) candidate k))
        (candidate-singletons (get-singletons (singleton-storage state) candidate k))
        (phi-minus-phi-s-singletons
         (loop for (b . count) in phi-minus-phi-s-bindings
               when (= count 1) collect b)))
    (+ candidate-delta-i
       (delta-extern-w-singletons candidate-singletons (mapcar #'car phi-minus-phi-s-bindings))
       (delta-extern-w-singletons phi-minus-phi-s-singletons (mappend #'third candidate)))))
 

;;;;
;;;; make state
;;;; ----------

(defun make-initial-generalisation-state (G1 G2 &rest configuration-plist)
  "Make an initial generalisation state for G1 and G2"
  (let* ((allowp (getf configuration-plist :allow-generalisation-over-constants))
         (gen (gen G1 G2 :allow-generalisation-over-constants allowp)))
    (make-instance 'generalisation-state
                   :gen gen :comp gen
                   :delta-i-storage (pre-compute-delta-i gen 2)
                   :singleton-storage (pre-compute-singletons gen 2)
                   :phi-s-storage (initialise-phi-s-candidates 2)
                   :configuration configuration-plist)))


(defun make-state-from-expansion (state A-pair A-pair-gain)
  (let* ((extended-phi (adjoin A-pair (copy-list (phi state)) :test #'equal))
         (extended-bindings (bindings-extension (phi-bindings state) A-pair))
         (extended-phi-delta-i (+ (phi-delta-i state) A-pair-gain))
         (extended-phi-s-candidates (extend-phi-s-candidates extended-phi extended-bindings state A-pair A-pair-gain))
         (extended-state
          (make-instance 'generalisation-state
                         :phi extended-phi :gen (get-gen state) :comp (comp extended-phi (get-gen state))
                         :phi-delta-i extended-phi-delta-i :phi-bindings extended-bindings
                         :A-pair A-pair :swaps (swaps state)
                         :delta-i-storage (delta-i-storage state)
                         :singleton-storage (singleton-storage state)
                         :phi-s-storage extended-phi-s-candidates
                         :parent state :configuration (configuration state))))
    ;; some checks
    (assert (= (length extended-phi) (1+ (length (phi state)))))
    (assert A-pair)
    (assert (>= extended-phi-delta-i (phi-delta-i state)))
    ;; notify
    (notify pair-added-to-phi A-pair A-pair-gain extended-phi
            extended-phi-delta-i (get-comp extended-state))
    extended-state))

(defun make-state-from-swap (state swap)
  (let* ((updated-phi (do-replacement (copy-list (phi state)) (phi-s swap) (phi-c swap)))
         (updated-bindings (bindings-swap (phi-bindings state) (phi-s swap) (phi-c swap)))
         (updated-phi-delta-i
          (if (and (eql (getf (configuration state) :phi-s-selection-mode) :gain)
                   (eql (getf (configuration state) :phi-c-selection-mode) :gain))
            (+ (- (phi-delta-i state) (phi-s-gain swap)) (phi-c-gain swap))
            (update-phi-delta-i-swap state swap)))
         (updated-phi-s-candidates (update-phi-s-candidates updated-phi updated-bindings state swap))
         (updated-state (make-instance 'generalisation-state
                                       :phi updated-phi :gen (get-gen state) :comp (comp updated-phi (get-gen state))
                                       :phi-delta-i updated-phi-delta-i :phi-bindings updated-bindings
                                       :A-pair (A-pair state) :swaps (cons swap (swaps state))
                                       :delta-i-storage (delta-i-storage state)
                                       :singleton-storage (singleton-storage state)
                                       :phi-s-storage updated-phi-s-candidates
                                       :parent state :configuration (configuration state))))
    ;; some checks
    (assert (= (length updated-phi) (length (phi state))))
    (assert (> updated-phi-delta-i (phi-delta-i state)))
    ;; notify
    (notify pair-swapped-in-phi (phi-s swap) (phi-c swap) updated-phi updated-phi-delta-i)
    (notify debug-swap-event (phi-s swap) (phi-c swap) (A-pair state) (phi-s-rank swap) (phi-c-rank swap))
    updated-state))




#|
(defun compute-cost (state)
  (let* ((pattern (remove-duplicates (mapcar #'first (get-gen state)) :test #'equal))
         (source (remove-duplicates (mapcar #'second (get-gen state)) :test #'equal))
         (pattern-predicates (mapcar #'first (phi state)))
         (source-predicates (mapcar #'second (phi state)))
         (pattern-delta (set-difference pattern pattern-predicates :test #'equal))
         (source-delta (set-difference source source-predicates :test #'equal)))
    (multiple-value-bind (generalisation pattern-bindings source-bindings pattern-delta source-delta)
        (anti-unify-predicate-sequence pattern-predicates source-predicates nil nil nil pattern-delta source-delta)
      (anti-unification-cost pattern source generalisation pattern-delta source-delta pattern-bindings source-bindings :default))))


(defun decoupled-variables (state)
  "count the number of variables of G1 that have
   been bound to more than 1 variable in G2 or vice-versa"
  (let* ((bindings (mappend #'third (phi state)))
         (G1-vars (remove-duplicates (mapcar #'car bindings)))
         (G2-vars (remove-duplicates (mapcar #'cdr bindings)))
         (number-of-decoupled-vars 0)
         (number-of-decouplings 0)
         decoupled-vars)
    (loop for var in G1-vars
          for all-bindings = (find-all var bindings :key #'car)
          for diff-bindings = (remove-duplicates (mapcar #'cdr all-bindings))
          when (> (length diff-bindings) 1)
          do (incf number-of-decoupled-vars)
             (incf number-of-decouplings
                   (loop for (x . rest) on (mapcar #'cdr all-bindings)
                         sum (length (remove x rest))))
             (push var decoupled-vars))
    (loop for var in G2-vars
          for all-bindings = (find-all var bindings :key #'cdr)
          for diff-bindings = (remove-duplicates (mapcar #'car all-bindings))
          when (> (length diff-bindings) 1)
          do (incf number-of-decoupled-vars)
             (incf number-of-decouplings
                   (loop for (x . rest) on (mapcar #'car all-bindings)
                         sum (length (remove x rest))))
             (push var decoupled-vars))
    (values number-of-decouplings number-of-decoupled-vars decoupled-vars)))
|#
               
  

;;;;
;;;; expand state
;;;; ------------

(defgeneric expand (state mode)
  (:documentation "expand the generalisation state"))

(defun %gain-of-pair (state pair)
  (+ (get-delta-i (delta-i-storage state) (list pair) 1)
     (delta-extern-w-singletons (get-singletons (singleton-storage state) (list pair) 1) (mappend #'third (phi state)))
     (delta-extern-w-singletons (get-singleton-bindings state) (third pair))))

(defmethod expand ((state generalisation-state) (mode (eql :random)))
  (let ((A-pair (random-elt (get-comp state))))
    (when A-pair
      (make-state-from-expansion state A-pair (%gain-of-pair state A-pair)))))

(defmethod expand ((state generalisation-state) (mode (eql :gain)))
  (multiple-value-bind (A-pair A-pair-gain)
      (the-biggest #'(lambda (pair) (%gain-of-pair state pair)) (get-comp state))
    (when A-pair
      (make-state-from-expansion state A-pair A-pair-gain))))


;;;;
;;;; get phi-s candidates
;;;; --------------------

(defgeneric get-phi-s-candidates (phi-s-storage Q-pair k conjecture)
  (:documentation "get phi-s candidates from storage"))

(defmethod get-phi-s-candidates (phi-s-storage Q-pair k conjecture)
  (declare (ignore Q-pair conjecture))
  (hash-table-alist (gethash k phi-s-storage)))

(defmethod get-phi-s-candidates (phi-s-storage Q-pair (k (eql 1)) conjecture)
  (declare (ignore Q-pair conjecture))
  (loop for (phi-s gain phi-minus-phi-s bindings) in (hash-table-alist (gethash k phi-s-storage))
        collect (list (list phi-s) gain phi-minus-phi-s bindings)))

(defmethod get-phi-s-candidates (phi-s-storage Q-pair (k (eql 2)) (conjecture (eql t)))
  (let ((candidates nil))
    (loop for phi-s being the hash-key of (gethash k phi-s-storage)
            using (hash-value gain-and-rest)
          when (member Q-pair phi-s :test #'equal)
            do (push (cons phi-s gain-and-rest) candidates))
    candidates))


;;;;
;;;; make phi-c candidates
;;;; ---------------------

(defgeneric make-phi-c-candidates (phi-s phi-minus-phi-s phi-minus-phi-s-bindings state k)
  (:documentation "generate candidates for phi-c"))

(defmethod make-phi-c-candidates (phi-s phi-minus-phi-s phi-minus-phi-s-bindings (state generalisation-state) (k (eql 1)))
  (let ((comp-set (set-difference (comp phi-minus-phi-s (get-gen state)) phi-s :test #'equal)))
    (loop for pair in comp-set
          for gain = (phi-c-candidate-gain (list pair) phi-minus-phi-s phi-minus-phi-s-bindings state k)
          collect (list (list pair) gain))))

(defmethod make-phi-c-candidates (phi-s phi-minus-phi-s phi-minus-phi-s-bindings (state generalisation-state) (k (eql 2)))
  (let* ((comp-set (comp phi-minus-phi-s (get-gen state)))
         (phi-c-candidates
          (loop for (first-pair . remaining-candidates) on comp-set
                append (loop for second-pair in remaining-candidates
                             when (compatible-pairs-p first-pair second-pair)
                               collect (list first-pair second-pair)) into candidates
                finally (return (remove phi-s candidates :test #'2-swap-equal)))))
    (loop for phi-c in phi-c-candidates
          for gain = (phi-c-candidate-gain phi-c phi-minus-phi-s phi-minus-phi-s-bindings state k)
          collect (list phi-c gain))))


;;;;
;;;; select swap
;;;; -----------

(defmethod select-swap ((state generalisation-state) queue k)
  ;; returns an instance of swap or NIL
  (let ((phi-s-selection-mode (getf (configuration state) :phi-s-selection-mode))
        (phi-c-selection-mode (getf (configuration state) :phi-c-selection-mode))
        (conjecture (getf (configuration state) :conjecture)))
    (with-slots (phi phi-delta-i gen delta-i-storage singleton-storage phi-s-storage) state
      (assert (member (first queue) phi :test #'equal))
      (loop until (null queue)
            for Q-pair = (pop queue)
            for phi-s-candidates = (get-phi-s-candidates phi-s-storage Q-pair k conjecture) ;; conjecture or not... 
            do (notify candidate-phi-s-generated phi-s-candidates)
               (loop for (phi-s phi-s-gain phi-minus-phi-s phi-minus-phi-s-bindings) in (sort phi-s-candidates #'< :key #'second)
                     for phi-s-rank from 1
                     for phi-c-candidates = (make-phi-c-candidates phi-s phi-minus-phi-s phi-minus-phi-s-bindings state k)
                     do (notify candidate-phi-c-generated phi-c-candidates)
                        (loop for (phi-c phi-c-gain) in (sort phi-c-candidates #'> :key #'second)
                              for phi-c-rank from 1
                              when (if (and (eql phi-s-selection-mode :gain) (eql phi-c-selection-mode :gain))
                                     (> phi-c-gain phi-s-gain)
                                     (improves-quality-p phi-s phi-c phi-minus-phi-s phi-delta-i delta-i-storage k))
                                do ;; sanity checks
                                   (assert (not (2-swap-equal phi-s phi-c)))
                                   (assert (= (length phi-s) k))
                                   (assert (= (length phi-c) k))
                                   ;; return the swap
                                   (return-from select-swap
                                     (make-instance 'swap
                                                    :k k :phi-s phi-s :phi-c phi-c
                                                    :phi-s-rank phi-s-rank :phi-c-rank phi-c-rank
                                                    :phi-s-gain phi-s-gain :phi-c-gain phi-c-gain
                                                    :num-phi-s-candidates (length phi-s-candidates)
                                                    :num-phi-c-candidates (length phi-c-candidates)))))))))

(defmethod select-all-swaps ((state generalisation-state) queue k)
  ;; returns a list of instances of swap or NIL
  (let ((phi-s-selection-mode (getf (configuration state) :phi-s-selection-mode))
        (phi-c-selection-mode (getf (configuration state) :phi-c-selection-mode))
        (conjecture (getf (configuration state) :conjecture))
        (swaps nil))
    (with-slots (phi phi-delta-i gen delta-i-storage singleton-storage phi-s-storage) state
      (assert (member (first queue) phi :test #'equal))
      (loop until (null queue)
            for Q-pair = (pop queue)
            for phi-s-candidates = (get-phi-s-candidates phi-s-storage Q-pair k conjecture) ;; conjecture or not... 
            do (loop for (phi-s phi-s-gain phi-minus-phi-s phi-minus-phi-s-bindings) in (sort phi-s-candidates #'< :key #'second)
                     for phi-s-rank from 1
                     for phi-c-candidates = (make-phi-c-candidates phi-s phi-minus-phi-s phi-minus-phi-s-bindings state k)
                     do (loop for (phi-c phi-c-gain) in (sort phi-c-candidates #'> :key #'second)
                              for phi-c-rank from 1
                              when (if (and (eql phi-s-selection-mode :gain) (eql phi-c-selection-mode :gain))
                                     (> phi-c-gain phi-s-gain)
                                     (improves-quality-p phi-s phi-c phi-minus-phi-s phi-delta-i delta-i-storage k))
                                do ;; sanity checks
                                  (assert (not (2-swap-equal phi-s phi-c)))
                                  (assert (= (length phi-s) k))
                                  (assert (= (length phi-c) k))
                                  ;; return the swap
                                  (push
                                   (make-instance 'swap
                                                  :k k :phi-s phi-s :phi-c phi-c
                                                  :phi-s-rank phi-s-rank :phi-c-rank phi-c-rank
                                                  :phi-s-gain phi-s-gain :phi-c-gain phi-c-gain
                                                  :num-phi-s-candidates (length phi-s-candidates)
                                                  :num-phi-c-candidates (length phi-c-candidates))
                                   swaps)))))
    swaps))

;;;;
;;;; make 2-swap stable
;;;; ------------------

(defmethod make-2-swap-stable ((state generalisation-state))
  "Performs swaps of max. size 2 until the state is 2-swap stable"
  (loop with stable-state = state
        with queue = (list (A-pair state))
        ;; try 1-swaps and 2-swaps until one succeeds
        for succeeded-swap
          = (loop for k from 1 to 2
                  for swap = (select-swap stable-state (copy-list queue) k)
                  when swap return swap)
        when succeeded-swap
        do ;; overwrite the stable state + update the queue
           ;; the queue starts with A-pair (used for extending the state)
           ;; when a 1-swap replaces A-pair by B-pair, the queue now contains B-pair
           ;; when a 1-swap replaces C-pair by B-pair, the queue now contains A-pair + B-pair
           ;; when a 2-swap replaced A-pair + B-pair with C-pair + D-pair, the queue now contains C-pair + D-pair
           (setf stable-state (make-state-from-swap stable-state succeeded-swap)
                 queue (union (set-difference queue (phi-s succeeded-swap) :test #'2-swap-equal)
                              (set-difference (phi-c succeeded-swap) (phi-s succeeded-swap) :test #'2-swap-equal)
                              :test #'2-swap-equal))
           ;; sanity check:
           (assert (= (phi-delta-i stable-state) (delta-intern (phi stable-state))))
        until (null succeeded-swap)
        ;; repeat until no more swaps can be found
        finally (return stable-state)))


;;;;
;;;; 2-swap generalise
;;;; -----------------

(defun proportion-of-candidates (list-of-swaps)
  "Compute the proportion of phi-s
   candidates that was considered before a candidate
   that satisfies the swap condition was found.
   Average over all swaps."
  (average
   (loop for swap in list-of-swaps
         collect (/ (phi-s-rank swap)
                    (num-phi-s-candidates swap)))))


(defun 2-swap-generalise (G1 G2
                             phi-s-selection-mode
                             phi-c-selection-mode
                             expansion-mode
                             conjecture
                             &key allow-generalisation-over-constants)
  ;; Start from an empty pairing phi
  ;; In each round of the algorithm:
  ;; - add an atomic pairing 'A-pair' to phi
  ;; - perform 2-swaps until the resulting generalisation is stable in its length
  (notify kswap-started G1 G2)
  (loop with queue = (list (make-initial-generalisation-state G1 G2
                                                              :phi-s-selection-mode phi-s-selection-mode
                                                              :phi-c-selection-mode phi-c-selection-mode
                                                              :expansion-mode expansion-mode
                                                              :conjecture conjecture
                                                              :allow-generalisation-over-constants allow-generalisation-over-constants))
        for state = (pop queue)
        ;; expand the state by adding a pair to the generalisation
        for expanded-state = (expand state expansion-mode)
        ;; repeat until no expanded state can be found
        while expanded-state
        ;; perform swaps until stable
        for 2-swap-stable-state = (make-2-swap-stable expanded-state)
        ;; schedule the stable state for the next expansion
        when 2-swap-stable-state
          do (push 2-swap-stable-state queue)
          ;; in the end, return the largest 2-swap stable state
        finally
          (progn
            (notify kswap-finished (phi-delta-i state) (length (swaps state)))
            (return (values (phi state) (phi-delta-i state)
                            (proportion-of-candidates (swaps state)))))))
       


;;;;
;;;; anti-unify-predicate-networks
;;;; -----------------------------

(defun identify-k-swap-alignments (pattern source
                                   allow-generalisation-over-constants
                                   phi-s-selection-mode
                                   phi-c-selection-mode
                                   expansion-mode
                                   conjecture)
  (multiple-value-bind (2-swap-stable-phi phi-delta-i proportion-phi-s)
      (2-swap-generalise pattern source phi-s-selection-mode phi-c-selection-mode expansion-mode conjecture
                        :allow-generalisation-over-constants allow-generalisation-over-constants)
    (let* ((pattern-predicates (mapcar #'first 2-swap-stable-phi))
           (source-predicates (mapcar #'second 2-swap-stable-phi))
           (pattern-delta (set-difference pattern pattern-predicates :test #'equal))
           (source-delta (set-difference source source-predicates :test #'equal)))
      (values
       (list
        (make-instance 'predicate-alignment-state
                       :pattern-predicates pattern-predicates
                       :source-predicates source-predicates
                       :pattern-delta pattern-delta
                       :source-delta source-delta))
       phi-delta-i
       proportion-phi-s))))


(defmethod anti-unify-predicate-networks (pattern source (mode (eql :2-swap))
                                                  &key (cost-mode :msg)
                                                  allow-generalisation-over-constants
                                                  (phi-s-selection-mode :gain)
                                                  (phi-c-selection-mode :gain)
                                                  (expansion-mode :gain)
                                                  (conjecture nil))
  "Anti-unify pattern and source using 2-swap. Returns the anti-unification result,
   its cost, and the size of 'gen'"
  
  ;; Assert that all predicates are unique in pattern and source (just to be safe)
  (assert (= (length pattern) (length (remove-duplicates pattern :test #'equalp))))
  (assert (= (length source) (length (remove-duplicates source :test #'equalp))))

  ;; Loop over all possible alignments of predicates in pattern and source and anti-unify them...
  (loop with (possible-alignments phi-delta-i proportion-phi-s)
          = (multiple-value-list
             (identify-k-swap-alignments pattern source
                                         allow-generalisation-over-constants
                                         phi-s-selection-mode
                                         phi-c-selection-mode
                                         expansion-mode
                                         conjecture))
        for alignment in possible-alignments
        for pattern-in-alignment = (pattern-predicates alignment)
        for source-in-alignment = (source-predicates alignment)
        for pattern-delta = (pattern-delta alignment)
        for source-delta = (source-delta alignment)
        collect (multiple-value-bind (resulting-generalisation
                                      resulting-pattern-bindings
                                      resulting-source-bindings
                                      resulting-pattern-delta
                                      resulting-source-delta)
                    (anti-unify-predicate-sequence pattern-in-alignment source-in-alignment nil nil nil pattern-delta source-delta)
                  (make-instance 'anti-unification-result
                                 :pattern pattern
                                 :source source
                                 :generalisation resulting-generalisation
                                 :pattern-bindings resulting-pattern-bindings
                                 :source-bindings resulting-source-bindings
                                 :pattern-delta resulting-pattern-delta
                                 :source-delta resulting-source-delta
                                 :cost (anti-unification-cost pattern source
                                                              resulting-generalisation
                                                              resulting-pattern-delta
                                                              resulting-source-delta
                                                              resulting-pattern-bindings
                                                              resulting-source-bindings
                                                              cost-mode)))
          into results
        ;; Sort results based on increasing cost.
        finally (return (values (sort results #'< :key #'cost)
                                (cost (first results))
                                phi-delta-i
                                proportion-phi-s))))