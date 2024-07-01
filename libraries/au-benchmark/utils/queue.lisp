(in-package :au-benchmark.base)

;; this file is a copy of Peter Norvig's aima queue implementation
;; http://aima.cs.berkeley.edu/lisp/utilities/queue.lisp

(export '(queue key elements make-empty-queue empty-queue? queue-front remove-front
          enqueue-at-front enqueue-at-end enqueue-by-priority queue-length heap-sort merge-sort merge-lists))

;;;; The Queue datatype

(defclass queue ()
  ((key :initarg :key :accessor key :initform #'identity)
   (last-cons :initarg :last-cons :accessor last-cons :initform nil)
   (elements :initarg :elements :accessor elements :initform nil)))
  
;;;; Basic Operations on Queues

(defun empty-queue? (q)
  "Are there no elements in the queue?"
  (= (length (elements q)) 0))

(defun queue-front (q)
  "Return the element at the front of the queue."
  (elt (elements q) 0))

(defun remove-front (q)
  "Remove the element from the front of the queue and return it."
  (pop (elements q)))

(defun queue-length (q)
  (length (elements q)))

;;;; The Enqueing Functions

(defun enqueue-at-front (q items)
  "Add a list of items to the front of the queue."
  (when (empty-queue? q)
    (setf (last-cons q) (last items)))
  (setf (elements q) (nconc items (elements q)))
  q)


(defun enqueue-at-end (q items)
  "Add a list of items to the end of the queue."
  ;; To make this more efficient, keep a pointer to the last cons in the queue
  (cond ((null items) nil)
	((or (null (last-cons q))
             (null (elements q)))
	 (setf (last-cons q) (last items)
	       (elements q) (nconc (elements q) items)))
	(t (setf (cdr (last-cons q)) items
		 (last-cons q) (last items))))
  q)