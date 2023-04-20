(in-package :duckie-language-learning)

;; ---------------------------------
;; + Diagnostic: partial utterance +
;; ---------------------------------

;; problem
(defclass partial-utterance-problem (problem)
  () (:documentation "Problem created when part of the utterance is known."))

;; diagnostic class
(defclass diagnose-partial-utterance (diagnostic)
  ((trigger :initform 'fcg::new-node)))

;; Every node that can have a possible 'partial utterance' diagnoses
;; is pushed onto a list (stored in the initial node). This is because
;; a solution may still appear later in the search tree and we want
;; the agent to use routine processing as much as possible. When the
;; search arrives back at the initial node, with all nodes expanded
;; and still no solution, the diagnositcs are triggered manually
;; over the candidate list until one of them succeeds. If all fail,
;; the unknown-utterance diagnostic will take over.
(defmethod diagnose ((diagnostic diagnose-partial-utterance)
                     (node cip-node) &key &allow-other-keys)
  (when (eql (direction (cip node)) '<-)
    (when (and (fully-expanded? node)
               (get-strings-from-root node)
               (not (some-applied-repair-in-tree node)))
      (cond ((applied-constructions node)
             ;; CASE 1: there are some applied constructions,
             (if (and (null (queue (cip node)))
                      (not (interpretation-failed-in-tree node)))
               ;; CASE 1A: queue is empty
               ;; -> diagnostic was called manually on an item from the candidate list
               ;; -> thus, create the actual problem object
               (let ((agent (find-data (blackboard (construction-inventory node)) :owner))
                     (problem (make-instance 'partial-utterance-problem)))
                 (push (type-of problem) (statuses node))
                 (push 'fcg::diagnostic-triggered (statuses node))
                 (notify diagnostic-trigger node diagnostic)
                 (set-data problem :owner agent)
                 problem)
               ;; CASE 1B: queue is not yet empty
               ;; -> push node on candidate list
               (progn (push-candidate node) nil)))
            ;; CASE 2: NO APPLIED CONSTRUCTIONS + EMPTY QUEUE
            ;; -> Arrived at the initial node without any solution   
            ((or (null (queue (cip node)))
                 (notany #'null (mapcar #'fully-expanded? (cons node (queue (cip node))))))
             ;; -> Thus, manually iterate over candidates diagnose each 
             ;; ->    until solution is found
             (loop for candidate in (sort-candidates (get-candidates node)) ;; sorted by most-applied
                   for (problems fixes) = (multiple-value-list
                                           (notify-learning candidate :trigger 'fcg::new-node))
                   when problems
                     do (progn
                          (loop for problem in problems
                                do (push (type-of problem) (statuses candidate)))
                          (push 'fcg::diagnostic-triggered (statuses candidate))
                          (notify diagnostic-trigger node diagnostic)
                          nil)
                   when fixes return nil))))))

;; helper functions

(defun some-applied-repair-in-tree (node)
  ;; some node in the tree can be added by a repair
  ;; luckily, the handle-fix methods write this
  ;; on the blackboard of the initial node, so
  ;; we don't have to traverse the entire tree
  (multiple-value-bind (some-repair-applied-p foundp)
      (find-data (gl::initial-node node) :some-repair-applied)
    (when foundp some-repair-applied-p)))

(defun push-candidate (node)
  ;; push this node on the candidate list,
  ;; stored in the initial node
  (push-data (gl::initial-node node) :candidates node))

(defun get-candidates (node)
  ;; get the candidate list from the
  ;; initial node
  (find-data (gl::initial-node node) :candidates))

(defun sort-candidates (candidates)
  ;; sort the candidates on number of applied cxns
  ;; and on the order in which they were created
  (sort candidates
        #'(lambda (node-1 node-2)
            (cond ((length= (applied-constructions node-1)
                            (applied-constructions node-2))
                   (< (created-at node-1) (created-at node-2)))
                  ((length> (applied-constructions node-1)
                            (applied-constructions node-2)) t)
                  (t nil)))))
