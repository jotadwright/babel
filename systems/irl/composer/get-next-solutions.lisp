(in-package :irl)

;; #########################################
;; composer search
;; -----------------------------------------

(define-event chunk-composer-configuration
  (composer chunk-composer))
(define-event chunk-composer-get-next-solutions-started
  (composer chunk-composer))
(define-event chunk-composer-get-all-solutions-started
  (composer chunk-composer))
(define-event chunk-composer-get-solutions-until-started
  (composer chunk-composer))
(define-event chunk-composer-node-handled
  (node chunk-composer-node)
  (handler symbol))
(define-event chunk-composer-new-nodes
  (nodes list))
(define-event chunk-composer-next-node
  (node chunk-composer-node))
(define-event chunk-composer-finished
  (solutions list) (composer chunk-composer))

(export '(get-next-solutions
          get-all-solutions
          get-solutions-until))

(defgeneric enqueue-chunk-node (node composer mode)
  (:documentation "Puts a node in the queue"))

(defmethod enqueue-chunk-node ((node chunk-composer-node)
                               (composer chunk-composer)
                               (mode (eql :best-first)))
  (when (next-handler node)
    (setf (queue composer)
          (sorted-insert (queue composer) node
                         :key #'cost :test #'<))))
                 

(defun get-next-solutions (composer &key silent)
  (unless silent
    (notify chunk-composer-configuration composer)
    (notify chunk-composer-get-next-solutions-started composer))
  (when (queue composer)
    (loop
     with queue-mode = (get-configuration composer :queue-mode)
     for node = (pop (queue composer))
     for handler = (next-handler node)
     for (solutions new-nodes)
     ;; handle the node
     = (multiple-value-list
        (handle-node node handler composer))
     do (progn (unless silent (notify chunk-composer-node-handled node handler))
          (enqueue-chunk-node node composer queue-mode))
     ;; handle new nodes
     when new-nodes
     do (loop for new-node in new-nodes
              do (add-node composer new-node :parent node)
              do (enqueue-chunk-node new-node composer queue-mode)
              finally (unless silent (notify chunk-composer-new-nodes new-nodes)))
     ;; handle solutions
     when solutions
     do (progn
          (loop for solution in solutions
                do (setf (score solution)
                         (run-chunk-evaluation-result-score
                          solution composer)))
          (setf (solutions composer)
                (sort (append solutions (solutions composer))
                      #'> :key #'score)))
     ;; continue loop
     when (and (queue composer) (not silent))
     do (notify chunk-composer-next-node (first (queue composer)))
     while (queue composer)
     until solutions
     finally
     (progn
       ;; notify
       (unless silent (notify chunk-composer-finished solutions composer))
       ;; return the solutions
       (return solutions)))))


(defun get-all-solutions (composer &key silent)
  (unless silent
    (notify chunk-composer-configuration composer)
    (notify chunk-composer-get-all-solutions-started composer))
  (loop while (queue composer)
        do (get-next-solutions composer :silent silent))
  (solutions composer))


(defun get-solutions-until (composer &key (stop-criteria #'identity)
                                     silent)
  (unless silent
    (notify chunk-composer-configuration composer)
    (notify chunk-composer-get-solutions-until-started composer))
  (sort
   (loop while (and (queue composer)
                    (not (funcall stop-criteria composer)))
         append (get-next-solutions composer :silent silent))
   #'> :key #'score))