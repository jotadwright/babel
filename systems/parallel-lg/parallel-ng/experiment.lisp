(in-package :parallel-lg)

;; (ql:quickload :parallel-lg)

;;;;;;;;;;;;;;;;;;;;
;;                ;;
;; Configurations ;;
;;                ;;
;;;;;;;;;;;;;;;;;;;;

(define-configuration-default-value :world-size 5)
(define-configuration-default-value :population-size 10)
(define-configuration-default-value :nr-of-games-in-parallel 10)



(defclass experiment (blackboard configuration)
  ((id :initarg :id :accessor id :initform (make-id 'experiment)
       :documentation "id of the experiment")
   (population
    :documentation ""
    :type list
    :initarg :population
    :accessor population
    :initform nil)
   (world
    :documentation ""
    :type world
    :initarg :world
    :accessor world
    :initform nil)
   (timestep
    :documentation ""
    :type number
    :initarg :timestep
    :accessor timestep
    :initform 0)))

(defmethod print-object ((experiment experiment) stream)
  (format stream "<~(~:w~): timestep ~a>" (type-of experiment) (timestep experiment)))

(defclass agent (blackboard configuration)
  ((id
    :documentation ""
    :type (or symbol fixnum)
    :initarg :id
    :initform (make-id "AGENT")
    :reader id)
   (cxn-inventory
    :documentation "The construction inventory of the agent."
    :type (or null fcg-construcion-set)
    :accessor cxn-inventory
    :initarg :cxn-inventory)))

(defmethod print-object ((agent agent) stream)
  (format stream "<~(~:w~) ~a>" (type-of agent) (id agent))))

(defclass world ()
  ())


;#################################################################################


(defclass parallel-ng-experiment (experiment)
  ())

(defclass parallel-ng-agent (agent)
  ())

(defclass parallel-ng-world (world)
  ((objects
    :documentation ""
    :type list
    :initarg :objects
    :accessor objects
    :initform nil)))

(defmethod initialize-instance :after ((experiment parallel-ng-experiment) &key)

  (setf (world experiment) (make-instance 'parallel-ng-world
                                          :objects (loop for i from 1 upto (get-configuration experiment :world-size)
                                                         collect (intern (format nil "O-~a" i)))))

  (setf (population experiment) (loop for i from 1 upto (get-configuration experiment :population-size)
                                      collect (make-instance 'parallel-ng-agent
                                                             :id i
                                                             :cxn-inventory (make-empty-cxn-inventory)
                                                             :configuration (configuration experiment)
                                                             :data (data experiment)))))

(defmethod run-experiment ((experiment parallel-ng-experiment) &key (nr-of-steps 100))

  ;; Start client processes

  ;; run steps

  ;; aggregate data

  )


#|
 
(defun process-corpus-in-parallel (input-file output-file &key
                                              (external-lisp *external-lisp*)
                                              (asdf-systems nil)
                                              (read-fn 'read-line-from-stream)
                                              (process-fn 'identity)
                                              (write-fn 'identity)
                                              (nr-of-processes 4)
                                              (keep-order nil))
  ;; First make sure all fasl files exist, so that the client processes do not get confused with each others compiled files
  (ql:quickload asdf-systems)
  ;; First start up all the client-processes
  (let ((processes (loop for i from 1 upto nr-of-processes
                         collect (spawn-client-process :id (intern (format nil "CLIENT-PROCESS-~a" i))
                                                       :external-lisp external-lisp
                                                       :asdf-systems asdf-systems
                                                       :process-fn process-fn
                                                       :write-fn write-fn
                                                       :keep-order keep-order))))
    (unwind-protect (progn
                      ;; Now, open the corpus
                      (with-open-file (corpus input-file :direction :input)
                        ;; Loop through the corpus
                        (loop for input-item = (funcall read-fn corpus)
                              for item-nr from 1
                              ;; until you're through
                              while input-item
                              ;; Select an idle client-process, potentially waiting for one...
                              for idle-process = (wait-for-idle-client-process processes) 
                              do
                              ;; Process the current corpus item on this process and lock it
                              (lock idle-process)
                              (write-to-client-process idle-process (format nil "(run ~a ~a)" item-nr
                                                                            (if (stringp input-item)
                                                                              (format nil "~s" input-item)
                                                                              input-item)))))

                      ;; Make sure to wait until all processes have finished
                      (wait-until-all-process-idle processes)

                      ;; Then, aggregate all the output data and optionally sort everything
                      (format t "~%Aggregating output of client processes...~%" output-file)
                      (if keep-order
                        (progn (format t "~%Sorting output...~%")
                          (let* ((output-data (loop for temp-file in (mapcar #'output-file processes)
                                                    when (probe-file temp-file)
                                                    append
                                                    (with-open-file (temp-stream temp-file :direction :input)
                                                      (loop for output-line = (read-line temp-stream nil)
                                                            while output-line
                                                            collect (let ((output-parts (multiple-value-list (split-sequence:split-sequence #\. output-line :count 1))))
                                                                      (cons (parse-integer (first (first output-parts)))
                                                                            (subseq output-line (second output-parts))))))))
                                 (output-data-array (let ((array (make-array (length output-data))))
                                                      (loop for el in output-data
                                                            do (setf (aref array (- (car el) 1))
                                                                     (cdr el))
                                                            finally (return array)))))
                            (with-open-file (output-stream output-file :direction :output :if-does-not-exist :create :if-exists :supersede)
                              (loop for el across output-data-array
                                    do (format output-stream "~a~%" el)))))
                        (uiop/stream:concatenate-files (loop for file in (mapcar #'output-file processes)
                                                             when (probe-file file)
                                                             collect file) output-file))
                      
                      (loop for file in (mapcar #'output-file processes)
                            when (probe-file file)
                            do (delete-file file))
                      (format t "~%Output written to: ~a~%" output-file)
                          
                      ;; Finally kill the processes
                      (format t "~%Closing client processes...~%~%")
                      (mapcar #'terminate processes)
                      (format t "~%Finished!~%"))
      (mapcar #'terminate processes))))
|#


(defmethod run-step ()

  ;; select interacting agents
  ;; send to client processes
  
  )


;;(defparameter *A* (make-instance 'parallel-ng-experiment))






(defun make-empty-cxn-inventory ()
  (let ((grammar-name (make-const "cxn-inventory")))
    (eval
     `(def-fcg-constructions ,grammar-name :cxn-inventory ,grammar-name))))





