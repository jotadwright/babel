;;;; monitors.lisp

(in-package :clevr-learning)

;;;; Printing dots
(define-monitor print-a-dot-for-each-interaction
                :documentation "Prints a '.' for each interaction
                 and prints the number after :dot-interval")

(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
  (cond ((= (interaction-number interaction) 1)
         (format t "~%."))
        ((= (mod (interaction-number interaction)
                 (get-configuration experiment :dot-interval)) 0)
         (format t ". (~a)~%" (interaction-number interaction))
         (wi:clear-page))
        (t (format t "."))))

;;;; export failed sentences and applied cxns
(define-monitor log-interactions)

(defvar *log-file* nil)

(define-event-handler (log-interactions log-parsing-finished)
  (unless *log-file*
    (setf *log-file*
          (babel-pathname :directory '("experiments" "clevr-learning" "raw-data")
                          :name (format nil "log-~a" (make-random-string 5))
                          :type "txt")))
  (let ((succeededp
         (when (rest (assoc 'cipn process-result-data))
           (find 'fcg::succeeded
                 (fcg::statuses
                  (rest (assoc 'cipn process-result-data)))))))
    (unless succeededp
      (let* ((interaction-nr
              (interaction-number (current-interaction (experiment agent))))
             (applied-cxns
              (when (rest (assoc 'applied-cxns process-result-data))
                (mapcar (compose #'downcase #'mkstr #'name)
                        (rest (assoc 'applied-cxns process-result-data)))))
             (utterance (utterance agent)))
        (with-open-file (stream *log-file* :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
          (write-line
           (format nil "~%Interaction ~a - Parsing failed - \"~a\" - ~{~a~^, ~}"
                   interaction-nr
                   utterance
                   (if applied-cxns
                     applied-cxns '(nil)))
           stream))))))

(define-event-handler (log-interactions log-interaction-finished)
  (unless *log-file*
    (setf *log-file*
          (babel-pathname :directory '("experiments" "clevr-learning" "raw-data")
                          :name (format nil "log-~a" (make-random-string 5))
                          :type "txt")))
  (unless success
    (let* ((interaction-nr
            (interaction-number (current-interaction (experiment agent))))
           (applied-cxns
            (mapcar (compose #'downcase #'mkstr #'name)
                    (find-data process-input 'applied-cxns)))
           (utterance (utterance agent)))
      (with-open-file (stream *log-file* :direction :output
                              :if-exists :append
                              :if-does-not-exist :create)
        (write-line
         (format nil "~%Interaction ~a - Interpretation failed - \"~a\" - ~{~a~^, ~}"
                 interaction-nr
                 utterance
                 (if applied-cxns
                   applied-cxns '(nil)))
         stream)))))
          


;;;; export type hierarchy after series
(define-monitor export-type-hierarchy
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :name (format nil "type-hierarchy-~a"
                                                          (make-random-string 5))
                                            :type "pdf"
                                            :directory '("experiments" "clevr-learning" "raw-data"))))

(defun export-type-hierarchy (type-hierarchy path)
  (type-hierarchy->image
   type-hierarchy :render-program "circo" :weights? t
   :path (make-pathname :directory (pathname-directory path))
   :file-name (pathname-name path)
   :format "pdf"))

(defun remove-non-connected-nodes (th)
  (let ((graph (type-hierarchies::graph th)))
    (loop for category being each hash-key of (graph-utils::nodes graph)
          when (= (graph-utils::degree graph category) 0)
          do (graph-utils::delete-node graph category))
    th))

(define-event-handler (export-type-hierarchy run-series-finished)
  (let* ((th (get-type-hierarchy (grammar (learner experiment))))
         (th-copy (copy-object th))
         (path (file-name monitor)))
    (export-type-hierarchy
     (remove-non-connected-nodes th-copy)
     path)))

;;;; export type hierarchy every nth interaction
(define-monitor export-type-hierarchy-every-nth-interaction
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :name "type-hierarchy" :type "pdf"
                                            :directory '("experiments" "clevr-learning" "raw-data"))))

(define-event-handler (export-type-hierarchy-every-nth-interaction interaction-finished)
  (let ((interaction-nr (interaction-number (current-interaction experiment)))
        (n (get-configuration-or-default experiment :export-interval 100)))
    (when (= (mod interaction-nr n) 0)
      (let ((th (get-type-hierarchy (grammar (learner experiment))))
            (path (make-pathname
                   :directory (pathname-directory (file-name monitor))
                   :name (format nil "~a-~a"
                                 (pathname-name (file-name monitor))
                                 interaction-nr)
                   :type (pathname-type (file-name monitor)))))
        (type-hierarchy-components->images
         th :render-program "circo" :weights? t
         :path (pathname-directory path)
         :file-name (pathname-name path) :format "pdf"
         :minimum-component-size 1)))))

;;;; export grammar after series
(define-monitor export-learner-grammar
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :name (format nil "learner-grammar-~a"
                                                          (make-random-string 5))
                                            :type "store"
                                            :directory '("experiments" "clevr-learning" "raw-data"))))

(defun export-grammar (cxn-inventory path)
  #-ccl (cl-store:store cxn-inventory path))

(define-event-handler (export-learner-grammar run-series-finished)
  (export-grammar (grammar (learner experiment))
                  (file-name monitor)))

;;;; export grammar every nth interaction
(define-monitor export-learner-grammar-every-nth-interaction
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :name "learner-grammar" :type "store"
                                            :directory '("experiments" "clevr-learning" "raw-data"))))

(define-event-handler (export-learner-grammar-every-nth-interaction interaction-finished)
  (let ((interaction-nr (interaction-number (current-interaction experiment)))
        (n (get-configuration-or-default experiment :export-interval 100)))
    (when (= (mod interaction-nr n) 0)
      (let ((pathname
             (make-pathname :directory (pathname-directory (file-name monitor))
                            :name (format nil "~a-~a" (pathname-name (file-name monitor))
                                          interaction-nr)
                            :type (pathname-type (file-name monitor)))))
        (export-grammar (grammar (learner experiment))
                        pathname)))))





(defun get-all-export-monitors ()
  '("export-type-hierarchy"
    "export-learner-grammar"))
  
    
