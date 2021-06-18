;;;; monitors.lisp

(in-package :clevr-grammar-learning)

;;;; Printing dots
(define-monitor print-a-dot-for-each-interaction
                :documentation "Prints a '.' for each interaction
                 and prints the number after :dot-interval")

(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
  (let ((symbol-to-print (last-elt (repair-buffer experiment))))
    (cond ((= (interaction-number interaction) 1)
           (format t "~%~a" symbol-to-print))
          ((= (mod (interaction-number interaction)
                   (get-configuration experiment :dot-interval)) 0)
           (format t "~a (~a)~%" symbol-to-print (interaction-number interaction)))
         ;(wi:clear-page))
          (t (format t "~a" symbol-to-print)))))

;;;; export failed sentences and applied cxns
(define-monitor log-interactions)

(defvar *log-file* nil)

(define-event-handler (log-interactions log-parsing-finished)
  (unless *log-file*
    (setf *log-file*
          (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data")
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


;;;; export grammar after series
(define-monitor export-learner-grammar
                :class 'store-monitor
                :file-name (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data") :name "learner-grammar" :type "store"))

(defun make-file-name-with-time-and-series (file-name series)
  (make-file-name-with-time
   (mkstr
    (make-pathname :directory (pathname-directory file-name))
    (pathname-name file-name) "-series-" series  "." (pathname-type file-name))))
  
(define-event-handler (export-learner-grammar run-series-finished)  
  (export-grammar (grammar (learner experiment))
                  (make-file-name-with-time-and-series (file-name monitor) (series-number experiment))))
                  
(defun export-grammar (cxn-inventory path)
  #-ccl (cl-store:store cxn-inventory path))


(define-monitor export-type-hierarchy-to-json
                :class 'store-monitor
                :file-name (babel-pathname :directory '("experiments" "clevr-grammar-learning" "raw-data")
                                            :name "type-hierarchy"
                                            :type "json"))

(defun export-th-to-json (cxn-inventory path)
  (let* ((g (type-hierarchies::graph
             (get-type-hierarchy cxn-inventory)))
         ;; get a list of all node names
         (all-nodes
          (mapcar #'mkstr (graph-utils::list-nodes g)))
         ;; get a list of all the edges
         ;; this include the edge-type
         ;; but excludes the weight
         (all-edges (graph-utils::list-edges g))
         ;; so get the weight separately
         (all-edges-with-weight
          (loop for (from to etype) in all-edges
                for w = (graph-utils:edge-weight g from to etype)
                collect (list (mkstr from) (mkstr to) w))))
    (ensure-directories-exist path)
    (with-open-file (stream path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string
       (cl-json:encode-json-alist-to-string
        `((nodes . ,all-nodes)
          (edges . ,all-edges-with-weight)))
       stream)
      (force-output stream))))


(defun import-th-from-json (cxn-inventory path)
  (let* ((g-data (cl-json:decode-json-from-source path))
         (all-nodes (rest (assoc :nodes g-data)))
         (all-weighted-edges (rest (assoc :edges g-data)))
         (th (make-instance 'type-hierarchy)))
    (loop for node in all-nodes
          for name = (intern (upcase (mkstr node)) :type-hierarchies)
          do (add-category name th))
    (loop for (from to w) in all-weighted-edges
          for from-name = (intern (upcase (mkstr from)) :type-hierarchies)
          for to-name = (intern (upcase (mkstr to)) :type-hierarchies)
          do (add-link from-name to-name th :weight w))
    (set-type-hierarchy cxn-inventory th)))
         
   
(define-event-handler (export-type-hierarchy-to-json run-series-finished)
  (let ((cxn-inventory (grammar (learner experiment)))
        (path (make-file-name-with-time-and-series (file-name monitor) (series-number experiment))))
    (export-th-to-json cxn-inventory path)))


;;;; export type hierarchy to image
(define-monitor export-type-hierarchy-to-image
                :class 'store-monitor
                :file-name (babel-pathname :name "type-hierarchy" :type "pdf"
                                            :directory '("experiments" "clevr-grammar-learning" "raw-data")))

(defun export-type-hierarchy-to-image (type-hierarchy path)
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

(define-event-handler (export-type-hierarchy-to-image run-series-finished)
  (let* ((th (get-type-hierarchy (grammar (learner experiment))))
         (th-copy (copy-object th))
         (path (make-file-name-with-time-and-series (file-name monitor) (series-number experiment))))
    (export-type-hierarchy-to-image
     (remove-non-connected-nodes th-copy)
     path)))


(defun get-all-export-monitors ()
  '("export-type-hierarchy-to-image"
    "export-type-hierarchy-to-json"
    "export-learner-grammar"))
  
    
