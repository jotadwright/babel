;;;; monitors.lisp

(in-package :cgl)

;;;; Printing dots
(defparameter *repair-to-id-map*
  '((add-holophrase . "0")
    (holophrase->item-based . "1")
    (lexical->item-based . "2")
    (item-based->lexical . "3")
    (add-th-links . "4")
    (add-th-links-formulation . "5")))
    
(define-monitor print-a-dot-for-each-interaction
                :documentation "Prints a '.' for each interaction
                 and prints the number after :dot-interval")

(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
  (let ((symbol-to-print
         (cond ((find-data interaction :applied-repair)
                (rest (assoc (find-data interaction :applied-repair) *repair-to-id-map*)))
               ((communicated-successfully interaction) ".")
               (t "x"))))
    (cond ((= (interaction-number interaction) 1)
           (format t "~%~a" symbol-to-print))
          ((= (mod (interaction-number interaction)
                   (get-configuration experiment :dot-interval)) 0)
           (format t "~a (~a)~%" symbol-to-print (interaction-number interaction))
           (wi:clear-page))
          (t (format t "~a" symbol-to-print)))))


#|
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
|#


;;;; export type hierarchy to json
;;;; allows to reconstruct the type hierarchy, regardless
;;;; of which Lisp is used to import/export
(define-monitor export-type-hierarchy-to-json
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :directory '("experiments" "clevr-learning" "raw-data")
                                            :name (format nil "type-hierarchy-~a" (make-random-string 5))
                                            :type "json")))

(defun export-th-to-json (cxn-inventory path)
  (let* ((g (fcg::graph (categorial-network cxn-inventory)))
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
          for name = (intern (upcase (mkstr node)) :fcg)
          do (add-category name th))
    (loop for (from to w) in all-weighted-edges
          for from-name = (intern (upcase (mkstr from)) :fcg)
          for to-name = (intern (upcase (mkstr to)) :fcg)
          do (add-link from-name to-name th :weight w))
    (set-categorial-network cxn-inventory th)))
         
   
(define-event-handler (export-type-hierarchy-to-json run-series-finished)
  (let ((cxn-inventory (grammar (learner experiment)))
        (path (file-name monitor)))
    (export-th-to-json cxn-inventory path)))


;;;; export type hierarchy to image
(define-monitor export-type-hierarchy-to-image
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :name (format nil "type-hierarchy-~a" (make-random-string 5)) :type "pdf"
                                            :directory '("experiments" "clevr-learning" "raw-data"))))

(defun export-type-hierarchy-to-image (type-hierarchy path)
  (categorial-network->image
   type-hierarchy :render-program "fdp" :weights? t
   :path (make-pathname :directory (pathname-directory path))
   :file-name (pathname-name path)
   :format "pdf"))

(defun remove-non-connected-nodes (th)
  (let ((fcg::graph (fcg::graph th)))
    (loop for category being each hash-key of (graph-utils::nodes graph)
          when (= (graph-utils::degree graph category) 0)
          do (graph-utils::delete-node graph category))
    th))

(define-event-handler (export-type-hierarchy-to-image run-series-finished)
  (let* ((th (categorial-network (grammar (learner experiment))))
         (th-copy (copy-object th))
         (path (file-name monitor)))
    (export-type-hierarchy-to-image
     (remove-non-connected-nodes th-copy)
     path)))

;;;; export type hierarchy every nth interaction
(define-monitor export-type-hierarchy-to-image-every-nth-interaction
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :name "type-hierarchy" :type "pdf"
                                            :directory '("experiments" "clevr-learning" "raw-data"))))

(define-event-handler (export-type-hierarchy-to-image-every-nth-interaction interaction-finished)
  (let ((interaction-nr (interaction-number (current-interaction experiment)))
        (n (get-configuration-or-default experiment :export-interval 100)))
    (when (= (mod interaction-nr n) 0)
      (let ((th (categorial-network (grammar (learner experiment))))
            (path (make-pathname
                   :directory (pathname-directory (file-name monitor))
                   :name (format nil "~a-~a"
                                 (pathname-name (file-name monitor))
                                 interaction-nr)
                   :type (pathname-type (file-name monitor)))))
        (categorial-network-components->images
         th :render-program "circo" :weights? t
         :path (pathname-directory path)
         :file-name (pathname-name path) :format "pdf"
         :minimum-component-size 1)))))

;;;; export grammar after series
(define-monitor export-learner-grammar
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :name (format nil "learner-grammar-~a" (make-random-string 5)) :type "store"
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


;;;; export weighted coherence for lexical items
(define-monitor export-weighted-lexical-coherence
                :class 'store-monitor
                :file-name (make-file-name-with-time
                            (babel-pathname :name (format nil "lexical-coherence-~a" (make-random-string 5))
                                            :type "json"
                                            :directory '("experiments" "clevr-learning" "raw-data"))))

(define-event-handler (export-weighted-lexical-coherence run-series-finished)
  (let ((cxn-inventory (grammar (learner experiment)))
        (path (file-name monitor)))
    (export-lexical-coherence cxn-inventory path)))

(defun common-neighbours (node-1 node-2 graph)
  (let ((neighbours-node-1 (mapcar #'cdr (graph-utils::neighbors graph node-1)))
        (neighbours-node-2 (mapcar #'cdr (graph-utils::neighbors graph node-2))))
    (remove-duplicates (intersection neighbours-node-1 neighbours-node-2))))

(defun weighted-graph-cosine-similarity (node-1 node-2 graph)
  (let* ((node-id-1 (graph-utils:lookup-node graph node-1))
         (node-id-2 (graph-utils:lookup-node graph node-2)))
    (when (and node-id-1 node-id-2)
      (let ((denominator
             (sqrt
              (* (loop for (nil . degree-1) in (remove-duplicates (graph-utils::neighbors graph node-id-1))
                       sum (expt (graph-utils:edge-weight graph degree-1 node-id-1) 2))
                 (loop for (nil . degree-2) in (remove-duplicates (graph-utils::neighbors graph node-id-2))
                       sum (expt (graph-utils:edge-weight graph  degree-2 node-id-2) 2))))))
        (if (> denominator 0)
          (/ (loop for common-neighbour in (common-neighbours node-id-1 node-id-2 graph)
                   sum (* (graph-utils:edge-weight graph node-id-1 common-neighbour)
                          (graph-utils:edge-weight graph node-id-2 common-neighbour)))
             denominator)
          0)))))

(defun similar-nodes-weighted-cosine (node graph)
  "loop through all nodes in graph, sort by weighted cosine similarity"
  (loop for other-node in (remove-duplicates (remove node (graph-utils::list-nodes graph)))
        for node-similarity = (weighted-graph-cosine-similarity other-node node graph)
        when (> node-similarity 0)
        collect (cons other-node node-similarity) into similar-nodes
        finally (return (sort similar-nodes #'> :key #'cdr))))

(defun export-lexical-coherence (cxn-inventory path)
  (ensure-directories-exist path)
  (let* ((g (fcg::graph (categorial-network cxn-inventory)))
         (lexical-classes
          (mapcar #'lex-class-cxn
                  (find-all 'lexical
                            (constructions-list cxn-inventory)
                            :key #'get-cxn-type)))
         (coherence-data
          (loop for lex-class in lexical-classes
                for similar-nodes
                = (mapcar #'(lambda (lex-class-cosine-similarity-pair)
                              (cons (get-base-name (car lex-class-cosine-similarity-pair))
                                    (cdr lex-class-cosine-similarity-pair)))
                          (similar-nodes-weighted-cosine lex-class g))
              collect (cons (get-base-name lex-class) similar-nodes))))
    (with-open-file (stream path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string
       (downcase
        (cl-json:encode-json-alist-to-string coherence-data))
       stream)
      (force-output stream))))




(defun get-all-export-monitors ()
  '("export-type-hierarchy-to-image"
    ;"export-type-hierarchy-to-json"
    ;"export-learner-grammar"
    ;"export-weighted-lexical-coherence"
    ))
  
    
