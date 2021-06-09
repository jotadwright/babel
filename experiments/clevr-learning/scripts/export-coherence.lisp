;;;; export coherence


(ql:quickload :clevr-learning)
(in-package :clevr-learning)


(defun common-neighbours (node-1 node-2 graph)
  (let ((neighbours-node-1 (mapcar #'cdr (graph-utils::neighbors graph node-1)))
        (neighbours-node-2 (mapcar #'cdr (graph-utils::neighbors graph node-2))))
    (remove-duplicates (intersection neighbours-node-1 neighbours-node-2))))

(defun graph-cosine-similarity (node-1 node-2 graph)
  "(GCS i j) = (/ (number-common-neighbours i j) (sqrt (* (degree i) (degree j))))"
  (let* ((node-id-1 (graph-utils:lookup-node graph node-1))
         (node-id-2 (graph-utils:lookup-node graph node-2)))
    (when (and node-id-1 node-id-2)
      (let*
          ((common-neighbours (common-neighbours node-id-1 node-id-2 graph))
           (degree-node-1 (length (remove-duplicates (mapcar #'cdr (graph-utils::neighbors graph node-id-1)))))
           (degree-node-2 (length (remove-duplicates (mapcar #'cdr (graph-utils::neighbors graph node-id-2)))))
           (denominator (sqrt (* degree-node-1 degree-node-2))))
        (if (> denominator 0)
          (/ (length common-neighbours) denominator) 0)))))

(defun similar-nodes-cosine (node graph)
  "loop through all nodes in graph, sort by cosine similarity"
  (loop for other-node in (remove-duplicates (graph-utils::list-nodes graph))
        for node-similarity = (graph-cosine-similarity other-node node graph)
        when (> node-similarity 0)
        collect (cons other-node node-similarity)
        into similar-nodes
        finally (return (sort similar-nodes #'> :key #'cdr))))




(defun weighted-graph-cosine-similarity (node-1 node-2 graph)
  (let* ((node-id-1 (graph-utils:lookup-node graph node-1))
         (node-id-2 (graph-utils:lookup-node graph node-2)))
    (when (and node-id-1 node-id-2)
      (let ((denominator
             (sqrt
              (* (loop for degree-1 in (remove-duplicates (graph-utils::neighbors graph node-id-1))
                       sum (expt (graph-utils:edge-weight graph degree-1 node-id-1) 2))
                 (loop for degree-2 in (remove-duplicates (graph-utils::neighbors graph node-id-2))
                       sum (expt (graph-utils:edge-weight graph  degree-2 node-id-2) 2))))))
        (if (> denominator 0)
          (/ (loop for common-neighbour in (common-neighbours node-id-1 node-id-2 graph)
                   sum (* (graph-utils:edge-weight graph node-id-1 common-neighbour)
                          (graph-utils:edge-weight graph node-id-2 common-neighbour)))
             denominator)
          0)))))

(defun similar-nodes-weighted-cosine (node graph)
  "loop through all nodes in graph, sort by weighted cosine similarity"
  (loop for other-node in (remove-duplicates (graph-utils::list-nodes graph))
        for node-similarity = (weighted-graph-cosine-similarity other-node node graph)
        when (> node-similarity 0)
        collect (cons other-node node-similarity)
        into similar-nodes
        finally (return (sort similar-nodes #'> :key #'cdr))))




;; For each lexical category; export the most similar nodes
;; and their weighted cosine similarity in a JSON format.
;; The Python file "plot_coherence.py" can be used to create
;; plots from this JSON file
(defun export-coherence (cxn-inventory outputfile)
  (ensure-directories-exist outputfile)
  (let* ((type-hierarchy
          (type-hierarchies::graph
           (get-type-hierarchy cxn-inventory)))
         (lexical-cxns
          (find-all 'lexical (constructions-list cxn-inventory)
                    :key #'get-cxn-type))
         (lexical-classes
          (loop for cxn in lexical-cxns
                collect (gl::lex-class-cxn cxn)))
         (coherence-data
          (loop for lex-class in lexical-classes
                for similar-nodes
                = (mapcar #'(lambda (lex-class-cosine-similarity-pair)
                              (cons (get-base-name (car lex-class-cosine-similarity-pair))
                                    (cdr lex-class-cosine-similarity-pair)))
                          (remove lex-class
                                  (similar-nodes-weighted-cosine lex-class type-hierarchy)
                                  :key #'car))
              collect (cons (get-base-name lex-class) similar-nodes))))
  (with-open-file (stream outputfile :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (downcase (cl-json:encode-json-alist-to-string coherence-data)) stream)
    (force-output stream)))
  (format t "~%Data written to ~a" (namestring outputfile)))


;; Run the script here!
#|
(defparameter *grammar*
  (cl-store:restore
   (babel-pathname :directory '("experiments" "clevr-learning" "raw-data")
                   :name "grammar" :type "store")))

(export-coherence
 *grammar*
 (babel-pathname :directory '("experiments" "clevr-learning" "scripts")
                 :name "coherence-data" :type "json"))
|#