;(ql:quickload :muhai-cookingbot)
(in-package :muhai-cookingbot)

(defparameter *meaning-to-link*
  '((fetch-and-proportion ?bowl-with-butter
                          ?ks-out ?ks-in
                          ?empty-bowl
                          ?butter-concept
                          ?quantity ?unit)
    (bind ingredient ?b1 butter)
    (bind quantity ?b2 230)
    (bind unit ?b3 g)))

(add-element (make-html (create-chunk-from-irl-program *meaning-to-link*)))

;; give preference to chunks with
;; - fewer connected components
;; - fewer times the same variable in the same primitive
;; stop if the score of the child chunks decreases w.r.t the parent chunk!

(defun nr-connected-components (chunk)
  (multiple-value-bind (connectedp num-components components)
      (irl-program-connected? (irl-program chunk))
    num-components))

(defun nr-repeated-variable (chunk)
  (loop with count = 0
        for predicate in (irl-program chunk)
        unless (length= predicate (remove-duplicates predicate))
        do (incf count (- (length predicate) (length (remove-duplicates predicate))))
        finally (return count)))

(defun nr-open-variables (chunk)
  (length (get-open-vars (irl-program chunk))))

(defun sort-queue (queue new-nodes)
  (sort (append queue new-nodes)
        #'(lambda (chunk-1 chunk-2)
            (let ((components-1 (nr-connected-components chunk-1))
                  (components-2 (nr-connected-components chunk-2))
                  (repeated-vars-1 (nr-repeated-variable chunk-1))
                  (repeated-vars-2 (nr-repeated-variable chunk-2))
                  (open-vars-1 (nr-open-variables chunk-1))
                  (open-vars-2 (nr-open-variables chunk-2)))
              (if (= components-1 components-2)
                (if (= repeated-vars-1 repeated-vars-2)
                  (<= open-vars-1 open-vars-2)
                  (<= repeated-vars-1 repeated-vars-2))
                (<= components-1 components-2))))))

(defun worse-child-p (parent child)
  (let ((components-parent (nr-connected-components parent))
        (components-child (nr-connected-components child))
        (repeated-vars-parent (nr-repeated-variable parent))
        (repeated-vars-child (nr-repeated-variable child)))
    (or (> components-child components-parent)
        (and (<= components-child components-parent)
             (> repeated-vars-child repeated-vars-parent)))))

(defun link-open-variables-recursively (chunk)
  (loop with queue = (list chunk)
        with solutions = nil
        while queue
        for node = (pop queue)
        for children = (irl::link-open-variables node)
        for good-children = (loop for child in children
                                  unless (worse-child-p node child)
                                  collect child)
        if (null good-children)
        do (unless (find (irl-program node) solutions
                         :test #'equivalent-irl-programs?
                         :key #'irl-program)
             (push node solutions))
        else
        do (setf queue (sort-queue queue good-children))
        finally (return solutions)))

(defparameter *linked-open-variables-rec*
  (link-open-variables-recursively
    (create-chunk-from-irl-program *meaning-to-link*)))

(add-element (make-html (first *linked-open-variables-rec*)))
(add-element (make-html (last-elt *linked-open-variables-rec*)))

(loop for chunk in *linked-open-variables-rec*
      do (format t "~%chunk ~a: ~a components, ~a repeated variables, ~a open variables"
                 (id chunk) (nr-connected-components chunk)
                 (nr-repeated-variable chunk)
                 (length (get-open-vars (irl-program chunk))))
      do (add-element (make-html chunk :expand-initially t)))

