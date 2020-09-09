
(in-package :irl)

;; ============================================================================
;; match-chunk
;; ----------------------------------------------------------------------------

#|
(define-event-handler ((trace-irl-in-web-browser
                        trace-irl-in-web-browser-verbose)
                       match-chunk-started)
  (add-element '((hr)))
  (add-element `((p) "matching chunk " ,(make-html chunk :expand-initially t)))
  (add-element `((p) "with meaning " ,(html-pprint meaning :max-width 100)
                 ,(irl-program->svg meaning))))

(define-event-handler ((trace-irl-in-web-browser 
                        trace-irl-in-web-browser-verbose) match-chunk-finished)
  (if matched-chunks
      (add-element `((p) "matched-chunks: " ((br))
                     ,@(loop for chunk in matched-chunks
                          collect (make-html chunk :expand-initially t))))
      (add-element `((p) ((b) "no results")))))
|#

;; ============================================================================
;; chunk-composer
;; ----------------------------------------------------------------------------

(defparameter *composer-start-time* nil)

(define-event-handler ((trace-irl trace-irl-verbose)
                       chunk-composer-get-next-solutions-started)
  (setf *composer-start-time* (get-universal-time))
  (add-element '((hr)))
  (add-element '((h2) "Computing next composer solution"))
  (add-element `((p) ,(make-html composer)))
  (add-element '((h3) "in the following ontology:"))
  (add-element (make-html (ontology composer))))


(define-event-handler ((trace-irl trace-irl-verbose)
                       chunk-composer-get-all-solutions-started)
  (setf *composer-start-time* (get-universal-time))
  (add-element '((hr)))
  (add-element '((h2) "Computing all composer solutions"))
  (add-element `((p) ,(make-html composer)))
  (add-element '((h3) "in the following ontology:"))
  (add-element (make-html (ontology composer))))

(define-event-handler ((trace-irl trace-irl-verbose)
                       chunk-composer-get-solutions-until-started)
  (setf *composer-start-time* (get-universal-time))
  (add-element '((hr)))
  (add-element '((h2) "Computing all composer solutions until stop criterion"))
  (add-element `((p) ,(make-html composer)))
  (add-element '((h3) "in the following ontology:"))
  (add-element (make-html (ontology composer))))

(define-event-handler (trace-irl-verbose
                       chunk-composer-node-handled)
  (add-element 
   `((table :class "two-col")
     ((tbody)
      ((tr)
       ((td) ,(format nil "This node was handled (~a):" handler))
       ((td) ,(make-html node :draw-as-tree nil)))))))

(define-event-handler (trace-irl-verbose
                       chunk-composer-new-nodes)
  (let ((expand/collapse-all-id (make-id 'successors)))
    (add-element 
     `((table :class "two-col")
       ((tbody) 
        ((tr)
         ((td) ,(make-expand/collapse-all-link expand/collapse-all-id "All new nodes:"))
         ((td) ,@(loop for node in nodes
                    collect (make-html node :draw-as-tree nil
                                       :expand/collapse-all-id expand/collapse-all-id)))))))))

(define-event-handler (trace-irl-verbose
                       chunk-composer-next-node)
  (add-element '((hr)))
  (add-element 
   `((table :class "two-col")
     ((tbody)
      ((tr) 
       ((td) "Current node:")
       ((td) ,(make-html node :draw-as-tree nil)))))))

(define-event-handler (trace-irl-verbose
                       chunk-composer-node-changed-status)
  (add-element
   `((table :class "two-col")
     ((tbody)
      ((tr)
       ((td) "Node:")
       ((td) ,(make-html node :draw-as-tree nil)))
      ((tr)
       ((td) "Previous status:")
       ((td) ,(second (statuses node))))
      ((tr)
       ((td) "Current status:")
       ((td) ,(first (statuses node))))))))

(define-event-handler ((trace-irl trace-irl-verbose)
                       chunk-composer-finished)
  (let ((finish-time (get-universal-time)))
    (multiple-value-bind (h m s)
        (seconds-to-hours-minutes-seconds (- finish-time *composer-start-time*))
      (add-element '((hr)))
      (add-element '((h2) "Result"))
      (add-element
       (make-html-for-composition-process composer))
      (add-element '((h3) "Composer queue:"))
      (add-element
       (html-hide-rest-of-long-list
        (queue composer) 3
        #'(lambda (n)
            (make-html n :draw-as-tree nil))))
      (add-element `((h3) ,(format nil "Composer took ~a hours, ~a minutes and ~a seconds"
                                   h m s)))
      (if solutions
        (progn
          (add-element `((h3) ,(format nil "Found ~a solutions:"
                                       (length solutions))))
          (composer-solutions->html solutions))
        (add-element '((h3) "No solutions found.")))
      (when (and (solutions composer)
                 (not (length= (solutions composer) solutions)))
        (add-element '((h3) "All composer solutions so far"))
        (composer-solutions->html (solutions composer))))))
