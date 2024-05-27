(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;; Functionality for monitoring both routine application and learning ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(trace-fcg-learning))

(define-monitor trace-fcg-learning 
    :documentation "Graphically traces routine processing as well as learning.")

(define-event routine-comprehension-started (speech-act speech-act) (n t))

(define-event-handler (trace-fcg-learning routine-comprehension-started)
  (add-element `((hr :style "margin-block-end: 0px;")))
  (add-element `((h2 :style "padding: 15px; margin: 0px; background-color: #33FFA4;") ,(format nil "Routine comprehension of: &quot;~a&quot; ~@[~a~]"
                                                                                               (form speech-act)
                               (when n (format nil "(max ~a solution~:p considered)" n)))))
  (add-element `((hr :style "margin-block-start: 0px;"))))

(define-event-handler (trace-fcg-learning all-diagnostics-run)
  (when problems
    (add-element `((hr :style "margin-block-end: 0px;")))
    (add-element `((h2 :style "padding: 15px; margin: 0px; background-color: #ff5252;") "Meta-level learning"))
    (add-element `((hr :style "margin-block-start: 0px;")))
    (add-element `((h4) ,(format nil "~a problem~:p diagnosed:" (length problems))))
    (loop for problem in problems
          do (add-element (make-html problem)))))

(define-event-handler (trace-fcg-learning notify-learning-finished)
  (when problems
    (if fixes
      (progn
        (add-element `((h4) ,(if (> (length fixes) 1)
                               (format nil "~a fixes created:" (length fixes))
                               "1 fix created:")))
        (loop for fix in fixes
              do (add-element (make-html fix))))
      (add-element `((h4) "No fixes created.")))))


(define-event routine-comprehension-finished (solution t) (cip construction-inventory-processor))

(define-event-handler (trace-fcg-learning routine-comprehension-finished)
  (add-element '((hr)))
  (add-element (make-html-fcg-light cip :solutions (when solution (list solution))))
  (if solution
    (add-element `((h3) "Solution found!"))
    (add-element `((h3) "No solution found.")))
  (add-element `((p) " ")))


(define-event meta-level-learning-finished (cip construction-inventory-processor) (solution cip-node)
  (consolidated-cxns list) (consolidated-categories list) (consolidated-links list))

(define-event-handler (trace-fcg-learning meta-level-learning-finished)
  (add-element `((h4) "Appying fixes:"))
  (let ((subtree-id (mkstr (make-id 'subtree-id))))
    (add-element `((div :id ,subtree-id)
                 ,(make-html-fcg-light 
                   (top-node cip)
                   :subtree-id subtree-id
                   :hide-subtrees-with-duplicates t
                   :configuration (configuration (construction-inventory cip))))))
  (add-element `((h4) "Consolidation:"))
  (add-element
  `((div)
        ((table :class "two-col")
         ((tbody)
          ,@(when consolidated-cxns
              `(((tr) 
                 ((td) "constructions added:")
                 ((td) ,@(loop for cxn in consolidated-cxns
                               collect (make-html cxn :cxn-inventory (construction-inventory cip) :expand-initially nil))))))
          ,@(when consolidated-categories
              `(((tr) 
                 ((td) "categories added:")
                 ((td) ,@(loop for cat in consolidated-categories
                               collect (make-html cat))))))
          ,@(when consolidated-links
              `(((tr) 
                 ((td) "categorial links added:")
                 ((td) ,@(loop for link in consolidated-links
                               collect (make-html link)))))))))))



(define-event-handler (trace-fcg-learning fcg-apply-w-n-solutions-started)
  (add-element 
   `((h4)
     ,(make-html (original-cxn-set construction-inventory))
     ,(make-html (categorial-network (original-cxn-set construction-inventory))
                 :weights? t :render-program "circo" :expand-initially nil))))







