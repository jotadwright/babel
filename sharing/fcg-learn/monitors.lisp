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



(define-event meta-level-learning-finished (cip construction-inventory-processor) (solution cip-node))

(define-event-handler (trace-fcg-learning meta-level-learning-finished)
  (add-element `((h4) "Appying fixes:"))
  (add-element (make-html-fcg-light cip :solutions (succeeded-nodes cip))))



(define-event-handler (trace-fcg-learning fcg-apply-w-n-solutions-started)
  (add-element 
   `((h4)
     ,(make-html (original-cxn-set construction-inventory))
     ,(make-html (categorial-network (original-cxn-set construction-inventory))
                 :weights? t :render-program "circo" :expand-initially nil))))

(define-event-handler (trace-fcg-learning fcg-apply-w-n-solutions-finished)
  (add-element '((hr)))
  (add-element (make-html-fcg-light cip :solutions solutions)))