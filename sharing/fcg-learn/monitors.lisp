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
                               (when n (format nil "(max ~a solution~p considered)" n n)))))
  (add-element `((hr :style "margin-block-start: 0px;"))))


(define-event-handler (trace-fcg-learning fcg-apply-w-n-solutions-started)
  (add-element 
   `((h4)
     ,(make-html (original-cxn-set construction-inventory))
     ,(make-html (categorial-network (original-cxn-set construction-inventory))
                 :weights? t :render-program "circo" :expand-initially nil))))

(define-event-handler (trace-fcg-learning fcg-apply-w-n-solutions-finished)
  (add-element '((hr)))
  (add-element (make-html-fcg-light cip :solutions solutions)))