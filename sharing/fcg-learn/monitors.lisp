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
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Routine comprehension: &quot;~a&quot; ~@[~a~]"
                               (cadar (form speech-act))
                               (when n (format nil "(max ~a solution~p)" n n))))))


