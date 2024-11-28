(in-package :experiment-framework)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; This file declares all web monitors of the crs conventionality package   ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-event-handler (trace-interaction interaction-started)
  (add-element `((h2) ,(format nil "Interaction: ~a" interaction-number)))
  (add-element `((h3) ,(format nil "Interacting agents: ~a (speaker) and ~a (hearer)"
                               (utils:id (speaker interaction))
                               (utils:id (hearer interaction)))))
  (add-element (make-html (speaker interaction)))
  (add-element (make-html (hearer interaction)))
  (add-element `((h3) ,(format nil "Scene: ~a"
                               (utils:id (crs-conventionality::scene interaction)))))
  (add-element (make-html (crs-conventionality::scene interaction))))


(in-package :crs-conventionality)

;(run-interaction *naming-game-canonical*)

;(activate-monitor trace-interaction)

