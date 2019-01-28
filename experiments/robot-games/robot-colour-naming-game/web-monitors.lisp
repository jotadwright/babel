;;;; ./web-monitors.lisp

(in-package :robot-colour-game)

;; -----------------------------
;; + Trace Tasks and Processes +
;; -----------------------------

(define-monitor trace-tasks-and-processes)

(define-event-handler (trace-tasks-and-processes object-run-task-started)
  (add-element `((h3) ,(format nil "The ~a started running the ~a"
                               (downcase (discourse-role tasks-and-processes::object))
                               (downcase (label task))))))

(define-event-handler (trace-tasks-and-processes object-run-task-finished)
  (add-element `((h3) ,(format nil "The ~a finished running the ~a"
                               (downcase (discourse-role tasks-and-processes::object))
                               (downcase (label task)))))
  (add-element (make-html task)))

;; --------------------------------------
;; + Trace Interaction in Web Interface +
;; --------------------------------------


(define-monitor trace-interaction-in-web-interface)

(define-event-handler (trace-interaction-in-web-interface interaction-started)
  (add-element '((hr)))
  (add-element `((h1) ,(format nil "Interaction ~a"
                               (interaction-number interaction))))
  (add-element '((hr)))
  (loop for agent in (interacting-agents interaction)
        do (add-element `((h3) ,(format nil "Agent ~a is the ~a"
                                        (id agent)
                                        (downcase (discourse-role agent)))))))

(define-event-handler (trace-interaction-in-web-interface interaction-finished)
  (add-element '((hr)))
  (add-element `((h2) "Interaction "
                 ,(if (communicated-successfully interaction)
                    `((b :style "color:green") "succeeded")
                    `((b :style "color:red") "failed")))))

(define-event-handler (trace-interaction-in-web-interface observe-scene-finished)
  (let ((source-path (babel-pathname :directory '(".tmp" "nao-img")
                                     :name pathspec
                                     :type "jpg"))
        (dest-path (pathname "~/Sites/")))
    (when pathspec
      (copy-file source-path dest-path))
    (if (null cl-user::*localhost-user-dir*)
      (warn "Set your *localhost-user-dir* in init-babel")
      (progn
        (when pathspec
          (add-element
           `((table)
             ((tr)
              ((th :align "center") "Observed scene:"))
             ((tr)
              ((td :align "center") ((img :src ,(string-append cl-user::*localhost-user-dir* (format nil "~a.jpg" pathspec)) :width "90%")))))))
        (add-element
         `((table)
           ((tr)
            ((th :align "center") "Processed scene:"))
           ((tr)
            ((td :align "center") ,(make-html context :expand-initially t)))))))))

(define-event-handler (trace-interaction-in-web-interface choose-topic-finished)
  (add-element `((h3) "The topic is " ((i) ,(format nil "~a" (id topic))))))

(define-event-handler (trace-interaction-in-web-interface new-category-created)
  (add-element '((h3) "The agent created a new color category"))
  (add-element (make-html category)))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-failed)
  (add-element '((h3) "The agent could not conceptualise the topic")))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-succeeded)
  (add-element `((h3) ,(format nil "The agent conceptualised the topic using ~a"
                               (id category))))
  (add-element (irl-program->svg irl-program)))

(define-event-handler (trace-interaction-in-web-interface new-cxn-created)
  (add-element '((h3) "The agent created a new construction"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface production-succeeded)
  (add-element `((h3) ,(format nil "The agent utters the word ~a" utterance)))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface parsing-failed)
  (add-element '((h3) "The agent could not parse the utterance")))

(define-event-handler (trace-interaction-in-web-interface parsing-succeeded)
  (add-element '((h3) "The agent parsed the utterance"))
  (add-element (make-html cxn))
  (add-element (irl-program->svg irl-program)))

(define-event-handler (trace-interaction-in-web-interface interpretation-failed)
  (add-element '((h3) "The agent could not interpret the utterance")))

(define-event-handler (trace-interaction-in-web-interface interpretation-succeeded)
  (add-element `((h3) ,(format nil "The agents points to ~a" (id topic)))))

(define-event-handler (trace-interaction-in-web-interface cxn-rewarded)
  (add-element '((h3) "The agent rewarded the construction:"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface cxn-punished)
  (add-element '((h3) "The agent punished the construction:"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface discriminating-category-found)
  (add-element '((h3) "The agent found a discriminating category"))
  (add-element (make-html category)))

(define-event-handler (trace-interaction-in-web-interface adoption-finished)
  (add-element '((h3) "The agent adopted a new word"))
  (add-element (make-html cxn)))
