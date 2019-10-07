(in-package :robot-concept-learning)

(define-monitor robot-monitor)

(define-event-handler (robot-monitor interaction-started)
  (let ((agent (first (population experiment)))
        (interaction-nr (interaction-number interaction)))
    (speak (robot agent) (format nil "interaction ~a" interaction-nr))))

(define-event-handler (robot-monitor interaction-finished)
  nil)

(define-event-handler (robot-monitor context-determined)
  (let ((agent (first (population experiment))))
    (speak (robot agent) (format nil "i detected ~a objects"
                                 (length (objects (context agent)))))))

(define-event-handler (robot-monitor utterance-received)
  nil)

(define-event-handler (robot-monitor parsing-finished)
  (speak (robot agent)
         (if (parsed-meaning agent)
           (format nil "i know the word ~a" (utterance agent))
           (format nil "i do not know the word ~a" (utterance agent)))))

(define-event-handler (robot-monitor interpretation-finished)
  (when (topic agent)
    (speak (robot agent) "i think i found the topic")
    (let ((sorted-context (sort (objects (context agent)) #'< :key #'(lambda (obj) (get-attr-val obj :x-pos)))))
      (cond ((eql (topic agent) (first sorted-context))
             (point (robot agent) :left))
            ((eql (topic agent) (last-elt sorted-context))
             (point (robot agent) :right))
            (t (point (robot agent) :both))))))

(define-event-handler (robot-monitor ask-for-feedback)
  (speak (robot agent) "please show me the topic you intended"))

(define-event-handler (robot-monitor detection-error)
  (speak (robot agent) (format nil "sorry, I detected ~a objects." num-detected)))

(define-event-handler (robot-monitor alignment-started)
  nil)

(define-event-handler (robot-monitor adopt-unknown-word)
  (speak (robot agent) "i am learning a new word"))

(define-event-handler (robot-monitor update-known-word)
  nil)

(define-event-handler (robot-monitor new-cxn-added)
  nil)

(define-event-handler (robot-monitor scores-updated)
  nil)