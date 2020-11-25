;;;; web-monitors.lisp

(in-package :clevr-learning)

(define-monitor trace-interactions-in-wi)

(define-event-handler (trace-interactions-in-wi challenge-level-questions-loaded)
  (add-element `((h1) ,(format nil "Level ~a questions loaded"
                               level))))

(define-event-handler (trace-interactions-in-wi challenge-level-primitives-set)
  (add-element `((h1) ,(format nil "Level ~a primitives set"
                               level))))

(define-event-handler (trace-interactions-in-wi interaction-started)
  (add-element `((h1) ,(format nil "Interaction ~a"
                               (interaction-number interaction)))))

(define-event-handler (trace-interactions-in-wi interaction-before-finished)
  (let* ((img-src-path (image scene))
         (img-dst-path (make-pathname :directory `(:absolute "Users" ,(who-am-i) "Sites")
                                      :name (pathname-name img-src-path)
                                      :type (pathname-type img-src-path))))
    (copy-file img-src-path img-dst-path)
    (add-element '((h2) "Current Scene:"))
    (add-element `((img :src ,(mkstr cl-user::*localhost-user-dir*
                                     (pathname-name img-src-path)
                                     "." (pathname-type img-src-path)))))
    (add-element `((h2) ,(format nil "Current Question: \"~a\"" question)))
    (add-element '((h2) "Expected Answer:"))
    (if (subtypep (type-of answer) 'entity)
      (add-element (make-html answer))
      (add-element `((p) ,(format nil "\"~a\"" answer))))))

(define-event-handler (trace-interactions-in-wi parsing-succeeded)
  (add-element '((h2) "Parsing succeeded")))

(define-event-handler (trace-interactions-in-wi new-program-repair-started)
  (add-element '((h2) "Parsing failed")))

(define-event-handler (trace-interactions-in-wi chosen-composer-solution)
  (add-element '((h3) "Chose the following composer solution:"))
  (add-element (make-html solution)))

(define-event-handler (trace-interactions-in-wi interpretation-succeeded)
  (add-element '((h2) "Interpretation succeeded"))
  (add-element '((h3) "Computed answer:"))
  (if (subtypep (type-of answer) 'entity)
    (add-element (make-html answer))
    (add-element `((p) ,(format nil "\"~a\"" answer)))))

(define-event-handler (trace-interactions-in-wi new-holophrase-cxn)
  (add-element '((h3) "New holophrase construction:"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interactions-in-wi alignment-started)
  (add-element '((h2) "Alignment started")))

(define-event-handler (trace-interactions-in-wi cxns-rewarded)
  (add-element '((h3) "The following cxns are rewarded:"))
  (mapcar #'(lambda (cxn)
              (add-element (make-html cxn)))
          cxns))

(define-event-handler (trace-interactions-in-wi cxns-punished)
  (unless (null cxns)
    (add-element '((h3) "The following cxns are punished:"))
    (mapcar #'(lambda (cxn)
                (add-element (make-html cxn)))
            cxns)))

(define-event-handler (trace-interactions-in-wi adjust-program-started)
  (add-element '((h2) "Composing a new program")))

(define-event-handler (trace-interactions-in-wi agent-confidence-level)
  (add-element `((h2) ,(format nil "The agent is ~$% confident"
                               (* 100 level)))))

(define-event-handler (trace-interactions-in-wi interaction-finished)
  (add-element `((h1) "Interaction "
                 ,(if (communicated-successfully interaction)
                    `((b :style "color:green") "succeeded")
                    `((b :style "color:red") "failed"))))
  (add-element '((hr))))