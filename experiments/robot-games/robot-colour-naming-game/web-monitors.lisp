;;;; ./web-monitors.lisp

(in-package :grounded-color-naming-game)

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

(defun lexicon->s-dot (agent interaction-number &key (sorted-on :meaning))
  (let ((graph '(((s-dot::margin "0")) s-dot::graph))
        (id 0)
        (cluster `(((s-dot::id ,(format nil "cluster~a" (id agent)))
                    (s-dot::label ,(format nil "agent-~a (interaction ~a)" (id agent) interaction-number))
                    (s-dot::style "dashed")) s-dot::cluster))
        (sorted-cxns (cond ((eql sorted-on :form)
                            (sort (constructions (grammar agent)) #'string<
                                  :key #'(lambda (cxn) (attr-val cxn :form))))
                           ((eql sorted-on :meaning)
                            (sort (constructions (grammar agent)) #'<
                                  :key #'(lambda (cxn)
                                           (let ((color (find (attr-val cxn :category)
                                                              (get-data (ontology agent) 'color-categories)
                                                              :key #'id)))
                                             (first (value color)))))))))
                       
    (loop for cxn in sorted-cxns
          for form = (attr-val cxn :form)
          for category-id = (attr-val cxn :category)
          for color = (find category-id (get-data (ontology agent) 'color-categories) :key #'id)
          for score = (attr-val cxn :score)
          do (push
              `(s-dot::node
                ((s-dot::id ,(format nil "colour~a" (incf id)))
                 (s-dot::label ,(format nil "~a (~,1f)" form score))
                 (s-dot::shape "box")
                 (s-dot::fillcolor ,(format nil "#~a" (rgb->rgbhex (mapcar #'round (lab->rgb (value color))))))
                 (s-dot::style "filled")
                 (s-dot::height "1.5")
                 (s-dot::width "1.5")
                 (s-dot::fixedsize "true")
                 (s-dot::fontcolor "#FFFFFF")))
              cluster))
    (push (reverse cluster) graph)
    (reverse graph)))

(defun lexicon->svg (agent interaction-number)
  (s-dot->svg (lexicon->s-dot agent interaction-number)))

(defun lexicon->pdf (agent interaction-number series-number
                           &key (path (babel-pathname :directory '(".tmp")
                                                      :name (format nil "series-~a-interaction-~a-agent-~a"
                                                                    series-number
                                                                    interaction-number
                                                                    (id agent))
                                                      :type "pdf"))
                           (format "pdf"))                 
  (s-dot->image (lexicon->s-dot agent interaction-number)
                :path path :format format :open nil))
                

(define-event-handler (trace-interaction-in-web-interface interaction-finished)
  (let ((speaker (speaker interaction))
        (hearer (hearer interaction))
        (i-number (interaction-number interaction)))
    (when (get-data (ontology speaker) 'color-categories)
      (add-element '((h3) "The speaker's ontology:"))
      (add-element `((div) ,(lexicon->svg speaker i-number))))
    (when (get-data (ontology hearer) 'color-categories)
      (add-element '((h3) "The hearer's ontology:"))
      (add-element `((div) ,(lexicon->svg hearer i-number)))))
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
  (add-element `((h3) "The speaker chooses " ((i) ,(format nil "~a" (id topic))) " as the topic.")))

(define-event-handler (trace-interaction-in-web-interface new-category-created)
  (add-element '((h3) "A new color category is created"))
  (add-element (make-html category :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-started)
  (add-element '((h3) "The speaker tries to conceptualise the topic...")))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-failed)
  (add-element '((h3) "The speaker could not conceptualise the topic")))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-succeeded)
  (add-element `((h3) "The speaker conceptualises the topic using " ((i) ,(format nil "~a" (id category)))))
  (add-element (make-html category :expand-initially t))
  (add-element (irl-program->svg irl-program)))

(define-event-handler (trace-interaction-in-web-interface new-cxn-created)
  (add-element '((h3) "A new construction is created"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface production-succeeded)
  (add-element `((h3) "The speaker utters the word " ((i) ,(format nil "\"~a\"" utterance))))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface utterance-passed)
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "The utterance \"~a\" is passed from speaker to hearer" utterance)))
  (add-element '((hr))))

(define-event-handler (trace-interaction-in-web-interface parsing-failed)
  (add-element '((h3) "The hearer could not parse the utterance")))

(define-event-handler (trace-interaction-in-web-interface parsing-succeeded)
  (add-element '((h3) "The hearer parsed the utterance"))
  (add-element (make-html cxn))
  (add-element (irl-program->svg irl-program)))

(define-event-handler (trace-interaction-in-web-interface interpretation-started)
  (add-element `((h3) ,(format nil "The hearer tries to interpret the utterance using ~a" (id category))))
  (add-element (make-html category :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface interpretation-failed)
  (add-element '((h3) "The hearer could not interpret the utterance")))

(define-event-handler (trace-interaction-in-web-interface interpretation-succeeded)
  (add-element `((h3) ,(format nil "The hearer points to ~a as the topic" (id topic)))))

(define-event-handler (trace-interaction-in-web-interface cxn-rewarded)
  (add-element '((h3) "The agent rewarded the construction:"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface cxn-punished)
  (add-element '((h3) "The agent punished the construction:"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface discriminating-category-found)
  (add-element '((h3) "The hearer found a discriminating category"))
  (add-element (make-html category :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface adoption-started)
  (add-element '((h3) "The hearer needs to learn"))
  (add-element '((h3) "The hearer looks for a discriminating color category")))

(define-event-handler (trace-interaction-in-web-interface adoption-finished)
  (add-element '((h3) "The hearer adopted a new word"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface alignment-started)
  (add-element `((h3) ,(format nil "The ~a started alignment"
                               (downcase (discourse-role agent))))))

(define-event-handler (trace-interaction-in-web-interface category-shifted)
  (add-element `((h3) ,(format nil "The agent shifted ~a" (id category)))))
