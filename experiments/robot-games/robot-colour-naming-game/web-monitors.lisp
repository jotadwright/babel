(in-package :gcng)

;; ------------------
;; + Sensory Object +
;; ------------------

(define-css 'color "div.color { display:inline-block;position:relative;overflow:hidden; }")

(defmethod make-html-for-entity-details ((obj sensory-object) &key)
  (let ((width 170)
        (height 120))
    (mapcar #'round (rgb-color obj))
    (append
     `(((div :class "color")
        ((svg :xmlns "http://www.w3.org/2000/svg"
              :width ,(format nil "~,2f" width)
              :height ,(format nil "~,2f" height))
         ((rect :x "0" :y "0"
                :width ,(mkstr width)
                :height ,(mkstr height)
                :style ,(format nil "fill:rgb(~{~a~^, ~})" (mapcar #'round (rgb-color obj))))))))
     `(((div :class "entity-detail")
        ,(format nil "lab = (~{~,2f~^, ~})" (lab-color obj)))))))

;; --------------
;; + Object Set +
;; --------------

(defmethod make-html-for-entity-details ((set sensory-object-set) &key)
  `(((div :class "entity-detail") 
     ,@(loop for e in (entities set)
             collect (make-html e :expand-initially t)))))

;; ------------
;; + Category +
;; ------------

(defmethod make-html-for-entity-details ((color-cat color-category) &key)
  (let ((width 200)
        (height 120))
    (append
     `(((div :class "color")
        ((svg :xmlns "http://www.w3.org/2000/svg"
              :width ,(format nil "~,2f" width)
              :height ,(format nil "~,2f" height))
         ((rect :x "0" :y "0"
                :width ,(mkstr width)
                :height ,(mkstr height)
                :style ,(format nil "fill:rgb(~{~a~^, ~})" (mapcar #'round (lab->rgb (value color-cat)))))))))
     `(((div :class "entity-detail")
        ,(format nil "lab = (~{~,2f~^, ~})" (value color-cat)))))))

;; ------------------
;; + Export Lexicon +
;; ------------------

(defun lexicon->s-dot (agent interaction-number)
  (let ((graph '(((s-dot::margin "0")) s-dot::graph))
        (id 0)
        (cluster `(((s-dot::id ,(format nil "cluster~a" (id agent)))
                    (s-dot::label ,(format nil "agent ~a (interaction ~a)"
                                           (id agent) interaction-number))
                    (s-dot::style "dashed")) s-dot::cluster))
        (sorted-color-categories
         (let ((all-colors (get-data (ontology agent) 'color-categories)))
           (sort all-colors #'< :key #'(lambda (cc) (first (value cc)))))))
    (loop for color in sorted-color-categories
          for cxns-with-color = (find-all (id color) (constructions (grammar agent))
                                          :key #'(lambda (cxn) (attr-val cxn :category)))
          for forms = (mapcar #'(lambda (cxn) (attr-val cxn :form)) cxns-with-color)
          for scores = (mapcar #'(lambda (cxn) (attr-val cxn :score)) cxns-with-color)
          for sorted-forms-and-scores = (sort (pairlis forms scores) #'> :key #'cdr)
          for node-label = (list-of-strings->string
                            (loop for l below (length sorted-forms-and-scores)
                                  for (form . score) in sorted-forms-and-scores
                                  if (= l (1- (length sorted-forms-and-scores)))
                                  collect (format nil "~a (~,1f)" form score)
                                  else
                                  collect (format nil "~a (~,1f)~%" form score)))
          do (push
              `(s-dot::node
                ((s-dot::id ,(format nil "colour~a" (incf id)))
                 (s-dot::label ,node-label)
                 (s-dot::shape "box")
                 (s-dot::fillcolor ,(format nil "#~a" (rgb->rgbhex (mapcar #'round (lab->rgb (value color))))))
                 (s-dot::style "filled")
                 (s-dot::height "1.5")
                 (s-dot::width "1.5")
                 (s-dot::fixedsize "true")
                 (s-dot::fontcolor ,(let* ((rgb (lab->rgb (value color)))
                                           (i-rgb (mapcar #'(lambda (c) (- 255 c)) rgb)))
                                      (format nil "#~a" (rgb->rgbhex (mapcar #'round i-rgb))))))) ;;"#FFFFFF")))
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
                                        (downcase (discourse-role agent))))))
  (add-element `((h3) ,(format nil "Running in ~a mode"
                               (if (get-configuration experiment :simulation-mode)
                                 "simulation" "robot"))))
  (add-element '((hr))))

(define-event-handler (trace-interaction-in-web-interface interaction-finished)
  (let ((speaker (speaker interaction))
        (hearer (hearer interaction))
        (i-number (interaction-number interaction)))
    (add-element '((hr)))
    (add-element `((h2) "Interaction "
                 ,(if (communicated-successfully interaction)
                    `((b :style "color:green") "succeeded")
                    `((b :style "color:red") "failed"))))
    (when (get-data (ontology speaker) 'color-categories)
      (add-element `((h2) ,(format nil "The speaker's ontology after ~a interactions:"
                                   (interaction-number interaction))))
      (add-element `((div) ,(lexicon->svg speaker i-number))))
    (when (get-data (ontology hearer) 'color-categories)
      (add-element `((h2) ,(format nil "The hearer's ontology after ~a interactions:"
                                   (interaction-number interaction))))
      (add-element `((div) ,(lexicon->svg hearer i-number))))))

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
  (add-element `((h2) "The speaker chooses " ((i) ,(format nil "~a" (id topic))) " as the topic")))

(define-event-handler (trace-interaction-in-web-interface new-category-created)
  (add-element '((h2) "A new color category is created"))
  (add-element (make-html category :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-started)
  (add-element '((h2) "The speaker tries to conceptualise the topic")))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-failed)
  (add-element '((h2 :style "color:red") "Conceptualisation failed")))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-succeeded)
  (add-element '((h2 :style "color:green") "Conceptualisation succeeded"))
  (add-element `((h3) "The speaker conceptualises the topic using " ((i) ,(format nil "~a" (id category)))))
  (add-element (make-html category :expand-initially t))
  (add-element '((h3) "Meaning representation:"))
  (add-element (irl-program->svg irl-program)))

(define-event-handler (trace-interaction-in-web-interface new-cxn-created)
  (add-element '((h2) "A new construction is created"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface production-started)
  (add-element '((h2) "The speaker tries to formulate")))

(define-event-handler (trace-interaction-in-web-interface production-failed)
  (add-element '((h2 :style "color:red") "Formulation failed")))

(define-event-handler (trace-interaction-in-web-interface production-succeeded)
  (add-element '((h2 :style "color:green") "Formulation succeeded"))
  (add-element `((h3) "The speaker utters the word " ((i) ,(format nil "\"~a\"" utterance))))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface utterance-passed)
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "The utterance \"~a\" is passed from speaker to hearer" utterance)))
  (add-element '((hr))))

(define-event-handler (trace-interaction-in-web-interface parsing-started)
  (add-element '((h2) "The hearer tries to parse the utterance")))

(define-event-handler (trace-interaction-in-web-interface parsing-failed)
  (add-element '((h2 :style "color:red") "Parsing failed"))
  (add-element `((h3) "The hearer does not know the word " ((i) ,(format nil "~a" (utterance agent))))))

(define-event-handler (trace-interaction-in-web-interface parsing-succeeded)
  (add-element '((h2 :style "color:green") "Parsing succeeded"))
  (add-element '((h3) "The hearer could parse the utterance:"))
  (add-element (make-html cxn))
  (add-element '((h3) "Meaning representation:"))
  (add-element (irl-program->svg irl-program)))

(define-event-handler (trace-interaction-in-web-interface interpretation-started)
  (add-element `((h2) ,(format nil "The hearer tries to interpret the utterance using ~a" (id category))))
  (add-element (make-html category :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface interpretation-failed)
  (add-element '((h2 :style "color:red") "Interpretation failed"))
  (add-element '((h3) "The hearer could not find a unique topic")))

(define-event-handler (trace-interaction-in-web-interface interpretation-succeeded)
  (add-element '((h2 :style "color:green") "Interpretation succeeded"))
  (add-element `((h3) ,(format nil "The hearer points to ~a as the topic" (id topic)))))

(define-event-handler (trace-interaction-in-web-interface cxn-rewarded)
  (add-element '((h3) "The agent rewarded the construction:"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface cxn-punished)
  (add-element '((h3) "The agent punished the construction:"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface discriminating-category-found)
  (add-element '((h2) "The hearer found a discriminating category"))
  (add-element (make-html category :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface new-category-created-adoption)
  (add-element '((h2) "The hearer could not find a discriminating category"))
  (add-element '((h2) "A new color category is created"))
  (add-element (make-html category :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface adoption-started)
  (add-element '((h2) "The hearer will learn a new word"))
  (add-element '((h2) "The hearer looks for a discriminating color category")))

(define-event-handler (trace-interaction-in-web-interface adoption-finished)
  (add-element '((h2) "The hearer adopted a new word"))
  (add-element (make-html cxn)))

(define-event-handler (trace-interaction-in-web-interface alignment-started)
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "The ~a started alignment"
                               (downcase (discourse-role agent))))))

(define-event-handler (trace-interaction-in-web-interface alignment-finished)
  (add-element `((h2) ,(format nil "The ~a finished alignment"
                               (downcase (discourse-role agent))))))

(define-event-handler (trace-interaction-in-web-interface category-shifted)
  (add-element `((h3) ,(format nil "The agent shifted ~a" (id category)))))

(define-event-handler (trace-interaction-in-web-interface success-determined)
  (if success
    (add-element '((h2) "This is correct. The speaker nods"))
    (add-element `((h2) ,(format nil "This is wrong. The speaker points to ~a" (id topic))))))
