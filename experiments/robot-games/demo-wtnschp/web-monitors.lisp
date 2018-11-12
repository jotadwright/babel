;;;; /monitors/web-monitors.lisp

(in-package :demo-wtnschp)

;; ---------------------------------------
;; + Trace Interactions in Web Interface +
;; ---------------------------------------

(define-monitor trace-interaction-in-web-interface)

;; + Processes +
(define-event-handler (trace-interaction-in-web-interface interaction-started)
  (add-element '((hr)))
  (add-element `((h1) ,(format nil "Interaction ~a"
                               (interaction-number interaction))))
  (add-element '((hr)))
  (add-element `((h2) "The agent is the "
                 ((b) ,(discourse-role (first (interacting-agents interaction))))))
  (show-lexicon (first (interacting-agents interaction))))

(define-event-handler (trace-interaction-in-web-interface interaction-finished)
  (add-element '((hr)))
  (add-element `((h2) "Interaction "
                 ,(if (communicated-successfully interaction)
                    `((b :style "color:green") "succeeded")
                    `((b :style "color:red") "failed")))))

(define-event-handler (trace-interaction-in-web-interface observe-scene-finished)
  (let ((source-path (babel-pathname :directory '(".tmp" "nao-img")
                                     :name img-path
                                     :type "jpg"))
        (dest-path (pathname "~/Sites/")))
    (copy-file source-path dest-path)
    (if (null cl-user::*localhost-user-dir*)
      (warn "Set your *localhost-user-dir* in init-babel")
      (progn
        (add-element `((table)
                       ((tr)
                        ((th :align "center") "Observed scene:"))
                       ((tr)
                        ((td :align "center") ((img :src ,(string-append cl-user::*localhost-user-dir* (format nil "~a.jpg" img-path)) :width "90%"))))))
        (add-element `((table)
                 ((tr)
                  ((th :align "center") "Processed scene:"))
                 ((tr)
                  ((td :align "center") ,(make-html scene :expand-initially t)))))))))

(define-event-handler (trace-interaction-in-web-interface choose-topic-finished)
  (add-element `((h3) "The topic is " ((i) ,(format nil "~a" topic-id)))))

(define-event-handler (trace-interaction-in-web-interface speech-input-finished)
  (add-element `((h3) "The agent understood the word \"" ((i) ,utterance "\""))))

(defun show-lexicon (agent)
  (unless (null (constructions (grammar agent)))
    (let* ((all-words (loop for cxn in (constructions (grammar agent))
                            collect (attr-val cxn :form) into forms
                            finally (remove nil (remove-duplicates forms :test #'string=))))
           (all-categories (loop for color-category in (find-data (ontology agent) 'color-categories)
                                 collect (id color-category) into meanings
                                 finally (remove nil (remove-duplicates meanings))))
           (all-links (loop for cxn in (constructions (grammar agent))
                            for form = (attr-val cxn :form)
                            for meaning = (attr-val cxn :meaning)
                            for score = (attr-val cxn :score)
                            unless (or (null form) (null meaning))
                            collect (list form score meaning))))
      (add-element '((h3) "The agent's lexicon:"))
      (add-element `((div :class "lex-item")
                     ,(s-dot->svg (lexicon->s-dot all-words all-categories all-links))))
      (add-element '((hr))))))

(defun replace-dashes (string)
  (coerce
   (substitute #\_ #\- (coerce string 'list))
   'string))

(defun lexicon->s-dot (words categories links)
  (let ((graph '(((s-dot::rankdir "LR")) s-dot::graph)))
    (loop for node in (append words categories)
          do (push `(s-dot::node ((s-dot::id ,(replace-dashes (mkstr node)))
                                  (s-dot::label ,(replace-dashes (mkstr node)))
                                  (s-dot::fontsize "11")))
                   graph))
    (loop for edge in links
          do (push `(s-dot::edge ((s-dot::from ,(replace-dashes (mkstr (first edge))))
                                  (s-dot::to ,(replace-dashes (mkstr (third edge))))
                                  (s-dot::label ,(format nil "~$" (second edge)))
                                  (s-dot::fontsize "11")
                                  (s-dot::minlen "2")
                                  (s-dot::dir "both")))
                   graph))
    (reverse graph)))

(define-event-handler (trace-interaction-in-web-interface alignment-started)
  (add-element '((hr)))
  (add-element '((h2) "The agent aligns")))

(define-event-handler (trace-interaction-in-web-interface lexicon-alignment)
  (when rewarded
    (add-element '((h3) "Rewarded lexical constructions:"))
    (loop for cxn in rewarded
          do (add-element (make-html cxn :expand-initially t))))
  (when punished
    (add-element '((h3) "Punished lexical constructions:"))
    (loop for cxn in punished
          do (add-element (make-html cxn :expand-initially t)))))

(define-event-handler (trace-interaction-in-web-interface category-alignment)
  (when shifted
    (add-element '((h3) "Updated categories:"))
    (loop for cat in shifted
          do (add-element (make-html cat :expand-initially t)))))

(define-event-handler (trace-interaction-in-web-interface lexicon-added)
  (add-element '((h3) "The agent added a new word:"))
  (add-element (make-html cxn :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface category-added)
  (add-element '((h3) "The agent added a new category:"))
  (add-element (make-html cat :expand-initially t)))

;; + Lexical +
(define-event-handler (trace-interaction-in-web-interface new-category-repair-triggered)
  (add-element '((h3) "The agent creates a new category")))

(define-event-handler (trace-interaction-in-web-interface conceptualise-finished)
  (add-element '((h3) "The agent uses category:"))
  (add-element (make-html (first topic-cat) :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface new-word-repair-triggered)
  (add-element '((h3) "The agent invents a new word")))

(define-event-handler (trace-interaction-in-web-interface produce-finished)
  (add-element `((h3) "The agent says \"" ((i) ,utterance) "\""))
  (add-element (make-html applied-cxn :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface parse-succeeded)
  (add-element '((h3) "The agent knows this word"))
  (add-element (make-html applied-cxn :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface parse-failed)
  (add-element '((h3) "The agent does not know this word")))

(define-event-handler (trace-interaction-in-web-interface interpret-finished)
  (add-element `((h3) "Possible topic: " ((i) ,(format nil "~a" topic-id)))))

(define-event-handler (trace-interaction-in-web-interface hearer-conceptualise-finished)
  (add-element `((h3) "The agent uses category:"))
  (add-element (make-html topic-cat :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface hearer-create-finished)
  (add-element `((h3) "The agent creates a new category:"))
  (add-element (make-html topic-cat :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface hearer-learning-finished)
  (add-element '((h3) "The agent learned:"))
  (add-element (make-html new-lex-cxn :expand-initially t)))

;; -----------------------------
;; + Trace Tasks and Processes +
;; -----------------------------

(define-monitor trace-tasks-and-processes)

(define-event-handler (trace-tasks-and-processes object-run-task-finished)
  (add-element `((h3) ,(format nil "The ~a finished running the ~a"
                               (discourse-role tasks-and-processes::object)
                               (label task))))
  (add-element (make-html task)))
