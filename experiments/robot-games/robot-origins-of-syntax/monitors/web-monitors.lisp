;;;; /monitors/web-monitors.lisp

(in-package :roos)

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
  (show-lexicon (first (interacting-agents interaction)))
  (when (eql (get-configuration experiment :game-stage) :grammatical)
    (add-element (make-html (grammar (first (interacting-agents interaction)))))))

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
  
(define-event-handler (trace-interaction-in-web-interface sensory-scale-finished)
  (add-element `((table)
                 ((tr)
                  ((th :align "center") "Scaled scene:"))
                 ((tr)
                  ((td :align "center") ,(make-html scaled-scene :expand-initially t))))))

(define-event-handler (trace-interaction-in-web-interface choose-topics-finished)
  (cond ((length= topic-ids 1)
         (add-element `((h3) "The topic is " ((i) ,(format nil "~a" (first topic-ids))))))
        (t
         (add-element `((h3) "The topics are " ((i) ,(format nil "~{~a~^, ~}" topic-ids)))))))

(define-event-handler (trace-interaction-in-web-interface speech-input-finished)
  (cond ((length= utterance 1)
         (add-element `((h3) "The agent understood the word \"" ((i) ,(first utterance) "\""))))
        (t
         (add-element `((h3) "The agent understood the words \"" ((i) ,(format nil "~{~a ~}" utterance)) "\"")))))

#|
(define-event-handler (trace-interaction-in-web-interface show-context-scaled-topics)
  (let ((topic-set (make-object-set (loop for id in topic-ids
                                          collect (find-entity-by-id scene id))
                                    :id (make-id 'topic-set))))
    (add-element `((table)
                   ((tr)
                    ((th :align "center") "Scaled topic:"))
                   ((tr)
                    ((td :align "center") ,(make-html topic-set :expand-initially t)))))))
|#

(defun show-lexicon (agent)
  (unless (null (constructions (grammar agent)))
    (let* ((all-words (loop for cxn in (constructions (grammar agent))
                            collect (attr-val cxn :form) into forms
                            finally (remove nil (remove-duplicates forms :test #'string=))))
           (all-categories (loop for channel in (get-configuration agent :features)
                                 for channel-categories = (find-data (ontology agent) channel)
                                 collect (mapcar #'id channel-categories) into meanings
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

(define-event-handler (trace-interaction-in-web-interface lex-conceptualise-finished)
  (add-element `((h3) ((i) ,(format nil "~a" most-salient-channel)) " is the most salient channel"))
  (add-element '((h3) "The agent uses category:"))
  (add-element (make-html (first topic-cat) :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface invention-repair-triggered)
  (add-element '((h3) "The agent invents a new word")))

(define-event-handler (trace-interaction-in-web-interface lex-produce-finished)
  (add-element `((h3) "The agent says \"" ((i) ,utterance) "\""))
  (add-element (make-html applied-cxn :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface lex-parse-succeeded)
  (add-element '((h3) "The agent knows this word"))
  (add-element (make-html applied-cxn :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface lex-parse-failed)
  (add-element '((h3) "The agent does not know this word")))

(define-event-handler (trace-interaction-in-web-interface lex-interpret-finished)
  (add-element `((h3) "Possible topic: " ((i) ,(format nil "~a" topic-id)))))

(define-event-handler (trace-interaction-in-web-interface lex-hearer-conceptualise-finished)
  (add-element `((h3) ((i) ,(format nil "~a" most-salient-channel)) " is the most salient channel"))
  (add-element `((h3) "The agent uses category:"))
  (add-element (make-html topic-cat :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface lex-hearer-create-finished)
  (add-element `((h3) ,(format nil "~a is the most salient channel" most-salient-channel)))
  (add-element `((h3) "The agent creates a new category:"))
  (add-element (make-html topic-cat :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface lex-hearer-learning-finished)
  (add-element '((h3) "The agent learned:"))
  (add-element (make-html new-lex-cxn :expand-initially t)))

;; + Grammatical +
(define-event-handler (trace-interaction-in-web-interface gr-conceptualise-finished)
  (add-element '((h3) "Discriminatory features:"))
  (add-element (make-html discriminatory-features)))

(define-event-handler (trace-interaction-in-web-interface could-not-conceptualise)
  (add-element '((h3) "The agent could not conceptualise the topics")))

(define-event-handler (trace-interaction-in-web-interface gr-produce-started)
  (add-element '((h3) "Conceptualised meaning:"))
  (add-element (predicate-network->svg meaning-network :only-variables nil)))

(define-event-handler (trace-interaction-in-web-interface formulation-repair-triggered)
  (add-element '((h3) "The agent needs to invent")))

(define-event-handler (trace-interaction-in-web-interface gr-produce-finished)
  (add-element `((h3) "The agent says \"" ((i) ,(format nil "~{~a ~}" utterance)) "\"")))

(define-event-handler (trace-interaction-in-web-interface gr-parse-finished)
  (add-element '((h3) "Parsed meaning:"))
  (add-element (predicate-network->svg meaning-network :only-variables nil)))

(define-event-handler (trace-interaction-in-web-interface gr-interpret-finished)
  (cond ((null topic-ids)
         (add-element '((h3) "The agent could not find possible topics")))
        ((< (length topic-ids) 6)
         (add-element `((h3) "All possible topics: " ((i) ,(format nil "~{~a~^, ~}" topic-ids)))))
        (t
         (add-element `((h3) ,(format nil "The agent found ~a possible topics. Not showing them all."
                                      (length topic-ids)))))))

(define-event-handler (trace-interaction-in-web-interface comprehension-repair-triggered)
  (add-element '((h3) "The agent learns")))

(define-event-handler (trace-interaction-in-web-interface could-not-discriminate)
  (add-element '((h3) "The agent could not discriminate based on the utterance")))

(define-event-handler (trace-interaction-in-web-interface type-hierarchy-rewarded)
  (when th-links
    (add-element '((h3) "Rewarded type hierarchy links:"))
    (loop with th = (make-instance 'type-hierarchy)
          for (lex-cat . gramm-cat) in th-links
          do
          (add-categories (list lex-cat gramm-cat) th)
          (add-link lex-cat gramm-cat th :weight (link-weight lex-cat gramm-cat (get-type-hierarchy (grammar agent))))
          finally (add-element (make-html th :weights? t)))))

(define-event-handler (trace-interaction-in-web-interface type-hierarchy-punished)
  (when th-links
    (add-element '((h3) "Punished type hierarchy links:"))
    (loop with th = (make-instance 'type-hierarchy)
          for (lex-cat . gramm-cat) in th-links
          do
          (add-categories (list lex-cat gramm-cat) th)
          (add-link lex-cat gramm-cat th :weight (link-weight lex-cat gramm-cat (get-type-hierarchy (grammar agent))))
          finally (add-element (make-html th :weights? t)))))

(define-event-handler (trace-interaction-in-web-interface type-hierarchy-updated)
 (add-element '((h3) "Updated Type Hierarchy: "))
 (add-element (make-html type-hierarchy :weights? t)))

;; -----------------------------
;; + Trace Tasks and Processes +
;; -----------------------------

(define-monitor trace-tasks-and-processes)

(define-event-handler (trace-tasks-and-processes object-run-task-finished)
  (add-element `((h3) ,(format nil "The ~a finished running the ~a"
                               (discourse-role tasks-and-processes::object)
                               (label task))))
  (add-element (make-html task)))
