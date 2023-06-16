(in-package :cle)

;; --------------------------
;; + Web interface handlers +
;; --------------------------

;; Add cxn to wi
(defun add-cxn-to-interface (cxn &key certainty-threshold)
  (add-element
   `((div :style ,(format nil "margin-left: 50px;"))
     ,(s-dot->svg
       (cxn->s-dot cxn :certainty-threshold certainty-threshold)))))

(defun add-cxn-diff-to-interface (cxn previous-copy &key certainty-threshold)
  (add-element
   `((div :style ,(format nil "margin-left: 50px;"))
     ,(s-dot->svg
       (cxn->s-dot-diff cxn previous-copy :certainty-threshold certainty-threshold)))))
  

;;;; Show lexicon in web interface
(defun display-lexicon (agent &key (entrenchment-threshold 0) (certainty-threshold 0) (sort nil))
  (if (length= (lexicon agent) 0)
    (add-element
     `((h3) ,(format nil "Lexicon is empty!")))
    (let ((lexicon (if sort
                     (sort (lexicon agent) #'(lambda (x y) (> (score x) (score y))))
                     (lexicon agent))))
      (add-element `((h3) ,(format nil "Lexicon:")))
      (loop for cxn in lexicon and idx from 0
            do (add-element
                `((h4) ,(format nil "CXN ~a w score ~a" idx (score cxn))))
            when (>= (score cxn) entrenchment-threshold)
              do (add-cxn-to-interface cxn :certainty-threshold certainty-threshold)))))

(defun display-lexicon-simple (agent)
  (if (length= (lexicon agent) 0)
    (add-element
     `((h4) ,(format nil "Lexicon is empty!")))
    (loop for cxn in (lexicon agent)
          for i from 1
          do (add-element
              `((h4) ,(format nil " -> ~a: (~a, ~a)"
                              i
                              (downcase (mkstr (form cxn)))
                              (downcase (mkstr (score cxn)))))))))

(defun show-in-wi (args)
  (add-element `((h4) ,(format nil "~{~a~^, ~}" args))))

;; ---------
;; + TIIWI +
;; ---------
(define-monitor trace-interaction-in-web-interface)

;; --------------------------------
;; + Interaction start and finish +
;; --------------------------------
(define-event-handler (trace-interaction-in-web-interface interaction-started)
  (add-element '((hr)))
  (add-element
   `((h1) ,(format nil "Interaction ~a"
                   (interaction-number interaction))))
  (add-element
   `((h2) ,(format nil "The ~a is the speaker with lexicon (size = ~a):"
                   (downcase (mkstr (id (speaker interaction))))
                   (downcase (mkstr (length (lexicon (speaker interaction))))))))
  
  ;(display-lexicon (speaker interaction) :sort t)
  (add-element
   `((h2) ,(format nil "The ~a is the hearer with lexicon (size = ~a):"
                   (downcase (mkstr (id (hearer interaction))))
                   (downcase (mkstr (length (lexicon (hearer interaction))))))))
  ;(display-lexicon (hearer interaction) :sort t)
  (add-element '((hr))))


(define-event-handler (trace-interaction-in-web-interface interaction-finished)
  (add-element '((hr)))
  (add-element
   `((h2) "Interaction "
     ,(if (communicated-successfully interaction)
        `((b :style "color:green") "succeeded")
        `((b :style "color:red") "failed"))))
  (add-element '((hr)))
  (add-element '((hr))))

;; ---------------------------
;; + Setup context and topic +
;; ---------------------------

(define-event-handler (trace-interaction-in-web-interface event-context-determined)
  (add-element `((h2) ,(format nil "Scene: ~a" (file-namestring (image (get-data (speaker experiment) 'context))))))
  (add-element `((div :class "image" :style ,(format nil "margin-left: 50px; margin-bottom: 20px; width: fit-content; border-radius: 8px; overflow: hidden; border: 1px; border-color: #000000; box-shadow: 8px 8px 12px 1px rgb(0 0 0 / 10%);"))
                 ((img :src ,(string-append
                              cl-user::*localhost-user-dir*
                              (concatenate 'string
                                           "val/"
                                           (file-namestring (image (get-data (speaker experiment) 'context)))))))))
  (add-element `((table :style ,(format nil "margin-left: 50px;"))
                 ((tr) ((td) ,(make-html (get-data (speaker experiment) 'context)
                                         :topic (id (get-data (speaker experiment) 'topic))
                                         :expand-initially t))))))

;; ---------------------
;; + Conceptualisation +
;; ---------------------
(define-event-handler (trace-interaction-in-web-interface event-conceptualisation-start)
  (add-element '((hr)))
  (add-element
   `((h2) ,(format nil "Step 1: Conceptualising as the ~a..." (discourse-role agent)))))

(define-event-handler (trace-interaction-in-web-interface event-conceptualisation-end)
  (add-element `((h2) ,(format nil " === CONCEPTUALISATION ===")))
  (add-element `((h3) ,(format nil " === PHASE 1 : CHOSE CONSTRUCTIONS WITH POSITIVE DISCRIMINATING POWER === ")))
  (loop for cxn in discriminating-cxns and idx from 0
        for form = (form (assqv :cxn cxn))
        for entrenchment = (score (assqv :cxn cxn))
        
        for discriminative-power = (abs (- (assqv :topic-sim cxn) (assqv :best-other-sim cxn)))
        do (add-element `((h4) ,(format nil " -> ~a - ~a: (~,3f, ~,3f]) => SCORE = ~,3f"
                                        idx
                                        (downcase (mkstr form))
                                        entrenchment
                                        discriminative-power
                                        (* entrenchment discriminative-power)))))
  (add-element `((h3) ,(format nil " === PHASE 2 : SELECT BASED ON OVERALL SCORE ===")))
  (if (car applied-cxn)
    (add-element `((h3) ,(format nil " == RESULT: (~a, ~a) == "
                                 (downcase (mkstr (form (car applied-cxn))))
                                 (downcase (mkstr (score (car applied-cxn)))))))
    (add-element `((h4) ,(format nil " == RESULT: 'nil' ==")))))

;; -------------
;; + Invention +
;; -------------
(define-event-handler (trace-interaction-in-web-interface event-invention-end)
  (add-element '((hr)))
  (add-element
   `((h2) ,(format nil "Search process failed")))
  (add-element
   `((h3) ,(format nil "Invented: ~a" (form cxn)))))

;; -------------
;; + Production +
;; -------------
(define-event-handler (trace-interaction-in-web-interface event-production-end)
  (add-element '((hr)))
  (if (utterance agent)
    (progn
      (add-element `((h2) ,(format nil "Step 2: ~@(~a~) produced an utterance: \"~a\" " (id agent) (utterance agent))))
      (add-cxn-to-interface (find-data agent 'applied-cxn))
      )
    (add-element `((h2) ,(format nil "Step 2: ~@(~a~) could not produce an utterance" (id agent))))))

;; -----------
;; + Parsing +
;; -----------
(define-event-handler (trace-interaction-in-web-interface event-parsing-end)
  (when (hearerp agent)
    (if (find-data agent 'applied-cxn)
      (progn
        (add-element '((h2) "Step 3: Hearer parsed the utterance:"))
        (add-cxn-to-interface (find-data agent 'applied-cxn)))
      (add-element
       '((h2) "Step 3: Hearer could not parse the utterance.")))))

;; ------------------
;; + Interpretation +
;; ------------------
(define-event-handler (trace-interaction-in-web-interface event-interpretation-end)
  (if (find-data agent 'interpreted-topic)
    (progn
      (add-element
       `((h2) ,(format nil "Step 4: The ~a interpreted the utterance:"
                       (downcase (mkstr (id agent))))))
      (add-element `((div :class "image" :style ,(format nil "margin-left: 50px;"))
                     ,(make-html (find-data agent 'interpreted-topic) :expand-initially t))))
    (add-element
     `((h2) ,(format nil "Step 4: The ~a could not interpret the utterance."
                     (downcase (mkstr (id agent))))))))

;; -------------
;; + Coherence +
;; -------------

(define-event-handler (trace-interaction-in-web-interface event-coherence-p)
  (let* ((interaction (current-interaction experiment)))
    (if hearer-cxn
      (progn
        (add-element `((h2) ,(format nil "~a: ~a"
                                     (id (hearer interaction))
                                     (form hearer-cxn))))
        (add-cxn-to-interface hearer-cxn))
      (add-element `((h2) ,(format nil "~a could not conceptualise!"
                                   (id (hearer interaction))))))
    (add-element `((h2) ,(format nil " ^^^ check for coherence: ~a ^^^ "
                                 (if coherence "True" "False"))))
    )
  (add-element `((h2) ,(format nil " ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ")))
  (add-element '((hr))))

;; -------------
;; + Alignment +
;; -------------
(define-event-handler (trace-interaction-in-web-interface event-align-start)
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "Step 5: Alignment ~a: ~a" (discourse-role agent) (id agent)))))

(define-event-handler (trace-interaction-in-web-interface event-adopt-start)
  (add-element
   `((h3) ,(format nil "Hearer will adopt a new cxn for the form \"~a\"" (form cxn)))))

(define-event-handler (trace-interaction-in-web-interface event-align-cxn)
  (add-element
   `((h4) ,(format nil "[~a] - aligned the cxn for the form \"~a\"" reason (form cxn))))
  (add-cxn-diff-to-interface cxn previous-copy))
