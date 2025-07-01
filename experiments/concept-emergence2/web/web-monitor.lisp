(in-package :cle)

;; --------------------------
;; + Web interface handlers +
;; --------------------------

;; Add cxn to wi
(defun add-cxn-to-interface (cxn &key (weight-threshold 0.1) disabled-features)
  (add-element
   `((div :style ,(format nil "margin-left: 50px;"))
     ,(s-dot->svg
       (cxn->s-dot cxn
                   :weight-threshold weight-threshold
                   :disabled-features disabled-features)))))

(defun add-cxn-diff-to-interface (cxn previous-copy &key (weight-threshold 0.1))
  (add-element
   `((div :style ,(format nil "margin-left: 50px;"))
     ,(s-dot->svg
       (cxn->s-dot-diff cxn previous-copy
                        :weight-threshold weight-threshold)))))
  

;;;; Show lexicon in web interface
(defun display-lexicon (agent &key (entrenchment-threshold 0) (weight-threshold 0) (sort nil))  
  "Shows the lexicon in web interface."
  (if (empty-lexicon-p agent)
    (add-element
     `((h3) ,(format nil "Lexicon is empty!")))
    (let* ((lexicon (hash-values (get-inventory (lexicon agent) :fast)))
           (new-lexicon (if sort
                          (sort lexicon #'(lambda (x y) (> (score x) (score y))))
                          new-lexicon)))
      
      (add-element `((h3) ,(format nil "Lexicon:")))
      (loop for cxn in new-lexicon
            when (and
                  ;; check entrenchment above threshold
                  (>= (score cxn) entrenchment-threshold)
                  ;; check that the cxn has at least one weighted-distribution above threshold
                  (loop for wd in (concept-representations::get-weighted-distributions (meaning cxn))
                        thereis (>= (weight wd) weight-threshold)))
              do (progn
                   (add-element
                    `((h4) ,(format nil "Construction with entrenchment score ~,2f (appeared during interaction ~a)"
                                    (score cxn)
                                    (first (first (history cxn))))))
                   (add-cxn-to-interface cxn :weight-threshold weight-threshold :disabled-features (disabled-features agent)))))))

;; helper function to add some text to the web interface
(defun show-in-wi (args)
  (add-element `((h4) ,(format nil "~{~a~^, ~}" args))))

;; show a scene in the web interface
(defun multi-view-p (experiment)
  "Returns true if an agent in an interaction of an experiment have multiple views."
  ;; only need to check one
  (> (length (views (first (interacting-agents experiment)))) 1))

(defun get-image-fpath (scene)
  (let ((dataset (dataset scene))
        (dataset-split (dataset-split scene))
        (image-fname (image-fname scene)))
    (merge-pathnames (merge-pathnames (make-pathname :directory `(:relative ,dataset "scenes" ,dataset-split))
                                      cl-user:*babel-corpora*)
                     image-fname)))

(defun show-scene (experiment)
  "Outputs the scene of the current interaction to the web-interface."
  (loop with world = (world experiment)
        with split = (get-configuration experiment :dataset-split)
        with multi-view-p = (multi-view-p experiment)
        with shown-p = nil
        for agent in (interacting-agents experiment)
        for context = (get-data agent 'context)
        for topic = (get-data agent 'topic)
        when (or (and multi-view-p (not shown-p))
                 (not multi-view-p))
          do (progn
               (if (not multi-view-p)
                 (add-element `((h2) ,(format nil "Scene as seen by the ~a (with view '~a')"
                                              (discourse-role agent)
                                              (downcase (mkstr (current-view agent))))))
                 (add-element `((h2) ,(format nil "Scene as seen by both agents."
                                              (loop for agent in (interacting-agents experiment)
                                                    collect (mkstr (id agent)))
                                              (downcase (mkstr (current-view agent)))))))
                                        ;(file-namestring (get-image-fpath context)))))
               #|(add-element `((div :class "image"
                               :style ,(format nil "margin-left: 50px; margin-bottom: 20px; width: fit-content; border-radius: 8px; overflow: hidden; border: 1px; border-color: #000000; box-shadow: 8px 8px 12px 1px rgb(0 0 0 / 10%);"))
                          ((img :src ,(string-append
                                       cl-user::*localhost-user-dir*
                                       (concatenate 'string
                                                    split
                                                    "/"
                                                    (file-namestring (get-image-fpath context))))))))|#
               (add-element `((table :style ,(format nil "margin-left: 50px;"))
                              ((tr) ((td) ,(make-html context
                                                      :agent agent
                                                      :topic (id topic)
                                                      :world world
                                                      :expand-initially nil)))))
               (setf shown-p t)))

  (add-element `((h2) ,(format nil "The speaker chooses object as the topic of the conversation:")))
  (add-element `((div :class "image" :style ,(format nil "margin-left: 50px;"))
                 ,(make-html (find-data (speaker (current-interaction experiment)) 'topic)
                             :agent (speaker (current-interaction experiment))
                             :world (world experiment)
                             :expand-initially nil))))

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
   `((h2) ,(format nil "~a is the SPEAKER with lexicon (size = ~a):"
                   (mkstr (id (speaker interaction)))
                   (downcase (mkstr (lexicon-size (lexicon (speaker interaction))))))))
  
  ;(display-lexicon (speaker interaction) :sort t)
  (add-element
   `((h2) ,(format nil "~a is the HEARER with lexicon (size = ~a):"
                   (mkstr (id (hearer interaction)))
                   (downcase (mkstr (lexicon-size (lexicon (hearer interaction))))))))
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
  (show-scene experiment))

;; ---------------------
;; + Conceptualisation +
;; ---------------------
(define-event-handler (trace-interaction-in-web-interface event-conceptualisation-start)
  (add-element '((hr)))
  (add-element
   `((h2) ,(format nil "Step 1: ~a performs conceptualisation..." (discourse-role agent)))))

(define-event-handler (trace-interaction-in-web-interface event-conceptualisation-end)
  )

;; -------------
;; + Invention +
;; -------------
(define-event-handler (trace-interaction-in-web-interface event-invention-end)
  (add-element
   `((h3) ,(format nil " - SPEAKER failed conceptualisation")))
  (add-element
   `((h3) ,(format nil " - SPEAKER invented: ~a" (form cxn)))))

;; -------------
;; + Production +
;; -------------
(define-event-handler (trace-interaction-in-web-interface event-production-end)
  (if (utterance agent)
    (progn
      (add-element `((h2) ,(format nil "Step 2: SPEAKER produced an utterance: \"~a\" " (utterance agent))))
      (add-cxn-to-interface (find-data agent 'applied-cxn) :disabled-features (disabled-features agent))
      )
    (add-element `((h2) ,(format nil "Step 2: SPEAKER could not produce an utterance")))))

;; -----------
;; + Parsing +
;; -----------
(define-event-handler (trace-interaction-in-web-interface event-parsing-end)
  (when (hearerp agent)
    (if (find-data agent 'applied-cxn)
      (progn
        (add-element '((h2) "Step 3: HEARER knows the utterance"))
        (add-cxn-to-interface (find-data agent 'applied-cxn) :disabled-features (disabled-features agent)))
      (progn
        (add-element
         `((h2) ,(format nil "Step 3: HEARER does not know the utterance  \"~a\"" (utterance agent))))
        (add-element
         `((h2) "Step 4: HEARER indicates failure"))))))

;; ------------------
;; + Interpretation +
;; ------------------

(define-event-handler (trace-interaction-in-web-interface event-interpretation-end)
  (when (find-data agent 'interpreted-topic)
    (progn
      (add-element
       `((h2) ,(format nil "Step 4: HEARER points to")))
      (add-element `((div :class "image" :style ,(format nil "margin-left: 50px;"))
                     ,(make-html (find-data agent 'interpreted-topic)
                                 :agent agent
                                 :world (world (experiment agent))
                                 :expand-initially nil))))))

;; -------------
;; + Alignment +
;; -------------
(define-event-handler (trace-interaction-in-web-interface event-align-start)
  (add-element '((hr)))
  (add-element `((h2) ,(format nil "Step 5: ~a updated representations" (discourse-role agent)))))

(define-event-handler (trace-interaction-in-web-interface event-adopt-start)
  (add-element
   `((h3) ,(format nil " - HEARER adopts a new cxn with form \"~a\"" (form cxn))))
  (add-cxn-to-interface cxn))

(define-event-handler (trace-interaction-in-web-interface event-align-cxn)
  (add-element
   `((h4) ,(format nil "[~a] - aligned cxn with form \"~a\"" reason (form cxn))))
  (add-cxn-diff-to-interface cxn previous-copy))
