(in-package :gcng)

(defun get-form-competitors (agent cxn)
  "Get the form-competitors of the given cxn.
   These are cxns with the same meaning"
  (remove cxn
          (find-all (attr-val cxn :category)
                    (constructions (grammar agent))
                    :key #'(lambda (cxn) (attr-val cxn :category)))))

(defun get-meaning-competitors (agent cxn)
  "Get the meaning-competitors of the given cxn.
   These are cxns with the same form"
  (remove cxn
          (find-all (attr-val cxn :form)
                    (constructions (grammar agent))
                    :key #'(lambda (cxn) (attr-val cxn :form))
                    :test #'string=)))

(defun reward-applied-cxn-and-punish-competitors (agent applied-cxn)
  "Increase the score of the applied cxn and punish both the form-
   and meaning-competitors"
  (inc-cxn-score applied-cxn :delta (get-configuration agent :cxn-incf-score))
  (loop for competitor in (get-form-competitors agent applied-cxn)
        do (dec-cxn-score agent competitor :delta (get-configuration agent :cxn-decf-score)))
  (loop for competitor in (get-meaning-competitors agent applied-cxn)
        do (dec-cxn-score agent competitor :delta (get-configuration agent :cxn-decf-score))))

(defun punish-applied-cxn (agent applied-cxn)
  "Decrease the score of the applied cxn, unless when the cxn
   was just added in this interaction."
  (let ((interaction-number
         (interaction-number (current-interaction (experiment agent)))))
    (unless (= interaction-number (attr-val (applied-cxn agent) :added))
      (dec-cxn-score agent applied-cxn :delta (get-configuration agent :cxn-decf-score)))))
    
(define-event cxn-rewarded (cxn fcg-construction))
(define-event cxn-punished (cxn fcg-construction))

(defun inc-cxn-score (cxn &key (delta 0.1) (upper-bound 1.0))
  "Increment the cxn score, taking into account the upper bound"
  (incf (attr-val cxn :score) delta)
  (notify cxn-rewarded cxn)
  (when (> (attr-val cxn :score) upper-bound)
    (setf (attr-val cxn :score) upper-bound)))

(defun dec-cxn-score (agent cxn &key (delta 0.1) (lower-bound 0.0)
                            (remove-on-lower-bound t))
  "Decrement the cxn score, taking into account the lower bound.
   When the score reaches 0, the cxn is removed."
  (decf (attr-val cxn :score) delta)
  (notify cxn-punished cxn)
  (when (< (attr-val cxn :score) 0.1)
    (if remove-on-lower-bound
      (delete-cxn cxn (grammar agent))
      (setf (attr-val cxn :score) lower-bound))))

(define-event category-shifted (category color-category))

(defun shift-color-prototype (agent category object)
  "Shift the color prototype towards the object
   with a factor alpha.
   P' = (1 - alpha)*P + alpha*object"
  (let ((alpha (get-configuration agent :alpha)))
    (setf (value category)
          (mapcar #'(lambda (c o)
                      (+ (* (- 1 alpha) c)
                         (* alpha o)))
                  (value category)
                  (lab-color object)))
    (notify category-shifted category)))

;; ------------
;; + Adoption +
;; ------------

(define-event discriminating-category-found (category color-category))

(defun find-discriminating-category (agent correct-topic)
  "the hearer conceptualises the topic and this
   needs to be discriminating
   when successful, the hearer stores a new cxn"
  (let* ((irl-program `((bind sensory-object ?topic ,correct-topic)
                        (filter-by-closest-color ?topic ?context ?color)
                        (get-context ?context)))
         (solutions (evaluate-irl-program irl-program (ontology agent))))
    (when (and solutions (length= solutions 1))
      (let* ((binding (find '?color (first solutions) :key #'var))
             (color-category (value binding)))
        (notify discriminating-category-found color-category)
        color-category))))

(define-event adoption-started)
(define-event adoption-finished (cxn fcg-construction))

(defmethod adopt ((agent grounded-color-naming-game-agent)
                  (correct-topic sensory-object))
  "the hearer looks for a discriminating category for the topic
   if this fails, a new one is created
    a new cxn is stored (either between existing or new category)"
  (notify adoption-started)
  (let ((discriminating-color-category
         (find-discriminating-category agent correct-topic))
        new-cxn)
    (unless discriminating-color-category
      (setf discriminating-color-category (make-color-category correct-topic))
      (notify new-category-created discriminating-color-category)
      (unless (get-configuration agent :silent)
        (speak (robot agent) "I created a new color category"))
      (push-data (ontology agent) 'color-categories discriminating-color-category))
    (setf new-cxn (add-lex-cxn agent (utterance agent) discriminating-color-category))
    (notify adoption-finished new-cxn)
    (unless (get-configuration agent :silent)
      (speak (robot agent) "I adopted a new word"))
    new-cxn))

;; -------------
;; + Alignment +
;; -------------

(define-event alignment-started (agent grounded-color-naming-game-agent))
  
(defmethod align-agent ((agent grounded-color-naming-game-agent) correct-topic)
  "If the interaction was a success, the agents shift their
   applied color category slightly towards the topic. Also, they
   reward the applied form/meaning mapping and punish its competitors

   If the interaction was not a success, the agents punish the applied
   form/meaning mapping, if there is any.
   Alternatively, if there is none and the agent is the hearer,
   adopt a new word."
  (if (communicated-successfully agent)
    (progn (notify alignment-started agent)
      (shift-color-prototype agent (applied-category agent) correct-topic)
      (reward-applied-cxn-and-punish-competitors agent (applied-cxn agent)))
    (progn
      (when (applied-cxn agent)
        (notify alignment-started agent)
        (punish-applied-cxn agent (applied-cxn agent)))
      (when (and (hearerp agent)
                 (null (applied-cxn agent)))
        (adopt agent correct-topic)))))