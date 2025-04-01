(in-package :cle)

;; ---------
;; + Agent +
;; ---------

(defclass cle-agent (agent)
  ((lexicon
    :documentation "The agent's lexicon."
    :type lexicon :accessor lexicon :initarg :lexicon :initform nil)
   (views
    :documentation "The views that the agent has over a world."
    :type list :accessor views :initarg :views :initform nil)
   (current-view
    :documentation "The current view assigned to the agent."
    :type string :accessor current-view :initform nil)
   (invented-or-adopted
    :documentation "Whether the agent invented or adopted during the interaction."
    :type boolean :accessor invented-or-adopted :initform nil)
   (usage-table
    :documentation "Keeps track of the cxns used with a sliding window."
    :type usage-table :accessor usage-table :initarg :usage-table)
   (perceived-objects
    :documentation "Stores perceived objects"
    :type perceived-objects :accessor perceived-objects :initform (make-hash-table))
   (social-network
    :documentation "Neighbors of an agent"
    :type list :accessor social-network :initform nil)
   (neighbor-q-values
    :documentation "Stores the q-values associated to its neighbors."
    :type hash-table :accessor neighbor-q-values :initform (make-hash-table))))

(defmethod clear-agent ((agent cle-agent))
  "Clear the slots of the agent for the next interaction."
  (setf (blackboard agent) nil
        (utterance agent) nil
        (invented-or-adopted agent) nil
        (perceived-objects agent) (make-hash-table)
        (communicated-successfully agent) nil))

(defmethod find-in-lexicon ((agent cle-agent) (form string))
  "Finds constructions with the given form in the lexicon of the given agent."
  (find-form-in-lexicon (lexicon agent) form))

(defmethod empty-lexicon-p ((agent cle-agent))
  (eq (lexicon-size (lexicon agent)) 0))

;; ---------------------
;; + Partner selection +
;; ---------------------
(defun calculate-new-q-value (q-value reward lr)
  (+ q-value (* lr (- reward q-value))))

(defun boltzmann-exploration (q-values tau)
  "Boltzmann exploration for partner selection.

   tau corresponds to an inverse temperature:
     if tau = 0: no preference, random sampling
     if tau > 0: preference to select partners you understand well
     if tau < 0: curiosity-driven partner selection

    Inspired by Leung's et al. paper on curiosity-driven partner selection (2025)."
  (let* ((exp-values (mapcar (lambda (q) (exp (* tau q))) q-values))
         (total-sum (reduce #'+ exp-values)))
    (mapcar (lambda (exp-value) (/ exp-value total-sum)) exp-values)))

(defun sample-partner (neighbors probabilities)
  "Randomly sample a partner from the neighbors using the given probabilities."
  (loop with r = (random 1.0)
        with cumulative = 0.0
        for neighbor in neighbors
        for probability in probabilities
        do (setf cumulative (+ cumulative probability))
        when (<= r cumulative)
          return neighbor
        ;; if the sum of probabilities is not equal to 1 (due to some numerical stability)
        ;; always make a decision and pick the last agent
        finally (return (car (last neighbors)))))

(define-event event-partner-selection
  (agent cle-agent)
  (neighbors list)
  (q-values list)
  (probabilities list)
  (partner-id symbol))

(defmethod choose-partner ((agent cle-agent) (neighbors list) (tau number))
  "Choose a partner for a given agent using Boltzmann exploration."

  (let* ((tuples (loop for neighbor in neighbors
                       collect (cons (id neighbor)
                                     (gethash (id neighbor) (neighbor-q-values agent)))))
         (neighbor-ids (mapcar #'car tuples))
         (q-values (mapcar #'cdr tuples))
         (probabilities (boltzmann-exploration q-values tau))
         ;; sample a partner (by id)
         (partner-id (sample-partner neighbor-ids probabilities))
         ;; match id to the other agent
         (partner (find partner-id neighbors :test (lambda (x y) (eq x (id y))))))

    (notify event-partner-selection agent neighbors q-values probabilities partner-id)
    partner))

(defmethod initialise-neighbor-q-values ((agent cle-agent) (neighbor cle-agent) &key (default-q 0.5))
  "Initialise a q-value for a given agent's neighbor."
  (let ((q-values (neighbor-q-values agent)))
    (when (not (gethash (id neighbor) q-values))
      (setf (gethash (id neighbor) q-values) default-q))))

(defmethod update-neighbor-q-value ((agent cle-agent) (neighbor cle-agent) (reward number) (lr float))
  "Update the partner selection q-value."
  (let* ((q-values (neighbor-q-values agent))
         (q-old (gethash (id neighbor) q-values))
         (q-new (calculate-new-q-value q-old reward lr)))
    (setf (gethash (id neighbor) q-values) q-new)))