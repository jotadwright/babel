(in-package :cle)

;; ------------------------
;; + Set a scene manually +
;; ------------------------
(defmethod set-scene (experiment scene-id)
  "Set a scene manually."
  (let* ((channels-in-play (get-configuration experiment :clevr-channels))
         (symbolic-clevr-context (get-scene-by-index (world experiment) scene-id))
         (cle-context (clevr->simulated symbolic-clevr-context channels-in-play)))
    (loop for agent in (interacting-agents experiment)
          do (set-data agent 'context cle-context))))

;; ------------------
;; + Scene sampling +
;; ------------------
(defgeneric sample-scene (experiment mode)
  (:documentation "Samples a scene and assigns it to experiment"))

(defmethod sample-scene (experiment (mode (eql :random-scene)))
  "Sample a random scene and assign to experiment."
  (let* ((channels-in-play (get-configuration experiment :clevr-channels))
         (symbolic-clevr-context (random-scene (world experiment)))
         (cle-context (clevr->simulated symbolic-clevr-context channels-in-play)))
    (loop for agent in (interacting-agents experiment)
          do (set-data agent 'context cle-context))))

(defmethod sample-scene (experiment (mode (eql :deterministic)))
  "Cycles through a list of scenes (specified by get-scene-by-indexes)."
  (let* ((channels-in-play (get-configuration experiment :clevr-channels))
         (scene-ids (get-configuration experiment :scene-ids))
         (symbolic-clevr-context (get-scene-by-indexes experiment scene-ids))
         (cle-context (clevr->simulated symbolic-clevr-context channels-in-play)))
    (loop for agent in (interacting-agents experiment)
          do (set-data agent 'context cle-context))))

;; helper functions
(defun get-scene-by-indexes (experiment scene-ids)
  (let ((current-idx (get-configuration experiment :current-scene-idx)))
    (set-configuration experiment :current-scene-idx (mod (+ current-idx 1) (length scene-ids)))
    (get-scene-by-index (world experiment) (nth current-idx scene-ids))))
