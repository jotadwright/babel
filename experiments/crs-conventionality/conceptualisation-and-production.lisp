(in-package :crs-conventionality)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                    ;;
;; Code implementing conceptualisation and production ;;
;;                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric conceptualise-and-produce (speaker scene topic)
  (:documentation "Based on the topic and scene, the speaker produces an utterance."))


(defmethod conceptualise-and-produce ((speaker naming-game-agent) (scene crs-conventionality-scene) (topic naming-game-entity))
  "In the naming game, conceptualisation simply returns the topic entity."
  (declare (ignore scene))
  )

