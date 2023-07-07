(in-package :cle)

;; ----------------------
;; + Available channels +
;; ----------------------

(defmethod get-all-channels ((mode (eql :clevr-simulated)))
  (reverse `(
             ,'xpos ,'ypos ,'zpos ;; position
             ,'area ;; size
             ,'wh-ratio ;; shape
             ,'nr-of-sides ,'nr-of-corners ;; shape
             ,'r ,'g ,'b ;; color
             ,'roughness ;; material
             ,'xpos-3d ,'ypos-3d ,'zpos-3d ;; 3d-position
             ,'rotation ;; rotation
             )))

(defmethod get-all-channels ((mode (eql :clevr-extracted)))
  (reverse `(
             ,'xpos ,'ypos
             ,'width ,'height
             ,'angle
             ,'corners
             ,'area ,'relative-area
             ,'bb-area ,'bb-area-ratio
             ,'wh-ratio
             ,'circle-distance
             ,'white-level ,'black-level
             ,'color-mean-l ,'color-mean-a ,'color-mean-b
             ,'color-std-l ,'color-std-a ,'color-std-b
             )))

#|(defmethod get-all-channels ((mode (eql :winery)))
    (reverse `(
               ,'fixed-acidity
               ,'volatile-acidity
               ,'citric-acid
               ,'residual-sugar
               ,'chlorides
               ,'free-sulfur-dioxide
               ,'total-sulfur-dioxide
               ,'density
               ,'pH
               ,'sulphates
               ,'alcohol)))|#

;; ------------------
;; + Topic sampling +
;; ------------------

(defmethod sample-topic (experiment (mode (eql :english-concepts)))
  "Only objects that can be distinguished using a single metadata dimension can serve as topic."
  (let* ((interaction (current-interaction experiment))
         (agent (first (interacting-agents experiment)))
         (cle-scene (find-data agent 'context))
         (candidate-topics (filter-discriminative-topics (objects cle-scene))))
    (if candidate-topics
      (let ((cle-topic (random-elt candidate-topics)))
        (set-data interaction 'channel-type (get-symbolic-discriminative-feature cle-topic cle-scene))
        (loop for agent in (interacting-agents experiment)
              do (set-data agent 'topic cle-topic)))
      (progn
        (sample-scene experiment (get-configuration experiment :scene-sampling))
        (sample-topic experiment (get-configuration experiment :topic-sampling))))))

;; --------------------
;; + Helper functions +
;; --------------------
(defun get-symbolic-discriminative-feature (topic context)
  "Returns which symbolic features of the topic are discriminative."
  (let ((other-objects (remove topic (objects context))))
    (loop for (attr . val) in (description topic)
          for available = (is-channel-available attr (attributes topic))
          for discriminative = (loop for other-object in other-objects
                                     for other-val = (assqv attr (description other-object))
                                     always (not (equal val other-val)))
          if (and available discriminative)
            collect (cons attr val))))

(defun filter-discriminative-topics (context)
  "Determines which objects in the context are discriminative."
  (loop for object in context
        when (is-discriminative object (remove object context))
        collect object))

(defun is-discriminative (object other-objects)
  "Checks if the object has a single channel dimension that is different from all other objects."
  (loop for (attr . val) in (description object)
        do (when (is-channel-available attr (attributes object))
             (let ((discriminative (loop for other-object in other-objects
                                         for other-val = (assqv attr (description other-object))
                                         always (not (equal val other-val)))))
               (when discriminative
                 (return t))))))

(defun is-channel-available (symbolic-attribute raw-attributes)
  (let ((continuous-attributes (mapcar 'first raw-attributes)))
    (case symbolic-attribute
      (:COLOR (or (if (member 'R continuous-attributes) t nil)
                  (if (member 'G continuous-attributes) t nil)
                  (if (member 'B continuous-attributes) t nil)))
      (:SIZE (if (member 'AREA continuous-attributes) t nil))
      (:SHAPE (or (if (member 'NR-OF-CORNERS continuous-attributes) t nil)
                  (if (member 'NR-OF-SIDES continuous-attributes) t nil)
                  (if (member 'WH-RATIO continuous-attributes) t nil)))
      (:MATERIAL (if (member 'ROUGHNESS continuous-attributes) t nil))
      (:XPOS (or (if (member 'XPOS continuous-attributes) t nil)
                 (if (member 'YPOS continuous-attributes) t nil)))
      (:ZPOS (or (if (member 'ZPOS continuous-attributes) t nil)
                 (if (member 'YPOS continuous-attributes) t nil))))))