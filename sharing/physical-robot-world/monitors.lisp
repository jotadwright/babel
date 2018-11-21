
(in-package :physical-robot-world)

(export 'record-physical-robot-world-scene-name)

(define-monitor
 record-physical-robot-world-scene-name
 :class 'data-recorder
 :documentation "Records the name of the current scene of the physical-robot-world")

(define-event-handler (record-physical-robot-world-scene-name interaction-finished)
  (when (slot-exists-p experiment 'world)
    (record-value monitor (name (current-scene (slot-value experiment 'world))))))


