;;;; get-context.lisp

(in-package :mwm-evaluation)


;; --------------------------
;; SEGMENT-SCENE primitive ;;
;; --------------------------
;; go from pathname to a segmented scene containing continuous valued objects

(defprimitive segment-scene ((segmented-scene mwm::mwm-object-set)
                             (scene pathname-entity))
  ;; first case; read the scene file and create a clevr-scene
  ((scene => segmented-scene)
   (bind (segmented-scene 1.0 (mwm::clevr->simulated (load-clevr-scene (pathname scene))))))

  ;; second case; get the pathname from the segmented-scene
  ((segmented-scene => scene)
   (bind (scene 1.0 (make-instance 'pathname-entity :pathname (source-path segmented-scene)))))

  ;; third case; consistency check
  ((scene segmented-scene =>)
   (equal (source-path segmented-scene)
          (pathname scene)))
  :primitive-inventory *mwm-primitives*)

