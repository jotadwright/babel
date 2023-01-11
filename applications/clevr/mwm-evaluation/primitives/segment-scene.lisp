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
   (bind (segmented-scene
          1.0
          (case (find-data ontology 'world-type)
            (:simulated (mwm::clevr->simulated (load-clevr-scene (clevr-world::get-pathname scene))))
            (:extracted (mwm::clevr->extracted (load-clevr-scene (clevr-world::get-pathname scene))
                                               :directory (get-data ontology 'extracted-scenes-path)))))))

  ;; second case; get the pathname from the segmented-scene
  ((segmented-scene => scene)
   (bind (scene 1.0 (make-instance 'pathname-entity :pathname (source-path segmented-scene)))))

  ;; third case; consistency check
  ((scene segmented-scene =>)
   (equal (source-path segmented-scene)
          (clevr-world::get-pathname pathname scene)))
  :primitive-inventory *mwm-primitives*)

