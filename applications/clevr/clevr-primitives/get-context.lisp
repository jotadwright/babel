;;;; get-context.lisp

(in-package :clevr-primitives)

;; -----------------------
;; GET-CONTEXT primtive ;;
;; -----------------------

;(export '(get-context))

(defprimitive get-context ((context clevr-object-set))
  ;; first case; consistency check
  ((context =>)
   (equal-entity (get-data ontology 'clevr-context) context))
  
  ;; second case; bind the context from the ontology
  ((=> context)
   (bind (context 1.0 (get-data ontology 'clevr-context))))
  :primitive-inventory *clevr-primitives*)


;; --------------------------
;; SEGMENT-SCENE primitive ;;
;; --------------------------

(defprimitive segment-scene ((segmented-scene clevr-object-set)
                             (scene pathname-entity))
  ;; first case; read the scene file and create a clevr-scene
  ((scene => segmented-scene)
   (bind (segmented-scene 1.0 (load-clevr-scene (pathname scene)))))

  ;; second case; get the pathname from the segmented-scene
  ((segmented-scene => scene)
   (bind (scene 1.0 (make-instance 'pathname-entity :pathname (source-path segmented-scene)))))

  ;; third case; consistency check
  ((scene segmented-scene =>)
   (equal (source-path segmented-scene)
          (pathname scene)))
  :primitive-inventory *clevr-primitives*)

