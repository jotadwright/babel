(in-package :geoquery-lsfb)

;;-------------;;
;; Interaction ;;
;;-------------;;

(defmethod run-interaction ((experiment geoquery-lsfb-exeriment))
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment))
        (current-example-pathname (random-elt )))