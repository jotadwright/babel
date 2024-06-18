(in-package :fcg)

;;;;;;;;;;;;;;;;;
;;             ;;
;; Handle fix  ;;
;;             ;;
;;;;;;;;;;;;;;;;;

(defmethod handle-fix ((fix cxn-inventory-fix) (repair repair) (problem problem) (cip construction-inventory-processor) &key &allow-other-keys)
  "Handle fix should not do anything apart from adding the fix to the problem object."
  (call-next-method))