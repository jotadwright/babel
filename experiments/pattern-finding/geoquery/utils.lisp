(in-package :pf-for-sql)

;;----------------------;;
;; working with strings ;;
;;----------------------;;

(defun remove-last-character (str)
  (subseq str 0 (- (length str) 1)))