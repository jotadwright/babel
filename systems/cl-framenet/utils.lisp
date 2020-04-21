(in-package :cl-framenet)

;;;;;;;;;;;;;;;;
;;            ;;
;; Utilities  ;;
;;            ;;
;;;;;;;;;;;;;;;;

(defmacro fn-directory ()
  `(pathname-directory ,(or *load-truename* *compile-file-truename*)))
