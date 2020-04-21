(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;; Getting Started with cl-framenet ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package requires that either your the global variable *framenet-data-directory* is set to point to the directory where the raw framenet data can be found, or that a file framenet-data.store is found in this directory.

(ql:quickload :cl-framenet)
(in-package :cl-framenet)

