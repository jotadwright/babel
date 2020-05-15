(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;; Getting Started with cl-propbank ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package requires that either your the global variable *propbank-data-directory* is set to point to the directory where the raw propbank data (frames) can be found, or that a file pb-data.store is found in this directory.

(ql:quickload :cl-propbank)
(in-package :cl-propbank)

(load-pb-data :store-data t
              :ignore-stored-data t)

(length *pb-data*)