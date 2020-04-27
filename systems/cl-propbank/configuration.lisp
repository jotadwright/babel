(in-package :cl-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                            ;;
;; Configuration for the cl-propbank system   ;;
;;                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Directory pointing to PropBank data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *propbank-data-directory* (if (boundp '*propbank-data-directory*)
                                          *propbank-data-directory*
                                          "<Add your /path/to/Propbank-frames here> (configuration.lisp)"))
  
(defparameter *pb-data-storage-file* (make-pathname :directory (pb-directory)
                                                    :name "pb-data"
                                                    :type "store"))

(defparameter *pb-data* "Variable into which Propbank data is loaded.")

