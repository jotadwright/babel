(in-package :cl-framenet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                            ;;
;; Configuration for the cl-framenet system   ;;
;;                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Directory pointing to framenet data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *framenet-data-directory* (if (boundp '*framenet-data-directory*)
                                          *framenet-data-directory*
                                          "<Add your /path/to/Framenet-1.5 here> (configuration.lisp)"))
  
(defparameter *fn-data-storage-file* (make-pathname :directory (fn-directory)
                                                    :name "fn-data"
                                                    :type "store"))

(defparameter *fn-data* "Variable into which FrameNet data is loaded.")

