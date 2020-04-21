(in-package :cl-framenet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            ;;
;; Loading the  Framenet data ;;
;;                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-fn-data (&key (store-data t) ignore-stored-data)
  "Loads FrameNet data and stores the result. It is loaded from a fn-data.store file if it
   is available, from the raw fn-data files otherwise. :store-data nil avoids storing the data,
   ignore-stored-data t forces to load the data from the original files."
  ;; Load the data into *fn-data*
  (if (and (probe-file *fn-data-storage-file*)
           (not ignore-stored-data))
    ;; Option 1: Load from storage file if file exists ignore-stored-data is nil.
    (setf *fn-data* (cl-store:restore *fn-data-storage-file*))
    ;; Option 2: Load from original framenet files.
    (setf *fn-data* (load-fn-data-from-original-xml-files)))
  ;; Store the data into an *fn-data-storage-file*
  (if (and store-data (or ignore-stored-data
                          (not (probe-file *fn-data-storage-file*))))
    (cl-store:store *fn-data* *fn-data-storage-file*))
  ;; Finally return fn-data
  *fn-data*)

(defun load-fn-data-from-original-xml-files ()
       "TODO")
