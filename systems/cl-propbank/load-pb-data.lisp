(in-package :cl-propbank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            ;;
;; Loading the  Propbank data ;;
;;                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-pb-data (&key (store-data t) ignore-stored-data)
  "Loads ProbBank data and stores the result. It is loaded from a pb-data.store file if it
   is available, from the raw pb-data files otherwise. :store-data nil avoids storing the data,
   ignore-stored-data t forces to load the data from the original files."
  ;; Load the data into *pb-data*
  (if (and (probe-file *pb-data-storage-file*)
           (not ignore-stored-data))
    ;; Option 1: Load from storage file if file exists ignore-stored-data is nil.
    (setf *pb-data* (cl-store:restore *pb-data-storage-file*))
    ;; Option 2: Load from original propbank files.
    (setf *pb-data* (load-pb-data-from-original-xml-files)))
  ;; Store the data into an *fn-data-storage-file*
  (if (and store-data (or ignore-stored-data
                          (not (probe-file *pb-data-storage-file*))))
    (cl-store:store *pb-data* *pb-data-storage-file*))
  ;; Finally return pb-data
  (format nil "Loaded ~a predicates into *pb-data*." (length *pb-data*)))

(defun load-pb-data-from-original-xml-files ()
  "Loads all framesets and returns the predicate objects."
  (loop for file in (directory (merge-pathnames
                                (make-pathname :directory '(:relative "frames") :name :wild :type "xml")
                                *propbank-data-directory*))
        do (format t "Loading ~a~%" file)
        append (load-pb-file file)))

; (load-pb-data-from-original-xml-files)

(defun load-pb-file (pathname-or-frameset)
  "Loads a PropBank file and returns a list of predicate objects."
  (let* ((frameset-xml (read-pb-file pathname-or-frameset))
         (predicates-xml (xmls:xmlrep-find-child-tags "predicate" frameset-xml)))
    (loop for predicate-xml in predicates-xml
          collect (xml-predicate predicate-xml))))

;; (load-pb-file 'feel)

(defun read-pb-file (pathname-or-frameset)
  (let ((pathname (if (pathnamep pathname-or-frameset)
                    pathname-or-frameset
                    (merge-pathnames
                                (make-pathname :directory '(:relative "frames")
                                               :name (string-downcase (symbol-name pathname-or-frameset))
                                               :type "xml")
                                               *propbank-data-directory*))))
    (with-open-file (inputstream pathname :direction :input)
      (xmls:parse inputstream))))

;; (read-pb-file 'feel)
