(in-package :visual-dialog)

(defmethod initialize-instance :after ((world visual-dialog::world) &key )
  "Initialize the clevr world. Specify which data sets to load (e.g. (\"val\" \"train\"))
   You can specify which scenes to exclude by their name (e.g. CLEVR_val_000002)
   You can specify whether or not to also load the questions.
   By default, the questions will be loaded from the same data-sets/sub-folders as
   specified in data-sets. If this should be different, these can be specified
   using question-data-sets."
  (let ((data-path
         (cond ((eql (get-configuration world :dataset) :clevr) *clevr-data-path*)
               ((eql (get-configuration world :dataset) :mnist) *mnist-data-path*)
               ((eql (get-configuration world :dataset) :gqa) *gqa-data-path*)))
         ;(if (eql (get-configuration world :dataset) :clevr)
         ;   *clevr-data-path*
         ;   *mnist-data-path*))
        (datasplit
         (cond ((eql (get-configuration world :datasplit) :train)
                (list "train"))
               ((eql (get-configuration world :datasplit) :val)
                (list "val"))
               ((eql (get-configuration world :datasplit) :test)
                (list "test")))))
  ;; check if datapath exists
  (unless (probe-file data-path)
    (error "Could not find the 'data-path' directory in ~%~a" data-path))
  ;; check data-sets
  (unless (and (listp datasplit)
               (length> datasplit 0)
               (apply #'always (mapcar #'stringp datasplit)))
    (error "Keyword argument :data-sets should be a list of strings"))
  ;; load the scenes
  (let ((scenes-path
         (if (eql (get-configuration world :mode) :symbolic)
           (merge-pathnames (make-pathname :directory '(:relative "scenes"))
                            data-path)
           (if (eql (get-configuration world :dataset) :mnist)
             (merge-pathnames (make-pathname :directory '(:relative "imgs"))
                              data-path)
             (merge-pathnames (make-pathname :directory '(:relative "images"))
                              data-path)))))
    (print scenes-path)
    (unless (probe-file scenes-path)
      (error "Could not find a 'scenes' subdirectory in ~a~%" data-path))
    (setf (slot-value world 'visual-dialog::scenes)
          (loop for data-set in datasplit
                for set-directory = (merge-pathnames (make-pathname :directory `(:relative ,data-set))
                                                     scenes-path)
                unless (probe-file set-directory)
                do (error "~a is not a subdirectory of ~%~a" data-set scenes-path)
                append (sort (directory
                              (make-pathname :directory (pathname-directory set-directory)
                                             :name :wild :type (if (eql (get-configuration world :mode) :symbolic) "json" (if (eql (get-configuration world :dataset) :mnist) "jpg" "png"))))
                             #'string< :key #'namestring))))
  ;; load the dialogs, if requested
    (let ((dialogs-path
           (merge-pathnames (make-pathname :directory '(:relative "dialogs"))
                            data-path)))
      (unless (probe-file dialogs-path)
        (error "Could not find a 'dialogs' subdirectory in ~a~%" data-path))
      (setf (slot-value world 'dialog-sets)
            (loop for data-set in datasplit
                  for set-directory = (merge-pathnames (make-pathname :directory `(:relative ,data-set))
                                                       dialogs-path)
                  unless (probe-file set-directory)
                  do (error "~a is not a subdirectory of ~%~a" data-set dialogs-path)
                  append (sort (directory
                                (make-pathname :directory (pathname-directory set-directory)
                                               :name :wild :type "json"))
                               #'string< :key #'namestring))))
  ))