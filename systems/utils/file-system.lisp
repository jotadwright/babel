(in-package :utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;; Interacting with the file system ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(make-directory remove-directory))

(defun make-directory (path)
  "Creates a directory if it does not exist already."
  (ensure-directories-exist
   (uiop:ensure-directory-pathname
    (uiop:ensure-absolute-pathname path))))

(defun remove-directory (path &key force)
  "Removes the directory including its contents."
  (let ((pathname (uiop:ensure-directory-pathname
                   (uiop:ensure-absolute-pathname path))))
    (when (and (uiop:directory-exists-p pathname)
               (or force
                   (y-or-n-p "Delete directory ~a?" path)))
      (uiop:delete-directory-tree pathname :validate t))))
