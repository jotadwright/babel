(in-package :utils)

;; ############################################################################
;; utilities for making file names:
;; ----------------------------------------------------------------------------

(export '(make-file-name-with-experiment-class
          make-file-name-with-time-and-experiment-class
          make-file-name-with-time-and-series
          make-file-name-with-series
          make-file-name-with-job-and-task-id
          make-file-name-with-time
          make-file-name-with-unix-time))

(defun make-file-name-with-experiment-class (file-name experiment-class)
  "A helper function for making file-names that contains the experiment class"
  (if (pathname-name file-name)
      (format nil "~a~a-~a.~a" 
              (make-pathname :directory (pathname-directory file-name))
              (string-downcase experiment-class)
              (pathname-name file-name)
              (pathname-type file-name))
      (format nil "~a~a.~a"
              (make-pathname :directory (pathname-directory file-name))
              (string-downcase experiment-class)
              (pathname-type file-name))))


(defun make-file-name-with-time-and-experiment-class (file-name experiment-class)
  "A helper function for making file-names that contain the time and the experiment class"
  (mkstr (make-pathname :directory (pathname-directory file-name))
	 (multiple-value-bind (sec min hour day month year)
	     (decode-universal-time (get-universal-time))
	   (format nil "~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-"
                   year month day hour min sec))
	 (string-downcase experiment-class)
	 "-" (pathname-name file-name) "." (pathname-type file-name)))


#+sbcl
(defun make-file-name-with-job-and-task-id (file-name experiment-class)
  "A helper function for making file-names that contain the time and the experiment class"
  (mkstr (make-pathname :directory (pathname-directory file-name))
 	 (format nil "~a-~a-" (sb-unix::posix-getenv "SLURM_ARRAY_JOB_ID")
                 (sb-unix::posix-getenv "SLURM_ARRAY_TASK_ID"))
	 (string-downcase experiment-class)
 	 "-" (pathname-name file-name) "." (pathname-type file-name)))


#+(or lispworks ccl)
(defun make-file-name-with-job-and-task-id (file-name experiment-class)
  "A helper function for making file-names that contain the time and the experiment class"
  (mkstr (make-pathname :directory (pathname-directory file-name))
 	 (format nil "~a-~a-" (ccl::getenv "SLURM_ARRAY_JOB_ID")
                (ccl::getenv "SLURM_ARRAY_TASK_ID"))
	 (string-downcase experiment-class)
         "-" (pathname-name file-name) "." (pathname-type file-name)))

(defun make-file-name-with-time-and-series (file-name series)
  (make-file-name-with-time
   (mkstr
    (make-pathname :directory (pathname-directory file-name))
    (pathname-name file-name) "-series-" series  "." (pathname-type file-name))))

(defun make-file-name-with-series (file-name series)
  (mkstr
   (make-pathname :directory (pathname-directory file-name))
   (pathname-name file-name) "-series-" series  "." (pathname-type file-name)))

(defun make-file-name-with-time (file-name)
  "A helper function for making file-names that contain the time"
  (mkstr (make-pathname :directory (pathname-directory file-name))
	 (multiple-value-bind (sec min hour day month year)
	     (decode-universal-time (get-universal-time))
	   (format nil "~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-"
                   year month day hour min sec))
	 (pathname-name file-name) "." (pathname-type file-name)))


(defun make-file-name-with-unix-time (file-name)
  "A helper function for making file-names that contain the unix time"
  (mkstr (make-pathname :directory (pathname-directory file-name))
	 (get-universal-time) "-"
         (pathname-name file-name) "."
         (pathname-type file-name)))


;; #+sbcl
;; (defun make-file-name-with-job-and-task-id (file-name experiment-class)
;;   "A helper function for making file-names that contain the time and the experiment class"
;;   (mkstr (make-pathname :directory (pathname-directory file-name))
;; 	 (format nil "~a-~a-" (sb-unix::posix-getenv "SLURM_ARRAY_JOB_ID")
;;                  (sb-unix::posix-getenv "SLURM_ARRAY_TASK_ID"))
;; 	 (string-downcase experiment-class)
;; 	 "-" (pathname-name file-name) "." (pathname-type file-name)))