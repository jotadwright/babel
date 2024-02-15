(in-package :parallel-lg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;
;; This file implements the server process side of the :parallel-lg package ;;
;;                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default directories and files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro compile-truename ()
  `',*compile-file-truename*)

(defvar *this-directory*
  (make-pathname :directory (pathname-directory (compile-truename))))

(defun temp-directory ()
  (let ((temp-directory (append (pathname-directory *this-directory*) (list ".tmp"))))
    (ensure-directories-exist (make-pathname :directory temp-directory))
    temp-directory))

;; External Lisps ;;
;;;;;;;;;;;;;;;;;;;;

(defparameter *external-lisps*
  '((:sbcl . ((:command . "sbcl")
              (:arguments . ("--noinform" "--disable-debugger"))))
    (:ccl . ((:command . "ccl")
             (:arguments . ("--batch" "--quiet"))))
    (:lispworks . ((:command . "lispworks")
                   (:arguments . ()))))
  "Configurations for starting up external common lisp processes.")

(defun external-lisp (lisp)
  "Returns all info for starting up an external lisp."
  (cdr (assoc lisp *external-lisps*)))

(defvar *external-lisp*
  #+sbcl :sbcl #+ccl :ccl #+lispworks :lispworks)


;; Client process ;;
;;;;;;;;;;;;;;;;;;;;

(defclass client-process ()
  ((id :accessor id :initarg :id :initform nil)
   (lisp :accessor lisp :initarg :lisp :initform *external-lisp*)
   (process :accessor process)
   (reader-thread :accessor reader-thread)
   (lock-file :accessor lock-file :initarg :lock-file :initform nil)
   (output-file :accessor output-file :initarg :output-file :initform nil))
  (:documentation "Class representing client process."))

(defmethod print-object ((client-process client-process) stream)
  "Prints a client process."
  (format stream "<client-process ~(~a~), ~(~a~), ~(~a~)>"
          (id client-process) (lisp client-process)
          (uiop/launch-program:process-alive-p (process client-process))))

(defmethod initialize-instance :after ((client-process client-process) &key)
  "Initializing a client-process object."
  ;; ID
  (cond ((and (id client-process) (symbolp (id client-process))))
        ((null (id client-process))
         (setf (id client-process) (intern (symbol-name (gensym "CLIENT-PROCESS-")))))
        (t (error "The id of a client-process should be a symbol.")))
  ;; Process
  (setf (process client-process)
        (uiop/launch-program:launch-program (cons (cdr (assoc :command (external-lisp (lisp client-process))))
                                                  (cdr (assoc :arguments (external-lisp (lisp client-process)))))
                                            :input :stream
                                            :output :stream))
  ;; Reader thread
  (setf (reader-thread client-process)
        (bt:make-thread (lambda ()
                          (loop for line = (read-line (uiop/launch-program:process-info-output (process client-process)) nil)
                                while line
                                do (format t "~a: ~A~%" (id client-process) line)))
                        :name (format nil "Reader thread for ~a" (id client-process))))
  ;; Lock-file
  (unless (lock-file client-process)
    (setf (lock-file client-process) (make-pathname :directory (temp-directory) :name (format nil "~(~a~)" (id client-process)) :type "lock")))
  (when (probe-file (lock-file client-process))
    (delete-file (lock-file client-process)))
  ;; Output-file
  (unless (output-file client-process)
    (setf (output-file client-process) (make-pathname :directory (temp-directory) :name (format nil "~(~a~)" (id client-process)) :type "out")))
  (when (probe-file (output-file client-process))
    (delete-file (output-file client-process))))

(defmethod terminate ((client-process client-process))
  "Terminates a client-process cleanly."
  (uiop/launch-program:terminate-process (process client-process))
  (bt:join-thread (reader-thread client-process)))

(defun locked (client-process)
  "Is client process locked?"
  (probe-file (lock-file client-process)))

(defun idle (client-process)
  "Is client process idle?"
  (not (probe-file (lock-file client-process))))

(defun lock (client-process)
  "Lock client process."
  (let ((lock-file (lock-file client-process)))
    (with-open-file (stream lock-file :direction :output :if-does-not-exist :create :if-exists :supersede)
      (declare (ignore stream)))))

(defun unlock (client-process)
  "Unlock client process."
  (when (probe-file (lock-file client-process))
    (delete-file (lock-file client-process))))

(defun spawn-client-process (&key (id (intern (symbol-name (gensym "CLIENT-PROCESS-"))))
                                  (external-lisp *external-lisp*)
                                  asdf-systems
                                  lock-file
                                  output-file)
  "Starts up a client-process and intialises it."
  ;; Start the client process.
  (let ((client-process (make-instance 'client-process
                                       :id id
                                       :lisp external-lisp
                                       :lock-file lock-file
                                       :output-file output-file)))
    ;; check it
    (unless (uiop/launch-program:process-alive-p (process client-process))
      (error "Could not start client process '~a ~{~a~^ ~} :input :stream :output :stream'"
             (cdr (assoc :command (external-lisp external-lisp))) (cdr (assoc :arguments (external-lisp external-lisp))))
      (terminate client-process))
    ;; initialise it
    (lock client-process)
    (write-to-client-process client-process "(ql:quickload :parallel-lg)")
    (write-to-client-process client-process "(in-package :parallel-lg)")
    ;; If ever, the web-interface system is loaded, do not start it automatically
    (write-to-client-process client-process "(setf cl-user::*automatically-start-web-interface* nil)")
    (write-to-client-process client-process (format nil "(load-asdf-systems '(~{~s~^ ~}))" asdf-systems))
    ;;;;;
    client-process))

(defun wait-for-idle-client-process (client-processes)
  "Returns an idle process, potentially waiting until one is available."
  (loop with idle-process = nil
        while (not idle-process)
        do (loop for process in client-processes
                 when (idle process)
                 do
                 (setf idle-process process)
                 (return)
                 else do (sleep 0.01))
        finally (return idle-process)))

(defun wait-until-all-process-idle (client-processes)
  "Returns t once all processes are idle."
  (loop with all-process-idle = nil
        while (not all-process-idle)
        when (loop for  process in client-processes
                   always (idle process))
        do
        (return t)
        else do (sleep 0.1)))
