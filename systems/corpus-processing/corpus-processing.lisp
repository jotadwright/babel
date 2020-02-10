
;; (ql:quickload :corpus-processing)

(in-package :utils)

(export '(process-corpus run-client-process))

;; if *max-nr-parallel-processes* is set to a number,
;; then a maximum of parallel processes is started
;; e.g., use this if you have large number of series to run on
;; hardware which only has few cores
(defparameter *max-nr-parallel-processes* nil) 

;; the following two defparams are used for starting a particular client/inferior lisp 
(defparameter *inferior-lisps*
  '((sbcl . ("sbcl" ("--noinform" "--disable-debugger")))
    (ccl . #+linux ("lx86cl64" ("--batch" "--quiet"))
           #+darwin ("dx86cl64" ("--batch" "--quiet"))
           #+windows ("wx86cl64" ("--batch" "--quiet")))
    (lispworks . ("lispworks" ())))
  "inferior lisps default command line options 
   format is command (arguments)")

;; default argument to run-client-process
(defvar *inferior-lisp*
  (assqv #+sbcl 'sbcl
         #+ccl 'ccl
         #+lispworks 'lispworks
         *inferior-lisps*)
  "the standard inferior lisp used in run-client-processes")



(defun process-corpus (&key asdf-system package
                            (name (make-id "corpus"))
                            function function-kwargs
                            inputfile outputfile
                            (number-of-processes
                             *max-nr-parallel-processes*)
                            (number-of-lines-per-process 2000)
                            (write-empty-lines-p nil)
                            (inferior-lisp *inferior-lisp*))
  "Applies function to every line in inputfile and writes the result in outputfile.
   A higher number-of-threads results in a higher speed (if these threads are available).
   A higher number-of-lines-per-thread results in a higher speed, but also in a higher
   memory use."
   ;; Printing some information
  (format t "~%~%****************** Started Corpus Processing ******************")
  (format t "~%ASDF System: ~a" asdf-system)
  (format t "~%Package: ~a" package)
  (format t "~%Inputfile: ~a" inputfile)
  (format t "~%Outputfile: ~a" outputfile)
  (format t "~%Applying function: ~a" function)
  (format t "~%Passing function arguments: ~a" function-kwargs)
  (format t "~%Processes: ~a" number-of-processes)
  (format t "~%Lines per process: ~a" number-of-lines-per-process)
  (format t "~%Lines per batch: ~a" (* number-of-processes number-of-lines-per-process))

  ;; count number of lines in file
  (format t "~%Total number of lines: ")
  (let ((start-time (get-universal-time))
        (number-of-lines (number-of-lines inputfile))
        (number-of-lines-per-batch (* number-of-processes number-of-lines-per-process))
        (tmpfile (babel-pathname :directory '(".tmp") :name (downcase (mkstr name))
                                 :type "dat")))
    (format t "~a" number-of-lines)

    ;; clear outputfile
    (with-open-file (stream outputfile :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (declare (ignore stream)))

    ;; Divide inputfile in batches, that are processed sequentionally (avoids reading whole corpus in memory)
    (multiple-value-bind (number-of-complete-batches number-of-lines-in-last-batch)
        (floor number-of-lines number-of-lines-per-batch)
      (format t "~%Number of batches: ~a complete + ~a lines in extra batch.~%"
              number-of-complete-batches number-of-lines-in-last-batch)
      (let ((batch-start 0))
        ;; process the complete batches
        (dotimes (n number-of-complete-batches)
          (format t "~%Starting complete batch ~a/~a..." (+ 1 n) number-of-complete-batches)
          (process-batch-multi-process asdf-system package function function-kwargs
                                       n batch-start number-of-processes number-of-lines-per-batch
                                       inputfile tmpfile write-empty-lines-p inferior-lisp)
          (incf batch-start number-of-lines-per-batch)
          (format t "~%Finished"))
        ;; process an optional extra batch
        (when (> number-of-lines-in-last-batch 0)
          (format t "~%Started extra batch...")
          (process-batch-multi-process asdf-system package function function-kwargs
                                       number-of-complete-batches batch-start number-of-processes number-of-lines-in-last-batch
                                       inputfile tmpfile write-empty-lines-p inferior-lisp)
          (format t "~%Finished")))

      ;; Create outputfile by combining the output of the separate processes
      (format t "~%.. reading the temp files written by the client processes~%")
      (write-output tmpfile outputfile number-of-complete-batches number-of-processes number-of-lines-in-last-batch))

    ;; Finish
    (let ((finish-time (get-universal-time)))
      (multiple-value-bind (h m s)
          (seconds-to-hours-minutes-seconds (- finish-time start-time))
      (format t "~%~%Processing took ~a hours, ~a minutes and ~a seconds." h m s))))
  (format t "~%~%Wrote output to file ~a." outputfile)
  (format t "~%~%***************** Finished Corpus Processing *****************~%~%"))



(defun write-output (tmpfile outputfile number-of-complete-batches number-of-processes number-of-lines-in-last-batch)
  "Reads the output of the separate processes and combines it into the requested output file"
  (let ((complete-batch-temp-file-names
         (loop for i from 0 below number-of-complete-batches
               append (loop for j from 0 below number-of-processes
                            collect (make-pathname :directory (pathname-directory tmpfile)
                                                   :name (format nil "~a-batch-~a-process-~a"
                                                                 (pathname-name tmpfile) i j)
                                                   :type (pathname-type tmpfile)))))
        (last-batch-temp-file-names
         (when (> number-of-lines-in-last-batch 0)
           (loop for j from 0 below number-of-processes
                 for pathname
                 = (make-pathname :directory (pathname-directory tmpfile)
                                  :name (format nil "~a-batch-~a-process-~a"
                                                (pathname-name tmpfile)
                                                number-of-complete-batches j)
                                  :type (pathname-type tmpfile))
                 when (probe-file pathname)
                 collect pathname))))
    (with-open-file (out outputfile :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (loop for file in (append complete-batch-temp-file-names
                                last-batch-temp-file-names)
            do (with-open-file (in file :direction :input)
                 (loop for line = (read-line in nil nil)
                       while line
                       do (write-line line out))))
      (finish-output out))))
    
    



(defun process-batch-multi-process (asdf-system package function function-kwargs
                                    batch-number batch-start number-of-processes number-of-lines
                                    inputfile tmpfile write-empty-lines-p
                                    inferior-lisp)
  "takes a batch of lines, divides them over processes, applies function on each line and writes
   the output to tmpfiles"
  (let ((list-of-process-batches nil))
    ;; Divide lines over processes
    (multiple-value-bind (lines-per-process lines-last-process)
        (floor number-of-lines number-of-processes)
      ;; Read from input
      (format t "~%      Launching ~a processes with ~a lines and 1 process with ~a lines. "
              (- number-of-processes 1) lines-per-process (+ lines-per-process lines-last-process))
      (dotimes (n (- number-of-processes 1)) ;; For each process, except last
        (pushend (cons (+ batch-start (* n lines-per-process))
                       (+ batch-start (* (1+ n) lines-per-process)))
                 list-of-process-batches))
      ;; For last process
      (pushend (cons (+ batch-start (* (- number-of-processes 1) lines-per-process))
                     (+ batch-start (* number-of-processes lines-per-process) lines-last-process))
               list-of-process-batches))

    ;; Start the processes
    (let* ((temp-file-names (loop for i from 0 below number-of-processes
                                  collect (make-pathname :directory (pathname-directory tmpfile)
                                                         :name (format nil "~a-batch-~a-process-~a"
                                                                       (pathname-name tmpfile)
                                                                       batch-number i)
                                                         :type (pathname-type tmpfile))))
           (external-process-fn #+sbcl 'sb-ext:run-program
                                #+ccl 'ccl:run-program
                                #+lispworks 'system::run-shell-command)
           (external-process-fn-options #+sbcl '(:wait nil :input :stream :output :stream :search t)
                                        #+ccl '(:wait nil :input :stream :output :stream 
                                                :error :output :sharing :external)
                                        #+lispworks '(:input :stream :output :stream
                                                      :error-output :output
                                                      :wait nil :save-exit-status t))
           (continue-running t))
      (loop while continue-running
            for processes = nil
            for input-streams = nil
            for output-streams = nil
            do
            ;; collect processes and i/o streams
            (loop for j from 0 below number-of-processes
                  for process = nil
                  for input-stream = nil
                  for output-stream = nil
                  do
                  (format t "~%.. starting batch ~a - process ~a:" (1+ batch-number) (1+ j))
                  (format t "~%  ~(~w~)~%" (cons external-process-fn inferior-lisp))
                  do
                  #+:sbcl (progn
                            (setf process (apply (symbol-function external-process-fn)
                                                 (append inferior-lisp external-process-fn-options)))
                            (setf output-stream (sb-ext:process-input process))
                            (setf input-stream (sb-ext:process-output process)))
                  #+:ccl (progn
                           (setf process (apply (symbol-function external-process-fn)
                                                (append inferior-lisp external-process-fn-options)))
                           (setf output-stream (ccl:external-process-input-stream  process))
                           (setf input-stream (ccl:external-process-output-stream process)))
                  #+:lispworks (multiple-value-bind (in/out/err temp pid)
                                   (apply (symbol-function external-process-fn)
                                          (format nil "~a ~{~a~^ ~}" (first inferior-lisp) (second inferior-lisp))
                                          external-process-fn-options)
                                 (declare (ignore temp))
                                 (setf process pid)
                                 (setf output-stream in/out/err)
                                 (setf input-stream in/out/err))
                  do (pushend process processes)
                  (pushend output-stream output-streams)
                  (pushend input-stream input-streams))
         
            ;; test process pipes
            (loop for input-stream in input-streams
                  for c = (read-char input-stream nil)
                  if c  do (unread-char c input-stream)
                  else do
                  (error "could not start lisp process~% ~a"
                         (format nil "~(~w~)"
                                 (cons external-process-fn 
                                       (append inferior-lisp external-process-fn-options)))))
            
            ;; piping the commands
            (loop for output-stream in output-streams
                  for file-name in temp-file-names
                  for (start . end) in list-of-process-batches
                  for i from 1
                  for commands = (list
                                  "(setf cl-user::*automatically-start-web-interface* nil)"
                                  "(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)"
                                  "(ql:quickload :corpus-processing :silent t)"
                                  (mkstr 
                                   "(utils::run-client-process"
                                   " :asdf-system :" asdf-system 
                                   " :package \"" package
                                   "\" :function \"" function
                                   "\" :function-kwargs \"" function-kwargs
                                   "\" :start " start " :end " end
                                   " :write-empty-lines-p " write-empty-lines-p
                                   " :input-file \"" inputfile
                                   "\" :output-file \"" file-name 
                                   "\""
                                   ")")
                                  "(utils:quit-lisp)"
                                  "(sleep 0.1)" ;; make sure the status of the process can be changed
                                  )
                  do
                  ;; write commands to processes
                  (format t "~%.. writing commands to process ~a:~%" i)
                  (loop for c in commands
                        do
                        (format t "  ~a~%" c)
                        (write-string c output-stream)
                        (princ  #\lf output-stream))
                  (force-output t)
                  (force-output output-stream)
                  (finish-output output-stream)
                  #-lispworks
                  (close output-stream))
     
            ;; wait until the clients terminate and print the output
            (format t "~%.. waiting for processes finish~%~%")
            (loop with running-processes = (copy-list processes)
                  with running-input-streams = (copy-list input-streams)
                  with running-process-numbers = (loop for i from 1 to (length running-processes)
                                                       collect i)
                  while running-processes
                  do
                  ;; wait
                  (sleep 0.01)
                  ;; output stdout/stderr of processes
                  (loop for input-stream in running-input-streams
                        for n in running-process-numbers
                        for char = nil
                        while (setf char (read-char-no-hang input-stream nil))
                        do
                        (unread-char char input-stream)
                        (format t "~%process ~d: ~a" n (read-line input-stream)))

                  ;; check processes if still running
                  (loop for p in running-processes
                        when 
                        #+sbcl(not (equal (sb-ext:process-status p) :running))
                        #+ccl(not (equal (ccl:external-process-status p) :running))
                        #+lispworks7 (loop for stream in output-streams
                                           when (eql (system::pipe-stream-pid stream) p)
                                           return (sys:pipe-exit-status stream :wait nil))
                        collect p into processes-finished
                        finally
                        (loop for p in processes-finished
                              for i = (position p running-processes)
                              do
                              (setf running-processes (remove p running-processes))
                              (setf running-input-streams (remove (nth i running-input-streams)
                                                                  running-input-streams))
                              (setf running-process-numbers
                                    (remove (nth i running-process-numbers)
                                            running-process-numbers))
                              (setf continue-running running-processes))))
            #+sbcl(loop for p in processes
                     do (sb-ext:process-close p)))
      (format t "~% Batch finished~%~%"))))
      

  
  
;; this is the function that is run by the inferior/client lisp
(defun run-client-process (&key asdf-system package function function-kwargs
                                start end write-empty-lines-p
                                input-file output-file)
  ;; set random seed
  (setf *random-state* 
        #+sbcl (sb-ext:seed-random-state t)
        #+ccl (make-random-state t)
        #+lispworks (make-random-state))

  ;; load the requested asdf system
  (ql:quickload asdf-system :verbose nil)

  ;; set the package
  (setf *package* (find-package (read-from-string package)))

  ;; do the actual processing
  (let ((process-batch nil)
        (fn (symbol-function (read-from-string function)))
        (kwargs (loop for elem in (read-from-string function-kwargs)
                      for i from 0
                      if (evenp i) collect (make-kw elem)
                      else collect elem))
        (batch-output nil))
    ;; open the input-file and get the lines between start and end
    (with-open-file (stream input-file :direction :input)
      (setf process-batch (read-lines-by-index stream start end)))
    ;; apply the function to them
    (setf batch-output
          (mapcar #'(lambda (line)
                      (if kwargs
                        (apply fn line kwargs)
                        (funcall fn line)))
                  process-batch))
    ;; write the result to an output file    
    (format t ".. writing recorded data to ~s.~%" output-file)
    (force-output t)
    (ensure-directories-exist output-file)
    (with-open-file (stream output-file :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (unless stream (error "could not open ~s" output-file))
      (if write-empty-lines-p
        (mapcar #'(lambda (line) (format stream "~a~%" line)) batch-output)
        (mapcar #'(lambda (line) (when line (format stream "~a~%" line))) batch-output))
      (finish-output stream)))
      
  ;; finish
  (format t ".. finished.~%")
  (force-output t)
  (sleep 0.2))


(defun read-lines-by-index (stream start end)
  (loop for line = (read-line stream nil nil)
        for i from 0
        if (and (>= i start) (< i end) line) collect line into lines
        else if (= i end) return lines))
