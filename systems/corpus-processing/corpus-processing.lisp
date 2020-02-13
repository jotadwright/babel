
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
                            (tmpdir (babel-pathname :directory '(".tmp")))
                            (number-of-processes
                             *max-nr-parallel-processes*)
                            (number-of-lines-per-process 2000)
                            (write-empty-lines-p nil)
                            (inferior-lisp *inferior-lisp*)
                            (silent-processes nil))
  "Applies function to every line in inputfile and writes the result in outputfile.
   A higher number-of-processes results in a higher speed (if these processes are available).
   A higher number-of-lines-per-process results in a higher speed, but also in a higher
   memory use. Every process writes to a temporary file using the name argument."

  ;; Checking some stuff
  (unless (probe-file inputfile)
    (error "The file ~a does not exist" inputfile))
  (ensure-directories-exist outputfile)
  (ensure-directories-exist tmpdir)
  
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
        (tmpfile (merge-pathnames
                  (make-pathname :name (downcase (mkstr name)) :type "dat")
                  tmpdir)))
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
                                       inputfile tmpfile write-empty-lines-p inferior-lisp
                                       silent-processes)
          (incf batch-start number-of-lines-per-batch)
          (format t "~%Finished"))
        ;; process an optional extra batch
        (when (> number-of-lines-in-last-batch 0)
          (format t "~%Started extra batch...")
          (process-batch-multi-process asdf-system package function function-kwargs
                                       number-of-complete-batches batch-start number-of-processes number-of-lines-in-last-batch
                                       inputfile tmpfile write-empty-lines-p inferior-lisp silent-processes)
          (format t "~%Finished")))

      ;; Create outputfile by combining the output of the separate batches
      (format t "~%.. reading the temp files written by the client processes~%")
      (write-output tmpfile outputfile number-of-complete-batches number-of-lines-in-last-batch))

    ;; Finish
    (let ((finish-time (get-universal-time)))
      (multiple-value-bind (h m s)
          (seconds-to-hours-minutes-seconds (- finish-time start-time))
      (format t "~%~%Processing took ~a hours, ~a minutes and ~a seconds." h m s))))
  (format t "~%~%Wrote output to file ~a." outputfile)
  (format t "~%~%***************** Finished Corpus Processing *****************~%~%")) 
    


(defun process-batch-multi-process (asdf-system package function function-kwargs
                                    batch-number batch-start number-of-processes number-of-lines
                                    inputfile tmpfile write-empty-lines-p
                                    inferior-lisp silent-processes)
  "takes a batch of lines, divides them over processes, applies function on each line and writes
   the output to a single temp file per batch."
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
    (let* ((temp-file-name (make-pathname :directory (pathname-directory tmpfile)
                                          :name (format nil "~a-batch-~a"
                                                        (pathname-name tmpfile)
                                                        batch-number)
                                          :type (pathname-type tmpfile)))
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

      ;; clear the temp file
      (with-open-file (stream temp-file-name :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (declare (ignore stream)))
    
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
                  (unless silent-processes
                    (format t "~%.. starting batch ~a - process ~a:" (1+ batch-number) (1+ j))
                    (format t "~%  ~(~w~)~%" (cons external-process-fn inferior-lisp)))
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
                  for (start . end) in list-of-process-batches
                  for process-number from 1
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
                                   "\" :output-file \"" temp-file-name 
                                   "\" :process-number " process-number
                                   " :silent " silent-processes
                                   ")")
                                  "(utils:quit-lisp)"
                                  "(sleep 0.1)" ;; make sure the status of the process can be changed
                                  )
                  do
                  ;; write commands to processes
                  (unless silent-processes
                    (format t "~%.. writing commands to process ~a:~%" process-number))
                  (loop for c in commands
                        do (unless silent-processes
                             (format t "  ~a~%" c))
                        (write-string c output-stream)
                        (princ  #\lf output-stream))
                  (force-output t)
                  (force-output output-stream)
                  (finish-output output-stream)
                  #-lispworks
                  (close output-stream))
     
            ;; wait until the clients terminate and print the output
            (unless silent-processes
              (format t "~%.. waiting for processes finish~%~%"))
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
                        do (unread-char char input-stream)
                        (unless silent-processes
                          (format t "~%process ~d: ~a" n (read-line input-stream))))

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
                     do (sb-ext:process-close p))))))
      

  
  
;; this is the function that is run by the inferior/client lisp
(defun run-client-process (&key asdf-system package function function-kwargs
                                start end write-empty-lines-p
                                input-file output-file process-number
                                (silent nil))
  "this is the function that is run by the client lisp. It loads the
   requested asdf-system and package, applies the function to the lines
   between start and end and writes these to the output file. The process
   number is added in front in order to sort the complete output later"
  ;; set random seed
  (setf *random-state* 
        #+sbcl (sb-ext:seed-random-state t)
        #+ccl (make-random-state t)
        #+lispworks (make-random-state))

  ;; load the requested asdf system
  (ql:quickload asdf-system :verbose nil :silent silent)

  ;; set the package
  (setf *package* (find-package (read-from-string package)))

  ;; do the actual processing
  (let ((fn (symbol-function (read-from-string function)))
        (kwargs (loop for elem in (read-from-string function-kwargs)
                      for i from 0
                      if (evenp i) collect (make-kw elem)
                      else collect elem))
        (batch-input nil)
        (batch-output nil))
    ;; open the input-file and get the lines between start and end
    (with-open-file (stream input-file :direction :input)
      (setf batch-input (read-lines-by-index stream start end)))
    ;; apply the function to them and cons the process-number to it
    (setf batch-output
          (mapcar #'(lambda (line)
                      (if kwargs
                        (apply fn line kwargs)
                        (funcall fn line)))
                  batch-input))
    ;; write the result to an output file    
    (unless silent
      (format t ".. writing recorded data to ~s.~%" output-file))
    (force-output t)
    (with-open-file (stream output-file :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (unless stream (error "could not open ~s" output-file))
      (if write-empty-lines-p
        (mapcar #'(lambda (line) (format stream "(~a . \"~a\")~%" process-number line)) batch-output)
        (mapcar #'(lambda (line) (when line (format stream "(~a . \"~a\")~%" process-number line))) batch-output))
      (finish-output stream)))
      
  ;; finish
  (unless silent
    (format t ".. finished.~%"))
  (force-output t)
  (sleep 0.2))



(defun write-output (tmpfile outputfile number-of-complete-batches number-of-lines-in-last-batch)
  "Reads the output of the separate batches and combines it into the requested output file.
   Here, the content of the temp files is sorted."
  (let ((complete-batch-temp-file-names
         (loop for batch from 0 below number-of-complete-batches
               collect (make-pathname :directory (pathname-directory tmpfile)
                                      :name (format nil "~a-batch-~a"
                                                    (pathname-name tmpfile) batch)
                                      :type (pathname-type tmpfile))))
        (last-batch-temp-file-names
         (when (> number-of-lines-in-last-batch 0)
           (list (make-pathname :directory (pathname-directory tmpfile)
                                :name (format nil "~a-batch-~a"
                                              (pathname-name tmpfile)
                                              number-of-complete-batches)
                                :type (pathname-type tmpfile))))))
    (with-open-file (out outputfile :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (loop for batch-file in (append complete-batch-temp-file-names
                                      last-batch-temp-file-names)
            for batch-lines = (mapcar #'read-from-string
                                      (with-open-file (in batch-file :direction :input)
                                        (stream->list in)))
            do (loop for line in (sort batch-lines #'< :key #'car)
                     do (write-line (cdr line) out)))
      (finish-output out))))


(defun read-lines-by-index (stream start end)
  "Read the lines from position start to end from the stream"
  (loop for line = (read-line stream nil nil)
        for i from 0
        if (and (>= i start) (< i end) line) collect line into lines
        else if (= i end) return lines))
