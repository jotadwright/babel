(in-package :printer-interface)

(defun print-document (file &key
                            (printer nil printer-supplied-p) ;; printername as string
                            (copies nil copies-supplied-p) ;; number of copies to print as integer
                            (server-and-port nil server-and-port-supplied-p)
                            (job-name nil job-name-supplied-p)
                            (username nil user-name-supplied-p)
                            (encrypted nil)
                            (no-banner nil)
                            (no-filtering nil)
                            (email-when-job-done nil)
                            (add-header nil)
                            (hold-job nil)
                            (delete-files-after-printing nil)
                            (collate nil)
                            (fit-to-page nil)
                            (job-hold-until nil)
                            (job-priority nil) ;; 0-100
                            (job-sheets nil)
                            (size nil) ;; "a4", "letter", "legal", ...
                            (mirror nil)
                            (pages-per-sheet nil) ;; 2,4,8 or 16
                            (pages-per-sheet-layout nil pages-per-sheet-layout-supplied-p) ;; "btlr",  "btrl",  "lrbt",  "lrtb",
                            ;;"rlbt", "rltb", "tblr", or "tbrl"
                            (orientation nil) ;; 4 (landscape 90 counterclockwise), 5 (landscape 90 clockwise ) or 6 (portrait 180))
                            (reverse-order nil) 
                            (page-border nil) ;; "double", "double-thick", "single", or "single-thick"
                            (page-ranges nil) ;; "1,3-5,16"
                            (sides nil) ;; "one-sided", "two-sided-long-edge", or "two-sided-short-edge"
                            (other-options nil)) ;; list of other options in lpr format e.g. '("-o option=value" "-o option2=value")
  "Prints a document. For more information on the options, type 'man lpr' in a terminal."
  ;; Check file and printer
  (unless (probe-file file)
    (error "The file '~a' does not exist." file))
  ;; Compose LPR Command
  (let ((lpr-command '("lpr")))
    (when printer-supplied-p
      (if (find printer (available-printers) :key #'first :test #'string=)
        (setf lpr-command (append lpr-command (list (format nil "-P ~a" printer))))
        (error "The printer '~a' could not be found. Available printers are: ~{~s, ~^ ~}" printer (mapcar #'first (available-printers)))))
    (when copies-supplied-p
      (setf lpr-command (append lpr-command (list (format nil "-# ~a" copies)))))
    (when server-and-port-supplied-p
      (setf lpr-command (append lpr-command (list (format nil "-H ~a" server-and-port)))))
    (when job-name-supplied-p
      (setf lpr-command (append lpr-command (list (format nil "-T ~a" job-name)))))
    (when user-name-supplied-p
      (setf lpr-command (append lpr-command (list (format nil "-U ~a" username)))))
    (when encrypted
      (setf lpr-command (append lpr-command (list (format nil "-E")))))
    (when no-banner
      (setf lpr-command (append lpr-command (list (format nil "-h")))))
    (when no-filtering
      (setf lpr-command (append lpr-command (list (format nil "-l")))))
    (when email-when-job-done
      (setf lpr-command (append lpr-command (list (format nil "-m")))))
    (when add-header
      (setf lpr-command (append lpr-command (list (format nil "-p")))))
    (when hold-job
      (setf lpr-command (append lpr-command (list (format nil "-q")))))
    (when delete-files-after-printing
      (setf lpr-command (append lpr-command (list (format nil "-r")))))
    (when collate
      (setf lpr-command (append lpr-command (list (format nil "-o collate=true")))))
    (when fit-to-page
      (setf lpr-command (append lpr-command (list (format nil "-o fit-to-page")))))
    (when job-hold-until
      (setf lpr-command (append lpr-command (list (format nil "-o job-hold-until=~a" job-hold-until)))))
    (when job-priority
      (setf lpr-command (append lpr-command (list (format nil "-o job-priority=~a" job-priority)))))
    (when job-sheets
      (setf lpr-command (append lpr-command (list (format nil "-o job-sheets=~a" job-sheets)))))
    (when size
      (setf lpr-command (append lpr-command (list (format nil "-o media=~a" size)))))
    (when mirror
      (setf lpr-command (append lpr-command (list (format nil "-o mirror")))))
    (when pages-per-sheet
      (setf lpr-command (append lpr-command (list (format nil "-o number-up=~a" pages-per-sheet)))))
    (when pages-per-sheet-layout-supplied-p
      (setf lpr-command (append lpr-command (list (format nil "-o number-up-layout=~a" pages-per-sheet-layout)))))
    (when orientation
      (setf lpr-command (append lpr-command (list (format nil "-o orientation-requested=~a" orientation)))))
    (when reverse-order
      (setf lpr-command (append lpr-command (list (format nil "-o outputorder=reverse")))))
    (when page-border
      (setf lpr-command (append lpr-command (list (format nil "-o page-border=~a" page-border)))))
    (when page-ranges
      (setf lpr-command (append lpr-command (list (format nil "-o page-ranges=~a" page-ranges)))))
    (when sides
      (setf lpr-command (append lpr-command (list (format nil "-o sides=~a" sides)))))
    (when other-options
      (setf lpr-command (append lpr-command other-options)))
    ;; append file
    (setf lpr-command (append lpr-command (list file)))
    ;; format command
    (setf lpr-command (format nil "~{~a ~}" lpr-command))
    ;; Submit the job to the print queue
    (run-program lpr-command)
    ))

;; (print-document "/Users/paul/Desktop/test.pdf"  :printer "ai14_vub_ac_be" :size "A4" )

(defun available-printers ()
  "Returns an a-list containing the names of the available printers and their status."
  (let ((available-printers (split-sequence #\Newline
                                            (run-program '("lpstat" "-p")
                                                         :output '(:string :stripped t)))))
    (loop for printer in available-printers
          for printer-info = (split-sequence #\Space printer)
          collect (cons (second printer-info) (first (split-sequence #\. (fourth printer-info)))))))

;; (available-printers)

(defun default-printer ()
  "Returns the name of the default printer on the system."
  (fourth (split-sequence #\Space (run-program
                                   '("lpstat" "-d")
                                   :output '(:string :stripped t)))))

;; (default-printer)
