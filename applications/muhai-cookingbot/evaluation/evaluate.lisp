(in-package :muhai-cookingbot)

(defvar *port* 8000)

(defun internal-evaluate (input output &key show-output lib-dir metrics)
  "Entry function for solution evaluation.
   - input should contain the path to a .solution file
   - ouput should contain the path to a .csv file to which the results will be written
   - show-output is a boolean specifying whether the browser should be opened to visualize the simulation process
   - metrics can be any of all of the following values to indicate which evaluation metrics should be used: smatch-score, subgoals-ratio, dish-score, execution-time. Defaults to all metrics."
  (let ((metrics (if metrics metrics (remove 'smatch-score *metrics*)))) ; default is everything except the smatch-score
    (when show-output
      ; trace IRL execution
      (wi::start-web-interface :port 8000 :stream nil)
      (activate-monitor trace-irl)
      (clear-output))

    ; evaluate the input
    (unless (and (find 'none metrics) (not show-output))
      (let ((solutions (evaluate-solutions input metrics *simulation-environments* lib-dir)))
      ; write away the solutions to a csv file
        (unless (find 'none metrics)
          (write-solutions-to-csv solutions output metrics))
        (print-solutions solutions metrics)))

    (cond (show-output
    ; open the browser to show the output
           (print (format nil "The simulation process has finished. It can be investigated by opening http://localhost:~d in your browser. We recommend using Chrome." *port*))
           (sys:open-url (format nil "http://localhost:~d" *port*))) ; (open-browser (format nil "http://localhost:~d" *port*)) can be used for non-LispWorks support
          (t
           (print "The simulation process has finished.")))))

(defun evaluate ()
  ; perform evaluation inside the muhai-cookingbot package
  (in-package :muhai-cookingbot)

  ; set some path variables needed for web visualization
  (setf (lispworks:environment-variable "PATH") 
        (concatenate 'string
                     #+macosx "/usr/local/bin:/sw/bin:/opt/homebrew/bin:"
                     #+mswindows"C:\\Program Files\\gnuplot\\bin;C:\\Program Files\\Graphviz2.38\\bin;"
                     (lispworks:environment-variable "PATH")))

   ; use 128K instead of the default 16K stack during evaluation
  (setf sys:*sg-default-size* 128000)

  ; gather the command line arguments and use them for the internal-evaluate call
  (let ((input '())
        (output '())
        (show-output '())
        (metrics '())
        (lib-dir '())
        (line-args (rest sys:*line-arguments-list*)))

    (loop while line-args
          do
            (cond ((string-equal (first line-args) "-input")
                   (setf input (second line-args)))
                  ((string-equal (first line-args) "-output")
                   (setf output (second line-args)))
                  ((string-equal (first line-args) "-show-output")
                   (setf show-output (or (string-equal (second line-args) "t")
                                         (string-equal (second line-args) "true"))))
                  ((string-equal (first line-args) "-lib-dir")
                   (setf lib-dir (second line-args)))
                  ((string-equal (first line-args) "-metrics")
                   (let ((metrics-args (rest line-args)))
                     (loop while metrics-args
                           do
                             (if (not (find (first metrics-args) (list "-input" "-output" "-show-output" "-lib-dir" "-metrics") :test #'string-equal))
                               (progn
                                 (push (first metrics-args) metrics)
                                 (setf metrics-args (rest metrics-args)))
                               (setf metrics-args nil)))
                     (setf metrics (mapcar  #'read-from-string metrics)))))
            (setf line-args (rest line-args)))

    (cond ((and input output)
           (when (and (not lib-dir)
                      (find 'smatch-score metrics))
             (error "No -lib-dir parameter provided while requesting Smatch Score computation."))
           (internal-evaluate input output :show-output show-output :metrics metrics :lib-dir lib-dir))
          ;  (when (hcl:delivered-image-p) (lw:quit)))
          (t
           (unless (<= (length sys:*line-arguments-list*) 1)
             (print "No -input and -output parameters provided. Opening GUI for parameter specification."))
           (let ((evaluation-init (make-instance 'init-panel)))
             (capi:display evaluation-init))))))

;; CAPI panels
(defun select-input (item choice)
  (let ((selected-file (capi:prompt-for-file "please select a file")))
    (when selected-file
      (capi:apply-in-pane-process
       (input-viewer choice) #'(setf capi:display-pane-text)
       (list (namestring selected-file)) (input-viewer choice))
      (activate-start-button choice))))

(defun select-output (item choice)
  (let ((selected-dir (capi:prompt-for-directory "please select a directory")))
    (when selected-dir
      (capi:apply-in-pane-process
       (output-viewer choice) #'(setf capi:display-pane-text)
       (list (namestring selected-dir)) (output-viewer choice))
      (activate-start-button choice))))

(defun select-lib (item choice)
  (let ((selected-dir (capi:prompt-for-directory "please select a directory")))
    (when selected-dir
      (capi:apply-in-pane-process
       (lib-viewer choice) #'(setf capi:display-pane-text)
       (list (namestring selected-dir)) (lib-viewer choice))
      (activate-start-button choice))))

(defun activate-start-button (choice)
  (if (and (listp (capi:display-pane-text (input-viewer choice)))
           (listp (capi:display-pane-text (output-viewer choice)))
           (or (listp (capi:display-pane-text (lib-viewer choice)))
               (not (find "Smatch Score" (capi:choice-selected-items (metric-checks choice)) :test #'string-equal))))
    (capi:apply-in-pane-process
     (start-button choice) #'(setf capi:button-enabled) t (start-button choice))
    (capi:apply-in-pane-process
     (start-button choice) #'(setf capi:button-enabled) nil (start-button choice))))

(defun metric-selection-callback (item choice)
  (when (string-equal item "Smatch Score")
    (capi:apply-in-pane-process
     (lib-button choice) #'(setf capi:button-enabled) t (lib-button choice))
    (capi:apply-in-pane-process
     (lib-viewer choice) #'(setf capi:simple-pane-enabled) t (lib-viewer choice))
    (activate-start-button choice)))

(defun metric-retraction-callback (item choice)
  (when (string-equal item "Smatch Score")
    (capi:apply-in-pane-process
     (lib-button choice) #'(setf capi:button-enabled) nil (lib-button choice))
    (capi:apply-in-pane-process
     (lib-viewer choice) #'(setf capi:simple-pane-enabled) nil (lib-viewer choice))
    (activate-start-button choice)))

(defun start-evaluation (item choice)
  (let ((input (first (capi:display-pane-text (input-viewer choice))))
        (output (concatenate 'string (first (capi:display-pane-text (output-viewer choice)))
                             (capi:text-input-pane-text (output-filename choice))))
        (show-output (if (capi:choice-selected-items (web-check choice)) t nil))
        (metrics '())
        (lib-dir (if (listp (capi:display-pane-text (lib-viewer choice)))
                   (first (capi:display-pane-text (lib-viewer choice)))
                   nil)))

    (when (find "Smatch Score" (capi:choice-selected-items (metric-checks choice)) :test #'string-equal)
      (push 'smatch-score metrics))
    (when (find "Goal-Condition Success" (capi:choice-selected-items (metric-checks choice)) :test #'string-equal)
      (push 'goal-condition-success  metrics))
    (when (find "Dish Approximation Score" (capi:choice-selected-items (metric-checks choice)) :test #'string-equal)
      (push 'dish-approximation-score metrics))
    (when (find "Execution Time" (capi:choice-selected-items (metric-checks choice)) :test #'string-equal)
      (push 'execution-time metrics))
    (when (not metrics)
      (push 'none metrics))

    (capi:apply-in-pane-process
     choice #'(setf capi:top-level-interface-display-state) :hidden choice)

    (internal-evaluate input output :show-output show-output :metrics metrics :lib-dir lib-dir)))
    ;(when (hcl:delivered-image-p) (lw:quit)))) ; we can close the console line if we want
                    
(capi:define-interface init-panel ()
  ()
  (:panes
   (input-button capi:push-button
                 :text "Select Solution File"
                 :visible-min-width '(:character 30)
                 :visible-max-width t
                 :selection-callback 'select-input)
   (input-viewer capi:display-pane
                 :text "No file selected."
                 :visible-min-width '(:character 50)
                 :visible-max-width nil
                 :visible-min-height '(:character 1)
                 :visible-max-height t
                 :reader input-viewer)
   (output-button capi:push-button
                  :text "Select Results Directory"
                  :visible-min-width '(:character 30)
                  :visible-max-width t
                  :selection-callback 'select-output)
   (output-viewer capi:display-pane
                  :text "No directory selected."
                  :visible-min-width '(:character 50)
                  :visible-max-width nil
                  :visible-min-height '(:character 1)
                  :visible-max-height '(:character 1)
                  :reader output-viewer)
   (output-filename capi:text-input-pane
                    :text "results.csv"
                    :visible-min-width '(:character 50)
                    :visible-max-width nil
                    :visible-min-height '(:character 1)
                    :visible-max-height t
                    :reader output-filename)
   (web-check capi:check-button-panel
              :items '("Visualize Simulation Process")
              :reader web-check)
   (metric-checks capi:check-button-panel
                  :items '("Smatch Score" "Goal-Condition Success" "Dish Approximation Score" "Execution Time")
                  :selection-callback 'metric-selection-callback
                  :retract-callback   'metric-retraction-callback
                  :layout-class 'capi:column-layout
                  :reader metric-checks)
   (lib-button capi:push-button
               :text "Select Smatch Directory"
                 :visible-min-width '(:character 30)
                 :visible-max-width t
                 :enabled nil
                 :selection-callback 'select-lib
                 :reader lib-button)
   (lib-viewer capi:display-pane
                 :text "No library directory selected."
                 :visible-min-width '(:character 50)
                 :visible-max-width nil
                 :visible-min-height '(:character 1)
                 :visible-max-height t
                 :enabled nil
                 :reader lib-viewer)
   (start-button capi:push-button
                 :text "Start Evaluation"
                 :visible-max-width nil
                 :enabled nil
                 :reader start-button
                 :selection-callback 'start-evaluation))
  (:layouts
   (main-layout
    capi:grid-layout
    '(" " "Input:"  input-button input-viewer " "
      " " "Output:" output-button output-viewer " "
      " " "" "Results Filename:" output-filename " "
      " " "Web Interface:" web-check :right-extend " "
      " " "Evaluation Metrics:" metric-checks :right-extend " "
      " " "" lib-button lib-viewer " "
      " " start-button :right-extend :right-extend " ")
      :x-gap 10
      :y-gap 20
      :columns 5))
   (:default-initargs
    :title "Evaluator Settings"
    :auto-menus nil))

; Test the GUI:
;(evaluate)