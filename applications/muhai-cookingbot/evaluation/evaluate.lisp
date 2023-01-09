(in-package :muhai-cookingbot)

(defvar *port* 8000)

(defun internal-evaluate (input output &optional (show-output nil) &rest metrics)
  "Entry function for solution evaluation.
   - input should contain the path to a .solution file
   - ouput should contain the path to a .csv file to which the results will be written
   - show-output is a boolean specifying whether the browser should be opened to visualize the simulation process
   - metrics can be any of all of the following values to indicate which evaluation metrics should be used: smatch-score, subgoals-ratio, dish-score, execution-time. Defaults to all metrics."
  (let ((metrics (if metrics metrics *metrics*)))
    (when show-output
      ; trace IRL execution
      (wi::start-web-interface :port 8000 :stream nil)
      (activate-monitor trace-irl)
      (clear-output))

    ; evaluate the input
    (unless (find 'none metrics)
      (let ((solutions (evaluate-solutions input metrics)))
      ; write away the solutions to a csv file
        (write-solutions-to-csv solutions output metrics)
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
  (let ((input (second sys:*line-arguments-list*))
        (output (third sys:*line-arguments-list*))
        (show-output (string-equal (fourth sys:*line-arguments-list*) "t"))
        (metrics (mapcar  #'read-from-string (nthcdr 4 sys:*line-arguments-list*))))

     (cond ((< 3 (length sys:*line-arguments-list*))
            (apply #'internal-evaluate `(,input ,output ,show-output ,@metrics)))
          ;  (when (hcl:delivered-image-p) (lw:quit)))
           (t
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
  (let ((selected-file (capi:prompt-for-directory "please select a directory")))
    (when selected-file
      (capi:apply-in-pane-process
       (output-viewer choice) #'(setf capi:display-pane-text)
       (list (namestring selected-file)) (output-viewer choice))
      (activate-start-button choice))))

(defun start-evaluation (item choice)
  (let ((input (first (capi:display-pane-text (input-viewer choice))))
        (output (concatenate 'string (first (capi:display-pane-text (output-viewer choice)))
                             (capi:text-input-pane-text (output-filename choice))))
        (show-output (if (capi:choice-selected-items (web-check choice)) t nil))
        (metrics '()))

    (when (find "Smatch Score" (capi:choice-selected-items (metric-checks choice)) :test #'string-equal)
      (push 'smatch-score metrics))
    (when (find "Goal-Condition Success" (capi:choice-selected-items (metric-checks choice)) :test #'string-equal)
      (push 'goal-condition-success  metrics))
    (when (find "Dish Approximation Score" (capi:choice-selected-items (metric-checks choice)) :test #'string-equal)
      (push 'dish-approximation-score metrics))
    (when (find "Execution Time" (capi:choice-selected-items (metric-checks choice)) :test #'string-equal)
      (push 'execution-time metrics))

    (capi:apply-in-pane-process
     choice #'(setf capi:top-level-interface-display-state) :hidden choice)

    (apply #'internal-evaluate `(,input ,output ,show-output ,@metrics))))
    ;(when (hcl:delivered-image-p) (lw:quit))))

(defun activate-start-button (choice)
  (when (and (listp (capi:display-pane-text (input-viewer choice)))
             (listp (capi:display-pane-text (output-viewer choice))))
    (capi:apply-in-pane-process
     (start-button choice) #'(setf capi:button-enabled) t (start-button choice))))
                    
(capi:define-interface init-panel ()
  ()
  (:panes
   (input-button capi:push-button
                 :text "Select Solution File"
                 :min-width '(:character 30)
                 :selection-callback 'select-input)
   (input-viewer capi:display-pane
                 :text "No file selected."
                 :min-width '(:character 50)
                 :min-height '(:character 1)
                 :max-height '(:character 1)
                 :reader input-viewer)
   (output-button capi:push-button
                  :text "Select Results Directory"
                  :min-width '(:character 30)
                  :selection-callback 'select-output)
   (output-viewer capi:display-pane
                  :text "No directory selected."
                  :visible-min-width '(:character 50)
                  :visible-min-height '(:character 1)
                  :visible-max-height '(:character 1)
                  :reader output-viewer)
   (output-filename capi:text-input-pane
                    :text "results.csv"
                    :max-width '(:character 50)
                    :min-height '(:character 1)
                    :reader output-filename)
   (web-check capi:check-button-panel
              :items '("Visualize Simulation Process")
              :reader web-check)
   (metric-checks capi:check-button-panel
                  :items '("Smatch Score" "Goal-Condition Success" "Dish Approximation Score" "Execution Time")
                  :layout-class 'capi:column-layout
                  :reader metric-checks)
   (start-button capi:push-button
                 :text "Start Evaluation"
                 :max-width nil
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
      " " start-button :right-extend :right-extend " ")
      :x-gap 10
      :y-gap 20
      :columns 5))
   (:default-initargs
    :title "Evaluator Settings"
    :auto-menus nil))