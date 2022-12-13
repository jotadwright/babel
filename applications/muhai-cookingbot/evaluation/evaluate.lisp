(in-package :muhai-cookingbot)

(defun evaluate (input output &optional (show-output nil) &rest metrics)
  "Entry function for solution evaluation.
   - input should contain the path to a .solution file
   - ouput should contain the path to a .csv file to which the results will be written
   - show-output is a boolean specifying whether the browser should be opened to visualize the simulation process
   - metrics can be any of all of the following values to indicate which evaluation metrics should be used: smatch-score, subgoals-ratio, dish-score, execution-time. Defaults to all metrics."
  (let ((metrics (if metrics metrics *metrics*)))
    (when show-output
      ; trace IRL execution
      (activate-monitor trace-irl)
      (clear-output))

    ; evaluate the input
    (let ((solutions (evaluate-solutions input metrics)))
      ; write away the solutions to a csv file
      (write-solutions-to-csv solutions output metrics)
      (print-solutions solutions metrics))

    (when show-output
    ; open the browser to show the output
      (print "The simulation process can be investigated by opening http://localhost:8000 in your browser. We recommend using Chrome.")
      (open-browser "http://localhost:8000")))) ; TODO RD: if we use lispworks executable we can simply use: (sys:open-url "http://localhost:8000")