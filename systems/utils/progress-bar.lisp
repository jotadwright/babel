(in-package :utils)

;; This file contains the macro 'with-progress-bar'
;; that can be wrapped around some loop, dolist, etc
;; and will print a progress bar to the output stream.
;; For this progress bar, the number of iterations in
;; the loop has to be known beforehand. So it is not
;; applicable in all situations. At every loop, the
;; 'update' function has to be called on the progress
;; bar.

;; The macro has the following arguments:
;;   bar: a local variable that will be used to call 'update'
;;   steps: the total number of steps in the loop
;;   description: the description to print before the bar
;;   format-args: the description can be a format string and
;;                will be filled with these args
;;   :width: a keyword argument to control the width of the bar
;;           default = 100
;;   :show-elapsed-time: a keyword argument to control whether
;;                       or not the elapsed time should be
;;                       displayed at every bar update + at the
;;                       end of processing

(export '(with-progress-bar update))

(defmacro with-progress-bar ((bar steps (description &rest format-args)
                                  &key (width 100) (show-elapsed-time t)
                                  &allow-other-keys)
                             &body body)
  `(let ((,bar (make-bar ,width ,steps ,show-elapsed-time)))
     (format t "~%")
     (format t ,description ,@format-args)
     (format t "~%")
     (draw ,bar)
     ,@body
     (when ,show-elapsed-time
       (multiple-value-bind (h m s) (elapsed-time ,bar)
         (format t "~%Processing took ~a hours, ~a minutes and ~a seconds."
                 h m s)))))




;; #########################################################

(defclass progress-bar ()
  ((bar-width :type number :accessor bar-width
              :initarg :bar-width :initform 100
              :documentation "The width of the progress bar on the screen")
   (loop-steps :type number :accessor loop-steps
               :initarg :loop-steps
               :documentation "The number of steps in the loop")
   (bar-progress :type number :accessor bar-progress
                 :initarg :bar-progress :initform 0
                 :documentation "The progress of the bar, between 0 and bar-width")
   (loop-progress :type number :accessor loop-progress
                  :initarg :loop-progress :initform 0
                  :documentation "The progress of the loop, between 0 and loop-steps")
   (bar-loop-ratio :type number :accessor bar-loop-ratio
                   :initarg :bar-loop-ratio
                   :documentation "How many loop steps trigger a bar step")
   (start-time :accessor start-time :initarg :start-time
               :initform (get-universal-time)
               :documentation "Unix time at which the loop started")
   (show-time-p :accessor show-time-p :initarg :show-time-p
                :initform t :documentation "Show the elapsed time at every bar update?")))

(defun make-bar (bar-width loop-steps show-time-p)
  (make-instance 'progress-bar
                 :bar-width bar-width
                 :loop-steps loop-steps
                 :bar-loop-ratio (/ loop-steps bar-width)
                 :show-time-p show-time-p))

(defmethod draw ((bar progress-bar))
  (format t "[")
  (loop repeat (bar-progress bar) do (format t "#"))
  (loop repeat (- (bar-width bar) (bar-progress bar))
        do (format t "."))
  (format t "] ~a% [~a/~a"
          (round
           (* (float
               (/ (loop-progress bar)
                  (loop-steps bar)))
              100))
          (loop-progress bar)
          (loop-steps bar))
  (if (show-time-p bar)
    (multiple-value-bind (h m s) (elapsed-time bar)
      (format t " - ~ah ~am ~as]" h m s))
    (format t "]"))
  (format t "~%"))

(defmethod update ((bar progress-bar))
  (incf (loop-progress bar))
  (when (>= (loop-progress bar)
            (* (1+ (bar-progress bar))
               (bar-loop-ratio bar)))
    (incf (bar-progress bar)
          (ceiling
           (/ 1 (bar-loop-ratio bar))))
    (draw bar)))

(defmethod elapsed-time ((bar progress-bar))
  (let ((now (get-universal-time)))
    (multiple-value-bind (h m s)
        (seconds-to-hours-minutes-seconds (- now (start-time bar)))
      (values h m s))))        
                 




