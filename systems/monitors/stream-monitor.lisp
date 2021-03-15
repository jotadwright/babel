;; Copyright 2019 AI Lab, Vrije Universiteit Brussel - Sony CSL Paris

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================
;;;;
;;;; File: stream-monitor.lisp
;;;;
;;;; A simple monitor class for writing values to streams. This monitor
;;;; writes the raw values provided by the monitor to an output stream.
;;;; Each interaction is split using the interaction-separator and series
;;;; are split using the series-separator. A default value can be provided
;;;; in case the current interaction cannot provide one. Additionally, the
;;;; user can choose the store the last used value and use that one in case
;;;; there is no new value provided.
;;;;

(in-package :monitors)

(export '(stream-monitor))

(defclass stream-monitor (monitor)
  ((file-name
    :documentation "The file name of the file to write"
    :initarg :file-name :reader file-name)
   (monitor-stream
    :documentation "The output stream all values will be written to"
    :accessor monitor-stream :initform nil)
   (interaction-separator
    :documentation "The character that is used to separate interactions"
    :initform #\, :type character :initarg :interaction-separator
    :reader interaction-separator)
   (series-separator
    :documentation "The charachter that is used to separate series"
    :initform #\linefeed :type character :initarg :series-separator
    :reader series-separator)
   (default-value
    :documentation "A default value that is used if no other value was passed"
    :accessor default-value :initarg :default-value :initform 0.0)
   (current-value
    :documentation "The value recorded in the current interaction"
    :accessor current-value :initarg :current-value :initform 0.0)
   (keep-previous-value
    :documentation "Choose whether or not the previous or the default value should be written
                    when no other value is provided"
    :initform nil :reader keep-previous-value))
  (:documentation 
   "Writes the raw values of the monitor directly to an output stream, so they don't have to be stored in memory.
    This is especially useful when collecting many metrics and running many interactions."))

(defmethod initialize-instance :around ((monitor stream-monitor) 
					&key id file-name
                                        &allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  ;; check the file name
  (unless file-name
    (error "Parameter :file-name not provided"))
  (unless (pathnamep file-name)
    (error ":file-name parameter ~a should be a pathname" file-name))
  (setf (error-occured-during-initialization monitor) nil)
  ;; initialise the monitor
  (call-next-method)
  ;; set the current value to the default value
  (unless (keep-previous-value monitor)
    (setf (current-value monitor) (default-value monitor)))
  ;; subscribe to events
  (subscribe-to-event id 'interaction-started)
  (subscribe-to-event id 'interaction-finished)
  (subscribe-to-event id 'series-finished)
  (subscribe-to-event id 'batch-finished)
  (subscribe-to-event id 'reset-monitors))

(defmethod handle-interaction-started-event :before ((monitor stream-monitor)
                                                     (monitor-id symbol)
						     (event (eql 'interaction-started)) 
						     (experiment t) (interaction t)
                                                     (interaction-number number))
  (declare (ignorable experiment interaction interaction-number))
  ;; opens the stream if this is not done yet
  (unless (monitor-stream monitor)
    (setf (monitor-stream monitor)
          (open (file-name monitor) :direction :output
                :if-exists :supersede
                :if-does-not-exist :create)))
  ;; set the previous value of the monitor to the default value
  (with-slots (default-value keep-previous-value current-value) monitor
    (unless keep-previous-value
      (setf current-value default-value)))
  ;; write the interaction separator to the stream,
  ;; except for the first interaction
  (unless (= 1 interaction-number)
    (format (monitor-stream monitor) "~a" (interaction-separator monitor))
    (force-output (monitor-stream monitor))))

(defmethod handle-interaction-finished-event :after ((monitor stream-monitor)
                                                     (monitor-id symbol)
						     (event (eql 'interaction-finished))
						     (experiment t) (interaction t)
                                                     (interaction-number number))
  (declare (ignorable monitor-id event experiment interaction interaction-number))
  (format (monitor-stream monitor) "~a" (current-value monitor))
  (force-output (monitor-stream monitor)))

(defmethod handle-series-finished-event :before ((monitor stream-monitor)
                                                 (monitor-id symbol)
						 (event (eql 'series-finished))
						 (series-number number))
  (declare (ignorable monitor-id event series-number))
  (format (monitor-stream monitor) "~a" (series-separator monitor))
  (force-output (monitor-stream monitor)))

(defmethod handle-batch-finished-event ((monitor stream-monitor)
                                        (monitor-id symbol)
					(event (eql 'batch-finished))
					(experiment-class string))
  (declare (ignorable event experiment-class))
  (force-output (monitor-stream monitor))
  (close (monitor-stream monitor))
  (setf (monitor-stream monitor) nil)
  (format t "~%Monitor ~a closed stream to ~a"
          monitor-id (file-name monitor)))

(defmethod handle-reset-monitors-event ((monitor stream-monitor)
                                        (monitor-id symbol)
					(event (eql 'reset-monitors)))
  (declare (ignorable monitor-id event))
  ;; reset the current value
  (unless (keep-previous-value monitor)
    (setf (current-value monitor) (default-value monitor)))
  ;; close and re-open the stream
  (when (monitor-stream monitor)
    (close (monitor-stream monitor)))
  (setf (monitor-stream monitor)
        (open (file-name monitor) :direction :output
              :if-exists :supersede
              :if-does-not-exist :create)))

(defmethod record-value ((monitor stream-monitor) (value t))
  (setf (current-value monitor) value))

(defmethod incf-value ((monitor stream-monitor) (value t))
  (incf (current-value monitor) value))

(defmethod print-object ((monitor stream-monitor) stream)
  (format stream "<stream-recorder ~a value: ~a>" (id monitor) (current-value monitor)))

