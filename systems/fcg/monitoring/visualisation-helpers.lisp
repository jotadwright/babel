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
(in-package :fcg)

(export '(add-final-transient-structure-to-wi
          add-cxn-to-wi
          final-transient-structure
          get-meaning-network))

(defun add-final-transient-structure-to-wi (function input  &optional (cxn-inventory *fcg-constructions*))
  "Adds the final transient structure to the web interface.
   Use: (add-final-transient-structure-to-wi 'comprehend \"the cat\")"
  (add-element (make-html-fcg-light (final-transient-structure function input cxn-inventory)
                                    :feature-types (feature-types cxn-inventory)
                                    :configuration (visualization-configuration cxn-inventory))))

(defun final-transient-structure (function input &optional (cxn-inventory *fcg-constructions*))
  "Returns the final transient structure.
   Use: (final-transient-structure 'comprehend \"the cat\")"
  (multiple-value-bind (solution cipn)
      (with-disabled-monitor-notifications
        (funcall function input :cxn-inventory cxn-inventory))
    (declare (ignore solution))
    (car-resulting-cfs (cipn-car cipn))))

(defun get-meaning-network (utterance &optional (cxn-inventory *fcg-constructions*))
  "Comprehends utterance using cxn-inventory, writes the meaning network to a pdf file, and opens it."
  (let ((meaning (comprehend utterance :cxn-inventory cxn-inventory :silent t)))
    (s-dot->image (wi::predicate-network->s-dot (shuffle meaning)) :format "pdf" :open t)))

(defun add-cxn-to-wi (cxn &optional (cxn-inventory *fcg-constructions*))
  "Adds cxn to the web interface."
  (add-element (make-html (find-cxn cxn cxn-inventory :key 'name :test 'string=))))

(defun get-constructional-dependencies (cip-node &key (cxn-inventory *fcg-constructions*)
                                                 (labeled-paths 'no-bindings)
                                                 (colored-paths nil)
                                                 (trace-units nil)
                                                 (format "pdf")
                                                 (open t))
  "Comprehends utterance using cxn-inventory, writes the meaning network to a pdf file, and opens it."
  (set-configuration (visualization-configuration cxn-inventory)
                   :labeled-paths labeled-paths)
  (set-configuration (visualization-configuration cxn-inventory)
                   :colored-paths colored-paths)
  (set-configuration (visualization-configuration cxn-inventory)
                     :trace-units trace-units)
  
  (let* ((gp-data (analyse-solution cip-node '<-))
         (s-dot (unit-bindings->graph :data gp-data
                                      :preferred-font "Arial"
                                      :visualization-configuration (visualization-configuration cxn-inventory)))
         (path (make-file-name-with-time-and-experiment-class
                (merge-pathnames (babel-pathname :directory '(".tmp"))
                                 (make-pathname :name "constructional-dependencies"
                                                :type format))
                (mkstr (make-id 'graph)))))            
    (s-dot->image s-dot :path path :format format :open open)))
