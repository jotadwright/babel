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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tutorial for corpus processing system ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load grammar and corpus-processing system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :dutch-vp)
(ql:quickload :corpus-processing)
(in-package :dutch-vp)

;; Specify input and ouputfiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *input-file*
  (babel-pathname :directory '("tutorial" "corpus-processing") :name "testfile-long" :type "txt"))
(defparameter *output-file*
  (babel-pathname :directory '("tutorial" "corpus-processing") :name "testfile-output" :type "txt"))

;; the function you want to call on each line

(defun comprehend-and-formulate-with-dutch-vp-grammar (line)
  (comprehend-and-formulate line :cxn-inventory *fcg-constructions*))


;; Call the function

;; Single batch - 8 threads
(process-corpus :inputfile *input-file*
                :outputfile *output-file*
                :function #'comprehend-and-formulate-with-dutch-vp-grammar
                :number-of-threads 8
                :number-of-lines-per-thread 2000)

;; Multiple-batches - 4 threads
(process-corpus :inputfile *input-file*
                :outputfile *output-file*
                :function #'comprehend-and-formulate-with-dutch-vp-grammar
                :number-of-threads 4
                :number-of-lines-per-thread 20)



