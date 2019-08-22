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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;; How to Use the Grammar Evaluation Module in FCG                    ;;
;; November 2016 - Katrien                                            ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(ql:quickload 'fcg)
(in-package :fcg)

;;1. First create files for evaluating your grammar
;;-------------------------------------------------
;; The grammar
(defparameter *demo-grammar* (load-demo-grammar))

;; Sentences you want to evaluate should be stored in a separate file (one sentence per line)
(defparameter *test-sentences*
  '("the mouse" "the linguist"
    "the linguist likes the mouse"
    "the mouse likes the linguist"))

(defparameter *test-file-with-sentences*
  (with-open-file (out (babel-pathname :directory '(".tmp")
                                  :name "test-sentences" :type "txt")
                       :direction :output :if-does-not-exist :create :if-exists :overwrite)
    (loop for sentence in *test-sentences*
          do (write-line sentence out))
    out))

(defparameter *test-file-with-sentences-and-gold-standard-meanings*
  (make-gold-standard-meaning-file *test-file-with-sentences* *demo-grammar*
                                   (babel-pathname :directory '(".tmp")
                                  :name "test-sentences-with-meanings" :type "txt")))
 


;;2. Call the evaluation functions (two directions)
;;-------------------------------------------------

;;a) Starting from form -> meaning (and reformulate)
(evaluate-grammar-for-comprehension *test-file-with-sentences-and-gold-standard-meanings* ;;test set
                                    *demo-grammar* ;;grammar
                                    :bi-directional? t ;;comprehend and reformulate
                                    :series 4) ;;iterations per sentence (4 by default)

;;You can now check the output buffer for an evaluation report.

;;b) Starting from meaning -> form (and recomprehend)
(evaluate-grammar-for-production *test-file-with-sentences-and-gold-standard-meanings*
                                 *demo-grammar* 
                                 :series 4)

;;c) Evaluate in two directions
(full-grammar-evaluation *test-file-with-sentences-and-gold-standard-meanings* *demo-grammar*)

 
;;3. Activating monitors for plotting the results
;;-------------------------------------------------


