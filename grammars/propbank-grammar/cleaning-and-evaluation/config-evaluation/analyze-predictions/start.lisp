(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "analyze-predictions")
                      :name "get-grammar-info" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "analyze-predictions")
                      :name "restore-predictions-evaluate" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "config-evaluation" "analyze-predictions")
                      :name "predictions-report" :type "lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions to generate statistics and get data from the predictions and grammars. They are stored in four .csv files, used to make the grammar objects in the python project.
;; Note that the grammars and predictions should have a four digit identifier number in their name, e.g. "grammar_0001.fcg" and "predictions_0001.store".
(restore-grammars-and-save-configs "grammars-path" "grammar_data.csv")

;;(defparameter *corpus-experiment* (cl-store:restore "path-to-corpus-exp.store"))

(save-corpus-to-csv *corpus-experiment* "corpus_data.csv")

(generate-predictions-files "predictions-path" "grammars-path" "micro_evaluation_data.csv" "macro_evaluation_data.csv" "frame_prediction_data.csv")

(defun generate-predictions-files (predictions-directory-path grammars-directory-path output-file-micro output-file-macro output-file-report
                              &key (batch-size 1)
                                   (core-roles-only nil)
                                   (selected-rolesets nil)
                                   (include-word-sense t)
                                   (include-timed-out-sentences nil)
                                   (excluded-rolesets nil)
                                   (include-sentences-with-incomplete-role-constituent-mapping nil))
  (get-predictions-info predictions-directory-path grammars-directory-path output-file-micro output-file-macro
                                                        :batch-size batch-size
                                                        :core-roles-only core-roles-only
                                                        :selected-rolesets selected-rolesets
                                                        :include-word-sense include-word-sense
                                                        :include-timed-out-sentences include-timed-out-sentences
                                                        :excluded-rolesets excluded-rolesets
                                                        :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping)
  (get-predictions-reports predictions-directory-path grammars-directory-path output-file-report
                                                        :core-roles-only core-roles-only
                                                        :include-word-sense include-word-sense
                                                        :include-timed-out-sentences include-timed-out-sentences
                                                        :excluded-rolesets excluded-rolesets
                                                        :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping))
