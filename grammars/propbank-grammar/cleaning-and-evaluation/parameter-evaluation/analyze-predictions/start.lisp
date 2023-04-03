(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation" "analyze-predictions")
                      :name "get-grammar-info" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation" "analyze-predictions")
                      :name "restore-predictions-evaluate" :type "lisp"))

(load (babel-pathname :directory
                      '("grammars" "propbank-grammar" "cleaning-and-evaluation" "parameter-evaluation" "analyze-predictions")
                      :name "predictions-report" :type "lisp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions to generate statistics from the predictions and grammars used to make the grammar objects in the python script.
(restore-grammars-and-save-configs "/Users/ehai-guest/Projects/babel/grammars/propbank-grammar/cleaning-and-evaluation/parameter-evaluation/grammars-parameter" "grammar_data.csv")

(generate-predictions-files "/Users/ehai-guest/Projects/babel/grammars/propbank-grammar/cleaning-and-evaluation/parameter-evaluation/predictions-parameter" "micro_evaluation_data.csv" "macro_evaluation_data.csv" "frame_prediction_data.csv" :core-roles-only nil :include-timed-out-sentences nil)

(defun generate-predictions-files (directory-path output-file-micro output-file-macro output-file-report
                              &key (batch-size 1)
                                   (core-roles-only nil)
                                   (selected-rolesets nil)
                                   (include-word-sense t)
                                   (include-timed-out-sentences nil)
                                   (excluded-rolesets nil)
                                   (include-sentences-with-incomplete-role-constituent-mapping t))
  (get-predictions-info directory-path output-file-micro output-file-macro
                                                        :batch-size batch-size
                                                        :core-roles-only core-roles-only
                                                        :selected-rolesets selected-rolesets
                                                        :include-word-sense include-word-sense
                                                        :include-timed-out-sentences include-timed-out-sentences
                                                        :excluded-rolesets excluded-rolesets
                                                        :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping)
  (get-predictions-reports directory-path output-file-report
                                                        :core-roles-only core-roles-only
                                                        :include-word-sense include-word-sense
                                                        :include-timed-out-sentences include-timed-out-sentences
                                                        :excluded-rolesets excluded-rolesets
                                                        :include-sentences-with-incomplete-role-constituent-mapping include-sentences-with-incomplete-role-constituent-mapping))


