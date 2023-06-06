(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (restore-grammars-and-save-configs "/Users/ehai-guest/Projects/babel/grammars/propbank-grammar/cleaning-and-evaluation/config-evaluation/grammars-config-eval" "grammars-info.csv")

(defun restore-grammars-and-save-configs (directory-path output-csv-path)
  "Restores grammars from files in the specified directory and saves their configuration data to a CSV file.
   Arguments:
   - DIRECTORY-PATH: The directory containing the grammar files.
   - OUTPUT-CSV-PATH: The file path for the output CSV file."
  (let ((directory (uiop:directory-files directory-path)))
    ;; Write the header to the output CSV file
    (with-open-file (stream output-csv-path :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
      (format stream "grammar_nr,grammar_size,heuristics,learning_modes,excluded_rolesets~%"))
    ;; Process the files in the directory
    (dolist (file directory)
      (when (cl-ppcre:scan "\\.fcg$" (namestring file)) ; Convert the pathname to a string here
        (let ((config-data (restore-grammar-and-get-config file)))
          (when config-data
            (save-config-to-csv config-data output-csv-path)))))))

(defun restore-grammar-and-get-config (file-path)
  "Restores a grammar from a file and returns its configuration data.
   Argument:
   - FILE-PATH: The file path of the grammar."
  (let ((file-path-string (namestring file-path)))
    (cl-ppcre:register-groups-bind (config-number-string)
        ("(\\d{4})\\.fcg" file-path-string)
      (when config-number-string
        (let* ((restored-grammar (cl-store:restore file-path-string))
              (config-number (parse-integer config-number-string))
              (grammar-size (size restored-grammar)))
          (let* ((heuristics-config (get-configuration restored-grammar :heuristics))
                 (learning-modes-config (get-configuration restored-grammar :learning-modes))
                 (excluded-rolesets-config (get-configuration restored-grammar :excluded-rolesets))
                 (all-configs (list :config-number config-number
                                    :grammar-size grammar-size
                                    :heuristics heuristics-config
                                    :learning-modes learning-modes-config
                                    :excluded-rolesets excluded-rolesets-config)))
            (when all-configs
              (format t "~A . ~A~%" config-number all-configs)
              all-configs)))))))

(defun save-config-to-csv (config-data file-path)
  "Saves configuration data to a CSV file.
   Arguments:
   - CONFIG-DATA: A list of configuration data to save. Should include :config-number, :grammar-size, :heuristics, :learning-modes, and :excluded-rolesets.
   - FILE-PATH: The file path for the output CSV file."
  (with-open-file (stream file-path :direction :output
                                       :if-exists :append
                                       :if-does-not-exist :create)
    (format stream "~A,~A,~A,~A,~A~%"
            (getf config-data :config-number)
            (getf config-data :grammar-size)
            (getf config-data :heuristics)
            (getf config-data :learning-modes)
            (getf config-data :excluded-rolesets))
    (format t "Saved config data to ~A~%" file-path)))









