
(ql:quickload :fcg)
(ql:quickload :cl-json)
(ql:quickload :utils)
(in-package :utils)

(defparameter *input-file* "/Users/jensnevens/corpora/CLEVR-pattern-finding-data/val/stage-1.jsonl")
(defparameter *output-file* "/Users/jensnevens/corpora/CLEVR-pattern-finding-data/val/stage-1-clean.jsonl")

(defun fresh-variables (set-of-predicates)
  (reset-id-counters)
  (let* ((all-variables (find-all-anywhere-if #'variable-p set-of-predicates))
         (unique-variables (remove-duplicates all-variables))
         (renamings (loop for var in unique-variables
                          for base-name = (get-base-name var)
                          collect (cons var (make-var base-name)))))
    (values (fcg::substitute-bindings renamings set-of-predicates)
            renamings)))

(with-open-file (in-stream *input-file* :direction :input)
  (with-open-file (out-stream *output-file* :direction :output)
    (loop for line = (read-line in-stream nil nil)
          while line
          do (let* ((line-data (cl-json:decode-json-from-string line))
                    (utterance (rest (assoc :utterance line-data)))
                    (meaning (read-from-string (rest (assoc :meaning line-data))))
                    (len (rest (assoc :len line-data)))
                    (out-data
                     `((:utterance . ,utterance)
                       (:meaning . ,(mkstr (fresh-variables meaning)))
                       (:len . ,len))))
               (write-line (cl-json:encode-json-alist-to-string out-data)
                           out-stream)))))
               