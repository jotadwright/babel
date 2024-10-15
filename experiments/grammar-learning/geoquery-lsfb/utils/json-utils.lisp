(in-package :geoquery-lsfb)

(defun jsonl->list-of-json-alists (jsonl-path)
  "reads in a jsonl file and returns it as a list of json-alists"
  (with-open-file (in-stream jsonl-path :direction :input)
    (loop with json-alists = nil
          for line = (read-line in-stream nil)
          while line do (push (cl-json::decode-json-from-string line) json-alists )
          finally (return (reverse json-alists))
          )))
