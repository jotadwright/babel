(in-package :propbank-grammar)

(define-monitor export-categorial-network-to-jsonl
                :documentation "Export the final state of the categorial network as JSONL.")

(define-event-handler (export-categorial-network-to-jsonl learning-finished)

  (export-categorial-network-evolution-to-jsonl
   (categorial-network cxn-inventory)
   :path (babel-pathname :directory '("grammars" "propbank-grammar" "raw-data")
                         :name (format nil "~a~a" (multiple-value-bind (sec min hour day month year)
                                                      (decode-universal-time (get-universal-time))
                                                    (format nil "~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-"
                                                            year month day hour min sec)) "categorial-network")
                         :type "jsonl")
   :link-type 'nil))

