(in-package :frame-extractor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading corpus file     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *corpus* (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                       :name "199-causation-frame-annotations"
                                       :type "conll"))

(defun read-corpus (corpus)
  ""
  (let ((corpus-lines (with-open-file (inputstream corpus :direction :input)
                        (loop for line = (read-line inputstream nil nil)
                              while line
                              collect (read-conll-line line)))))
    corpus-lines))

(defun read-conll-line (string)
  (let* ((parts (split-sequence:split-sequence #\Space string :remove-empty-subseqs t))
         (line-id (first parts))
         (id (second parts))
         (form (third parts))
         (lemma (fourth parts))
         (upos (fifth parts))
         (xpos (sixth parts))
         (feats (seventh parts))
         (head (eighth parts))
         (deprel (ninth parts)))
   string 
  ))


(find-all 12 (read-corpus *corpus*) :test #'< :key #'length)
                        
                        
    
    
