(in-package :frame-extractor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading corpus file     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *corpus* (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                       :name "199-causation-frame-annotations"
                                       :type "conll"))

(defun read-corpus (corpus)
  ""
  (let* ((corpus-lines (with-open-file (inputstream corpus :direction :input)
                        (loop for line = (read-line inputstream nil nil)
                              while line
                              collect (read-conll-line line))))
        (corpus-sentences (loop with sentence = nil
                                with all-sentences = nil
                                for line in corpus-lines
                                if (and sentence (= 1 (cdr (assoc :ID line))))
                                do
                                (setf all-sentences (append all-sentences (list sentence)))
                                (setf sentence (list line))
                                else do (setf sentence (append sentence (list line)))
                                finally
                                (progn (setf all-sentences (append all-sentences (list sentence)))
                                  (return all-sentences)))))
    corpus-sentences))

;; (pprint (read-corpus *corpus*))
;; (setf *conll-sentence* (first (read-corpus *corpus*)))

(defun read-conll-line (string)
  (let* ((parts (split-sequence:split-sequence #\Space string :remove-empty-subseqs t))
         (line-id (read-from-string (first parts)))
         (id (read-from-string (second parts)))
         (form (third parts))
         (lemma (fourth parts))
         (upos (fifth parts))
         (xpos (sixth parts))
         (feats (seventh parts))
         (head (read-from-string (eighth parts)))
         (deprel (ninth parts)))
    `((:line-id . ,line-id)
      (:id . ,id)
      (:form . ,form)
      (:lemma . ,lemma)
      (:upos . ,upos)
      (:xpos . ,xpos)
      (:feats . ,feats)
      (:head . ,head)
      (:deprel . ,deprel))))


;; testing
(setf *conll-corpus*  (read-corpus *corpus*))
(activate-monitor trace-fcg)
(set-configuration *fcg-constructions* :de-render-mode :de-render-conll)
(pie-comprehend (third *conll-corpus*))
                        
    
    
