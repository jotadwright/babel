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

(defun restore-conll-sentence (conll-sentence)
  (list-of-strings->string (mapcar #'(lambda(word) (cdr (assoc :FORM word))) conll-sentence)))

; (setf *conll-corpus* (read-corpus *corpus*))
; (activate-monitor trace-fcg)
;(restore-conll-sentence (third *conll-corpus*))
;; testing
;(set-configuration *fcg-constructions* :de-render-mode :raw-dependency-translation)
;(set-configuration *fcg-constructions* :de-render-mode :de-render-conll)
;(pie-comprehend-log (third *conll-corpus*) :strings-as-output nil)





(defparameter *fcg-frame-annotations* (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                      :name "frame-extractor-output-indices"
                                                      :type "json"))

(defparameter *fcg-frame-conll-annotations* (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                      :name "fcg-frame-conll-annotations"
                                                      :type "tsv"))

(defun annotate-conll (inputfile outputfile)
  (let* ((sentences (with-open-file (inputstream inputfile :direction :input)
                      (loop for line = (read-line inputstream nil nil)
                            while line
                            collect (decode-json-from-string line))))
         (annotated-sentences (loop with absolute-line-nr = 0
                                    for sentence in sentences
                                    for tokens = (loop for i from 1 upto (cdr (assoc :nr-of-words sentence))
                                                       collect "O")
                                    do
                      
                                    (loop for frame in (cdr (assoc :frame-elements sentence))
                                          for frame-evoking-element-indices = (cdr (assoc :frame-evoking-element frame))
                                          for cause-indices  = (cdr (assoc :cause frame))
                                          for effect-indices  = (cdr (assoc :effect frame))
                                          do
                                          (loop for index in frame-evoking-element-indices
                                                for annotation = (format nil "Causation:TARGET:~a:~a"
                                                                         (cdr (assoc :frame-evoking-element-id frame))
                                                                         (+ absolute-line-nr (first frame-evoking-element-indices)))
                                                do
                                                (if (listp (nth (- index 1) tokens))
                                                  (setf (nth (- index 1) tokens) (append (nth (- index 1) tokens) (list annotation)))
                                                  (setf (nth (- index 1) tokens) (list annotation))))
                                          (loop for index in cause-indices
                                                for annotation = (format nil "Causation:FE:cause:~a"
                                                                         (+ absolute-line-nr (first frame-evoking-element-indices)))
                                                do 
                                                (if (listp (nth (- index 1) tokens))
                                                  (setf (nth (- index 1) tokens) (append (nth (- index 1) tokens) (list annotation)))
                                                  (setf (nth (- index 1) tokens) (list annotation))))
                                          (loop for index in effect-indices
                                                for annotation = (format nil "Causation:FE:effect:~a"
                                                                         (+ absolute-line-nr (first frame-evoking-element-indices)))
                                                do 
                                                (if (listp (nth (- index 1) tokens))
                                                  (setf (nth (- index 1) tokens) (append (nth (- index 1) tokens) (list annotation)))
                                                  (setf (nth (- index 1) tokens) (list annotation)))))
                                    (setf absolute-line-nr (+ absolute-line-nr (length tokens)))
                                    collect tokens)))
    (with-open-file (outputstream outputfile :direction :output :if-does-not-exist :create :if-exists :supersede)
      (loop for sentence in annotated-sentences
            do
            (loop for word-annotation in sentence
                  do (format outputstream "~{~a~^ ~}~%" (listify word-annotation))))
      )))


; (pprint (annotate-conll *fcg-frame-annotations* *fcg-frame-conll-annotations*))



(defun evaluate-conll (predictions gold-standard)
  (let ((predictions (with-open-file (inputstream predictions :direction :input)
                      (loop for line = (read-line inputstream nil nil)
                            while line collect (split-frame-annotation-line line :gold nil))))
        (gold-standard (with-open-file (inputstream predictions :direction :input)
                      (loop for line = (read-line inputstream nil nil)
                            while line collect (split-frame-annotation-line line :gold t)))))
    (loop with correct-frames = 0
          for predicted-token in predictions
          for gold-token in gold-standard
          when (or (equalp (first gold-token) "O")
                   (equalp (first predicted-token) "O"))
          do
          (if (equalp (first predicted-token) (first predicted-token))
            (incf correct-frames))
          else do (loop for predicted-annotation in predicted-token
                   for predicted-frame = (cdr (assoc :frame predicted-annotation))
                   for predicted-target-or-fe = (cdr (assoc :target-or-fe predicted-annotation))
                   for predicted-lu-or-fe = (cdr (assoc :lu-or-fe predicted-annotation))
                   for predicted-frame-line = (cdr (assoc :line predicted-annotation))
                   when (find predicted-annotation :test #'(lambda (gold-annotation)
                                                             (equalp predicted-frame predicted-frame)
                                                             (equalp predicted-target-or-fe predicted-target-or-fe)
                                                             (equalp predicted-lu-or-fe predicted-lu-or-fe)
                                                             (equalp predicted-frame-line) predicted-frame-line)
                              gold-token)
                   do (incf correct-frames))
          finally (return correct-frames))))
    

;; (evaluate-conll *fcg-frame-conll-annotations* *corpus*)
  
(defun split-frame-annotation-line (line &key (gold nil))
  (let ((annotations (split-sequence:split-sequence #\Space line :remove-empty-subseqs t)))
    (mapcar #'(lambda (annotation)
                (split-frame-annotation annotation :gold gold))
            annotations)))

; (split-frame-annotation-line "Causation:FE:cause:5557 Causation:FE:cause:5583" :gold nil)
; (split-frame-annotation-line "Causation:FE:cause:5557" :gold nil)


(defun split-frame-annotation (frame-annotation &key (gold nil))
  (let ((parts (split-sequence:split-sequence #\: frame-annotation)))
    (if (< 1 (length parts))
      `((:frame . ,(first parts))
        (:target-or-fe . ,(second parts))
        (:lu-or-fe . ,(third parts))
        (:line . ,(if gold (fifth parts)
                    (fourth parts))))
      "O")))

; (split-frame-annotation "Causation:FE:cause:4548" :gold nil)
; (split-frame-annotation "Causation:TARGET:because-of:4587")
; (split-frame-annotation "Causation:FE:effect:G:2550" :gold t)
 

