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

(defun predict-semantic-frames (&key (inputfile *fcg-frame-annotations*) (outputfile *fcg-frame-conll-annotations*))
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



(defun evaluate-conll (&key predictions gold-standard)
  (let ((predictions (with-open-file (inputstream predictions :direction :input)
                       (loop for line = (read-line inputstream nil nil)
                             while line collect (split-frame-annotation-line line :gold nil))))
        (gold-standard (with-open-file (inputstream gold-standard :direction :input)
                         (loop for line = (read-line inputstream nil nil)
                               while line collect (split-frame-annotation-line line :gold t))))
        (results nil))
    ;; Counting number of frames in gold standard
    (loop with frames-gold = 0
          with targets-gold = 0
          with fes-gold = 0
          with causes-gold = 0
          with effects-gold = 0
          for gold-token in gold-standard
          unless (equalp (first gold-token) "O")
          do (loop for gold-annotation in gold-token
                   do
                   (incf frames-gold)
                   if (equalp (cdr (assoc :target-or-fe gold-annotation)) "TARGET")
                   do (incf targets-gold)
                   if (equalp (cdr (assoc :target-or-fe gold-annotation)) "FE")
                   do (incf fes-gold)
                   if (equalp (cdr (assoc :lu-or-fe gold-annotation)) "cause")
                   do (incf causes-gold)
                   if (equalp (cdr (assoc :lu-or-fe gold-annotation)) "effect")
                   do (incf effects-gold))
          finally
          (push `(:frames-gold . ,frames-gold) results)
          (push `(:targets-gold . ,targets-gold) results)
          (push `(:fes-gold . ,fes-gold) results)
          (push `(:causes-gold . ,causes-gold) results)
          (push `(:effects-gold . ,effects-gold) results))
    ;; Counting number of frames in prediction
    (loop with frames-predicted = 0
          with targets-predicted = 0
          with fes-predicted = 0
          with causes-predicted = 0
          with effects-predicted = 0
          for token in predictions 
          unless (equalp (first token) "O")
          do (loop for annotation in token
                   do (incf frames-predicted)
                   if  (equalp (cdr (assoc :target-or-fe annotation)) "TARGET")
                   do (incf targets-predicted)
                   if  (equalp (cdr (assoc :target-or-fe annotation)) "FE")
                   do (incf fes-predicted)
                   if  (equalp (cdr (assoc :lu-or-fe annotation)) "cause")
                   do (incf causes-predicted)
                   if  (equalp (cdr (assoc :lu-or-fe annotation)) "effect")
                   do (incf effects-predicted)
                   )
          finally
          (push `(:frames-predicted . ,frames-predicted) results)
          (push `(:targets-predicted . ,targets-predicted) results)
          (push `(:fes-predicted . ,fes-predicted) results)
          (push `(:causes-predicted . ,causes-predicted) results)
          (push `(:effects-predicted . ,effects-predicted) results))

    ;; Checking whether predications are correct
    (loop with frames-predicted-correct = 0
          with predicted-targets-correct = 0
          with predicted-fes-correct = 0
          with predicted-causes-correct = 0
          with predicted-effects-correct = 0
          for predicted-token in predictions
          for gold-token in gold-standard
          unless (equalp (first predicted-token) "O")
          do (loop for predicted-annotation in predicted-token
                   ;; for target classification
                   if (and (listp (first gold-token))
                           (equalp (cdr (assoc :target-or-fe predicted-annotation)) "TARGET")
                           (find predicted-annotation gold-token :test #'equal-frames))
                   do (incf predicted-targets-correct)
                   ;; for fe classification
                   if (and (listp (first gold-token))
                           (equalp (cdr (assoc :target-or-fe predicted-annotation)) "FE")
                           (find predicted-annotation gold-token :test #'equal-frames))
                   do (incf predicted-fes-correct)
                   ;; for cause classification
                   if (and (listp (first gold-token))
                           (equalp (cdr (assoc :lu-or-fe predicted-annotation)) "cause")
                           (find predicted-annotation gold-token :test #'equal-frames))
                   do (incf predicted-causes-correct)
                   ;; for effect classification
                   if (and (listp (first gold-token))
                           (equalp (cdr (assoc :lu-or-fe predicted-annotation)) "effect")
                           (find predicted-annotation gold-token :test #'equal-frames))
                   do (incf predicted-effects-correct)
                   ;; Overall: complete frame correct
                   if (and (listp (first gold-token))
                           (find predicted-annotation gold-token :test #'equal-frames))
                   do (incf frames-predicted-correct))
          finally
          (push `(:frames-predicted-correct . ,frames-predicted-correct) results)
          (push `(:predicted-targets-correct . ,predicted-targets-correct) results)
          (push `(:predicted-fes-correct . ,predicted-fes-correct) results)
          (push `(:predicted-causes-correct . ,predicted-causes-correct) results)
          (push `(:predicted-effects-correct . ,predicted-effects-correct) results))

    (let ((frame-level-precision (* 100 (float (/ (cdr (assoc :frames-predicted-correct results))
                                                  (cdr (assoc :frames-predicted results))))))
          (frame-level-recall (* 100 (float (/ (cdr (assoc :frames-predicted-correct results))
                                               (cdr (assoc :frames-gold results))))))
          ;; target
          (target-precision (* 100 (float (/ (cdr (assoc :predicted-targets-correct results))
                                             (cdr (assoc :targets-predicted results))))))
          (target-recall (* 100 (float (/ (cdr (assoc :predicted-targets-correct results))
                                          (cdr (assoc :targets-gold results))))))
          ;; fe
          (fe-precision (* 100 (float (/ (cdr (assoc :predicted-fes-correct results))
                                         (cdr (assoc :fes-predicted results))))))
          (fe-recall (* 100 (float (/ (cdr (assoc :predicted-fes-correct results))
                                      (cdr (assoc :fes-gold results))))))
          ;; cause
          (cause-precision (* 100 (float (/ (cdr (assoc :predicted-causes-correct results))
                                            (cdr (assoc :causes-predicted results))))))
          (cause-recall (* 100 (float (/ (cdr (assoc :predicted-causes-correct results))
                                         (cdr (assoc :causes-gold results))))))
          ;; effect
          (effect-precision (* 100 (float (/ (cdr (assoc :predicted-effects-correct results))
                                             (cdr (assoc :effects-predicted results))))))
          (effect-recall (* 100 (float (/ (cdr (assoc :predicted-effects-correct results))
                                          (cdr (assoc :effects-gold results))))))

          )

      (format t "~%================= Overall =================~%")
      (format t "Precision (overall): ~,2f%~%" frame-level-precision)
      (format t "Recall (overall): ~,2f%~%" frame-level-recall)
      (format t "F1 (overall): ~,2f%~%" (* 2 (/ (* frame-level-precision frame-level-recall)
                                                    (+ frame-level-precision frame-level-recall))))
      (format t "~%================= Target =================~%")
      (format t "Precision (target): ~,2f%~%" target-precision)
      (format t "Recall (target): ~,2f%~%" target-recall)
      (format t "F1 (target): ~,2f%~%" (* 2 (/ (* target-precision target-recall)
                                                    (+ target-precision target-recall))))
      (format t "~%================= Frame Element =================~%")
      (format t "Precision (fe): ~,2f%~%" fe-precision)
      (format t "Recall (fe): ~,2f%~%" fe-recall)
      (format t "F1 (fe): ~,2f%~%" (* 2 (/ (* fe-precision fe-recall)
                                           (+ fe-precision fe-recall))))

      (format t "~%================= Cause =================~%")
      (format t "Precision (cause): ~,2f%~%" cause-precision)
      (format t "Recall (cause): ~,2f%~%" cause-recall)
      (format t "F1 (cause): ~,2f%~%" (* 2 (/ (* cause-precision cause-recall)
                                           (+ cause-precision cause-recall))))

      (format t "~%================= Effect =================~%")
      (format t "Precision (effect): ~,2f%~%" effect-precision)
      (format t "Recall (effect): ~,2f%~%" effect-recall)
      (format t "F1 (effect): ~,2f%~%" (* 2 (/ (* effect-precision effect-recall)
                                           (+ effect-precision effect-recall)))))))
    

(defun equal-frames (predicted-frame gold-frame)
  (and (equalp (cdr (assoc :frame predicted-frame))
               (cdr (assoc :frame gold-frame)))
       (equalp (cdr (assoc :target-or-fe predicted-frame)) 
               (cdr (assoc :target-or-fe gold-frame)))
       (equalp (cdr (assoc :lu-or-fe predicted-frame))
               (cdr (assoc :lu-or-fe gold-frame)))
       (equalp (cdr (assoc :line predicted-frame))
               (cdr (assoc :line gold-frame)))))

(defun split-frame-annotation-line (line &key (gold nil))
  (let ((annotations (if gold
                       (subseq (split-sequence:split-sequence #\Space line :remove-empty-subseqs t) 11)
                       (split-sequence:split-sequence #\Space line :remove-empty-subseqs t))))
    (mapcar #'(lambda (annotation)
                (split-frame-annotation annotation :gold gold))
            annotations)))

; (split-frame-annotation-line "Causation:FE:cause:5557 Causation:FE:cause:5583" :gold nil)
; (split-frame-annotation-line "Causation:FE:cause:5557" :gold nil)
; (split-frame-annotation-line "1 1 This this DET DT _ 3 nsubj _ _ Causation:FE:cause:G:3 Causation:FE:cause:G:3" :gold t)

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
 
(defun run-fcg-evaluation (&key (conll-file *training-corpus-conll*)
                                (fcg-json-output *frame-extractor-output-indices*)
                                (predictions *FCG-FRAME-CONLL-ANNOTATIONS*)
                                (gold-standard *training-corpus-conll*)
                                (include-parsing? nil))
  (when include-parsing? (conll-comprehend->json
                          :conll-gold-standard conll-file
                          :frame-extractor-output fcg-json-output
                          :strings-as-output nil))
  (predict-semantic-frames
   :inputfile fcg-json-output
   :outputfile predictions)
  (evaluate-conll
   :predictions predictions
   :gold-standard gold-standard)
  )


;; (run-fcg-evaluation :include-parsing? t)
