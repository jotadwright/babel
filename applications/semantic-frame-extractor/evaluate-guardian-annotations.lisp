(in-package :frame-extractor)

(defun get-sentences-from-json (path)
  (with-open-file (s path)
    (loop while (peek-char t s nil nil)
          collect (json:decode-json s) into docs
          finally (return docs))))

(defun spit-json (path-name output-list)
  "Encodes given alist into json and writes resulting json-objects into file of given name."
  (with-open-file (out path-name
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
     (write-line (encode-json-alist-to-string `((:evaluations ,@output-list)))
                 out)))

(defun pie-comprehend-log (utterance &key (cxn-inventory *fcg-constructions*) (silent nil) (strings-as-output t))
  "Utility function to comprehend an utterance and extract the frames in one go.
   Returns both a frame-set and the last cip-node."
  (multiple-value-bind (meaning cipn) (comprehend utterance :cxn-inventory cxn-inventory :silent silent)
    (values cipn (run-pie cipn :strings-as-output strings-as-output))))

(defun log-parsing-output-into-json-file (target-frame-evoking-elements &key gold-standard frame-extractor-output)
  "Parses sentences from the Guardian training-corpus that contain the specified frame-evoking-elems.
   Encodes the resulting frame-sets into json-format and writes them into 'frame-extractor-output.json' file."
  (let* ((sentence-objs (get-sentences-from-json gold-standard))
         (sentences (loop for sentence-object in sentence-objs
                          for frame-elements-in-sentence = (rest (assoc :frame-elements sentence-object))
                          for frame-evoking-elements-in-sentence = (loop for frame-elts in frame-elements-in-sentence
                                                                         collect (rest (assoc :frame-evoking-element frame-elts)))
                          when (intersection frame-evoking-elements-in-sentence
                                             target-frame-evoking-elements :test #'string=)
                          collect (rest (assoc :sentence sentence-object)) into sentences
                          finally (return sentences))))
      (loop for sent in sentences
            for (last-cipn raw-frame-set) = (multiple-value-list
                                             (pie-comprehend-log (string-trim '(#\Space #\Backspace #\Linefeed #\Page #\Return) sent) :silent t))
            collect (encode-json-alist-to-string `((:sentence . ,sent)
                                                   (:frame-elements . ,(loop for frame in (pie::entities raw-frame-set)
                                                                             collect `((:frame-evoking-element . ,(pie::frame-evoking-element frame))
                                                                                       (:cause . ,(cause frame))
                                                                                       (:effect . ,(effect frame)))))
                                                   (:applied-cxns . ,(mapcar #'name (applied-constructions last-cipn))))) into results
            finally (with-open-file (out frame-extractor-output :direction :output :if-exists :supersede :if-does-not-exist :create)
                      (loop for result in results
                            do (progn
                                 (format out result)
                                 (format out  "~%")))))))

(defun filter-frames (filterfn sentence)
  "Applies given function to filter out unwanted frames in (annotated-)frame-elements of given sentence."
  (let* ((annotated-elems (cdr (assoc :annotated-frame-elements sentence)))
         (elems (cdr (assoc :frame-elements sentence)))
         (first-subst-sentence (subst (delete-if-not filterfn annotated-elems) annotated-elems sentence)))
    (subst (delete-if-not filterfn elems) elems first-subst-sentence)))

(defun load-parsings-with-annotations (parsing-path annotation-path)
  "Loads given frame-extractor output and corresponding annotations into one datastructure."
  (let* ((annotations (get-sentences-from-json annotation-path))
         (parsings (get-sentences-from-json parsing-path))
         (annotations-by-sentence (mapcar (lambda (a) (cons (cdr (assoc :sentence a)) (list a))) annotations)))
    (mapcar (lambda (parsing)
              (let ((annotation (cadr (assoc (cdr (assoc :sentence parsing)) annotations-by-sentence :test #'string=))))
                (append parsing (list `(:annotated-frame-elements . ,(cdr (assoc :frame-elements annotation)))))))
            parsings)))

(defun clean-slot-filler (frame-elem)
  "Cleans up the slot filler of given frame element by downcasing and replacing punctuation."
  (unless (stringp (rest frame-elem))
    (setf (rest frame-elem) (rest (assoc :utterance (rest frame-elem)))))
          
    (downcase
     (string-trim " ."
                  (cl-ppcre:regex-replace-all "\\s+" (cl-ppcre:regex-replace-all "[0-9]+|'|%" (cl-ppcre:regex-replace-all "-" (cl-ppcre:regex-replace-all "[,|\.|\"|:]" (cdr frame-elem) "") " ") #'(lambda (match &rest registers) (format nil " ~A" match)) :simple-calls t) " "))))

(defun clean-string (s)
  "Cleans up the given string by downcasing and replacing punctuation."
  (downcase (string-trim " ." (cl-ppcre:regex-replace-all "\\s+" (cl-ppcre:regex-replace-all "[0-9]+|'|%" (cl-ppcre:regex-replace-all "-" (cl-ppcre:regex-replace-all "[,|\.|\"|:]" s "") " ") #'(lambda (match &rest registers) (format nil " ~A" match)) :simple-calls t) " "))))

(defun coarse-frame-similarity (this-frame other-frame)
  "Returns similarity between given frames via string matching."
  (length
    (intersection this-frame other-frame :key #'clean-slot-filler :test #'string=)))

(defun evaluate-sentence-on-wordlevel (frames-alignment sentence)
  "Returns number of correctly parsed words and number of total words in given sentence from given parse annotations and gold standard."
  (let* ((clean-sentence-list (split-sequence:split-sequence #\Space (clean-string sentence)))
         (all-words (list-length clean-sentence-list))
         (parsed-frames (mapcar #'car frames-alignment))
         (gold-frames (mapcar #'cadr frames-alignment))
         (similarities (loop for parsed-frame in parsed-frames
                             for gold-frame in gold-frames
                             append (loop for parsed-slot-filler in parsed-frame
                                          for gold-slot-filler in gold-frame
                                          for parsed = (split-sequence:split-sequence #\Space (clean-slot-filler parsed-slot-filler))
                                          for gold = (split-sequence:split-sequence #\Space (clean-slot-filler gold-slot-filler))
                                          if (or (search parsed gold :test #'string=)
                                                 (search gold parsed :test #'string=))
                                          collect (list (min (list-length parsed) (list-length gold))
                                                (max (list-length parsed) (list-length gold))) ; based on assumption that annotated frame does not contain anything from outside the original sentence
                                          else
                                          collect (list 0 (list-length gold))))))
    (if similarities
      (list 
        (+ (- all-words (reduce #'+ similarities :key #'cadr)) (reduce #'+ similarities :key #'car))
        all-words)
      (let ((gold-slot-fillers (loop for gold-frame in gold-frames
                            append (loop for gold-slot-filler in gold-frame
                              collect (clean-slot-filler gold-slot-filler)))))
          (list (- all-words
                   (loop for gold-slot-filler in gold-slot-fillers
                         for gold-slot-fillers-without-current = (remove-nth (position gold-slot-filler gold-slot-fillers :test #'string=) gold-slot-fillers)
                         if (member gold-slot-filler gold-slot-fillers-without-current :test #'string=)
                            sum (/ (length (split-sequence:split-sequence #\Space  gold-slot-filler)) 2) ; ideally divided by calculated number of whole occurrences
                          else
                            if (not (reduce (lambda (x y) (or x y))
                                            (mapcar (lambda (other) (position (split-sequence:split-sequence #\Space gold-slot-filler) other :test #'equal))
                                                    (mapcar (lambda (other) (split-sequence:split-sequence #\Space other)) gold-slot-fillers-without-current))))
                                        sum (length (split-sequence:split-sequence #\Space gold-slot-filler)))) ; if filler is subseq of another filler, count only the words of the larger one
                all-words)))))

(defun frame-slots (this-frame other-frame)
  "Returns number of different frame slots in given frames."
  (length
    (union
      (mapcar #'car this-frame)
      (mapcar #'car other-frame))))

(defun all-permutations (lst &optional (remain lst))
  "Returns all possible permutations of a given list."
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst))) (rest remain))))))

(defun bruteforce-alignment (these-frames other-frames)
  "Tries to align the most similar frames from two given frame sets
   by trying out all possible permutations."
  (let* ((these-count (length these-frames))
         (other-count (length other-frames))
         (these-padded (concatenate 'list these-frames (make-list (max 0 (- other-count these-count)) :initial-element nil)))
         (other-padded (concatenate 'list other-frames (make-list (max 0 (- these-count other-count)) :initial-element nil))))
    (car (sort (mapcar
                (lambda (perm)
                    (mapcar (lambda (this that) (cons this that))
                            these-padded perm))
                (list (all-permutations other-padded)))
               #'> :key
               (lambda (e)
                   (reduce #'+ (mapcar (lambda (pair) (coarse-frame-similarity (car pair) (cdr pair))) e)))))))

(defun evaluate-sentence (sentence-structure)
  "Assigns the number of correct frames and frame-slot-fillers to each given sentence-output."
  (let* ((frames (cdr (assoc :frame-elements sentence-structure)))
         (annotated (cdr (assoc :annotated-frame-elements sentence-structure)))
         (sentence (cdr (assoc :sentence sentence-structure)))
         (alignment (bruteforce-alignment frames annotated))
         (frame-similarity
          (mapcar (lambda (pair)
                    (list (coarse-frame-similarity (car pair) (cadr pair))
                          (frame-slots (car pair) (cadr pair))))
                  alignment))
         (wordlevel-result (evaluate-sentence-on-wordlevel alignment sentence)))
    (append sentence-structure
            (list
              (cons
                :wordlevel-result
                wordlevel-result)
              (cons
              :frame-similarity
              frame-similarity)
              (cons
              :slot-similarity
              (reduce (lambda (a v) (mapcar #'+ a v)) frame-similarity :initial-value (list 0 0)))))))

(defun total-slot-similarity (sentences)
  "Calculates the total number of frame-slots and the number of correct slot-fillers
   over a set of sentences."
  (reduce (lambda (a v) (mapcar #'+ a v)) (mapcar (lambda (v) (cdr (assoc :slot-similarity v))) sentences) :initial-value (list 0 0)))

(defun total-correct-sentences (sentences)
  "Calculates the total number of sentences and the number of correct ones over a given set of sentences."
  (list (length
         (remove-if-not (lambda (slot-sim) (equal (first slot-sim) (second slot-sim))) sentences :key (lambda (sent) (cdr (assoc :slot-similarity sent)))))
        (length sentences)))

(defparameter *training-corpus* (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                :name "199-causation-frame-annotations" :type "json"))
(defparameter *test-corpus* (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                :name "146-causation-frame-annotations" :type "json"))
;;standard file with parse results:
(defparameter *frame-extractor-output* (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                       :name "frame-extractor-output" :type "json"))

(defun evaluate-grammar-output-for-evoking-elem (evoking-elems &key
                                                               (gold-standard *training-corpus*)
                                                               (frame-extractor-output nil)
                                                               (evaluation-results
                                                                (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                                                :name "frame-extractor-output-with-annotations" :type "json")))
  "Evaluates the frame-extractor output for given frame-evoking-elements by comparing it with corresponding annotations.
   Writes resulting output, annotations and correctness into json-file.
   Returns the total number of frame-slots and the number of correct slot-fillers as well as the number of correctly parsed sentences."
  (unless frame-extractor-output
    (log-parsing-output-into-json-file evoking-elems :gold-standard gold-standard :frame-extractor-output *frame-extractor-output*)
    (setf frame-extractor-output *frame-extractor-output*))
  (let* ((parsing-with-annotations (load-parsings-with-annotations frame-extractor-output gold-standard))
         (filtered-parsings (mapcar (lambda (s)
                                      (filter-frames (lambda (s)
                                                       (find (cdr (assoc :frame-evoking-element s)) evoking-elems :test #'string=))
                                                     s))
                                    parsing-with-annotations))
         (print-result (mapcar #'evaluate-sentence filtered-parsings))
         (total-correct-sentences (total-correct-sentences print-result))
         (total-slot-similarity (total-slot-similarity print-result))
         (total-word-result (reduce (lambda (a v) (mapcar #'+ a v))
                                    (mapcar (lambda (v) (cdr (assoc :wordlevel-result v))) print-result)
                                    :initial-value (list 0 0))))
    (spit-json evaluation-results print-result)
    (format t "~%Incorrectly parsed sentences:~%~%")
    (loop for parsing in print-result
          for slot-result = (cdr (assoc :slot-similarity parsing))
          when (not (equal (first slot-result) (second slot-result)))
          do (format t "~s: ~s (slots) ~s (words)~%~%" (cdr (assoc :sentence parsing))
                     slot-result (cdr (assoc :wordlevel-result parsing)))
          finally (format t "correct slots and total slots: ~s: ~a ~%correct sentences and total sentences: ~s: ~a ~%correct words and total words overall: ~s: ~a ~%~%" total-slot-similarity (coerce (apply #'/ total-slot-similarity) 'float) total-correct-sentences (coerce (apply #'/ total-correct-sentences) 'float) total-word-result (coerce (apply #'/ total-word-result) 'float)))
          (values
           total-slot-similarity
           total-correct-sentences
           total-word-result)))

;;##########################################################
;; EVALUATION
;;##########################################################

;; Running the evaluation (on training set - slow):
;; (evaluate-grammar-output-for-evoking-elem '("lead to" "cause" "because" "because of" "give rise" "due to" "result in"))

;; Running the evaluation when you have recently parsed all sentences (on training set - faster):
;; (evaluate-grammar-output-for-evoking-elem '("lead to" "cause" "because" "because of" "give rise" "due to" "result in") :frame-extractor-output *frame-extractor-output* )


;; Running the evaluation (on test set):
;; (evaluate-grammar-output-for-evoking-elem '("lead to" "cause" "because" "because of" "give rise" "due to" "result in") :gold-standard *test-corpus*)
                                          
