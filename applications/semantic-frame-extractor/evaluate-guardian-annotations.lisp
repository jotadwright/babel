;(ql:quickload :frame-extractor)
(ql:quickload :cl-ppcre)
(in-package :frame-extractor)

(defun get-sentences-from-json (path)
  (with-open-file (s path)
    (loop while (peek-char t s nil nil)
          collect (json:decode-json s) into docs
          finally (return docs))))

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
  (downcase (string-trim " ." (cl-ppcre:regex-replace-all "\\s+" (cl-ppcre:regex-replace-all "[0-9]+|'|%" (cl-ppcre:regex-replace-all "-" (cl-ppcre:regex-replace-all "[,|\.|\"|:]" (cdr frame-elem) "") " ") #'(lambda (match &rest registers) (format nil " ~A" match)) :simple-calls t) " "))))

(defun coarse-frame-similarity (this-frame other-frame)
  "Returns similarity between given frames via string matching."
  (length
    (intersection this-frame other-frame :key #'clean-slot-filler :test #'string=)))

(defun substring-wordlevel-frame-similarity (this-frame other-frame)
  "Checks for matching substrings and returns similarity between given frames (correct words, total words and correct words per total words)."
  (let* ((this-count (length this-frame))
         (other-count (length other-frame))
         (this-padded (concatenate 'list this-frame (make-list (max 0 (- other-count this-count)) :initial-element nil)))
         (other-padded (concatenate 'list other-frame (make-list (max 0 (- this-count other-count)) :initial-element nil)))
         (similarities (loop for this-slot-filler in this-padded
                            for other-slot-filler in other-padded
                            for this = (split-sequence:split-sequence #\Space (clean-slot-filler this-slot-filler))
                            for other = (split-sequence:split-sequence #\Space (clean-slot-filler other-slot-filler))
                            if (or (search this other :test #'string=)
                                   (search other this :test #'string=))
                              collect (/ (min (list-length this) (list-length other))
                                       (max (list-length this) (list-length other))) into percentages
                              and collect (list (min (list-length this) (list-length other))
                                                (max (list-length this) (list-length other))) into fractions
                            else
                              collect 0 into percentages
                              and collect (list 0 (list-length other)) into fractions
                            finally (return (list percentages fractions)))))
    (if similarities
      (list (/ (reduce #'+ (car similarities)) (list-length (car similarities)))
              (list
                (reduce #'+ (cadr similarities) :key #'car)
                (reduce #'+ (cadr similarities) :key #'cadr)))
      (list 0 0))))

(defun substring-frame-similarity (this-frame other-frame)
  "Checks for matching substrings and returns similarity between given frames in correct characters per total characters."
  (let* ((this-count (length this-frame))
         (other-count (length other-frame))
         (this-padded (concatenate 'list this-frame (make-list (max 0 (- other-count this-count)) :initial-element nil)))
         (other-padded (concatenate 'list other-frame (make-list (max 0 (- this-count other-count)) :initial-element nil)))
         (similarities (loop for this-slot-filler in this-padded
                            for other-slot-filler in other-padded
                            for this = (clean-slot-filler this-slot-filler)
                            for other = (clean-slot-filler other-slot-filler)
                            if (or (search this other)
                                   (search other this))
                            collect (/ (min (length this) (length other))
                                       (max (length this) (length other)))
                            else
                            collect 0)))
    (if similarities
      (/ (reduce #'+ similarities) (list-length similarities))
      0)))

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
                (all-permutations other-padded))
               #'> :key
               (lambda (e)
                   (reduce #'+ (mapcar (lambda (pair) (coarse-frame-similarity (car pair) (cdr pair))) e)))))))

(defun evaluate-sentence (sentence-structure)
  "Assigns the number of correct frames and frame-slot-fillers to each given sentence-output."
  (let* ((frames (cdr (assoc :frame-elements sentence-structure)))
         (annotated (cdr (assoc :annotated-frame-elements sentence-structure)))
         (alignment (bruteforce-alignment frames annotated))
         (frame-similarity
          (mapcar (lambda (pair)
                    (list (coarse-frame-similarity (car pair) (cdr pair))
                          (frame-slots (car pair) (cdr pair))))
                  alignment))
         ;(char-similarity not needed
         ; (mapcar (lambda (pair)
         ;           (substring-frame-similarity (car pair) (cdr pair)))
         ;         alignment))
         (wordlevel-stats
          (mapcar (lambda (pair)
                    (substring-wordlevel-frame-similarity (car pair) (cdr pair)))
                  alignment))
         (word-percentage-similarity
           (list (caar wordlevel-stats)))
         (word-similarity-counts
           (cdar wordlevel-stats)))
    (append sentence-structure
            (list
             (cons
               :word-similarity-counts
               word-similarity-counts)
             (cons
              :word-percentage-similarity
              word-percentage-similarity)
             ;(cons
             ; :char-similarity
             ; char-similarity)
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

(defun total-word-similarity (sentences)
  "Calculates the total number of words and the number of correct words
   over a set of sentences."
  (reduce (lambda (a v) (mapcar #'+ a v)) (mapcar (lambda (v) (cadr (assoc :word-similarity-counts v))) sentences) :initial-value (list 0 0)))

(defun total-correct-sentences (sentences)
  "Calculates the total number of sentences and the number of correct ones over a given set of sentences."
  (list (length
         (remove-if-not (lambda (slot-sim) (equal (first slot-sim) (second slot-sim))) sentences :key (lambda (sent) (cdr (assoc :slot-similarity sent)))))
        (length sentences)))

(defun overall-similarity (sentences assoc-keyword)
  "Calculates the overall, average 'assoc-keyword'-similarity in correct items per frame slot filler."
  (/ (loop for sent in sentences
        sum (second (assoc assoc-keyword sent))) (list-length sentences)))

(defun evaluate-grammar-output-for-evoking-elem (evoking-elems)
  "Evaluates the frame-extractor output for given frame-evoking-elements by comparing it with corresponding annotations.
   Writes resulting output, annotations and correctness into json-file.
   Returns the total number of frame-slots and the number of correct slot-fillers as well as the number of correctly parsed sentences."
  (let* ((path-to-parse-results (babel-pathname :directory '(:up "Corpora" "Guardian") :name "frame-extractor-output" :type "json"))
         (path-to-annotations (babel-pathname :directory '(:up "Corpora" "Guardian") :name "111-causation-frame-annotations" :type "json"))
         (parsing-with-annotations (load-parsings-with-annotations path-to-parse-results path-to-annotations))
         (filtered-parsings (mapcar (lambda (s)
                                      (filter-frames (lambda (s)
                                                       (find (cdr (assoc :frame-evoking-element s)) evoking-elems :test #'string=))
                                                     s))
                                    parsing-with-annotations))
         (print-result (mapcar #'evaluate-sentence filtered-parsings))
         (total-correct-sentences (total-correct-sentences print-result))
         (total-slot-similarity (total-slot-similarity print-result))
         (total-word-similarity (total-word-similarity print-result))
         ;(overall-char-similarity (* 100 (overall-similarity print-result :char-similarity))) not needed
         (overall-word-percentage-similarity (* 100 (overall-similarity print-result :word-percentage-similarity))))
    (spit-json (babel-pathname :directory '(:up "Corpora" "Guardian") :name "frame-extractor-output-with-annotations" :type "json")
               print-result)
    (format t "Incorrectly parsed sentences:~%~%")
    (loop for parsing in print-result
          for result = (cdr (assoc :slot-similarity parsing))
          when (not (equal (first result) (second result)))
          do (format t "~s: ~s (slots) ~s (words)~%~%" (cdr (assoc :sentence parsing)) result (cadr (assoc :word-similarity-counts parsing)))
          finally (format t "correct slots and total slots: ~s~%correct sentences and total sentences: ~s~%correct words per slot on average: ~$%~%correct words and total words: ~s~%correct words per total words as ratio: ~$%~%~%" total-slot-similarity total-correct-sentences overall-word-percentage-similarity total-word-similarity (* 100 (/ (car total-word-similarity) (cadr total-word-similarity)))))
          (values
           total-slot-similarity
           total-correct-sentences
           overall-word-percentage-similarity
           total-word-similarity)))

(defun spit-json (path-name output-list)
  "Encodes given alist into json and writes resulting json-objects into file of given name."
  (with-open-file (out path-name
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
     (write-line (encode-json-alist-to-string `((:evaluations ,@output-list)))
                 out)))



;(evaluate-grammar-output-for-evoking-elem '("due to"))


