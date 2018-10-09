(in-package :frame-extractor)

(defun read-words-from-file (path)
  "The file contains words, separated by a whitespace, typically on one line."
  (let ((words ""))
    (with-open-file (in path)
      (loop for line = (read-line in nil nil)
            while line do (setf words (string-append words line))))
    (split-sequence:split-sequence #\Space words)))

(defun all-positions-aux (elt list positions)
  (if (null list)
    positions
    (let ((index (position elt list :test #'equal)))
      (if index
        (all-positions-aux elt (subseq list (+ 1 index))
                           (append positions
                                   (if positions
                                     (list (+ index (+ 1 (first positions))))
                                     (list index))))
        positions))))

(defun all-positions (elt list)
  "Return all the indexes of an element in a list."
  (all-positions-aux elt list nil)
  )

;(all-positions "a" '("a" "b" "a"))
;(all-positions "the" '("and" "the" "cat" "came" "back" "to" "the" "mouse"))

(defun get-sublist-start-and-end-indexes (sub-list full-list)
  "Find the start and end indexes of a sublist."
  (let ((possible-init-indexes (all-positions (first sub-list) full-list))
        (possible-end-indexes (all-positions (last-elt sub-list) full-list)))
    (loop for i from 0
          for start in possible-init-indexes 
          for end = (or (nth i possible-end-indexes)
                        (first possible-end-indexes))
          when (= (- (length sub-list) 1)
                  (- end start))
          do (return (list start end)))))

;(get-sublist-start-and-end-indexes '("the" "cat") '("the" "grumpy" "cat" "the" "cat"))

(defun remove-nth (n list)
  "Remove nth element from a list."
  (declare
   (type (integer 0) n)
   (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

(defun fill-range (start end)
  (loop for i from start to end
        collect i))


(defun join-strings (multiword-expression utterance-as-list)
  "Join all substrings corresponding to the multiword expression in
the original (list) utterance. E.g. \"Mickey Mouse\" '(\"Mickey\"
\"Mouse\" \"is\" \"a\" \"cartoon\" \"character\") >> '(\"Mickey
Mouse\" \"is\" \"a\" \"cartoon\" \"character\")."
  (let* ((mwe-indexes (get-sublist-start-and-end-indexes
                       (get-stanford-tokens multiword-expression) ;;split op basis van andere leestekens ook!!
                      utterance-as-list))
         (list-without-mwe
          (when mwe-indexes
            (loop with mw-range = (fill-range (first mwe-indexes) (second mwe-indexes))
                  for word in utterance-as-list
                  for i from 0
                  unless (member i mw-range)
                  collect word))))
    (if mwe-indexes
      (append
       (subseq list-without-mwe 0 (first mwe-indexes))
       (list multiword-expression)
       (subseq list-without-mwe (first mwe-indexes)))
      utterance-as-list)))
      
;(join-strings "Mickey Mouse" '("Mickey" "Mouse" "is" "a" "cartoon" "character"))
;(join-strings "Walt Disney" '("He" "is" "the" "Walt" "Disney" "ompany" "'s" "mascot"))
;(get-stanford-tokens "Geomar Helmholtz Centre")

(defun get-stanford-tokens (utterance)
  "Split an utterance into tokens according to the Stanford tokenizer. Makes use of the Stanford tagger."
  (let ((stanford-tag-list (run-stanford-postagger utterance))) ;;TODO: write our own splitter >> regular expressions
    (when stanford-tag-list
      (mapcar #'first stanford-tag-list))))

(defun get-penelope-named-entities-without-cardinals (sentence)
  "Return the named entities but not the cardinals."
  (loop for ne-result in (get-penelope-named-entities sentence)
        unless (string= "CARDINAL" (second ne-result))
        collect (first ne-result)))

(defun tokenize-english-sentence (sentence &key (group-named-entities t))
  "Split an English sentence into tokens, preserving the Named Entities as whole strings."
  (let ((sentence-as-list
         (or (get-stanford-tokens (decapitalize-string sentence :language-model "en"))
             (split-sequence:split-sequence #\Space (decapitalize-string sentence)
                                            :remove-empty-subseqs t))))
    (when group-named-entities
      (let ((named-entities (get-penelope-named-entities-without-cardinals sentence)))
        (loop for named-entity in named-entities
              do (setf sentence-as-list (join-strings named-entity sentence-as-list)))))
    sentence-as-list))

;(tokenize-english-sentence "The study, carried out at Geomar Helmholtz Centre for Ocean Research in Germany, was the most comprehensive of the subject to date.")

(defun single-solution-p (list)
  "Checks whether there is only one solution in a list and returns it."
  (and (null (rest list))
       (first list)))

(defun find-english-cxn (name &optional (inventory *fcg-english*))
  "If the construction is not found, search for possible constructions based on the name."
  (let ((the-cxn (find name (constructions inventory) :key #'name)))
    (if the-cxn
      the-cxn
      (let ((solutions (find-all (symbol-name name) (constructions inventory)
                                 :key #'(lambda(cxn)
                                          (symbol-name (name cxn)))
                                 :test #'(lambda(x y)
                                           (cl-ppcre::scan x y)))))
        (if (single-solution-p solutions)
          (first solutions)
          solutions)))))

(defun show-english-cxn (name &optional (inventory *fcg-english*))
  (let ((cxns (find-english-cxn name inventory)))
    (if (null cxns)
      (add-element (make-html (format nil "Could not find constructions matching ~a" name)))
      (progn
        (add-element (make-html "Found:"))
        (dolist (cxn (listify cxns))
          (add-element (make-html cxn)))))))


(in-package :fcg)
(defmethod make-html-construction-title ((construction fcg-construction))
  `((span) 
    ,(format nil "~(~a~)" (name construction)) ))
(defmethod make-html-construction-title ((construction construction))
  `((span) 
    ,(format nil "~(~a~)" (name construction)) ))