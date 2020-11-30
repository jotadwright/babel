;;;; preprocess-utterance.lisp

(in-package :coco-grammar)

(export '(preprocess-utterance))

;;;; evaluation helper functions
(defun detach-punctuation (word)
  "This function will check if the input string (word)
   has a punctuation at the end of it (e.g. it?)
   and return a list of the word + the punctuation mark
   (e.g. '('it' '?')"
  (let ((last-char (char word (1- (length word)))))
    (if (punctuation-p last-char)
      (if (eq last-char #\?)
        (list (subseq word 0 (1- (length word))))
        (list (subseq word 0 (1- (length word)))
              (subseq word (1- (length word)))))
      (list word))))

(defun tokenize (utterance)
  "Split the utterance in words, downcase every word,
   remove the punctuation from the word"
  (let ((words (split (remove-spurious-spaces utterance) #\space)))
    (loop for word in words
          append (detach-punctuation (downcase word)))))

(defun extract-chunks ()
  "Extract all chunks in the cxn inventory"
  (loop for cxn in (constructions-list *COCO*)
        for conditional-part = (conditional-part cxn)
        append (loop for unit in conditional-part
                     when (fcg:fcg-unit-feature unit 'form)
                     append (loop for constraint in (fcg:fcg-unit-feature-value unit 'form)
                              when (and (eql (first constraint) 'string)
                                        (find #\space (last-elt constraint)))
                              append (last constraint)))))

(defun group-and-sort-chunks (list-of-strings)
  "Group the chunks by length and sort them, largest first"
  (loop with grouped-chunks
        for chunk in list-of-strings
        for chunk-length = (1+ (count #\space chunk :test #'equal))
        if (assoc chunk-length grouped-chunks)
        do (nconc (rest (assoc chunk-length grouped-chunks))
                  (list chunk))
        else
        do (setf grouped-chunks (append (list (cons chunk-length (list chunk))) grouped-chunks))
        finally
        (return (sort grouped-chunks #'> :key #'first))))

(defun sublist-contains-chunk (list-of-words)
  "Check if a sublist already contains a chunk"
  (loop for word in list-of-words
        when (find #\space word :test #'equal)
        do (return t)))

(defun sublist-matches-chunk (list-of-words chunk)
  "Check if a sublist matches a chunk"
  (string= (list-of-strings->string list-of-words) chunk))

(defun replace-by-chunk (list-of-words start end chunk)
  "Replace the words between start and end by the chunk
   in the list of words"
  (append (when (> start 0)
            (subseq list-of-words 0 start))
          (list chunk)
          (subseq list-of-words end)))

(defparameter *sorted-form-chunks*
  (group-and-sort-chunks
   (remove-duplicates (extract-chunks) :test #'string=))
  "All form chunks from the CLEVR grammar, sorted by length, longest first")

(defun group-chunks (list-of-words)
  ;; group the chunks by length
  ;; largest chunks first
  ;; for every group size
  ;;   go over the list-of-words with a sliding window of that size
  ;;   this is called the sublist
  ;;   when the sublist already contains a chunk, don't even bother to check again
  ;;   otherwise, match it with all chunks of that size
  ;;   if it matches, replace the sublist by the chunk
  ;;   that is why we need to keep checking if 'end' is in bounds
  ;;   because the length of list-of-words changes as we go
  (loop for (chunk-size . chunks) in *sorted-form-chunks*
        do (loop for start from 0 to (- (length list-of-words) chunk-size)
                 for end from chunk-size to (length list-of-words)
                 for sublist = (unless (> end (length list-of-words))
                                 (subseq list-of-words start end))
                 unless sublist
                 return nil
                 unless (sublist-contains-chunk sublist)
                 do (loop for chunk in chunks
                          when (sublist-matches-chunk sublist chunk)
                          do (setf list-of-words (replace-by-chunk list-of-words start end chunk))
                          and return nil)))
  list-of-words)

(defun preprocess-utterance (utterance)
  "Preprocess the utterance for the clevr grammar"
  (group-chunks (tokenize utterance)))
