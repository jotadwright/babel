(in-package :frame-extractor)

(defparameter *decoded-twitter-stream*
  (with-open-file (inputstream
                   (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                   :name "twitter-climate"
                                   :type "json")
                   :direction :input :if-does-not-exist :error)
                 
    (decode-json-from-source inputstream)))

(defparameter *tweet-texts-with-caus*
  (loop for tweet in *decoded-twitter-stream*
        for tweet-text = (cdr (assoc :text tweet))
        when (and tweet-text
                  (cl-ppcre::scan-to-strings " [cC]aus.+" tweet-text))
        collect tweet-text))

(defparameter *test-tweets* (list (first *tweet-texts-with-caus*)
                                  (second *tweet-texts-with-caus*)))

(defparameter *unique-tweet-texts-with-caus*
  (loop with text-hash-table = (make-hash-table :test #'equalp)
        for text in *tweet-texts-with-caus*
        unless (gethash text text-hash-table)
        do (setf (gethash text text-hash-table) 'found)
        collect text))

(defparameter *causation-frames*
  (loop with sentence-count = 0
        with tweet-count = 0
        for text in *unique-tweet-texts-with-caus*
        for utterances = (get-penelope-sentence-tokens text)
        do (incf tweet-count) (format t "tweet nr. ~a ~%" tweet-count)
        append (loop for utterance in utterances
                     for utterance-with-caus-p = (cl-ppcre::scan-to-strings " [cC]aus.+" utterance)
                     when utterance-with-caus-p
                     collect (handler-case (pie-comprehend utterance :silent t :cxn-inventory *fcg-constructions*)
                               (error (e) "Error in precision language processing module!" e)))))

"That's the leading cause of climate change, ðð¤£ð" ;;= emoticons

(defun build-hash-tables-for-causes-and-effects (frame-sets)
  (let ((causes (make-hash-table :test #'equalp))
        (effects (make-hash-table :test #'equalp)))
                
    (loop for frame-set in frame-sets
          when (eql (type-of frame-set) 'PIE:FRAME-SET)
          do (loop for frame in (pie::entities frame-set)
                   for cause = (cause frame)
                   for effect = (effect frame)
                   if (and cause effect (gethash cause causes))
                   do (setf (gethash cause causes) (cons effect (gethash cause causes)))
                   else do (when (and cause effect)
                             (setf (gethash cause causes) (list effect)))
                   if (and cause effect (gethash effect effects))
                   do (setf (gethash effect effects) (cons cause (gethash effect effects)))
                   else do (when (and cause effect)
                             (setf (gethash effect effects) (list cause)))))

    (values causes effects)))


(multiple-value-bind (causes effects)
    (build-hash-tables-for-causes-and-effects *causation-frames*)
  (defparameter *twitter-causes* causes) ;;hash table with causes as key and effect as values
  (defparameter *twitter-effects* effects))



(with-open-file (outputstream
                   (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                   :name "twitter-causes"
                                   :type "json")
                   :direction :output)
  (format outputstream "~a"
          (encode-json-to-string (loop for k being the hash-keys in *twitter-causes*
                                             using (hash-value v)
                                             collect `((:cause . ,k)
                                                       (:effects . ,v))))))
      


(with-open-file (outputstream
                   (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                   :name "twitter-effects"
                                   :type "json")
                   :direction :output)
  (format outputstream "~a"
          (encode-json-to-string (loop for k being the hash-keys in *twitter-effects*
                                             using (hash-value v)
                                             collect `((:cause . ,k)
                                                       (:effects . ,v))))))