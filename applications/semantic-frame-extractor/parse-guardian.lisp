;(ql:quickload :frame-extractor)
(in-package :frame-extractor)

(defun pie-comprehend-log (utterance &key (cxn-inventory *fcg-constructions*) (silent nil))
  "Utility function to comprehend an utterance and extract the frames in one go.
   Returns both a frame-set and the last cip-node."
  (multiple-value-bind (meaning cipn) (comprehend utterance :cxn-inventory cxn-inventory :silent silent)
    (values cipn (run-pie cipn))))

(defun get-sentences-from-json (path)
  (with-open-file (s path)
    (loop while (peek-char t s nil nil)
          collect (json:decode-json s) into docs
          finally (return docs))))

(defun log-parsing-output-into-json-file (frame-evoking-elem-list)
  "Parses sentences from the Guardian training-corpus that contain the specified frame-evoking-elems.
   Encodes the resulting frame-sets into json-format and writes them into 'frame-extractor-output.json' file."
  (let* ((sentence-objs (get-sentences-from-json (babel-pathname :directory '(:up "Corpora" "Guardian") :name "100-causation-frame-annotations" :type "json")))
         (sentences (loop for sent in sentence-objs
                          when (intersection
                                (mapcar #'cdr (mapcar (lambda (x) (assoc :frame-evoking-element x)) (cdr (assoc :frame-elements sent))))
                                frame-evoking-elem-list :test #'string=)
                          collect (cdr (assoc :sentence sent)) into sentences
                          finally (return sentences))))
      (loop for sent in sentences
            for (last-cipn raw-frame-set) = (multiple-value-list (pie-comprehend-log (string-trim '(#\Space #\Backspace #\Linefeed #\Page #\Return) sent) :silent t))
            collect (encode-json-alist-to-string `((:sentence . ,sent)
                                                   (:frame-elements . ,(loop for frame in (pie::entities raw-frame-set)
                                                                             collect `((:frame-evoking-element . ,(pie::frame-evoking-element frame))
                                                                                       (:cause . ,(cause frame))
                                                                                       (:effect . ,(effect frame)))))
                                                   (:applied-cxns . ,(mapcar #'name (applied-constructions last-cipn))))) into results
            finally (with-open-file (out (babel-pathname :directory '(:up "corpora" "Guardian") :name "frame-extractor-output" :type "json")
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
                      (loop for result in results
                            do (progn
                                 (format out result)
                                 (format out  "~%")))))))

;(activate-monitor trace-fcg)
;(log-parsing-output-into-json-file '("cause"))

;(pie-comprehend "It deleted references to studies showing that global warming is caused by manmade emissions.") ;only working randomly since lenient XcausedbyY might interfere, working with stricter XcausedbyY

;(pie-comprehend-log "This includes the extinction of the dinosaurs 65m years ago , thought to have been caused by the impact of a large asteroid on the Yucatan peninsula and beneath the Gulf of Mexico.") ;NOT working with stricter XcausedbyY

;(pie-comprehend "Global warming is very likely to have been caused by human activity, the most authoritative global scientific body studying climate change said in a report today") ;NOT working with stricter XcausedbyY

;(pie-comprehend "In 2001, the body - which brings together 2,500 scientists from more than 30 countries - said global warming was only \"likely\", or 66% probable, to have been caused by humans.") ;NOT working with stricter XcausedbyY

;(pie-comprehend "As so many other polls have shown consistently, the majority of Australians believe climate change is happening and is caused by human activity.") ;NOT working with stricter XcausedbyY

; --> need for other/adapted cxns covering these sentences


