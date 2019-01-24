
;; (ql:quickload :frame-extractor)

(in-package :frame-extractor)

(defun fill-hash-tables ()
  (let ((twitter-cause->effect-hashtable (make-hash-table :test #'equalp))
        (twitter-effect->cause-hashtable (make-hash-table :test #'equalp))
        (twitter-cause->effect-list (with-open-file (inputstream (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                                         :name "twitter-causes"
                                                                         :type "json"))
                                                         (decode-json-from-string (read-line inputstream))))
        (twitter-effect->cause-list (with-open-file (inputstream (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                                         :name "twitter-effects"
                                                                         :type "json"))
                                                         (decode-json-from-string (read-line inputstream))))
        (guardian-cause->effect-hashtable (make-hash-table :test #'equalp))
        (guardian-effect->cause-hashtable (make-hash-table :test #'equalp))
        (guardian-cause->effect-list (with-open-file (inputstream (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                                         :name "guardian-causes"
                                                                         :type "json"))
                                                         (decode-json-from-string (read-line inputstream))))
        (guardian-effect->cause-list (with-open-file (inputstream (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                                         :name "guardian-effects"
                                                                         :type "json"))
                                                         (decode-json-from-string (read-line inputstream)))))
    

    (loop for el in twitter-cause->effect-list
          for cause = (cdr (assoc :CAUSE el))
          for effects = (cdr (assoc :EFFECTS el))
          if (gethash cause twitter-cause->effect-hashtable)
          do (setf (gethash cause twitter-cause->effect-hashtable) (append effects (gethash cause twitter-cause->effect-hashtable)))
          else do (setf (gethash cause twitter-cause->effect-hashtable) effects))

    (loop for el in twitter-effect->cause-list
          for effect = (cdr (assoc :EFFECT el))
          for causes = (cdr (assoc :CAUSES el))
          if (gethash effect twitter-effect->cause-hashtable)
          do (setf (gethash effect twitter-effect->cause-hashtable) (append causes (gethash effect twitter-effect->cause-hashtable)))
          else do (setf (gethash effect twitter-effect->cause-hashtable) causes))

        (loop for el in guardian-cause->effect-list
          for cause = (cdr (assoc :CAUSE el))
          for effects = (cdr (assoc :EFFECTS el))
          if (gethash cause guardian-cause->effect-hashtable)
          do (setf (gethash cause guardian-cause->effect-hashtable) (append effects (gethash cause guardian-cause->effect-hashtable)))
          else do (setf (gethash cause guardian-cause->effect-hashtable) effects))

    (loop for el in guardian-effect->cause-list
          for effect = (cdr (assoc :EFFECT el))
          for causes = (cdr (assoc :CAUSES el))
          if (gethash effect guardian-effect->cause-hashtable)
          do (setf (gethash effect guardian-effect->cause-hashtable) (append causes (gethash effect guardian-effect->cause-hashtable)))
          else do (setf (gethash effect guardian-effect->cause-hashtable) causes))

    (loop for k being the hash-keys in twitter-cause->effect-hashtable
          using (hash-value v)
          do (setf (gethash k twitter-cause->effect-hashtable) (list-to-set-with-frequencies v)))

    (loop for k being the hash-keys in twitter-effect->cause-hashtable
          using (hash-value v)
          do (setf (gethash k twitter-effect->cause-hashtable) (list-to-set-with-frequencies v)))

    (loop for k being the hash-keys in guardian-cause->effect-hashtable
          using (hash-value v)
          do (setf (gethash k guardian-cause->effect-hashtable) (list-to-set-with-frequencies v)))

    (loop for k being the hash-keys in guardian-effect->cause-hashtable
          using (hash-value v)
          do (setf (gethash k guardian-effect->cause-hashtable) (list-to-set-with-frequencies v)))
    
    (values twitter-cause->effect-hashtable guardian-cause->effect-hashtable
            twitter-effect->cause-hashtable guardian-effect->cause-hashtable)))

(defun list-to-set-with-frequencies (list)
  (loop with set = nil
        for item in list
        if (assoc item set :test #'equalp)
        do (incf (cdr (assoc item set :test #'equalp)))
        else do (unless (member item '("it" "that" "which" "they") :test #'equalp)
                  (push (cons item 1) set))
        finally (return (sort set #'> :key #'cdr))))

(multiple-value-bind (twitter-cause->effect-hashtable
                      guardian-cause->effect-hashtable
                      twitter-effect->cause-hashtable
                      guardian-effect->cause-hashtable)
    (fill-hash-tables)
  (defparameter *twitter-cause->effect* twitter-cause->effect-hashtable)
  (defparameter *twitter-effect->cause* twitter-effect->cause-hashtable)
  (defparameter *guardian-cause->effect* guardian-cause->effect-hashtable)
  (defparameter *guardian-effect->cause* guardian-effect->cause-hashtable))

(defun cause->effect-graph (cause data &key (max-nr-of-effects 10))
  (let* ((effects (gethash cause (if (string= data "guardian") *guardian-cause->effect* *twitter-cause->effect*)))
         (nodes (cons `((:name . ,cause)
                        (:group . 1))
                      (loop for i from 0 upto (- max-nr-of-effects 1)
                            for effect in effects
                            collect `((:name . ,(first effect))
                                      (:group . 1)))))
         (links (loop for i from 1 upto max-nr-of-effects
                      for effect in effects
                      collect `((:source . ,i)
                                (:target . 0)
                                (:weight . 1)))))
    (encode-json-alist-to-string `((:nodes . ,nodes)
                                   (:links . ,links)))))

;;  (cause->effect-graph "global warming" :guardian)
;;  (cause->effect-graph "global warming" :twitter)


(defun effect->cause-graph (effect data &key (max-nr-of-causes 10))
  (let* ((causes (gethash effect (if (string= data "guardian") *guardian-effect->cause* *twitter-effect->cause*)))
         (nodes (cons `((:name . ,effect)
                        (:group . 1))
                      (loop for i from 0 upto (- max-nr-of-causes 1)
                            for cause in causes
                            collect `((:name . ,(first cause))
                                      (:group . 1)))))
         (links (loop for i from 1 upto max-nr-of-causes
                      for cause in causes
                      collect `((:source . ,i)
                                (:target . 0)
                                (:weight . 1)))))
    (encode-json-alist-to-string `((:nodes . ,nodes)
                                   (:links . ,links)))))

;;  (effect->cause-graph "global warming" :guardian)
;;  (effect->cause-graph "global warming" :twitter)

;; curl  -H "Content-Type: application/json"  -d '{"cause":"humans"}' http://localhost:9004/semantic-frame-extractor/query-effect

;; curl -d '{"texts" : ["Over two-thirds agreed that if they had caused damage to their own clothes at work, the company should not be liable for repairs. This causes that.", "This is a sentence. This causes that."], "frames" : ["Causation"]}' http://localhost:9004/semantic-frame-extractor/texts-extract-frames