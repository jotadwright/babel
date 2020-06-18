
(in-package :hybrid-primitives)

(export '(load-image request-attn))

(defun do-irl-request (server-address endpoint data)
  (multiple-value-bind (response code headers
                                 uri stream must-close
                                 reason-phrase)
      (http-request (mkstr server-address endpoint)
                    :method :post :content-type "application/json"
                    :content (replace-char (downcase (to-json data)) #\- #\_))
    (declare (ignorable headers uri stream must-close reason-phrase))
    (values (parse (upcase (replace-char response #\_ #\-))) code)))


(defun load-image (server-address image-name)
  "Load a CLEVR image before starting
   the IRL program evaluation"
  (multiple-value-bind (response code)
      (do-irl-request server-address
                      "init_image"
                      `(:name ,image-name))
    (declare (ignorable response))
    (unless (= code 200)
      (error "Something went wrong while loading ~a" image-name))))


(defun evaluate-neural-primitive (server-address data)
  "Evaluate a neural primitive. Check if the response
   status code is ok and return the relevant data"
  (multiple-value-bind (response code)
      (do-irl-request server-address
                      "evaluate_neural_primitive"
                      data)
    (cond
     ((= code 400) ; something went wrong
      (let ((error-type (getf response :error-type))
            (error-msg (getf response :message)))
        (error "~a error: ~a" error-type error-msg)))
     ((= code 200) ; status normal
      (cond
       ;; if bindings, return them
       ((getf response :bindings)
        (process-bindings (getf response :bindings)))
       ;; if consistent, return it
       ((getf response :consistent)
        (getf response :consistent)))))))


(defun process-bindings (bindings)
  "Process the new binding such that they are easier
   to handle in the primitive definition. The variable
   should always refer to some internal symbol (i.e.
   a slot spec of the primitive), the score is always
   a float between 0 and 1 and the value can be anything,
   so it is left as is. Is is up to the primitive definition
   to further process the value.
   The new bindings are returned as a list of lists of dictionaries.
   This is because multiple variables can be bound in 1 go and
   the same variable can be bound multiple times."
  (let ((bind-scores
         (loop for bind-set in bindings
               collect (loop for bind-statement in bind-set
                             append (list
                                     (intern (getf bind-statement :variable)
                                             :hybrid-primitives)
                                     (getf bind-statement :score)))))
        (bind-values
         (loop for bind-set in bindings
               collect (loop for bind-statement in bind-set
                             append (list
                                     (intern (getf bind-statement :variable)
                                             :hybrid-primitives)
                                     (getf bind-statement :value))))))
    (values bind-scores bind-values)))


(defun request-attn (server-address attention)
  (multiple-value-bind (byte-array code headers
                                   uri stream must-close
                                   reason-phrase)
      (http-request (replace-char
                     (downcase
                      (mkstr server-address
                             (format nil "attn/~a"
                                     (id attention))))
                     #\- #\_)
                    :method :get)
    (declare (ignorable headers uri stream must-close reason-phrase))
    (when (= code 200)
      (let ((filepath (babel-pathname :directory '(".tmp" "attn") :type "png"
                                      :name (downcase (mkstr (id attention))))))
        (ensure-directories-exist filepath)
        (with-open-file (stream filepath :direction :output
                                :element-type 'unsigned-byte)
          (loop for byte across byte-array
                do (write-byte byte stream)))
        (setf (img-path attention) filepath)
        filepath))))