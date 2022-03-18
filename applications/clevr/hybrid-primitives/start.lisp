
(ql:quickload :hybrid-primitives)
(in-package :hybrid-primitives)

;; make a cookie jar!
(defparameter *cookie-jar*
  (make-instance 'drakma:cookie-jar))

;; initialize the server by loading a scene (i.e. an image)
(load-image "http://localhost:8888/" *cookie-jar* "CLEVR_val_000002.png")

;; close the session
(do-irl-request "http://localhost:8888/" "clear_session" nil *cookie-jar*)

(setf context-attn-id
  (multiple-value-bind (bind-scores bind-values)
      (evaluate-neural-primitive
       "http://localhost:8888/"
       `(:primitive get-context
         :slots (:context nil)))
    (loop for values in bind-values
          return (intern (getf values 'context)
                         :hybrid-primitives))))
(setf context-attn
      (make-instance 'attention
                     :id context-attn-id))
(request-attn "http://localhost:8888/" context-attn)
(add-element (make-html context-attn))

(defvar filter-bindings
  (evaluate-neural-primitive
   "http://localhost:8888/"
   `(:primitive filter
     :slots (:source-attn ,context-attn
             :category blue
             :target-attn nil))))
(defvar filter-attn
  (loop for bind-set in filter-bindings
        return (loop for statement in bind-set
                     return (internal-symb (third statement)))))

(defvar count-bindings
  (evaluate-neural-primitive
   "http://localhost:8888/"
   `(:primitive count
     :slots (:source-attn ,filter-attn
             :target-num nil))))
(defvar count-result
  (loop for bind-set in count-bindings
        return (loop for statement in bind-set
                     return (third statement))))

  
