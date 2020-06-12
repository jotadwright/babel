
(ql:quickload :hybrid-primitives)
(in-package :hybrid-primitives)

;; initialize the server by loading a scene (i.e. an image)
(load-image "CLEVR_val_000000.png")

;; evaluate the get-context primitive
(defvar context-bindings
  (evaluate-neural-primitive
   `(:primitive get-context
     :slots (:context nil))))
(defvar context-attn
  (loop for bind-set in context-bindings
        return (loop for statement in bind-set
                     return (internal-symb (third statement)))))

(defvar filter-bindings
  (evaluate-neural-primitive
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
   `(:primitive count
     :slots (:source-attn ,filter-attn
             :target-num nil))))
(defvar count-result
  (loop for bind-set in count-bindings
        return (loop for statement in bind-set
                     return (third statement))))

  
