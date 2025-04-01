(in-package :cle)

;; ----------------
;; + cxn -> s-dot +
;; ----------------

(defgeneric cxn->s-dot (cxn &key weight-threshold disabled-channels)
  (:documentation "Display a cxn using s-dot."))

(defmethod cxn->s-dot ((cxn cxn) &key (weight-threshold 0.1) (disabled-channels nil))
  (let ((g '(((s-dot::ranksep "0.3")
              (s-dot::nodesep "0.5")
              (s-dot::margin "0")
              (s-dot::rankdir "LR"))
             s-dot::graph)))
    ;; cxn node
    (push
     `(s-dot::record  
       ((s-dot::style "rounded")
        (s-dot::fillcolor "#FFFFFF")
        (s-dot::fontcolor ,*black*)
        (s-dot::fontsize "9.5")
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id ,(mkdotstr (id (meaning cxn))))
                     (s-dot::label ,(format nil "ID: ~a"
                                            ;(mkdotstr (id (meaning cxn)))
                                            (mkdotstr (obj-id (meaning cxn)))))
                     (s-dot::fontcolor "#AA0000"))))
     g)
    ;; form node
    (push
     `(s-dot::record  
       ((s-dot::style "rounded")
        (s-dot::fontsize "9.5")
        (s-dot::fontcolor ,*black*)
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id ,(mkdotstr (form cxn)))
                     (s-dot::label ,(downcase (mkdotstr (form cxn))))
                     (s-dot::style "dashed")
                     (s-dot::fontcolor "#AA0000"))))
     g)
    ;; edge between form node and cxn node
    (push `(s-dot::edge
            ((s-dot::from ,(downcase (mkdotstr (form cxn))))
             (s-dot::to ,(mkdotstr (id (meaning cxn))))
             (s-dot::label ,(format nil "e = ~,2f"
                                    (score cxn)))
             (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                   #-(or :win32 :windows) "Arial")
             (s-dot::dir "both")
             (s-dot::fontsize "8.5")
             (s-dot::arrowsize "0.5")
             (s-dot::fontcolor ,*black*)
             (s-dot::style ,(if (>= (score cxn) 0.99) "solid" "dashed")))) g)
    ;; return
    (reverse g)))