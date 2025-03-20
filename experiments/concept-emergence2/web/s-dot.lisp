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
                     (s-dot::label ,(format nil "~a"
                                            (mkdotstr (id (meaning cxn)))))
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
    ;; feature-channels nodes
    (loop with prototypes = (sort (get-prototypes (meaning cxn))
                                  (lambda (x y) (string< (symbol-name (channel x))
                                                         (symbol-name (channel y)))))
          for prototype in prototypes
          for record = (prototype->s-dot prototype)
          when (and (if disabled-channels
                      (not (gethash (channel prototype) disabled-channels))
                      t)
                    (>= (weight prototype) weight-threshold))
            do (push record g))
    ;; edges between cxn node and feature-channels
    (loop with prototypes = (sort (get-prototypes (meaning cxn))
                                  (lambda (x y) (string< (symbol-name (channel x))
                                                         (symbol-name (channel y)))))
          for prototype in prototypes and weight-idx from 1
          when (and (if disabled-channels
                      (not (gethash (channel prototype) disabled-channels))
                      t)
                    (>= (weight prototype) weight-threshold))
            do (push
                `(s-dot::edge
                  ((s-dot::from ,(mkdotstr (id (meaning cxn))))
                   (s-dot::to ,(mkdotstr (downcase (channel prototype))))
                   (s-dot::label ,(format nil "w~a = ~,2f" weight-idx (weight prototype)))
                   (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                         #-(or :win32 :windows) "Arial")
                   (s-dot::fontcolor ,*black*)
                   (s-dot::fontsize "8.5")
                   (s-dot::arrowsize "0.5")
                   (s-dot::style ,(if (>= (weight prototype) 0.99) "solid" "dashed"))))
                g))
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


(defgeneric prototype->s-dot (prototype &key)
  (:documentation "Display a prototype using s-dot."))

(defmethod prototype->s-dot ((prototype prototype) &key)
  (if (eq 'categorical (type-of (distribution prototype)))
    (categorical->s-dot prototype)
    (continuous->s-dot prototype)))

(defmethod continuous->s-dot ((prototype prototype) &key)
  (let* ((st-dev (st-dev (distribution prototype)))
         (record-properties (if (= (weight prototype) 1.0)
                              '((s-dot::style "rounded"))
                              '((s-dot::style "dashed")))))
    `(s-dot::record
      ((s-dot::style "rounded")
       (s-dot::fontsize "9.5")
       (s-dot::fontname #+(or :win32 :windows) "Sans"
                        #-(or :win32 :windows) "Arial")
       (s-dot::fontcolor "#000000")
       (s-dot::height "0.01"))
      (s-dot::node ((s-dot::id ,(downcase (mkdotstr (channel prototype))))
                    (s-dot::label ,(format nil "~a: ~,3f ~~ ~,3f"
                                           (downcase (mkdotstr (channel prototype)))
                                           (mean (distribution prototype))
                                           st-dev)))))))

(defmethod categorical->s-dot ((prototype prototype) &key)
  (let* ((record-properties (if (= (weight prototype) 1.0)
                              '((s-dot::style "solid"))
                              '((s-dot::style "dashed")))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::fontcolor "#000000")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(downcase (mkdotstr (channel prototype))))
                    (s-dot::label ,(format nil "~a: ~{~a~^, ~}"
                                           (downcase (mkdotstr (channel prototype)))
                                           (loop with tuples = (loop for key being the hash-keys of (cat-table (distribution prototype))
                                                                       using (hash-value value)
                                                                     if (> value 0)
                                                                       collect (cons key value))
                                                 with sorted-tuples = (sort tuples #'> :key #'cdr)
                                                 for (key . value) in sorted-tuples
                                                 if (> value 0)
                                                   collect (format nil "(~a, ~a)" key value)))))))))
