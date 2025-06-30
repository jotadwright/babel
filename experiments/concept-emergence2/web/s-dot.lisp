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
    (loop with wds = (sort (concept-representations::get-weighted-distributions (meaning cxn))
                                  (lambda (x y) (string< (symbol-name (channel x))
                                                         (symbol-name (channel y)))))
          for wd in wds and weight-idx from 1
          for record = (wd->s-dot wd weight-idx)
          when (and (if disabled-channels
                      (not (gethash (feature-name wd) disabled-channels))
                      t)
                    (>= (weight wd) weight-threshold))
            do (push record g))
    ;; edges between cxn node and feature-channels
    (loop with wds = (sort (concept-representations::get-weighted-distributions (meaning cxn))
                                  (lambda (x y) (string< (symbol-name (channel x))
                                                         (symbol-name (channel y)))))
          for wd in wds and weight-idx from 1
          when (and (if disabled-channels
                      (not (gethash (feature-name wd) disabled-channels))
                      t)
                    (>= (weight wd) weight-threshold))
            do (push
                `(s-dot::edge
                  ((s-dot::from ,(mkdotstr (id (meaning cxn))))
                   (s-dot::to ,(mkdotstr (downcase (feature-name wd))))
                   (s-dot::label ,(format nil "&omega;~a: ~,2f"
                                          (integer-to-subscript-html weight-idx)
                                          (weight wd)))
                   (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                         #-(or :win32 :windows) "Arial")
                   (s-dot::fontcolor ,*black*)
                   (s-dot::fontsize "8.5")
                   (s-dot::arrowsize "0.5")
                   (s-dot::style ,(if (>= (weight wd) 0.99) "solid" "dashed"))))
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


(defgeneric wd->s-dot (wd weight-idx &key)
  (:documentation "Display a weighted-distribution using s-dot."))

(defmethod wd->s-dot ((wd concept-representations::weighted-distribution) weight-idx &key)
  (if (eq 'categorical (type-of (distribution wd)))
    (categorical->s-dot wd weight-idx)
    (continuous->s-dot wd weight-idx)))

(defmethod continuous->s-dot ((wd concept-representations::weighted-distribution) weight-idx &key)
  (let* ((st-dev (st-dev (distribution wd))))
    `(s-dot::record
      ((s-dot::style "rounded")
       (s-dot::fontsize "9.5")
       (s-dot::fontname #+(or :win32 :windows) "Sans"
                        #-(or :win32 :windows) "Arial")
       (s-dot::fontcolor "#000000")
       (s-dot::height "0.01"))
      (s-dot::node ((s-dot::id ,(downcase (mkdotstr (feature-name wd))))
                    (s-dot::label ,(format nil "~a\\n&mu;~a ~,3f ~~ &sigma;~a ~,3f"
                                           (downcase (mkdotstr (feature-name wd)))
                                           (integer-to-subscript-html weight-idx)
                                           (mean (distribution wd))
                                           (integer-to-subscript-html weight-idx)
                                           st-dev)))))))

(defmethod categorical->s-dot ((wd concept-representations::weighted-distribution) weight-idx &key)
  `(s-dot::record
    ((s-dot::style "rounded")
     (s-dot::fontsize "9.5")
     (s-dot::fontname #+(or :win32 :windows) "Sans"
                      #-(or :win32 :windows) "Arial")
     (s-dot::fontcolor "#000000")
     (s-dot::height "0.01"))
    (s-dot::node ((s-dot::id ,(downcase (mkdotstr (feature-name wd))))
                  (s-dot::label ,(format nil "~a\\n&#123;~{~a~^, ~}&#125;"
                                         (downcase (mkdotstr (feature-name wd)))
                                         (loop with tuples = (loop for key being the hash-keys of (cat-table (distribution prototype))
                                                                     using (hash-value value)
                                                                   if (> value 0)
                                                                     collect (cons key value))
                                               with sorted-tuples = (sort tuples #'> :key #'cdr)
                                               for (key . value) in sorted-tuples
                                               if (> value 0)
                                                 collect (format nil "~a:~a" key value))))))))


(defun integer-to-subscript-html (n &optional (prefix ""))
  "Converts an integer N into a sequence of Unicode subscript HTML entities,
   optionally prefixing the result with PREFIX."
  (let ((subscript-digit-table (make-hash-table)))
    (loop for digit in '((#\0 "&#x2080;") (#\1 "&#8321;") (#\2 "&#8322;") (#\3 "&#8323;")
                         (#\4 "&#8324;") (#\5 "&#8325;") (#\6 "&#8326;") (#\7 "&#8327;")
                         (#\8 "&#8328;") (#\9 "&#8329;"))
          do (setf (gethash (car digit) subscript-digit-table) (cadr digit)))
    (concatenate 'string prefix
                 (with-output-to-string (out)
                   (dolist (digit (coerce (princ-to-string n) 'list))
                     (format out "~a" (gethash digit subscript-digit-table digit)))))))
