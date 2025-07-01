(in-package :cle)

;; ----------------
;; + cxn -> s-dot +
;; ----------------

(defparameter *white* "#FFFFFF")
(defparameter *red* "#E32D2D")
(defparameter *green* "#26AD26")
(defparameter *black* "#000000")

(defgeneric cxn->s-dot-diff (cxn delta &key weight-threshold disabled-features)
  (:documentation "Display a cxn using s-dot."))

(defmethod cxn->s-dot-diff ((cxn cxn) (previous-copy cxn)
                            &key
                            (weight-threshold 0.1)
                            (disabled-features nil))
  (let ((g '(((s-dot::ranksep "0.3")
              (s-dot::nodesep "0.5")
              (s-dot::margin "0")
              (s-dot::rankdir "LR"))
             s-dot::graph)))
    ;; cxn node
    (push
     `(s-dot::record  
       ((s-dot::style "rounded")
        (s-dot::fillcolor ,*white*)
        (s-dot::fontsize "9.5")
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01")
        (s-dot::fontcolor ,*black*))
       (s-dot::node ((s-dot::id ,(mkdotstr (id (meaning cxn))))
                     (s-dot::label ,(format nil "~a"
                                            (mkdotstr (id (meaning cxn)))))
                     (s-dot::fontcolor ,*red*))))
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

    ;; feature-features nodes
    (loop with wds = (sort (concept-representations::get-weighted-distributions (meaning cxn))
                                  (lambda (x y) (string< (symbol-name (feature-name x))
                                                         (symbol-name (feature-name y)))))
          for wd in wds
          for previous-wd = (gethash (feature-name wd) (concept-representations::representation (meaning previous-copy)))
          for record = (wd->s-dot-diff wd previous-wd)
          when (and (if disabled-features
                      (not (gethash (feature-name wd) disabled-features))
                      t)
                    (>= (weight previous-wd) weight-threshold))
            do (push record g))
    ;; edges between cxn node and feature-features
    (loop with wds = (sort (concept-representations::get-weighted-distributions (meaning cxn))
                           (lambda (x y) (string< (symbol-name (feature-name x))
                                                  (symbol-name (feature-name y)))))
          for wd in wds and weight-idx from 1
          for previous-wd = (gethash (feature-name wd) (concept-representations::representation (meaning previous-copy)))
          for delta = (- (weight wd) (weight previous-wd))
          when (and (if disabled-features
                      (not (gethash (feature-name wd) disabled-features))
                      t)
                    (>= (weight previous-wd) weight-threshold))
            do (push
                `(s-dot::edge
                  ((s-dot::from ,(mkdotstr (id (meaning cxn))))
                   (s-dot::to ,(mkdotstr (downcase (feature-name wd))))
                   (s-dot::label ,(format nil "w~a = ~,2f" weight-idx (float (weight wd))
                                          (cond ((> delta 0) (format nil " (+~,2f)" (float delta)))
                                                ((< delta 0) (format nil " (~,2f)" (float delta))))))
                   (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                         #-(or :win32 :windows) "Arial")
                   (s-dot::color ,(cond ((> delta 0) *green*)
                                        ((< delta 0) *red*)
                                        (t *black*)))
                                             
                   (s-dot::fontsize "8.5")
                   (s-dot::arrowsize "0.5")
                   (s-dot::color ,*black*)
                   (s-dot::style ,(if (>= (weight wd) 0.99) "solid" "dashed"))))
                g))
    ;; edge between form node and cxn node
    (push `(s-dot::edge
            ((s-dot::from ,(downcase (mkdotstr (form cxn))))
             (s-dot::to ,(mkdotstr (id (meaning cxn))))
             (s-dot::label ,(format nil "e = ~,2f~a"
                                    (float (score cxn))
                                    (let ((delta (- (score cxn) (score previous-copy))))
                                      (cond ((> delta 0) (format nil " (+~,2f)" (float delta)))
                                            ((< delta 0) (format nil " (~,2f)" (float delta)))
                                            (t "")))))
             (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                   #-(or :win32 :windows) "Arial")
             (s-dot::fontcolor ,(cond ((> (- (score cxn) (score previous-copy)) 0) *green*)
                                      ((< (- (score cxn) (score previous-copy)) 0) *red*)
                                      (t *black*)))
             (s-dot::dir "both")
             (s-dot::fontsize "8.5")
             (s-dot::arrowsize "0.5")
             (s-dot::style ,(if (>= (score cxn) 0.99) "solid" "dashed"))))
          g)
    ;; return
    (reverse g)))

(defgeneric wd->s-dot-diff (wd previous-wd &key)
  (:documentation "Display a weighted-distribution using s-dot."))

(defmethod wd->s-dot-diff ((wd concept-representations::weighted-distribution) (previous-wd concept-representations::weighted-distribution) &key)
  (if (eq 'categorical (type-of (distribution wd)))
    (categorical->s-dot-diff wd previous-wd)
    (continuous->s-dot-diff wd previous-wd)))

(defmethod continuous->s-dot-diff ((wd concept-representations::weighted-distribution) (previous-wd concept-representations::weighted-distribution) &key)
  (let* ((st-dev (st-dev (distribution wd)))
         (prev-st-dev (st-dev (distribution previous-wd))))
    `(s-dot::record
      ((s-dot::style "rounded")
       (s-dot::fontsize "9.5")
       (s-dot::fontname #+(or :win32 :windows) "Sans"
                        #-(or :win32 :windows) "Arial")
       (s-dot::height "0.01")
       (s-dot::fontcolor ,(cond ((< st-dev prev-st-dev) *black*) ;; *green*
                                ((> st-dev prev-st-dev) *black*)   ;; *red*
                                (t *black*))))
      (s-dot::node ((s-dot::id ,(downcase (mkdotstr (feature-name wd))))
                    (s-dot::label ,(format nil "~a: ~,3f~a ~~ ~,3f~a"
                                           (downcase (mkdotstr (feature-name wd)))
                                           (mean (distribution wd))
                                           (let ((delta (- (mean (distribution wd))
                                                           (mean (distribution previous-wd)))))
                                             (cond ((> delta 0) (format nil " (+~,3f)" (float delta)))
                                                   ((< delta 0) (format nil " (~,3f)" (float delta)))
                                                   (t " (+0.000)")))
                                           st-dev
                                           (let ((delta (- st-dev prev-st-dev)))
                                             (cond ((> delta 0) (format nil " (+~,3f)" (float delta)))
                                                   ((< delta 0) (format nil " (~,3f)" (float delta)))
                                                   (t " (+0.000)")))
                                           )))))))

(defmethod categorical->s-dot-diff ((wd concept-representations::weighted-distribution) (previous-wd concept-representations::weighted-distribution) &key)
  (let* ((current-dist (loop for key being the hash-keys of (cat-table (distribution wd))
                               using (hash-value value)
                             collect (cons key value)))
         (prev-dist (loop for key being the hash-keys of (cat-table (distribution previous-wd))
                            using (hash-value value)
                          collect (cons key value)))
         )
    `(s-dot::record
      ((s-dot::style "rounded")
       (s-dot::fontsize "9.5")
       (s-dot::fontname #+(or :win32 :windows) "Sans"
                        #-(or :win32 :windows) "Arial")
       (s-dot::height "0.01")
       (s-dot::fontcolor ,*black*))
      (s-dot::node ((s-dot::id ,(downcase (mkdotstr (feature-name wd))))
                    (s-dot::label ,(format nil "~a: ~{~a~^, ~}"
                                           (downcase (mkdotstr (feature-name wd)))
                                           (loop for (key . value) in current-dist
                                                 if (> value 0)
                                                   collect (format nil "(~a, ~a)" key value)))))))))
