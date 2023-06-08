(in-package :cle)

;; ----------------
;; + cxn -> s-dot +
;; ----------------

(defparameter *white* "#FFFFFF")
(defparameter *red* "#E32D2D")
(defparameter *green* "#26AD26")
(defparameter *black* "#000000")

(defgeneric cxn->s-dot-diff (cxn delta &key highlight-green highlight-red certainty-threshold)
  (:documentation "Display a cxn using s-dot."))

(defmethod cxn->s-dot-diff ((cxn cxn) (previous-copy cxn) &key highlight-green highlight-red (certainty-threshold 0.1))
  (let ((g '(((s-dot::ranksep "0.3")
              (s-dot::nodesep "0.5")
              (s-dot::margin "0")
              (s-dot::rankdir "LR"))
             s-dot::graph)))
    ;; cxn node
    (push
     `(s-dot::record  
       ((s-dot::style "filled")
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
       ((s-dot::style "solid")
        (s-dot::fontsize "9.5")
        (s-dot::fontcolor ,*black*)
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id ,(mkdotstr (form cxn)))
                     (s-dot::label ,(downcase (mkdotstr (form cxn))))
                     (s-dot::fontcolor "#AA0000"))))
     g)

    ;; feature-channels nodes
    (loop for prototype in (prototypes (meaning cxn))
          for previous-prototype in (prototypes (meaning previous-copy))
          for record = (prototype->s-dot-diff prototype
                                              previous-prototype
                                              :green (member (channel prototype) highlight-green)
                                              :red (member (channel prototype) highlight-red))
          when (> (weight prototype) 0.1)
            do (push record g))
    ;; edges between cxn node and feature-channels
    (loop for prototype in (prototypes (meaning cxn))
          for previous-prototype in (prototypes (meaning previous-copy))
          for delta = (- (weight prototype) (weight previous-prototype))
          when (> (weight previous-prototype) 0.1)
            do (push
                `(s-dot::edge
                  ((s-dot::from ,(mkdotstr (id (meaning cxn))))
                   (s-dot::to ,(mkdotstr (downcase (channel prototype))))
                   (s-dot::label ,(format nil "~,2f~a" (float (weight prototype)) (cond ((> delta 0) (format nil " (+~,2f)" (float delta)))
                                                                                        ((< delta 0) (format nil " (~,2f)" (float delta)))
                                                                                        (t ""))))
                   (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                         #-(or :win32 :windows) "Arial")
                   (s-dot::fontcolor ,(cond ((> delta 0) *green*)
                                            ((< delta 0) *red*)
                                            (t *black*)))
                                             
                   (s-dot::fontsize "8.5")
                   (s-dot::arrowsize "0.5")
                   (s-dot::style ,(if (>= (weight prototype) 0.99) "solid" "dashed"))))
                g))
    ;; edge between form node and cxn node
    (push `(s-dot::edge
            ((s-dot::from ,(downcase (mkdotstr (form cxn))))
             (s-dot::to ,(mkdotstr (id (meaning cxn))))
             (s-dot::label ,(format nil "~,2f~a - (~a/~a , ~a/~a)"
                                    (float (score cxn))
                                    (let ((delta (- (score cxn) (score previous-copy))))
                                      (cond ((> delta 0) (format nil " (+~,2f)" (float delta)))
                                            ((< delta 0) (format nil " (~,2f)" (float delta)))
                                            (t "")))
                                    0 0 0 0))
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

(defgeneric prototype->s-dot-diff (prototype previous-prototype &key green red)
  (:documentation "Display a prototype using s-dot."))

(defmethod prototype->s-dot-diff ((prototype prototype) (previous-prototype prototype) &key green red)
  (let* ((st-dev (st-dev (distribution prototype)))
         (prev-st-dev (st-dev (distribution previous-prototype)))
         )
    `(s-dot::record
      ((s-dot::fontsize "9.5")
       (s-dot::fontname #+(or :win32 :windows) "Sans"
                        #-(or :win32 :windows) "Arial")
       (s-dot::height "0.01")
       (s-dot::fontcolor ,(cond ((< st-dev prev-st-dev) *green*)
                                ((> st-dev prev-st-dev) *red*)
                                (t *black*))))
      (s-dot::node ((s-dot::id ,(downcase (mkdotstr (channel prototype))))
                    (s-dot::label ,(format nil "~a: ~,3f~a ~~ ~,3f~a"
                                           (downcase (mkdotstr (channel prototype)))
                                           (mean (distribution prototype))
                                           (let ((delta (- (mean (distribution prototype)) (mean (distribution previous-prototype)))))
                                             (cond ((> delta 0) (format nil " (+~,3f)" (float delta)))
                                                   ((< delta 0) (format nil " (~,3f)" (float delta)))
                                                   (t " (+0.000)")))
                                           st-dev
                                           (let ((delta (- st-dev prev-st-dev)))
                                             (cond ((> delta 0) (format nil " (+~,3f)" (float delta)))
                                                   ((< delta 0) (format nil " (~,3f)" (float delta)))
                                                   (t " (+0.000)")))
                                           )))))))
