(in-package :cle)

;; ----------------
;; + cxn -> s-dot +
;; ----------------

(defgeneric cxn->s-dot (cxn &key highlight-green highlight-red certainty-threshold)
  (:documentation "Display a cxn using s-dot."))

(defmethod cxn->s-dot ((cxn cxn) &key highlight-green highlight-red (certainty-threshold 0.1))
  (let ((g '(((s-dot::ranksep "0.3")
              (s-dot::nodesep "0.5")
              (s-dot::margin "0")
              (s-dot::rankdir "LR"))
             s-dot::graph)))
    ;; cxn node
    (push
     `(s-dot::record  
       ((s-dot::style "filled")
        (s-dot::fillcolor ,(get-hex-color cxn))
        ;(s-dot::fillcolor "#FFFFFF")
        (s-dot::fontcolor ,*black*)
        (s-dot::fontsize "9.5")
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id ,(mkdotstr (id (meaning cxn))))
                     (s-dot::label ,(format nil "~a [n: ~a, l: ~a]"
                                            (mkdotstr (id (meaning cxn)))
                                            (length (history cxn))
                                            (first (history cxn))))
                     (s-dot::fontcolor "#AA0000"))))
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
    (loop for prototype in (reverse (prototypes (meaning cxn)))
          for record = (prototype->s-dot prototype
                                         :green (member (channel prototype) highlight-green)
                                         :red (member (channel prototype) highlight-red))
          when (>= (weight prototype) 0.1)
            do (push record g))
    ;; edges between cxn node and feature-channels
    (loop for prototype in (prototypes (meaning cxn))
          when (>= (weight prototype) 0.1)
            do (push
                `(s-dot::edge
                  ((s-dot::from ,(mkdotstr (id (meaning cxn))))
                   (s-dot::to ,(mkdotstr (downcase (channel prototype))))
                   (s-dot::label ,(format nil "~,2f" (weight prototype)))
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
             (s-dot::label ,(format nil "~,2f-(~a/~a , ~a/~a)"
                                    (score cxn)
                                    0 0 0 0))
             (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                   #-(or :win32 :windows) "Arial")
             (s-dot::dir "both")
             (s-dot::fontsize "8.5")
             (s-dot::arrowsize "0.5")
             (s-dot::fontcolor ,*black*)
             (s-dot::style ,(if (>= (score cxn) 0.99) "solid" "dashed")))) g)
    ;; return
    (reverse g)))


(defgeneric prototype->s-dot (prototype &key green red)
  (:documentation "Display a prototype using s-dot."))

(defmethod prototype->s-dot ((prototype prototype) &key green red)
  (let* ((st-dev (st-dev (distribution prototype)))
         (record-properties
          (cond (green '((s-dot::style "filled")
                         (s-dot::fillcolor "#AAFFAA")))
                (red '((s-dot::style "filled")
                       (s-dot::fillcolor "#AA0000")
                       (s-dot::fontcolor "#FFFFFF")))
                (t (if (= (weight prototype) 1.0)
                     '((s-dot::style "solid"))
                     '((s-dot::style "dashed")))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::fontcolor "#000000")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(downcase (mkdotstr (channel prototype))))
                    (s-dot::label ,(format nil "~a: ~,3f ~~ ~,3f"
                                           (downcase (mkdotstr (channel prototype)))
                                           (mean (distribution prototype))
                                           st-dev)))))))

(defmethod get-hex-color (cxn &key (threshold 0.9))
  "Calculate the prototypical color of a cxn."
  (let ((r (loop for prototype in (prototypes (meaning cxn))
                 when (equal (channel prototype) 'R)
                   return (cons (round (* 255 (mean (distribution prototype)))) (weight prototype))))
        (g (loop for prototype in (prototypes (meaning cxn))
                 when (equal (channel prototype) 'G)
                   return (cons (round (* 255 (mean (distribution prototype)))) (weight prototype))))
        (b (loop for prototype in (prototypes (meaning cxn))
                 when (equal (channel prototype) 'B)
                   return (cons (round (* 255 (mean (distribution prototype)))) (weight prototype)))))
    (if (and (> (rest r) threshold) (> (rest g) threshold) (> (rest b) threshold))
      (rgb->rgbhex (list (first r) (first g) (first b)))
      (rgb->rgbhex (list 255 255 255)))))

(defun rgb->rgbhex (rgb)
  "Converts a RGB [0,1] value to an 8-bit hexadecimal string."
  (format nil "#~{~2,'0X~}" rgb))