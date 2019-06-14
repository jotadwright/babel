(in-package :clevr-world)

(defparameter *stroke-width* "20")

(defun rgb->rgbhex (rgb)
  "Converts a RGB value to an 8-bit hexadecimal string."
  (format nil "#~{~2,'0X~}" rgb))

(defun clevr-color->rgb (color)
  (case color
    (gray   '(87  87  87))
    (red    '(173 34  35))
    (blue   '(44  76  215))
    (green  '(29  105 20))
    (brown  '(126 72  25))
    (purple '(130 39  192))
    (cyan   '(40  208 208))
    (yellow '(255 238 51))))

;;;;; ############################################################
;;;;; clevr-shape->svg
;;;;; ############################################################

(defgeneric clevr-shape->svg (clevr-object shape)
  (:documentation "Draws a clevr-object of the given shape in SVG"))

;; for now, the material of the objects is not reflected in the SVG

(defmethod clevr-shape->svg ((object clevr-object)
                             (shape (eql 'sphere)))
  (declare (ignorable shape))
  (let* ((x (x-pos object))
         (y (y-pos object))
         (radius (case (size object)
                   (small 15) (large 30)))
         (rgb (clevr-color->rgb (color object))))
    `((g :transform
         ,(mkstr "translate(" (format nil "~,1f" x) ","
                 (format nil "~,1f" y) ")"))
      ((circle :cx ,(format nil "~,2f" radius)
               :cy ,(format nil "~,2f" 0)
               :r ,(format nil "~,2f" radius)
               :fill ,(rgb->rgbhex rgb))))))

(defmethod clevr-shape->svg ((object clevr-object)
                             (shape (eql 'cylinder)))
  (declare (ignorable shape))
  (let* ((x (x-pos object))
         (y (y-pos object))
         (radius (case (size object)
                   (small 15) (large 30)))
         (rgb (clevr-color->rgb (color object))))
    `((g :transform
         ,(mkstr "translate(" (format nil "~,1f" x) ","
                 (format nil "~,1f" y) ")")
         :fill "white"
         :stroke ,(rgb->rgbhex rgb)
         :stroke-width "5")
      ((circle :cx ,(format nil "~,2f" radius)
               :cy ,(format nil "~,2f" 0)
               :r ,(format nil "~,2f" radius))))))

(defmethod clevr-shape->svg ((object clevr-object)
                             (shape (eql 'cube)))
  (declare (ignorable shape))
  (let* ((x (x-pos object))
         (y (y-pos object))
         (width (case (size object)
                  (small 30) (large 60)))
         (rgb (clevr-color->rgb (color object))))
    `((g :transform
         ,(mkstr "translate(" (format nil "~,1f" x) ","
                 (format nil "~,1f" y) ")"
                 "rotate(" (format nil "~,1f" (* 90.0 (/ (rotation object) pi))) ")"))
      ((rect :x ,(format nil "~,2f" (* -0.5 width))
             :y ,(format nil "~,2f" (* -0.5 width))
             :width ,(format nil "~,2f" width)
             :height ,(format nil "~,2f" width)
             :fill ,(rgb->rgbhex rgb))))))

;;;;; ############################################################
;;;;; make-svg
;;;;; ############################################################

(defgeneric make-svg (thing)
  (:documentation "Draws a thing in SVG"))

(defmethod make-svg ((object clevr-object))
  (clevr-shape->svg object (shape object)))

(defmethod make-svg ((scene clevr-scene))
  `((g)
    ,@(loop for object in (objects scene)
            collect (make-svg object))))

;;;;; ############################################################
;;;;; draw-labels
;;;;; ############################################################

(defun draw-labels (scene width &key (show t))
  (declare (ignore height))
  (loop for object in (objects scene)
        for x = (x-pos object)
        for y = (y-pos object)
        for alignment = (if (> x (/ width 2)) "right" "left")
        collect `((div :class "entity-label" 
                       :style ,(format nil "~a:~,2fpx;top:~,2fpx;~a"
                                       alignment
                                       (if (equal alignment "right")
                                         (- width x)
                                         x)
                                       y
                                       (if show
                                         "display:inline"
                                         "display:none")))
                  ,(html-pprint (id object)))))

;;;;; ############################################################
;;;;; program->s-dot
;;;;; ############################################################

(defun better-function-name (name)
  (if (string= name "scene")
    "get-context"
    (if (find #\_ name)
      (let ((parts (split name #\_)))
        (cond ((or (string= (first parts) "filter")
                   (string= (first parts) "query")
                   (string= (first parts) "same"))
               (first parts))
              ((string= (first parts) "equal")
               (if (string= (second parts) "integer")
                 name
                 (first parts)))
              (t name)))
      name)))

(defun function->s-dot (function)
  `(s-dot::record  
    ((s-dot::color "#ffffff")
     (s-dot::fontsize "8.5")
     (s-dot::fontname #+(or :win32 :windows) "Sans"
                      #-(or :win32 :windows) "Arial")
     (s-dot::height "0.01"))
    (s-dot::node ((s-dot::id ,(mkdotstr (id function)))
                  (s-dot::label ,(mkdotstr
                                  (format nil "~a(~{~a~^,~})"
                                          (better-function-name (function-name function))
                                          (args function))))))))

(defun edge->s-dot (from to)
  `(s-dot::edge ((s-dot::from ,(mkdotstr (id from)))
                 (s-dot::to ,(mkdotstr (id to)))
                 (s-dot::dir "back")
                 (s-dot::arrowsize "0.5"))))

(defun program->s-dot (program)
  (let ((graph '(((s-dot::ranksep "0.3")
                  (s-dot::nodesep "0.5")
                  (s-dot::margin "0")
                  (s-dot::rankdir "LR"))
                 s-dot::graph)))
    (loop for node in (reverse (nodes program))
          do (push (function->s-dot node) graph))
    (loop for node in (reverse (nodes program))
          do (loop for child in (children node)
                   do (push (edge->s-dot node child) graph)))
    (reverse graph)))

;;;;; ############################################################
;;;;; make-html
;;;;; ############################################################

(defmethod make-html-for-entity-details ((object clevr-object) &key)
  `(((div :class "entity-detail") ,(format nil "size: ~a" (downcase (mkstr (size object)))))
    ((div :class "entity-detail") ,(format nil "color: ~a" (downcase (mkstr (color object)))))
    ((div :class "entity-detail") ,(format nil "material: ~a" (downcase (mkstr (material object)))))
    ((div :class "entity-detail") ,(format nil "shape: ~a" (downcase (mkstr (shape object)))))))

(defmethod make-html-for-entity-details ((q clevr-question) &key)
  (let ((expand-program-id (make-id)))
    `(((div :class "entity-detail")
       ,(make-expandable/collapsable-element
         expand-program-id (make-id)
         ;; collapsed
         `((a ,@(make-expand/collapse-link-parameters expand-program-id t)) "View Program")
         ;; expanded
         (lambda ()
           `((table)
             ((tr)
              ((td)
               ((a ,@(make-expand/collapse-link-parameters expand-program-id nil)) "Hide Program")))
             ((tr)
              ((td)
               ,(s-dot->svg (program->s-dot (program q)))))))))
      ((div :class "entity-detail")
       ((b) "Q: ") ((i) ,(mkstr (question q))))
      ((div :class "entity-detail" :style "text-align:right;")
       ((b) "A: ") ((i) ,(mkstr (answer q)))))))

(define-css 'clevr-scene "
div.clevr-scene {
   position:relative;overflow:hidden;
   argin-left:auto;margin-right:auto; }
div.clevr-scene > div.entity-label { 
   position:absolute; margin-top:-7px; margin-left:8px;margin-right:8px;
   padding:0px; background-color:#fff;display:inline;
   filter:alpha(opacity=70); -moz-opacity:.70; opacity:.70; }
div.clevr-scene > div.scene-title {
   position:absolute; padding:10px; background-color:#fff;top:10px;right:10px;  
   filter:alpha(opacity=70); -moz-opacity:.70; opacity:.70; }
")

(define-css 'clevr-scene-table "
div.clevr-scene-table {
    display:inline-block;position:relative;overflow:hidden;}
")

(defmethod make-html-for-entity-details ((set clevr-object-set) &key)
  ;; table of objects
  `(((div :class "entity-detail") 
     ,@(loop for object in (objects set)
             collect (make-html object :expand-initially t)))))

(defmethod make-html-for-entity-details ((scene clevr-scene)
                                         &key (width 500)
                                         (height 300)
                                         (show-labels t))
  (let ((expand-scene-id (make-id 'scene))
        (expand-all-id (make-id 'expand-all)))
    (append
     `(((div :class "clevr-scene-table")
        ,(make-expandable/collapsable-element
          expand-scene-id expand-all-id
          ;; collapsed
          `((table)
            ((tr)
             ((td)
              ((a ,@(make-expand/collapse-link-parameters expand-scene-id t)) "View Scene"))))
          ;; expanded
          (lambda ()
            `((table)
              ((tr)
               ((td)
                ((a ,@(make-expand/collapse-link-parameters expand-scene-id nil)) "Hide Scene")))
              ((tr)
               ((td) ,(format nil "~a" (name scene))))
              ((tr)
               ((td :style "text-align:center;border:1px dotted #aaa;")
                ((div :class "clevr-scene"
                      :style ,(format nil  "width:~,2fpx;height:~,2fpx;" width height))
                 ;; add svg of wm
                 ((svg :xmlns "http://www.w3.org/2000/svg"
                       :width ,(format nil "~apx" width)
                       :height ,(format nil "~apx" height))
                  ((g)
                   ,(make-svg scene)))
                 ;; draw labels
                 ,@(draw-labels scene width :show show-labels)))))))))
     ;; table of objects
     `(((div :class "entity-detail") 
        ,@(loop for object in (objects scene)
                collect (make-html object :expand-initially t)))))))

(defmethod make-html-for-entity-details ((set clevr-question-set) &key)
  `(((div :class "entity-detail")
     ,@(loop for q in (questions set)
             collect (make-html q :expand-initially t)))))

