
(in-package :physical-robot-world)

(defparameter *stroke-width* "20")

(export '(boundary average-y average-u average-v stdev-y stdev-u stdev-v min-y min-u min-v))

(defun color->rgbhex (y cr cb)
  ;; based on http://www.fourcc.org/fccyvrgb.php
  ;; avery lee's jfif clarification
  ;; full byte is used to store colour information
  (format nil "#~2,'0X~2,'0X~2,'0X"
          (floor (min 255 (max 0 (+ y (* 1.402 (- cr 128))))))
          (floor (min 255 (max 0 (- (- y (* 0.34414 (- cb 128))) 
                                    (* 0.71414 (- cr 128))))))
          (floor (min 255 (max 0 (+ y (* 1.772 (- cb 128))))))))

(defun orientation->rad (value)
  (* (- value 0.5) 2 pi))

;;;;; ############################################################
;;;;; physical-robot-world-object->svg
;;;;; ############################################################

(defgeneric physical-robot-world-object->svg (object type &key)
  (:documentation "draws a physical-robot-world-object of type type to svg"))

(defmethod physical-robot-world-object->svg ((r physical-robot-world-object)
                                             (type (eql 'robot)) &key)
  "robot to svg"
  (let ((x (get-fvalue r 'x))
        (y (get-fvalue r 'y))
        (a (orientation->rad (get-fvalue r 'orientation))))
    `((g :stroke "black" :stroke-width ,*stroke-width* :stroke-linecap "round"
         :transform ,(mkstr "translate(" (format nil "~,1f" x) "," 
                            (format nil "~,1f" y) ") "
                            "rotate(" (format nil "~,1f" 
                                              (* 180.0 (/ a pi))) ")"))
      ((line :x1 "0" :y1 "0" :x2 "-200" :y2 "0"))
      ((line :x1 "0" :y1 "0" :x2 "-50" :y2 "30"))
      ((line :x1 "0" :y1 "0" :x2 "-50" :y2 "-30")))))

(defmethod physical-robot-world-object->svg ((r physical-robot-world-object)
                                             (type (eql 'region)) &key)
  "region to svg"
  (let* ((boundary (get-fvalue r 'boundary))
         (avg-y (get-fvalue r 'average-y))
         (avg-u (get-fvalue r 'average-u))
         (avg-v (get-fvalue r 'average-v)))
    `((g) ((path :d ,(format nil "~{~a ~}z"
                             (loop
                              with flat-list = `(M ,(first (first boundary))
                                                   ,(second (first boundary)))
                              for p in (cdr boundary)
                              do (setf flat-list
                                       (append flat-list
                                               `(L ,(first p)
                                                   ,(second p))))
                              finally (return flat-list)))
                 :stroke ,(color->rgbhex avg-y avg-u avg-v)
                 :fill "none"
                 :stroke-width "6")))))

(defmethod physical-robot-world-object->svg ((b physical-robot-world-object)
                                             (type (eql 'box)) &key)
  (let ((x (get-fvalue b 'x))
        (y (get-fvalue b 'y))
        (a (orientation->rad (get-fvalue b 'orientation)))
        (width (get-fvalue b 'width))
        (length (get-fvalue b 'length)))
  `((g :stroke "blue" :stroke-width ,*stroke-width*
       :transform ,(mkstr "translate(" (format nil "~,1f" x) "," 
                          (format nil "~,1f" y) ") "
                          "rotate(" (format nil "~,1f" 
                                            (* 180.0 (/ a pi))) ")"))
     ((rect :x ,(format nil "~,2f" (* -0.5 length))
            :y ,(format nil "~,2f" (* -0.5 width))
            :width ,(format nil "~,2f" length)
            :height ,(format nil "~,2f" width)
            :fill "none"))
         ((line :x1 "0" :y1 "0" 
                :x2 ,(format nil "~,2f" (* 0.5 length))) :y2 "0"))))

(defmethod physical-robot-world-object->svg ((o physical-robot-world-object) (type t) &key)
  (let* ((x (get-fvalue o 'x))
         (y (get-fvalue o 'y))
         (width (get-fvalue o 'width))
         (a (atan y x))
         (r (/ width 2.0))
         (avg-y (get-fvalue o 'average-y))
         (avg-u (get-fvalue o 'average-u))
         (avg-v (get-fvalue o 'average-v)))
    `((g :transform
         ,(mkstr "translate(" (format nil "~,1f" x) "," 
                 (format nil "~,1f" y) ") "
                 "rotate(" (format nil "~,1f" 
                                   (* 180.0 (/ a pi))) ")"))
          ((circle :cx ,(format nil "~,2f" r)
                   :cy ,(format nil "~,2f" 0)
                   :r ,(format nil "~,2f" r)
                   :fill ,(color->rgbhex avg-y avg-u avg-v))))))

;;;;; ############################################################
;;;;; make-svg
;;;;; ############################################################

(defgeneric make-svg (object &key &allow-other-keys)
  (:documentation "draws an object of type type"))

(defmethod make-svg ((o physical-robot-world-object) &key)
  (physical-robot-world-object->svg o (get-type o)))

(defmethod make-svg ((wm physical-robot-world-model) &key)
  `((g)
    ,@(loop for e in (entities wm)
            collect (make-svg e))))

(defmethod make-svg ((rw dynamic-physical-robot-world-model)
                     &key (mode 'opacity)) ;; opacity/path
  "Supports two modes
   opacity - all world models painted on top of each other
   path - draws the first and last world models and path of objects with connected lines"
  (case mode
    (opacity
     `((g)
       ;; draw all world models over each other (first has lowest opacity)
       ,@(loop
          with opacity-step = (/ 1.0 (float (length (world-models rw))))
          with opacity = opacity-step
          for wm in (world-models rw)
          collect `((g :opacity ,(mkstr opacity))
                    ,(make-svg wm))
          do (setf opacity (+ opacity opacity-step)))))
    (path ;; trace path of (objects only)
     (let* ((objects (find-all-if #'object-p
                                  (mappend #'entities (world-models rw))))
            (object-ids (remove-duplicates (mapcar #'id objects))))
     `((g)
       ;; draw first world model
       ((g :opacity "0.1")
        ,(make-svg (first (world-models rw))))
       ;; draw lines
        ,@(loop for id in object-ids
                append
                (loop
                 with objss = (find-all id objects :key #'id)
                 with opacity-step = (/ 1.0 (float (length objss)))
                 with opacity = opacity-step
                 for objs on objss
                 when (< 1 (length objs))
                 collect `((g :opacity ,(mkstr opacity))
                           ((g :stroke ,(color->rgbhex
                                        (get-fvalue (first objs) 'average-y)
                                        (get-fvalue (first objs) 'average-u)
                                        (get-fvalue (first objs) 'average-v))
                              :stroke-width ,*stroke-width* :stroke-linecap "round")
                           ((line :x1 ,(mkstr (get-fvalue (first objs) 'x))
                                  :y1 ,(mkstr (get-fvalue (first objs) 'y))
                                  :x2 ,(mkstr (get-fvalue (second objs) 'x))
                                  :y2 ,(mkstr (get-fvalue (second objs) 'y))))))
                 do (setf opacity (+ opacity opacity-step))))
       ;; draw last world model
       ((g)
        ,(make-svg (car (last (world-models rw))))))))))
                         
;;;;; ############################################################
;;;;; draw-labels
;;;;; ############################################################

(defun draw-labels (wm width height scale cx cy &key (show t))
  (declare (ignore height))
  (loop for e in (entities wm)
        for x = (* -1 scale
                   (if (find-fvalue e 'x)
                     (get-fvalue e 'x)
                     (let ((boundary (get-fvalue e 'boundary)))
                       (average (mapcar #'first boundary)))))
        for y = (* scale
                   (if (find-fvalue e 'y)
                     (get-fvalue e 'y)
                     (let ((boundary (get-fvalue e 'boundary)))
                       (average (mapcar #'second boundary)))))
        for a = (/ pi 2)
        for px = (+ (* (cos a) x)  (* -1 (sin a) y) cx)
        for py = (+ (* (sin a) x)  (* (cos a) y) cy)
        for alignment = (if (> px (/ width 2)) "right" "left")
        when (equal alignment "right")
        do (setf px (- width px))
        collect `((div :class "entity-label" 
                       :style ,(format nil "~a:~,2fpx;top:~,2fpx;~a"
                                       alignment px py
                                       (if show
                                         "display:inline"
                                         "display:none")))
                  ,(html-pprint (id e)))))

;;;;; ############################################################
;;;;; make-html
;;;;; ############################################################

(define-css 'physical-robot-world-model "
div.physical-robot-world-model {position:relative;overflow:hidden;margin-left:auto;margin-right:auto;}
div.physical-robot-world-model > div.entity-label { 
   position:absolute; margin-top:-7px; margin-left:8px;margin-right:8px;
   padding:0px; background-color:#fff;display:inline;
   filter:alpha(opacity=70); -moz-opacity:.70; opacity:.70; }
div.physical-robot-world-model > div.wm-title {
position:absolute; padding:10px; background-color:#fff;top:10px;right:10px;  
filter:alpha(opacity=70); -moz-opacity:.70; opacity:.70; }
")

(defmethod make-html ((wm physical-robot-world-model) &key
                      (width 400)
                      (height 400)
                      (scale 0.1)
                      (cx 200)
                      (cy 200)
                      (show-labels t)
                      (title nil))
  `((div :class "physical-robot-world-model"
         :style ,(format nil  "width:~,2fpx;height:~,2fpx;" width height))
    ;; add svg of wm
    ((svg :xmlns "http://www.w3.org/2000/svg"
          ;; :width "100%"
          ;; :height "100%"
          ;; :viewBox ,(format nil "0 0 ~a ~a" width height)
          :width ,(format nil "~apx" width)
          :height ,(format nil "~apx" height))
     ((g :transform ,(format nil "translate(~a,~a)rotate(90)scale(-~a,~a)" cx cy scale scale))
      ,(make-svg wm)))
    ;; draw labels
    ,@(draw-labels wm width height scale cx cy :show show-labels)
    ;; draw title
    ,@(when title
        `(((div :class "wm-title") ,title)))))

(define-css 'physical-robot-scene "
div.physical-robot-scene { display:inline-block;position:relative;overflow:hidden;}")

(defmethod make-html ((scene physical-robot-scene) &key
                      (wm-width 400)
                      (wm-height 400)
                      (wm-scale 0.1)
                      (wm-cx 200)
                      (wm-cy 200))
  `((div :class "physical-robot-scene")
    ((table)
     ((tr)
      ((td) ((div :class "wm-title") ,(name scene) "/A"))
      ,@(when (wm-b scene)
          `(((td) "&#160;&#160;&#160;&#160;")
            ((td) ((div :class "wm-title") ,(name scene) "/B")))))
     ((tr)
      ((td :style "text-align:center;border:1px dotted #aaa;")
       ,(make-html (wm-a scene)
                   :width wm-width
                   :height wm-height
                   :scale wm-scale
                   :cx wm-cx
                   :cy wm-cy))
      ,@(when (wm-b scene)
          `(((td))
            ((td :style "text-align:center;border:1px dotted #aaa;")
             ,(make-html (wm-b scene)
                         :width wm-width
                         :height wm-height
                         :scale wm-scale
                         :cx wm-cx
                         :cy wm-cy))))))))

(define-css 'physical-robot-world "
div.physical-robot-world { display:inline-block;position:relative;overflow:hidden;}")

(defmethod make-html ((rw physical-robot-world) &key
                      (wm-width 400)
                      (wm-height 400)
                      (wm-scale 0.1)
                      (wm-cx 200)
                      (wm-cy 200))
  `((div :class "physical-robot-world")
    ,@(loop for scene in (scenes rw)
            collect (make-html scene
                               :width wm-width
                               :height wm-height
                               :scale wm-scale
                               :cx wm-cx
                               :cy wm-cy))))

(define-css 'dynamic-physical-robot-world-model "
div.dynamic-physical-robot-world-model {position:relative;overflow:hidden;margin-left:auto;margin-right:auto;}
div.dynamic-physical-robot-world-model > div.entity-label { 
   position:absolute; margin-top:-7px; margin-left:8px;margin-right:8px;
   padding:0px; background-color:#fff;display:inline;
   filter:alpha(opacity=70); -moz-opacity:.70; opacity:.70; }
div.dynamic-physical-robot-world-model > div.wm-title {
position:absolute; padding:10px; background-color:#fff;top:10px;right:10px;  
filter:alpha(opacity=70); -moz-opacity:.70; opacity:.70; }
")

(defmethod make-html ((wm dynamic-physical-robot-world-model) &key
                      (mode 'static) ;; static/dynamic
                      (svg-mode 'path) ;; opacity/path
                      (width 400)
                      (height 400)
                      (scale 0.1)
                      (cx 200)
                      (cy 200)
                      (show-labels t)
                      (title nil) ;; for dynamic, titles are computed from timestamps
                      (id nil))
  "Supports two modes
   static - one div for all world model svg (svg-mode is passed)
   dynamic - one div for every world model svg + one overview div"
  (case mode
    (static
     `((div :class "dynamic-physical-robot-world-model"
            :style ,(format nil  "width:~,2fpx;height:~,2fpx;" width height))
       ;; draw svg
       ((svg :xmlns "http://www.w3.org/2000/svg"
             :width ,(format nil "~apx" width)
             :height ,(format nil "~apx" height))
        ((g :transform
            ,(format nil "translate(~a,~a)rotate(90)scale(-~a,~a)" cx cy scale scale))
         ,(make-svg wm :mode svg-mode)))
       ;; draw labels
       ,@(draw-labels (car (last (world-models wm)))
                      width height scale cx cy :show show-labels)
       ;; draw title
       ,@(when title
           `(((div :class "wm-title") ,title))))) ;; end static
    (dynamic
     ;; draw all wms in divs at the same place,
     ;; give them nice ids for later reference
     `((div :class "dynamic-physical-robot-world-model-container"
            ,@(when id `(:id ,id)) 
            :style
            ,(format nil  "position:relative;width:~,2fpx;height:~,2fpx;" width height))
       ;; draw overview div incl labels
       ((div :name ,(mkstr (id wm) "-overview")
             :timestamp "0"
             :class "dynamic-physical-robot-world-model"
             :style ,(format nil  "position:absolute;width:~,2fpx;height:~,2fpx;"
                             width height))
        ((svg :xmlns "http://www.w3.org/2000/svg"
              :width ,(format nil "~apx" width)
              :height ,(format nil "~apx" height))
         ((g :transform
             ,(format nil "translate(~a,~a)rotate(90)scale(-~a,~a)"
                      cx cy scale scale))
          ,(make-svg wm :mode svg-mode)))
        ,@(draw-labels (first (world-models wm))
                       width height scale cx cy :show show-labels)
        ,@(when title
            `(((div :class "wm-title") ,(mkstr "robot-" (robot wm))))))
       ;; draw every wm in separate div
       ,@(loop
          for m in (world-models wm)
          collect
          `((div :name ,(mkstr (id m))
                 :timestamp ,(mkstr (timestamp m))
                 :class "dynamic-physical-robot-world-model"
                 :style ,(format
                          nil
                          "display:none;position:absolute;width:~,2fpx;height:~,2fpx;"
                          width height))
            ((svg :xmlns "http://www.w3.org/2000/svg"
                  :width ,(format nil "~apx" width)
                  :height ,(format nil "~apx" height))
             ((g :transform
                 ,(format nil "translate(~a,~a)rotate(90)scale(-~a,~a)"
                          cx cy scale scale))
              ,(make-svg m)))
            ,@(draw-labels m width height scale cx cy :show show-labels)
            ,@(when title
                `(((div :class "wm-title") ,(mkstr (timestamp m)))))))))))

(define-css 'dynamic-physical-robot-scene "
div.dynamic-physical-robot-scene { display:inline-block;position:relative;overflow:hidden;}")

(define-js 'dynamic-physical-robot-scene "
var scene_id = [];
var lock = true;
function onRangeChange( id_base, robot)
{
  // change the svg and labels
  id = id_base + '-' + robot;
  value = document.getElementById( id + '-range').value;
  wm = document.getElementById( id + '-wm');
  timestamp = value
  // change svg
  for( n = 0; n < wm.childNodes.length; ++n)
  {
    node = wm.childNodes[n];
    if( n == value)
    {
      node.style.display = 'inline';
      if( value != 0)
      {
        timestamp = parseInt( node.getAttribute('timestamp'));
      }
    }
    else
    {
      node.style.display = 'none';
    }
  }
  // if lock mode change other view as well
  if( lock)
  {
    otherRobot = robot == 'a' ? 'b' : 'a';
    id = id_base + '-' + otherRobot;
    wm = document.getElementById( id + '-wm');
    if( wm == null) // no other robot
    {
      return;
    }
    nodeFound = false;
    for( n = 0; n < wm.childNodes.length; ++n)
    {
      node = wm.childNodes[n];
      nextNode = n + 1 < wm.childNodes.length ? wm.childNodes[n+1] : null;
      d_node = Math.abs( parseInt( node.getAttribute('timestamp') - timestamp));
      d_nextNode = nextNode != null ?
        Math.abs( parseInt( nextNode.getAttribute('timestamp') - timestamp)) : 0;
      if( !nodeFound &&
          (// last node
           nextNode == null 
           // next node has bigger time difference
           || d_node < d_nextNode))
      {
        node.style.display = 'inline';
        document.getElementById( id + '-range').value = n;
        nodeFound = true;
      }
      else
      {
        node.style.display = 'none';
      }
    }
  }
}
")

(defmethod make-html ((scene dynamic-physical-robot-scene) &key
                      (mode 'static) ;; static or dynamic
                      (title nil) ;; add table column for the scene title
                      (wm-title t) ;; show wm title
                      (show-labels t)
                      (wm-width 400)
                      (wm-height 400)
                      (wm-scale 0.1)
                      (wm-cx 200)
                      (wm-cy 200))
  (let* ((unique-id (mkstr (make-id (name scene))))
         (unique-id-a (mkstr unique-id "-a"))
         (unique-id-b (mkstr unique-id "-b")))
    `((div :class "dynamic-physical-robot-scene"
           :id ,unique-id)
      ((table)
       ((tr)
        ,@(when title
            `(((td) ((div :class "scene-title") ,(name scene)))
              ,@(when (wm-b scene)
                `(((td) "&#160;&#160;&#160;&#160;")
                  ((td)))))))
       ((tr)
        ((td :style "text-align:center;border:1px dotted #aaa;")
         ,(make-html (wm-a scene)
                     :mode mode
                     :id (mkstr unique-id-a "-wm")
                     :title wm-title
                     :width wm-width
                     :height wm-height
                     :scale wm-scale
                     :show-labels show-labels
                     :cx wm-cx
                     :cy wm-cy))
        ,@(when (wm-b scene)
            `(((td) "&#160;&#160;&#160;&#160;")
              ((td :style "text-align:center;border:1px dotted #aaa;")
               ,(make-html (wm-b scene)
                           :mode mode
                           :id (mkstr unique-id-b "-wm")
                           :title wm-title
                           :width wm-width
                           :height wm-height
                           :scale wm-scale
                           :cx wm-cx
                           :cy wm-cy
                           :show-labels show-labels)))))
       ,@(when (eq mode 'dynamic)
           `(((tr)
              ((td)
               ((div :class "dynamic-physical-robot-scene-range"
                     :style "text-align:center;border:1px dotted #aaa;width:100%")
                ((input :type "range"
                        :id ,(mkstr unique-id-a "-range")
                        :style "width:100%"
                        :min "0"
                        :value "0"
                        :max ,(mkstr (length (world-models (wm-a scene))))
                        :onchange
                        ,(mkstr "onRangeChange('" unique-id "','a');")))))
              ,@(when (wm-b scene)
                  `(((td))
                    ((td)
                     ((div :class "dynamic-physical-robot-scene-range"
                           :style "text-align:center;border:1px dotted #aaa;")
                      ((input :type "range"
                              :id ,(mkstr unique-id-b "-range")
                              :style "width:100%"
                              :min "0"
                              :value "0"
                              :max ,(mkstr (length (world-models (wm-b scene))))
                              :onchange
                              ,(mkstr "onRangeChange('" unique-id "','b');"))))))))))))))

(define-css 'dynamic-physical-robot-world "
div.dynamic-physical-robot-world { display:inline-block;position:relative;overflow:hidden;}")

(defmethod make-html ((rw dynamic-physical-robot-world) &key
                      (mode 'static) ;; static/dynamic
                      (svg-mode 'path) ;; opacity/path
                      (title nil)
                      (wm-width 400)
                      (wm-height 400)
                      (wm-scale 0.1)
                      (wm-cx 200)
                      (wm-cy 200))
  `((div :class "dynamic-physical-robot-world")
    ,@(loop for scene in (scenes rw)
            collect (make-html scene
                               :mode mode
                               :svg-mode svg-mode
                               :title title
                               :wm-width wm-width
                               :wm-height wm-height
                               :wm-scale wm-scale
                               :wm-cx wm-cx
                               :wm-cy wm-cy))))
