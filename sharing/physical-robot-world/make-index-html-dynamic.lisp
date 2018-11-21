(ql:quickload :physical-robot-world)

(in-package :physical-robot-world)



;;;;; ############################################################
;;;;; dynamic world models (movement-box-...,movement-regions-...)
;;;;; ############################################################

;; 0) choose the data-set to be processed
(defparameter *data-set* "movement-description-video-1") ;; only one at a time please
(defparameter *width* 400)
(defparameter *height* 300)
(defparameter *cx* 200)
(defparameter *cy* 250)
(defparameter *scale* 0.1)

;; 1) create directory for svg's --> this is your problem,
;; but you can use "ls | xargs -I {} mkdir {}/svg"
;; or at your own risk you can use the following (only lispworks tested)
(let ((path
       (make-pathname :directory (append (pathname-directory cl-user::*babel-path*)
                                         (list ".." "robotdata" *data-set*)))))
  (run-prog
   (format nil "find  ~a -depth 1 -type d | xargs -I {} mkdir {}/svg" 
           (subseq (format nil "~a" path)
                   0
                   (- (length (format nil "~a" path)) 0)))))
               
;; 2) create svgs in the subfolder (set width, height, cx, cy and scale)
;;  NOTE: this really creates files, so use with care!
(loop
 with width = *width*
 with height = *height*
 with cx = *cx*
 with cy = *cy*
 with scale = *scale*
 with world = (make-instance 'dynamic-physical-robot-world
                             :data-sets (list *data-set*))
 for scene in (scenes world)
 for path = (source-path scene)
 for target-path = (merge-pathnames (make-pathname :directory (list :relative "svg")) path)
 for wms = (list (wm-a scene) (wm-b scene))
 do (loop for wm in wms
          do (loop for m in (world-models wm)
                   for file-name = (merge-pathnames
                                    (make-pathname
                                     :name (mkstr (robot m) "-" (timestamp m))
                                     :type "svg")
                                    target-path)
                   do
                   (progn
                     (pp "export:" file-name)
                     (with-open-file (stream (namestring file-name) :direction :output
                                             :if-exists :supersede)
                       (format stream (render-xml
                                       `((svg :xmlns "http://www.w3.org/2000/svg"
                                              :width ,(format nil "~apx" width)
                                              :height ,(format nil "~apx" height))
                                         ((g :transform
                                             ,(format nil
                                                      "translate(~a,~a)rotate(90)scale(-~a,~a)"
                                                      cx cy scale scale))
                                          ,(make-svg m :mode 'path))))))
                     (pp "done" file-name)))))

;; 3) overwrite the make-html for dynamic-physical-robot-world
(defmethod make-html ((wm dynamic-physical-robot-world-model) &key
                      (mode 'static) ;; static/dynamic
                      (svg-mode 'path) ;; opacity/path
                      (width *width*)
                      (height *height*)
                      (scale *scale*)
                      (cx *cx*)
                      (cy *cy*)
                      (show-labels t)
                      (title nil) ;; for dynamic, titles are computed from timestamps
                      (id nil))
  (declare (ignorable mode))
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
    ;; draw every wm in separate div img
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
         ;; add image container for wm
         ((div :class "image-container"
               :style ,(format nil "width:~a;height:~a;" width height)
               :image-src ,(namestring
                            (merge-pathnames
                             (make-pathname
                              :name (mkstr (robot wm) "-" (timestamp m))
                              :type "svg")
                             (make-pathname :directory (list :relative
                                                             (car (last (pathname-directory
                                                                         (source-path wm))))
                                                             "svg"))))
               :loaded-once "false"))
         ,@(draw-labels m width height scale cx cy :show show-labels)
         ,@(when title
             `(((div :class "wm-title") ,(mkstr (timestamp m)))))))))

;; 4) overwrite the javascript handling for scenes to load static images
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
      if( n != 0)
      {
        timestamp = parseInt( node.getAttribute('timestamp'));
        image_container = node.getElementsByClassName('image-container')[0];
        image_src = image_container.getAttribute('image-src');
        image_container.innerHTML = '<img src=\\\"' + image_src + '\\\" alt=\\\"hi\\\" width=\\\"100%\\\" height=\\\"100%\\\" />';
      }
    }
    else
    {
      if( n != 0)
      {
        image_container = node.getElementsByClassName('image-container')[0];
        // pre load all other images on first range change
        if( image_container.getAttribute('loaded-once') == 'false')
        {
          image_src = image_container.getAttribute('image-src');
          image_container.innerHTML = '<img src=\\\"' + image_src + '\\\" width=\\\"100%\\\" height=\\\"100%\\\" style=\\\"display:none;\\\"/>';
          image_container.setAttribute('loaded-once', 'true');
        }
        image_container.innerHTML = '';
      }
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
      image_container = node.getElementsByClassName('image-container')[0];
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
        if( n != 0)
        {
          image_container = node.getElementsByClassName('image-container')[0];
          image_src = image_container.getAttribute('image-src');
          image_container.innerHTML = '<img src=\\\"' + image_src  + '\\\" width=\\\"100%\\\" height=\\\"100%\\\">image</img>';
        }
      }
      else
      {
        if( n != 0)
        {
          image_container = node.getElementsByClassName('image-container')[0];
          image_container.innerHTML = '';
          // pre load all other images on first range change
          if( image_container.getAttribute('loaded-once') == 'false')
          {
            image_src = image_container.getAttribute('image-src');
            image_container.innerHTML = '<img src=\\\"' + image_src + '\\\" width=\\\"100%\\\" height=\\\"100%\\\" style=\\\"display:none;\\\"/>';
            image_container.setAttribute('loaded-once', 'true');
          }
        }
        node.style.display = 'none';
      }
    }
  }
}
")

;; 5) index html js definition
(define-js 'dynamic-physical-robot-world-index-html "
var entityLabelsOn = false;
function onLabelsOnOffClick()
{
  entityLabelsOn = !entityLabelsOn;
  var nodes = document.getElementsByClassName(\"entity-label\");
  for(var i=0;i<nodes.length;i++) {
    if ( entityLabelsOn) {
      nodes[i].style.display=\"inline\";
    } else {
      nodes[i].style.display=\"none\";
    }
  }
  if( entityLabelsOn)
  {
   document.getElementById(\"a-labels\").innerHTML = \"labels off\";
  }
  else
  {
   document.getElementById(\"a-labels\").innerHTML = \"labels on\"; 
  }
}
var lispOn = false;
function onLispOnOffClick()
{
  lispOn = !lispOn;
  var nodes = document.getElementsByClassName(\"wm-lisps\");
  for(var i=0;i<nodes.length;i++) {
    if ( lispOn) {
      nodes[i].style.display=\"table-row\";
    } else {
      nodes[i].style.display=\"none\";
    }
  }
  if( lispOn)
  {
   document.getElementById(\"a-lisp\").innerHTML = \"lisp off\";
  }
  else
  {
   document.getElementById(\"a-lisp\").innerHTML = \"lisp on\"; 
  }

}
")

;; 6) index.html definition
(define-css 'dynamic-physical-robot-scene-index-html "
div.index-menu {position:fixed;left:10px;right:850px;top:0px;display:inline}
div.index-menu-title { font-weight:bold;font-size:12px;margin-top:15px; padding-top:0px; margin-bottom:5px;}
div.index-menu-items { font-size:10px;}
div.scenes {float:right;padding:5px}
div.wms-title {font-weight: bold;}
tr.wm-lisps {display:none;}
td.wm-lisp {text-align:left;border:1px dotted #aaa;}
td.wm-image {border:1px dotted #aaa;}
div.entity-label {display:none;}")

(defmethod make-index-html ((robot-world dynamic-physical-robot-world) &key
                            (wm-width 400)
                            (wm-height 400)
                            (wm-scale 0.1)
                            (wm-cx 200)
                            (wm-cy 300))
  `((div)
    ((div :class "index-menu")
     ((div :class "index-menu-title")
      ,(format nil "~{~a ~}" (data-sets robot-world)))
     ((div :class "index-menu-buttons")
      ((a :id "a-labels" :href "javascript:onLabelsOnOffClick()") "labels on"))
     ((div :class "index-menu-items") ,@(loop for scene in (scenes robot-world)
                                              collect
                                              `((a :href ,(format nil "#~a" (name scene)))
                                                ,(concatenate 'string (subseq (name scene) 11)
                                                              " ")))))
    ((div :class "scenes")
     ((table)
      ,@(loop for scene in (scenes robot-world)
              for i from 1
              when (< 1 i)
              collect
              `((tr)
                ((div :style "height:10px;" )))
              collect
              `((tr)
                ((td) ((div :id ,(name scene)
                            :class "wms-title")
                       ,(name scene))))
              collect
              `((tr :class "wm-images")
                ,(make-html scene
                            :mode 'dynamic
                            :title nil
                            :wm-width wm-width
                            :wm-height wm-height
                            :wm-scale wm-scale
                            :wm-cx wm-cx
                            :wm-cy wm-cy
                            :wm-title t
                            :show-labels nil)))))))

;; 7) create index-html
(progn
  (setf *world* (make-instance 'dynamic-physical-robot-world
                               :data-sets (list *data-set*)))
  (let* ((title *data-set*)
         (wi::*static-html-output-dir* (merge-pathnames
                                        (make-pathname
                                         :directory (list :relative *data-set*))
                                        *robotdata-path*))
         (html-file (merge-pathnames 
                     wi::*static-html-output-dir*
                     (make-pathname :name "index" :type "xhtml"))))
    (setf wi::*static-elements* nil)
    (clear-page)
    (ensure-directories-exist wi::*static-html-output-dir*)
    (print wi::*static-html-output-dir*)
    (let ((wi::*static-html* t))
      (add-element (make-index-html *world*
                                    :wm-width *width*
                                    :wm-height *height*
                                    :wm-scale *scale*
                                    :wm-cx *cx*
                                    :wm-cy *cy*)))
    (with-open-file (stream html-file :direction :output :if-exists :supersede)
      ;;(wi::write-static-page stream title ;;:include-static-js nil
      ;;                       :include-async-src nil)
      ;;(force-output stream)
      (princ "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" stream)
      (princ (render-xml
              `((html :xmlns "http://www.w3.org/1999/xhtml")
                ((head)
                 ((title) ,title)
                 ,(wi::get-combined-js-definitions)
                 ,(wi::get-combined-css-definitions))
                ((body)
                 ,@(reverse wi::*static-elements*))))
             stream))
       
    (print html-file) (princ #\newline)
    (run-prog "open" :args (list (mkstr html-file)))
    html-file))
