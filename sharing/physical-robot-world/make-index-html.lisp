(ql:quickload :physical-robot-world)

(in-package :physical-robot-world)

;;;;; ############################################################
;;;;; space-game/grounded-naming-game index html creation
;;;;; ############################################################

(define-css 'physical-robot-scene-index-html "
div.index-menu {position:fixed;left:10px;right:1050px;top:0px}
div.index-menu-title { font-weight:bold;font-size:12px;margin-top:15px; padding-top:0px; margin-bottom:5px;}
div.index-menu-items { font-size:10px;}
div.scenes {float:right;padding:5px}
div.wms-title {font-weight: bold;}
tr.wm-lisps {display:none;vertical-align:top;}
td.wm-lisp {text-align:left;border:1px dotted #aaa;}
td.wm-image {border:1px dotted #aaa;}
div.entity-label {display:none;}
pre {margin-top:5px;margin-bottom:5px;margin-left:5px}")

(define-js 'physical-robot-world-index-html "
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

(defmethod make-index-html ((robot-world physical-robot-world) &key
                            (wm-width 400)
                            (wm-height 400)
                            (wm-scale 0.1)
                            (wm-cx 200)
                            (wm-cy 300)
                            (wm-lisp-format "~:w"))
  `((div)
    ((div :class "index-menu")
     ((div :class "index-menu-title")
      ,(format nil "~{~a ~}" (data-sets robot-world)))
     ((div :class "index-menu-buttons")
      ((a :id "a-lisp" :href "javascript:onLispOnOffClick()") "lisp on")
      ((br))
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
                ((td :class "wm-image")
                 ,(make-html (wm-a scene)
                             :width wm-width
                             :height wm-height
                             :scale wm-scale
                             :cx wm-cx
                             :cy wm-cy
                             :title (mkstr "robot-" (robot (wm-a scene)))
                             :show-labels nil))
                ((td))
                ((td :class "wm-image")
                 ,(make-html (wm-b scene)
                             :width wm-width
                             :height wm-height
                             :scale wm-scale
                             :cx wm-cx
                             :cy wm-cy
                             :title (mkstr "robot-" (robot (wm-b scene)))
                             :show-labels nil)))
              collect
              `((tr :class "wm-lisps")
                ((td :class "wm-lisp")
                 ((pre)
                  ,(let ((wm-lisp (read-from-string
                                   (with-open-file
                                       (s (namestring
                                           (merge-pathnames
                                            (make-pathname :name (mkstr (robot (wm-a scene)))
                                                           :type "lisp")
                                            (source-path (wm-a scene)))))
                                     (let* ((len (file-length s))
                                            (data (make-string len)))
                                       (read-sequence data s)
                                       data)))))
                     (format nil wm-lisp-format wm-lisp))))
                ((td))
                ((td :class "wm-lisp")
                 ((pre)
                  ,(let ((wm-lisp (read-from-string
                                   (with-open-file
                                       (s (namestring
                                           (merge-pathnames
                                            (make-pathname :name (mkstr (robot (wm-b scene)))
                                                           :type "lisp")
                                            (source-path (wm-b scene)))))
                                     (let* ((len (file-length s))
                                            (data (make-string len)))
                                       (read-sequence data s)
                                       data)))))
                     (format nil wm-lisp-format wm-lisp))))))))))

;; remove some static java script
;; note: you will have to restart your lisp or evaluate systems/web-interface/html-utils.lisp
;;  afterwards
(define-js 'wi::expand/collapse "")
(wi::define-static-js 'wi::expand/collapse "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define data sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *data-sets* (list "diss-scenes"))
  (loop for set from 10 to 18
                        collect (list (mkstr "space-game-" set))))

(progn
  ;; create index html (w checks)
  (loop
   for data-set in *data-sets*
   for world =  (make-instance 'physical-robot-world
                               :data-sets (list data-set)
                               :warn t
                               :configuration (make-configuration))
   do
   (format t "~%Number of scenes loaded ~a." (length (scenes world)))
   (let ((wi::*static-html-output-file-name*
          (make-pathname :name "index" :type "xhtml"))
         (wi::*static-html-output-dir*
          (merge-pathnames
           (make-pathname :directory (list :relative data-set))
           *robotdata-path*))
         (wi::*static-html-include-async-request-handler* nil))
     (clear-page)
     (create-static-html-page data-set
       (add-element
        (make-index-html world
                         :wm-width 500
                         :wm-cx 250)))))

  ;; create index all html (w/o checks)
  (loop
   for data-set in *data-sets*
   for world =  (make-instance 'physical-robot-world
                               :data-sets (list data-set)
                               :warn nil
                               :check-scene-methods '()
                               :configuration (make-configuration))
   do
   (format t "~%Number of scenes loaded ~a." (length (scenes world)))
   (let
       ((wi::*static-html-output-file-name*
         (make-pathname :name "index-all" :type "xhtml"))
        (wi::*static-html-output-dir*
         (merge-pathnames
          (make-pathname :directory (list :relative data-set))
          *robotdata-path*))
        (wi::*static-html-include-async-request-handler* nil))
     (clear-page)
     (create-static-html-page data-set
       (add-element
        (make-index-html world
                         :wm-width 500
                         :wm-cx 250)))))

  ;; create index html for different check failurees
  (loop for mode in '(pointing-fails different-nr-of-objects)
        do
        (loop
         for data-set in *data-sets*
         for world = (make-instance 'physical-robot-world
                                    :data-sets (list data-set)
                                    :warn nil
                                    :check-scene-methods (list mode)
                                    :configuration (make-configuration))
         do
         (format t "~%Number of scenes loaded ~a." (length (scenes world)))
         (clear-page)
         (let* ((wi::*static-html-include-async-request-handler* nil)
                (wi::*static-html-output-file-name*
                 (make-pathname :name (format nil "index-~(~a~)" mode)
                                :type "xhtml"))
                (wi::*static-html-output-dir* (merge-pathnames
                                               (make-pathname
                                                :directory (list :relative data-set))
                                               *robotdata-path*)))
           (create-static-html-page
               (format nil "~a-~(~a~)" data-set mode)
             (add-element
              (make-index-html world
                               :wm-width 500
                               :wm-cx 250)))))))





