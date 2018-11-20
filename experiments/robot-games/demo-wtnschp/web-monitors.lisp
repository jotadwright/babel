;;;; /monitors/web-monitors.lisp

(in-package :demo-wtnschp)

;; ---------------------------------------
;; + Trace Interactions in Web Interface +
;; ---------------------------------------

(define-monitor trace-interaction-in-web-interface)

;; + Processes +
(define-event-handler (trace-interaction-in-web-interface interaction-started)
  (add-element '((hr)))
  (add-element `((h1) ,(format nil "Interaction ~a"
                               (interaction-number interaction))))
  (add-element '((hr)))
  (let ((agent (first (interacting-agents interaction))))
    (add-element `((h2) "The agent is the "
                   ((b) ,(discourse-role agent))))
    (when (find-data (ontology agent) 'color-categories)
      (add-element (draw-3D-colour-set (mapcar #'prototype (find-data (ontology agent) 'color-categories)))))
    (when (constructions (grammar agent))
      (let ((mappings (grammar->mappings agent)))
        (add-element '((h2) "The agent's lexicon:"))
        (add-element (make-html mappings :expand-initially t))
        (add-element '((hr)))))))

(define-event-handler (trace-interaction-in-web-interface interaction-finished)
  (add-element '((hr)))
  (add-element `((h2) "Interaction "
                 ,(if (communicated-successfully interaction)
                    `((b :style "color:green") "succeeded")
                    `((b :style "color:red") "failed")))))

(define-event-handler (trace-interaction-in-web-interface observe-scene-finished)
  (let ((source-path (babel-pathname :directory '(".tmp" "nao-img")
                                     :name img-path
                                     :type "jpg"))
        (dest-path (pathname "~/Sites/")))
    (copy-file source-path dest-path)
    (if (null cl-user::*localhost-user-dir*)
      (warn "Set your *localhost-user-dir* in init-babel")
      (progn
        (add-element `((table)
                       ((tr)
                        ((th :align "center") "Observed scene:"))
                       ((tr)
                        ((td :align "center") ((img :src ,(string-append cl-user::*localhost-user-dir* (format nil "~a.jpg" img-path)) :width "90%"))))))
        (add-element `((table)
                 ((tr)
                  ((th :align "center") "Processed scene:"))
                 ((tr)
                  ((td :align "center") ,(make-html scene :expand-initially t)))))))))

(define-event-handler (trace-interaction-in-web-interface choose-topic-finished)
  (add-element `((h3) "The topic is " ((i) ,(format nil "~a" topic-id)))))

(define-event-handler (trace-interaction-in-web-interface speech-input-finished)
  (add-element `((h3) "The agent understood the word \"" ((i) ,utterance "\""))))

(defun show-lexicon (agent)
  (unless (null (constructions (grammar agent)))
    (let* ((all-words (loop for cxn in (constructions (grammar agent))
                            collect (attr-val cxn :form) into forms
                            finally (remove nil (remove-duplicates forms :test #'string=))))
           (all-categories (loop for color-category in (find-data (ontology agent) 'color-categories)
                                 collect (id color-category) into meanings
                                 finally (remove nil (remove-duplicates meanings))))
           (all-links (loop for cxn in (constructions (grammar agent))
                            for form = (attr-val cxn :form)
                            for meaning = (attr-val cxn :meaning)
                            for score = (attr-val cxn :score)
                            unless (or (null form) (null meaning))
                            collect (list form score meaning))))
      (add-element '((h3) "The agent's lexicon:"))
      (add-element `((div :class "lex-item")
                     ,(s-dot->svg (lexicon->s-dot all-words all-categories all-links))))
      (add-element '((hr))))))

(defun replace-dashes (string)
  (coerce
   (substitute #\_ #\- (coerce string 'list))
   'string))

(defun lexicon->s-dot (words categories links)
  (let ((graph '(((s-dot::rankdir "LR")) s-dot::graph)))
    (loop for node in (append words categories)
          do (push `(s-dot::node ((s-dot::id ,(replace-dashes (mkstr node)))
                                  (s-dot::label ,(replace-dashes (mkstr node)))
                                  (s-dot::fontsize "11")))
                   graph))
    (loop for edge in links
          do (push `(s-dot::edge ((s-dot::from ,(replace-dashes (mkstr (first edge))))
                                  (s-dot::to ,(replace-dashes (mkstr (third edge))))
                                  (s-dot::label ,(format nil "~$" (second edge)))
                                  (s-dot::fontsize "11")
                                  (s-dot::minlen "2")
                                  (s-dot::dir "both")))
                   graph))
    (reverse graph)))

(define-event-handler (trace-interaction-in-web-interface alignment-started)
  (add-element '((hr)))
  (add-element '((h2) "The agent aligns")))

(define-event-handler (trace-interaction-in-web-interface lexicon-alignment)
  (when rewarded
    (add-element '((h3) "Rewarded lexical constructions:"))
    (loop for cxn in rewarded
          do (add-element (make-html cxn :expand-initially t))))
  (when punished
    (add-element '((h3) "Punished lexical constructions:"))
    (loop for cxn in punished
          do (add-element (make-html cxn :expand-initially t)))))

(define-event-handler (trace-interaction-in-web-interface category-alignment)
  (when shifted
    (add-element '((h3) "Updated categories:"))
    (loop for cat in shifted
          do (add-element (make-html cat :expand-initially t)))))

(define-event-handler (trace-interaction-in-web-interface lexicon-added)
  (add-element '((h3) "The agent added a new word:"))
  (add-element (make-html cxn :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface category-added)
  (add-element '((h3) "The agent added a new category:"))
  (add-element (make-html cat :expand-initially t)))

;; + Lexical +
(define-event-handler (trace-interaction-in-web-interface new-category-repair-triggered)
  (add-element '((h3) "The agent creates a new category")))

(define-event-handler (trace-interaction-in-web-interface conceptualise-finished)
  (add-element '((h3) "The agent uses category:"))
  (add-element (make-html (first topic-cat) :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface new-word-repair-triggered)
  (add-element '((h3) "The agent invents a new word")))

(define-event-handler (trace-interaction-in-web-interface produce-finished)
  (add-element `((h3) "The agent says \"" ((i) ,utterance) "\""))
  (add-element (make-html applied-cxn :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface parse-succeeded)
  (add-element '((h3) "The agent knows this word"))
  (add-element (make-html applied-cxn :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface parse-failed)
  (add-element '((h3) "The agent does not know this word")))

(define-event-handler (trace-interaction-in-web-interface interpret-finished)
  (add-element `((h3) "Possible topic: " ((i) ,(format nil "~a" topic-id)))))

(define-event-handler (trace-interaction-in-web-interface hearer-conceptualise-finished)
  (add-element `((h3) "The agent uses category:"))
  (add-element (make-html topic-cat :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface hearer-create-finished)
  (add-element `((h3) "The agent creates a new category:"))
  (add-element (make-html topic-cat :expand-initially t)))

(define-event-handler (trace-interaction-in-web-interface hearer-learning-finished)
  (add-element '((h3) "The agent learned:"))
  (add-element (make-html new-lex-cxn :expand-initially t)))

;; -----------------------------
;; + Trace Tasks and Processes +
;; -----------------------------

(define-monitor trace-tasks-and-processes)

(define-event-handler (trace-tasks-and-processes object-run-task-finished)
  (add-element `((h3) ,(format nil "The ~a finished running the ~a"
                               (discourse-role tasks-and-processes::object)
                               (label task))))
  (add-element (make-html task)))

;; ##################################################################
;; draw a colour-set in a 3D space
;; ##################################################################

(define-css 'ap-colour-world "
div.ap-colour-world { padding:0px; display:inline-block; margin-right: 7px; margin-bottom:3px;margin-top:3px;position:relative;border:1px solid #40241A}
div.ap-colour-world > div.title { color:#fff;padding-left:2px;padding-right:2px;padding-top:0px;padding-bottom:1px;background-color: #AE1B1B; }
div.ap-colour-world > div.title a { color:#fff; font-size:9pt; font-weight:normal;}
div.ap-colour-world-expanded { border:1px solid #40241A; }
div.ap-colour-world-expanded > div.title { padding-left:5px;padding-right:5px;padding-top:1px;padding-bottom:2px; }
div.ap-colour-world > table > tbody > tr > td { padding-left:5px; padding-bottom:2px;padding-top:2px;}
")

(defmethod choose-3D-colour-set-options (&key (width "600px") (height "600px")
                                              (xLabel "L*") (xMax 100.0) (xMin 0.0)
                                              (yLabel "A*") (yMax 165.0) (yMin -165.0)
                                              (zLabel "B*") (zMax 145.0) (zMin -135.0)
                                              (backgroundColor "#dcdfe1") &allow-other-keys)
  (format nil "var options = {
                  width:'~a',
                  height:'~a',
                  style: 'dot-lab-color',
                  xLabel:'~a',
                  xMax:~a,
                  xMin:~a,
                  yLabel:'~a',
                  yMax:~a,
                  yMin:~a,
                  zLabel:'~a',
                  zMax:~a,
                  zMin:~a,
                  showPerspective:true,
                  showGrid:true,
                  keepAspectRatio:true,
                  verticalRatio:1.0,
                  backgroundColor:'~a',
                  tooltip: function (point) {
                      // parameter point contains properties x, y, z
                     return '~a~a~a: (' + point.x + ',' + point.y + ',' + point.z + ')';
                  },
                  cameraPosition: {
                      horizontal: -0.35,
                      vertical: 0.22,
                      distance: 1.8
                  }
              };" width height xLabel xMax xMin yLabel yMax yMin zLabel zMax zMin backgroundColor xLabel yLabel zLabel))

(defun add-3D-point-to-data (rgb)
  "get a list that corresponds to a point in a 3d space and returns a line in js that corresponds to adding that point to the data to plot
  Example: '(96.001 5.0047 -5.3132) => 'data.add({x:96.001,y:5.0047,z:-5.3132});'"
  (let ((lab (rgb->lab rgb)))
    (format nil "data.add({x:~a,y:~a,z:~a,style:3.0});" (first lab) (second lab) (third lab))))

(define-css-link 'vis-css (string-append cl-user::*localhost-user-dir* "vis/vis.css"))

(define-js-library 'vis-js (string-append cl-user::*localhost-user-dir* "vis/vis.js"))

(defmethod make-3D-colour-set-title (&key (title "3D colour-set") &allow-other-keys)
  `((span) 
    ,(format nil title)))

(defun draw-3D-colour-set (colour-set &key (width "600px") (height "600px")
                                      (xLabel "L*") (xMax 100.0) (xMin 0.0)
                                      (yLabel "A*") (yMax 165.0) (yMin -165.0)
                                      (zLabel "B*") (zMax 145.0) (zMin -135.0)
                                      (backgroundColor "#dcdfe1")
                                      (title "3D colour-set") &allow-other-keys)
  (let* ((element-id-1 (make-id '3D-colour-set))
         (graph-id (make-id 'graph-id))
         (name-func (fcg::replace-all (concatenate 'string "drawVisualization" (mkstr graph-id)) "-" ""))
         (coordinates-colour-set (loop for colour in colour-set
                                       collect (add-3D-point-to-data colour)))
         (add-colours-js (list-of-strings->string coordinates-colour-set :separator ""))
         (function-to-add
           (lambda () `((script :type "text/javascript")
                        ,(concatenate 'string
                                      "var data = null;
                                      var graph = null;
                                      function " name-func "() {
                                                           data = new vis.DataSet();
                                                           "
                                                           ;; fill the dataSet with the points in the colour-set
                                                           add-colours-js
                                                           ;;specify options
                                                           (choose-3D-colour-set-options :width width :height height
                                                                                         :xLabel xLabel :xMax xMax :xMin xMin
                                                                                         :yLabel yLabel :yMax yMax :yMin yMin
                                                                                         :zLabel zLabel :zMax zMax :zMin zMin
                                                                                         :backgroundColor backgroundColor)
                                                           (format nil 
                                                                   "// create our graph
                                                                   var container = document.getElementById('~a');
                                                                   graph = new vis.Graph3d(container, data, options);
                                                                   }" graph-id)))))
         (title (make-3D-colour-set-title :title title))
         (collapsed-version
           (lambda ()
             `((div :class "ap-colour-world")
               ((div :class "title") 
                ((a ,@(make-expand/collapse-link-parameters 
                        element-id-1 t "show details")) 
                 ,title)))))
         (expanded-version-1
           (lambda ()
             `((div :class "ap-colour-world ap-colour-world-expanded")
               ((div :class "title")
                ((a ,@(make-expand/collapse-link-parameters 
                        element-id-1 nil "hide details"))
                 ,title))
               ((div :id ,(mkstr graph-id))
                ((p) ((a :class "button"
                         :style "border-radius: 1px; font-size: 12px;color: #000000;background: #dcdfe1;padding:4px;"
                         :href ,(concatenate 'string "javascript:" name-func "();")) 
                      "show 3D visualisation")))
               ))))
    (add-element (funcall function-to-add))
    (make-expandable/collapsable-element 
      element-id-1 (make-id '3D-colour-set) collapsed-version 
      expanded-version-1 :expand-initially t)))
