(in-package :mwm)

;; -----------------
;; + Printing dots +
;; -----------------

(define-monitor print-a-dot-for-each-interaction
                :documentation "Prints a '.' for each interaction
                 and prints the number after :dot-interval")

(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
  (if (= (mod (interaction-number interaction)
                (get-configuration experiment :dot-interval)) 0)
    (format t ". (~a)~%" (interaction-number interaction))
    (format t ".")))


;; ---------
;; + TIIWI +
;; ---------
                                       

(define-monitor trace-interaction-in-web-interface)

(define-event-handler (trace-interaction-in-web-interface interaction-started)
  (add-element '((hr)))
  (add-element `((h1) ,(format nil "Interaction ~a"
                               (interaction-number interaction))))
  (add-element `((h2) ,(format nil "The ~a is the speaker"
                               (downcase (mkstr (id (speaker interaction)))))))
  (add-element `((h2) ,(format nil "The ~a is the hearer"
                               (downcase (mkstr (id (hearer interaction)))))))
  (add-element '((hr))))

(define-event-handler (trace-interaction-in-web-interface interaction-finished)
  (add-element '((hr)))
  (add-element `((h2) "Interaction "
                 ,(if (communicated-successfully interaction)
                    `((b :style "color:green") "succeeded")
                    `((b :style "color:red") "failed")))))

(define-event-handler (trace-interaction-in-web-interface context-determined)
  (add-element
   `((table)
     ((tr) ((th) "CLEVR context"))
     ((tr) ((td) ,(make-html (symbolic-context (speaker experiment)) :expand-initially t)))))
  (add-element `((table)
                 ((tr) ((th) "MWM context"))
                 ((tr) ((td) ,(make-html (context (hearer experiment))))))))

(define-event-handler (trace-interaction-in-web-interface conceptualisation-finished)
  (add-element `((h2) ,(format nil "The topic is ~a" (id (topic agent)))))
  (cond ((and (tutorp agent) (discriminative-set agent))
         (add-element '((h2) "Tutor found discriminating attributes:"))
         (add-element `((h3) ((i) ,(format nil "~{~a~^, ~}" (discriminative-set agent))))))
        ((and (learnerp agent) (applied-cxns agent))
         (add-element '((h2) "Learner found discriminating attributes:"))
         (add-element `((h3) ((i) ,(format nil "~{~a~^, ~}"
                                           (mapcar #'(lambda (cxn) (attr-val cxn :form))
                                                   (applied-cxns agent)))))))
        (t
         (add-element `((h2) ,(format nil "~@(~a~) did not find discriminating attributes"
                                      (id agent)))))))

(define-event-handler (trace-interaction-in-web-interface production-finished)
  (if (utterance agent)
    (progn (add-element `((h2) ,(format nil "~@(~a~) producted an utterance:" (id agent))))
      (add-element `((h3) ((i) ,(format nil "~{\"~a\"~^, ~}" (utterance agent))))))
    (add-element `((h2) ,(format nil "~@(~a~) could not produce an utterance" (id agent))))))

(defgeneric category->s-dot-node (category &key)
  (:documentation "How to display a category as an s-dot node"))

(defmethod category->s-dot-node ((category min-max-category) &key green red (show-cxns nil))
  (let ((record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#990000")
                                         (s-dot::fontcolor "#FFFFFF")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
                    (s-dot::label ,(format nil "~a, [~,2f - ~,2f]"
                                           (downcase (mkstr (attribute category)))
                                           (lower-bound category)
                                           (upper-bound category))))))))

(defmethod category->s-dot-node ((category prototype-category) &key green red (show-cxns nil))
  (let* ((variance (/ (M2 category) (nr-samples category)))
         (stdev (sqrt variance))
         (lower-bound (- (prototype category) (* 2 stdev)))
         (upper-bound (+ (prototype category) (* 2 stdev)))
         (record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#AA0000")
                                         (s-dot::fontcolor "#FFFFFF")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
                    (s-dot::label ,(if show-cxns
                                     (format nil "~a: ~,2f [~a]"
                                             (downcase (mkstr (attribute category)))
                                             (prototype category)
                                             (downcase (mkstr (name (construction category)))))
                                     (format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkstr (attribute category)))
                                           (prototype category)
                                           lower-bound
                                           upper-bound))))))))

(defmethod category->s-dot-node ((category prototype-min-max-category) &key green red (show-cxns nil))
  (let ((record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#AA0000")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
                    (s-dot::label ,(format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkstr (attribute category)))
                                           (prototype category)
                                           (lower-bound category)
                                           (upper-bound category))))))))

(defmethod category->s-dot-node ((category exponential-category) &key green red (show-cxns nil))
  (let ((record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#AA0000")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
                    (s-dot::label ,(format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkstr (attribute category)))
                                           (prototype category)
                                           (left-sigma category)
                                           (right-sigma category))))))))
  
(defun cxn->s-dot (cxn &optional highlight-green highlight-red)
  (let ((form (attr-val cxn :form))
        (meaning (attr-val cxn :meaning))
        (graph '(((s-dot::ranksep "0.3")
                  (s-dot::nodesep "0.5")
                  (s-dot::margin "0")
                  (s-dot::rankdir "LR"))
                 s-dot::graph)))
    (push
     `(s-dot::record  
       ((s-dot::style "solid")
        (s-dot::fontsize "9.5")
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id ,(format nil "~a" form))
                     (s-dot::label ,form)
                     (s-dot::fontcolor "#AA0000"))))
     graph)
    (loop for (category . certainty) in meaning
          for record
          = (category->s-dot-node category
                                  :green (member (attribute category) highlight-green)
                                  :red (member (attribute category) highlight-red))
          when (> certainty 0.0)
          do (push record graph))
    (loop for (category . certainty) in meaning
          when (> certainty 0.0)
          do (push
              `(s-dot::edge
                ((s-dot::from ,(format nil "~a" form))
                 (s-dot::to ,(mkdotstr (downcase (mkstr (attribute category)))))
                 (s-dot::label ,(format nil "~,2f" certainty))
                 (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                       #-(or :win32 :windows) "Arial")
                 (s-dot::fontsize "8.5")
                 (s-dot::arrowsize "0.5")))
              graph))
    (reverse graph)))

(defun meaning->s-dot (meaning &key (show-cxns nil) label)
  (let ((graph '(((s-dot::ranksep "0.3")
                  (s-dot::nodesep "0.5")
                  (s-dot::margin "0")
                  (s-dot::rankdir "LR"))
                 s-dot::graph)))
    (push
     `(s-dot::record  
       ((s-dot::style "solid")
        (s-dot::fontsize "9.5")
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id "root")
                     (s-dot::label (if label label "")))))
     graph)
    (loop for (category . certainty) in meaning
          for record = (category->s-dot-node category :show-cxns show-cxns)
          when (> certainty 0.0)
          do (push record graph))
    (loop for (category . certainty) in meaning
          when (> certainty 0.0)
          do (push
              `(s-dot::edge
                ((s-dot::from ,"root")
                 (s-dot::to ,(mkdotstr (downcase (mkstr (attribute category)))))
                 (s-dot::label ,(format nil "~,2f" certainty))
                 (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                       #-(or :win32 :windows) "Arial")
                 (s-dot::fontsize "8.5")
                 (s-dot::arrowsize "0.5")))
              graph))
    (reverse graph)))

(defun example-meaning->s-dot (meaning &key label)
  (let ((graph '(((s-dot::ranksep "0.3")
                  (s-dot::nodesep "0.5")
                  (s-dot::margin "0")
                  (s-dot::rankdir "LR"))
                 s-dot::graph)))
    (push
     `(s-dot::record  
       ((s-dot::style "solid")
        (s-dot::fontsize "9.5")
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id "root")
                     (s-dot::label ,(if label label "")))))
     graph)
    (loop for (category . certainty) in meaning
          for record = `(s-dot::record
                         ,(append '((s-dot::style "dashed"))
                                  '((s-dot::fontsize "9.5")
                                    (s-dot::fontname #+(or :win32 :windows) "Sans"
                                                     #-(or :win32 :windows) "Arial")
                                    (s-dot::height "0.01")))
                         (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (attribute category)))))
                                       (s-dot::label ,(format nil "~a" (downcase (mkstr (attribute category))))))))
          when (> certainty 0.0)
          do (push record graph))
    (loop for (category . certainty) in meaning
          when (> certainty 0.0)
          do (push
              `(s-dot::edge
                ((s-dot::from ,"root")
                 (s-dot::to ,(mkdotstr (downcase (mkstr (attribute category)))))
                 (s-dot::label ,(format nil "~,2f" certainty))
                 (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                       #-(or :win32 :windows) "Arial")
                 (s-dot::fontsize "8.5")
                 (s-dot::arrowsize "0.5")))
              graph))
    (reverse graph)))

(defmethod json-meaning->s-dot-node (meaning (type (eql :min-max-category)) &key green red)
  (let ((record-properties (cond (green '((s-dot::style "filled")
                                          (s-dot::fillcolor "#AAFFAA")))
                                 (red '((s-dot::style "filled")
                                        (s-dot::fillcolor "#990000")))
                                 (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (rest (assoc :attribute meaning))))))
                    (s-dot::label ,(format nil "~a, [~,2f - ~,2f]"
                                           (downcase (mkstr (rest (assoc :attribute meaning))))
                                           (rest (assoc :lower--bound meaning))
                                           (rest (assoc :upper--bound meaning)))))))))

(defmethod json-meaning->s-dot-node (meaning (type (eql :prototype-category)) &key green red)
  (let* ((variance (/ (rest (assoc :M2 meaning))
                      (rest (assoc :nr--samples meaning))))
         (stdev (sqrt variance))
         (lower-bound (- (rest (assoc :prototype meaning)) (* 2 stdev)))
         (upper-bound (+ (rest (assoc :prototype meaning)) (* 2 stdev)))
         (record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#AA0000")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (rest (assoc :attribute meaning))))))
                    (s-dot::label ,(format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkstr (rest (assoc :attribute meaning))))
                                           (rest (assoc :prototype meaning))
                                           lower-bound
                                           upper-bound)))))))

(defmethod json-meaning->s-dot-node (meaning (type (eql :prototype-min-max-category)) &key green red)
  (let ((record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#AA0000")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (rest (assoc :attribute meaning))))))
                    (s-dot::label ,(format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkstr (rest (assoc :attribute meaning))))
                                           (rest (assoc :prototype meaning))
                                           (rest (assoc :lower--bound meaning))
                                           (rest (assoc :upper--bound meaning)))))))))

(defmethod json-meaning->s-dot-node (meaning (type (eql :exponential-category)) &key green red)
  (let ((record-properties (cond (green '((s-dot::style "filled")
                                           (s-dot::fillcolor "#AAFFAA")))
                                  (red '((s-dot::style "filled")
                                         (s-dot::fillcolor "#AA0000")))
                                  (t '((s-dot::style "dashed"))))))
    `(s-dot::record
      ,(append record-properties
               '((s-dot::fontsize "9.5")
                 (s-dot::fontname #+(or :win32 :windows) "Sans"
                                  #-(or :win32 :windows) "Arial")
                 (s-dot::height "0.01")))
      (s-dot::node ((s-dot::id ,(mkdotstr (downcase (mkstr (rest (assoc :attribute meaning))))))
                    (s-dot::label ,(format nil "~a: ~,2f, [~,2f - ~,2f]"
                                           (downcase (mkstr (rest (assoc :attribute meaning))))
                                           (rest (assoc :prototype meaning))
                                           (rest (assoc :left--sigma meaning))
                                           (rest (assoc :right--sigma meaning)))))))))
  
(defun json-cxn->s-dot (cxn &optional highlight-green highlight-red)
  (let ((form (rest (assoc :form cxn)))
        (meaning (rest (assoc :meaning cxn)))
        (graph '(((s-dot::ranksep "0.3")
                  (s-dot::nodesep "0.5")
                  (s-dot::margin "0")
                  (s-dot::rankdir "LR"))
                 s-dot::graph)))
    ;; form node
    (push
     `(s-dot::record  
       ((s-dot::style "solid")
        (s-dot::fontsize "9.5")
        (s-dot::fontname #+(or :win32 :windows) "Sans"
                         #-(or :win32 :windows) "Arial")
        (s-dot::height "0.01"))
       (s-dot::node ((s-dot::id ,(format nil "~a" form))
                     (s-dot::label ,form)
                     (s-dot::fontcolor "#AA0000"))))
     graph)
    ;; meaning nodes
    (loop with type = (rest (assoc :type cxn))
          for m in meaning
          for record
          = (json-meaning->s-dot-node m (make-kw (upcase type))
                                      :green (member (rest (assoc :attribute m)) highlight-green)
                                      :red (member (rest (assoc :attribute m)) highlight-red))
          when (> (rest (assoc :certainty m)) 0.0)
          do (push record graph))
    ;; edges
    (loop for m in meaning
          when (> (rest (assoc :certainty m)) 0.0)
          do (push
              `(s-dot::edge
                ((s-dot::from ,(format nil "~a" form))
                 (s-dot::to ,(mkdotstr (downcase (mkstr (rest (assoc :attribute m))))))
                 (s-dot::label ,(format nil "~,2f" (rest (assoc :certainty m))))
                 (s-dot::labelfontname #+(or :win32 :windows) "Sans"
                                       #-(or :win32 :windows) "Arial")
                 (s-dot::fontsize "8.5")
                 (s-dot::arrowsize "0.5")))
              graph))
    (reverse graph)))

(define-event-handler (trace-interaction-in-web-interface parsing-finished)
  (add-element (make-html (grammar agent)))
  (if (parsed-meaning agent)
    (progn (add-element '((h2) "Agent parsed the utterance:"))
      (add-element `((div) ,(s-dot->svg
                             (meaning->s-dot (parsed-meaning agent)
                                             :show-cxns t)))))
    (add-element '((h2) "Agent could not parse the utterance."))))

(define-event-handler (trace-interaction-in-web-interface interpretation-finished)
  (if (topic agent)
    (progn (add-element '((h2) "Agent interpreted the utterance:"))
      (add-element (make-html (topic agent) :expand-initially t)))
    (add-element '((h2) "Agent could not interpret the utterance."))))

(define-event-handler (trace-interaction-in-web-interface alignment-started)
  (add-element '((hr)))
  (add-element '((h2) "Alignment started"))
  (add-element '((hr))))

(define-event-handler (trace-interaction-in-web-interface adopting-words)
  (add-element '((h2) "Learner will adopt new words:"))
  (add-element `((h3) ((i) ,(format nil "~{\"~a\"~^, ~}" words)))))

(define-event-handler (trace-interaction-in-web-interface aligning-words)
  (add-element '((h2) "Learner will align known words:"))
  (add-element `((h3) ((i) ,(format nil "~{\"~a\"~^, ~}" words)))))

(define-event-handler (trace-interaction-in-web-interface new-cxn-added)
  (add-element '((h2) "A new construction was created:"))
  (add-element `((div) ,(s-dot->svg
                         (cxn->s-dot cxn)))))

(define-event-handler (trace-interaction-in-web-interface scores-updated)
  (add-element `((h2) ,(format nil "Attributes rewarded and punished for \"~a\"" (attr-val cxn :form))))
  (add-element `((div) ,(s-dot->svg
                         (cxn->s-dot cxn
                                     rewarded-attrs
                                     punished-attrs)))))

(define-event-handler (trace-interaction-in-web-interface attr-removed)
  ; cxn, attr
  (add-element `((h2) ,(format nil "Removed attribute ~a from word \"~a\""
                               attr (attr-val cxn :form)))))

(define-event-handler (trace-interaction-in-web-interface cxn-removed)
  ; cxn
  (add-element `((h2) ,(format nil "Removed word \"~a\""
                               (attr-val cxn :form)))))

(define-event-handler (trace-interaction-in-web-interface found-discriminating-attributes)
  (add-element `((h3) ,(format nil "The following attributes are discriminating: ~{\"~a\"~^, ~}"
                               attributes))))

(define-event-handler (trace-interaction-in-web-interface found-subset-to-reward)
  (add-element `((h3) ,(format nil "The agent will reward the following subset: ~{\"~a\"~^, ~}"
                               (mapcar #'attribute (mapcar #'car subset))))))
