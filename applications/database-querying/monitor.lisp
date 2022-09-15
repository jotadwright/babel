(in-package :fcg)

;; ############################################################
;; trace-fcg-database-querying
;; ############################################################

(define-monitor trace-fcg-database-querying 
    :documentation "Traces results of querying databases with fcg")

(define-event query-started (utterance string))
(define-event query-completion-started (meaning-network list))
(define-event query-completion-finished (query string))
(define-event query-finished (result list))

(define-event-handler (trace-fcg-database-querying query-started)
  (add-element '((hr)))
  (add-element `((h1) ,(format nil "Querying: ~s" utterance))))

(define-event-handler (trace-fcg-database-querying parse-started)
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Step 1: Comprehending &quot;~{~a~^ ~}&quot; using Fluid Construction Grammar" utterance))))

(define-event-handler (trace-fcg-database-querying query-completion-started)
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Step 2: Constructing and Completing Query")))
  (add-element `((hr)))
  (add-element `((h3) ,(format nil "Source meaning network:")))
  (add-element (predicate-network->svg meaning-network :only-variables nil)))

(define-event-handler (trace-fcg-database-querying query-completion-finished)
  (add-element `((h3) ,(format nil "Your question wads understood perfectly and needed no query completion. Resulting Query: ")))
  (add-element `((p) ,(format nil "~a" query))))

(define-event-handler (trace-fcg-database-querying query-finished)
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Step 3: Answering Query")))
  (add-element `((hr)))
  (add-element `((p) ,(format nil "~{~{~a    |    ~}~%~}" result)))
  (add-element `((hr))))

;; ------------------------------------------------------------
;; construction application

(define-event-handler (trace-fcg-database-querying cxn-application-finished)
  (let ((cars (append failed-cars resulting-cars)))
    (cond ((> (length cars) 1)
           (add-element `((p) ((b) ,(length cars) " results:")))
           (loop for car in cars 
                 do (add-element '((hr)))
                 (add-element (make-html-fcg-light car
                                                   :feature-types (feature-types
                                                                   (original-cxn-set (cxn-inventory (car-applied-cxn car))))))))
          (cars (add-element `((div)
                               ,(make-html-fcg-light (first cars)
                                                     :feature-types (feature-types
                                                                     (original-cxn-set (cxn-inventory (car-applied-cxn (first cars)))))))))
          (t (add-element `((div) ((b) "no match")))))))

;; ------------------------------------------------------------
;; construction set application


(define-event-handler (trace-fcg-database-querying cip-finished)
  (add-element '((hr)))
  (add-element (make-html-fcg-light cip :solutions (when solution (list solution)))))

(define-event-handler (trace-fcg-database-querying produce-started)
  (add-element `((hr)))
  (add-element `((h2) "Formulating&#160;"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg meaning :only-variables nil)
                 (html-pprint meaning))))

(define-event-handler (trace-fcg-database-querying produce-finished)
  (add-element `((h3) ,(format nil "Utterance: &quot;~{~a~^ ~}&quot;" utterance)))
  (add-element `((p) " ")))
     


(define-event-handler (trace-fcg-database-querying parse-finished)
  (add-element `((h3 :style "margin-bottom:3px;") "Meaning:"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg meaning)
                 (html-pprint meaning)))
  (add-element `((p) " ")))


