(in-package :intention-reading)

(define-monitor trace-grammar-learning-in-web-interface
                :documentation "Prints progress of learning in the web interface")
(define-monitor trace-grammar-learning-verbose
                :documentation "Prints detailed progress of learning in the web interface after each interaction")
(define-monitor trace-grammar-learning-categories
                :documentation "Prints detailed progress of categorial networks after each interaction")

(define-event start-experiment)

(define-event start-series (series-number number))

(define-event show-progress (observation-count number) (coverage-count number) (coverage-window number) (total-samples number))

(define-event show-results)

(define-event show-observation-start (observation-count number) (utterance string) (meaning list))

(define-event show-observation-result (observation-count number) (utterance string) (meaning list) (car-status symbol) (repair symbol))

(define-event show-categories)


(define-event-handler (trace-grammar-learning-categories show-categories)
  (wi:add-element (make-html (categorial-network
                              *fcg-constructions*) :weights? t))
  (add-element '((hr))))

(define-event-handler (trace-grammar-learning-in-web-interface start-experiment)
  (wi::reset))

(define-event-handler (trace-grammar-learning-in-web-interface start-series)
  (add-element `((h2) ,(format nil "Start series: ~a" series-number)))
  (add-element '((hr))))

(define-event-handler (trace-grammar-learning-in-web-interface show-progress)
    (add-element `((h3) ,(format nil "Observation count: ~a/~a" observation-count total-samples)))
    (add-element `((h3) ,(format nil "Windowed coverage: ~a/~a" coverage-count coverage-window)))
    (add-element '((hr))))


(define-event-handler (trace-grammar-learning-in-web-interface show-results)
  (sort (constructions *fcg-constructions*) #'> :key (lambda (cxn) (attr-val cxn :score)))
  (wi:add-element (make-html (categorial-network
                              *fcg-constructions*) :weights? t))
  (wi:add-element (make-html *fcg-constructions*))
  (add-element '((hr))))

(define-event-handler (trace-grammar-learning-verbose show-observation-start)
    (add-element `((h2) ,(format nil "Observation: ~a" observation-count)))
    (add-element `((div) ((p) ((b) ,(format nil "Utterance: ")) ,(format nil "\"~a\"" utterance))))
    (add-element `((div) ((p) ((b) ,(format nil "Meaning: ")) ,(format nil "~a" meaning)))))
    

(define-event-handler (trace-grammar-learning-verbose show-observation-result)
    (add-element `((div) ((p) ((b) ,(format nil "Status: ")) ,(format nil "~a" car-status))))
    (add-element `((div) ((p) ((b) ,(format nil "Repair: ")) ,(format nil "~a" repair)))))
    

;; applied repairs

;; constructions per type

;; cxn inventory size


