(ql:quickload :propbank-grammar)
(ql:quickload :distributional-fcg)

(in-package :propbank-grammar)

;; activate the monitor
(activate-monitor trace-fcg)

(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")
(setf nlp-tools::*embedding-host* "http://127.0.0.1:5001")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Infuriate          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *cxn-inventory-infuriate*
               (cl-store:restore 
                  (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank"
                                                                                "distributional-fcg"))
                                                  :name "cxn-inventory-infuriate-example"
                                                  :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                   *babel-corpora*)))

(set-configuration *cxn-inventory-infuriate* :cosine-similarity-threshold 0.8)

;; (nth 621 (dev-split *ontonotes-annotations*))
(comprehend-all "So I mean that right there it enraged me ."
                :cxn-inventory *cxn-inventory-infuriate* :timeout 60 :n 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Teach        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defparameter *cxn-inventory-teach*
  (cl-store:restore 
                (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank"
                                                                                "distributional-fcg"))
                                                :name "cxn-inventory-teach-example"
                                                :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                 *babel-corpora*)))


(comprehend-all "Jesus taught the people in the Temple area every day ."
                :cxn-inventory *cxn-inventory-teach* :timeout 60 :n 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Google        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *cxn-inventory-google*
  (cl-store:restore 
                (merge-pathnames (make-pathname :directory (cons :relative '("Frames\ and\ Propbank"
                                                                             "distributional-fcg"))
                                                :name "cxn-inventory-google-example"
                                                :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                 *babel-corpora*)))

(comprehend-all "Try googling it for more info :-RRB-"
                :cxn-inventory *cxn-inventory-google* :n 1 :timeout 200)



(fcg::draw-categorial-network-node-and-neighbours *cxn-inventory-google*
                                                  'propbank-grammar::google\(v\)-1)

(pprint (graph-utils::neighbors
         (fcg::graph (categorial-network *cxn-inventory-google*))
         'propbank-grammar::google\(v\)-1 :return-ids? nil :edge-type 'lex-gram))


(fcg::draw-categorial-network-node-and-neighbours *cxn-inventory-google*
                                                  'propbank-grammar::disregard\(v\)-1)


(pprint (graph-utils::neighbors
         (fcg::graph (categorial-network *cxn-inventory-google*))
         'propbank-grammar::disregard\(v\)-1 :return-ids? nil :edge-type 'lex-gram))

