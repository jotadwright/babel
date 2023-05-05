;;;; start-server.lisp

(ql:quickload :propbank-grammar)
(in-package :propbank-grammar)


;; LOAD ANNOTATIONS
(load (babel-pathname :directory
                     '("applications" "ccxg-explorer-web-api")
                     :name "annotations"
                     :type "lisp"))

(load (babel-pathname :directory
                      '("applications" "ccxg-explorer-web-api")
                      :name "search" :type "lisp"))

(load (babel-pathname :directory
                      '("applications" "ccxg-explorer-web-api")
                      :name "web-service" :type "lisp"))

;; LOAD FRAME EXTRACTOR CONSTRUCTIONS
;(restore-annotations)
(defun add-array-locks! (construction-inventory)
  (let ((matrix-names '(nil gram-sense lex-sense lex-gram))
        (matrix (graph-utils::matrix (fcg::graph (categorial-network construction-inventory)))))
    (loop for name in matrix-names
          do  (setf (graph-utils::array-lock (gethash name matrix)) (bordeaux-threads:make-recursive-lock)))))



(defparameter *restored-grammar*
  (cl-store:restore
   #+sbcl (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                          :name "propbank-grammar-ontonotes-ccl-10000"
                          :type "fcg")
   #+lispworks (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                               :name "propbank-grammar-ontonotes-no-aux-lw"
                               :type "fcg")
   #+ccl (babel-pathname :directory '("grammars" "propbank-grammar" "grammars")
                         :name "propbank-grammar-ontonotes-ccl-10000"
                         :type "fcg")))

#+ccl (add-array-locks! *restored-grammar*)

(export '(*restored-grammar*))

;; WEB SERVER

(in-package :hunchentoot)

(export '(cors-acceptor))

(defclass cors-acceptor (easy-acceptor)
  ()
  (:documentation "Subclass of easy-acceptor to be able to set
cross-origin headers in the accetor-dispatch-request method"))

(defmethod acceptor-dispatch-request ((acceptor cors-acceptor) request)
  "The easy request dispatcher which selects a request handler
based on a list of individual request dispatchers all of which can
either return a handler or neglect by returning NIL."
  (loop for dispatcher in *dispatch-table*
     for action = (funcall dispatcher request)
     when action return (funcall action)
     finally (call-next-method)))

(defmethod acceptor-dispatch-request :around ((acceptor cors-acceptor) request)
  (setf (header-out "Access-Control-Allow-Origin") "*")
  (setf (header-out "Access-Control-Allow-Headers") "Content-Type,Accept,Origin")
  (call-next-method))


(in-package :propbank-grammar)



(defvar *ccxg-explorer-app* (snooze:make-hunchentoot-app))
(push *ccxg-explorer-app* hunchentoot:*dispatch-table*)
(defvar *ccxg-explorer-acceptor* (make-instance 'hunchentoot:cors-acceptor :port 8500)) ;; Kortrijk

(if (hunchentoot:started-p *ccxg-explorer-acceptor*)
    (hunchentoot:stop *ccxg-explorer-acceptor*))

(hunchentoot:start *ccxg-explorer-acceptor*)



