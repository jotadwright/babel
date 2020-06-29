;;;; start-server.lisp

(ql:quickload :propbank-english)

(in-package :propbank-english)

(load (babel-pathname :directory '("grammars" "propbank-english" "web-service") :name "web-service" :type "lisp"))

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


(in-package :propbank-english)

(defvar *propbank-frame-extractor-app* (snooze:make-hunchentoot-app))
(push *propbank-frame-extractor-app* hunchentoot:*dispatch-table*)
(defvar *propbank-frame-extractor-acceptor* (make-instance 'hunchentoot:cors-acceptor :port 9007))
(hunchentoot:start *propbank-frame-extractor-acceptor*)

