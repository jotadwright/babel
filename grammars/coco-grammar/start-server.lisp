;;;; start-server.lisp

(ql:quickload :coco-grammar)

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

(in-package :coco-web-service)

(defvar *coco-app* (snooze:make-hunchentoot-app))
(push *coco-app* hunchentoot:*dispatch-table*)
(defvar *coco-acceptor* (make-instance 'hunchentoot:cors-acceptor :port 9009))
(hunchentoot:start *coco-acceptor*)

;; (hunchentoot:stop *coco-acceptor*)