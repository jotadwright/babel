;;;; start-server.lisp

(ql:quickload :clevr-web-service)
(in-package :clevr-web-service)

(defvar *clevr-app* (snooze:make-hunchentoot-app))
(push *clevr-app* hunchentoot:*dispatch-table*)
(defvar *clevr-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 9003))
(hunchentoot:start *clevr-acceptor*)

;; (hunchentoot:stop *clevr-acceptor*)