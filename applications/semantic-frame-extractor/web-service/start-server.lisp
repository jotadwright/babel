;;;; start-server.lisp

(ql:quickload :frame-extractor)
(in-package :frame-extractor)

(defvar *frame-extractor-app* (snooze:make-hunchentoot-app))
(push *frame-extractor-app* hunchentoot:*dispatch-table*)
(defvar *frame-extractor-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 9004))
(hunchentoot:start *frame-extractor-acceptor*)

;(hunchentoot:stop *frame-extractor-acceptor*)