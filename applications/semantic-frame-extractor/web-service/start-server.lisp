;;;; start-server.lisp

(ql:quickload :clevr-irl)
(in-package :clevr-irl)

(hunchentoot:start *clevr-acceptor*)

;; (hunchentoot:stop *clevr-acceptor*)