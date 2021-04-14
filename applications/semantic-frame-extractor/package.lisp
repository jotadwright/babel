
(in-package :common-lisp-user)

(defpackage :frame-extractor
  (:use :cl-user
	:common-lisp
        :utils
	:test-framework
	:fcg
        :pie
        :nlp-tools
        :irl
        #+:hunchentoot-available-on-this-platform :web-interface
        :monitors
        :meta-layer-learning
        :cl-json
        :tasks-and-processes
        :snooze
        :trivial-timeout)
  (:shadow "PROTOTYPE" "PP")
  )