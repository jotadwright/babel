
(in-package :common-lisp-user)

(defpackage :frame-extractor
  (:use :cl-user
	:common-lisp
        :utils
	:test-framework
	:fcg
        :irl :pie
        :nlp-tools
        #+:hunchentoot-available-on-this-platform :web-interface
        :monitors
        :meta-layer-learning
        :cl-json
        :tasks-and-processes
        :snooze)
  (:shadow "PROTOTYPE" "PP")
  )