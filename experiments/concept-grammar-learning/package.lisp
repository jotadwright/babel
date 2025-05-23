;;;; package.lisp

(in-package :cl-user)

(defpackage :clg
  (:documentation "Tutor-learner experiment to learn the clevr grammar with concepts")
  (:use :common-lisp :common-lisp-user :utils :experiment-framework
        :plot-raw-data :monitors :web-interface
        :tasks-and-processes :meta-layer-learning
        :irl :fcg  :concept-representations)
  (:import-from :cl-json :decode-json-from-source)
  
  (:local-nicknames (:jzon :com.inuoe.jzon)))