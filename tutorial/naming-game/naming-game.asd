;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.


(in-package :asdf)

(defsystem :naming-game
  :description "Basic functions to implement a simple naming game"
  :depends-on (:experiment-framework :utils :monitors
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t 
  :components 
  ((:file "package")
   (:file "create-word")
   (:file "create-world")
   (:file "create-agent")
   (:file "create-interaction")
))
