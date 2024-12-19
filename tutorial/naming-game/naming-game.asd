
(in-package :asdf)

(defsystem :naming-game
  :description "Basic functions to implement a simple naming game"
  :depends-on (:experiment-framework :utils :monitors :fcg :plot-raw-data
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t 
  :components 
  ((:file "package")
   (:file "create-agent")
   (:file "web-monitors")
   (:file "create-word")
   (:file "create-world")
   (:file "create-interaction")
   (:file "plot-monitors")))
