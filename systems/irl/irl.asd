
(in-package :asdf)

(defsystem :irl
  :description "Incremental Recruitment Language"
  :depends-on (:test-framework :utils :monitors
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
  ((:file "package")
   (:file "entity")
   (:file "slot-spec")
   (:file "evaluation-spec")
   (:file "primitive")
   (:file "primitive-inventory")
   (:file "binding")
   (:file "check-irl-program")
   (:file "irl-program-processor")
   (:file "node-tests")
   (:file "goal-tests")
   (:file "evaluate-primitive")
   (:file "evaluate-irl-program")
   (:file "irl-utils")
   (:file "chunk")
   (:module monitoring
    :serial t
    :components ((:file "draw-irl-program")
                 (:file "html")
                 (:file "web-monitors")))
   (:module composer
    :serial t
    :components ((:file "substitute-variables")
                 (:file "composer")
                 (:file "match-chunk")
                 (:file "evaluate-chunk")
                 (:file "check-chunk-evaluation-result")
                 (:file "check-node")
                 (:file "chunk-wrapper")
                 (:file "expand-chunk")
                 (:file "rating-and-scoring")
                 (:file "handle-node")
                 (:file "get-next-solutions")
                 (:module monitoring
                  :serial t
                  :components ((:file "html")
                               (:file "web-monitors")))))))

