(defpackage :pattern-finding-old
  (:nicknames :pf)
  (:use :cl
        :utils
        :monitors
        :plot-raw-data
        :experiment-framework
        :test-framework
        :irl
        :amr
        :web-interface
        :fcg
        :meta-layer-learning)
  (:import-from :cl-json :decode-json-from-string)
  (:shadowing-import-from :fcg :size :attributes)
  (:export :args))



