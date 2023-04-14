(defpackage duckie-language-learning
  (:nicknames :dll)
  (:use
   :cl
   :utils
   :monitors
   :plot-raw-data
   :experiment-framework
   :test-framework
   :irl
   :web-interface
   :fcg
   :meta-layer-learning)
  (:import-from :cl-json
   :decode-json-from-source
   :encode-json)
  (:import-from :dexador
   :post)
  (:shadowing-import-from :jonathan
   :to-json :parse)
  (:shadowing-import-from :fcg :size :attributes)
  (:export :args))



