(defpackage duckie-language-learning
  (:nicknames :lll)
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
   :meta-layer-learning
   :drakma
   :dexador)
  (:import-from :cl-json
   :decode-json-from-source
   :encode-json)
  (:shadowing-import-from :jonathan
   :to-json :parse)
  (:shadowing-import-from :fcg :size :attributes)
  (:export :args))



