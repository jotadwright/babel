
(in-package :nao-interface)

(export '(nao-take-picture nao-analyze-scene))

(defgeneric nao-take-picture (nao)
  (:documentation "Takes a picture on the Nao"))

(defmethod nao-take-picture ((nao nao))
  (nao-send-http nao "/vision/capture"))

(defgeneric nao-analyze-scene (nao filename)
  (:documentation "Analyze the scene using opencv"))

(defmethod nao-analyze-scene ((nao nao) filename)
  (nao-send-http nao "/vision/analyse" :data `((filename . ,(namestring filename)))))

