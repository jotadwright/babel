
(in-package :nao-interface)

(export '(nao-take-picture nao-analyze-scene))

#|
(defmethod nao-take-picture ((nao nao))
  ;; (break "Please change the world and carry on.")
  (sleep 0.5)
  (nao-send-http nao (make-json 'vision :data '((action . "capture")))))

(defmethod nao-analyze-scene ((nao nao) filename)
  (sleep 1.0)
  (nao-send-http nao (make-json 'vision :data `((action . "analyze")
                                                (filename . ,filename)))))
|#

(defgeneric nao-take-picture (nao)
  (:documentation "Takes a picture on the Nao"))

(defmethod nao-take-picture ((nao nao))
  (nao-send-http nao "/vision/capture"))

(defgeneric nao-analyze-scene (nao filename)
  (:documentation "Analyze the scene using opencv"))

(defmethod nao-analyze-scene ((nao nao) filename)
  (nao-send-http nao "/vision/analyse" :data `((filename . ,(namestring filename)))))

