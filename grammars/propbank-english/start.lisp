(in-package :cl-user)

;; Load and start web service
(load (babel-pathname :directory '("grammars" "propbank-english" "web-service") :name "start-server" :type "lisp"))

(in-package :propbank-english)

;; Set penelope host address
(setf nlp-tools::*penelope-host* "http://localhost:5000")


;; curl -H "Content-Type: application/json" -d '{"utterance" : "I believe in you."}' http://localhost:9007/propbank-frame-extractor/extract-frames-string
;; curl -H "Content-Type: application/json" -d '{"utterances" : ["I believe in you.", "I think that you believe in me."]}' http://localhost:9007/propbank-frame-extractor/extract-frames-list

;; Rolesets: all rolesets van het opnion-frame