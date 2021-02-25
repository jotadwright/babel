
(in-package :cl-jonathan)

(export '(encode-plist-to-json
          encode-alist-to-json))

(defun encode-plist-to-json (object)
  "Encode a plist as a JSON string"
  (jonathan.encode:to-json object :from :plist))

(defun encode-alist-to-json (object)
  "Encode an alist as a JSON string"
  (jonathan.encode:to-json object :from :alist))