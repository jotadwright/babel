
(in-package :cl-jonathan)

(export '(decode-json-as-plist
          decode-json-as-alist
          decode-json-as-plist-from-source
          decode-json-as-alist-from-source))

(defun decode-json-as-plist (string &key junk-allowed
                                    keywords-to-read
                                    keyword-normalizer
                                    normalize-all
                                    exclude-normalize-keys
                                    (unescape-unicode-escape-sequence t))
  "Decode the JSON string as a plist. Keys are returned as :|key|"
  (jonathan.decode:parse string :as :plist :junk-allowed junk-allowed
                         :keywords-to-read keywords-to-read
                         :keyword-normalizer keyword-normalizer
                         :normalize-all normalize-all
                         :exclude-normalize-keys exclude-normalize-keys
                         :unescape-unicode-escape-sequence unescape-unicode-escape-sequence))

(defun decode-json-as-plist-from-source (pathname &key junk-allowed
                                                  keywords-to-read
                                                  keyword-normalizer
                                                  normalize-all
                                                  exclude-normalize-keys
                                                  (unescape-unicode-escape-sequence t))
  "Decode the JSON file as a plist. Keys are returned as :|key|"
  (jonathan.decode:parse (list-of-strings->string
                          (uiop/stream:read-file-lines pathname))
                         :as :plist :junk-allowed junk-allowed
                         :keywords-to-read keywords-to-read
                         :keyword-normalizer keyword-normalizer
                         :normalize-all normalize-all
                         :exclude-normalize-keys exclude-normalize-keys
                         :unescape-unicode-escape-sequence unescape-unicode-escape-sequence))

(defun alist-key-normalizer (key)
  "The typical alist key normalizer. It takes a key (string)
   and transforms it into a keyword. Also, underscores (_) are
   replaced by double hyphens (--)"
  (make-kw (regex-replace-all "_" (upcase key) "--")))

(defun decode-json-as-alist (string &key junk-allowed
                                    keywords-to-read
                                    (keyword-normalizer #'alist-key-normalizer)
                                    (normalize-all t)
                                    exclude-normalize-keys
                                    (unescape-unicode-escape-sequence t))
  "Decode the JSON string as an alist. Uses the default alist key normalizer.
   Normalize all is set to t such that nested keys would also be normalized."
  (jonathan.decode:parse string :as :alist :junk-allowed junk-allowed
                         :keywords-to-read keywords-to-read
                         :keyword-normalizer keyword-normalizer
                         :normalize-all normalize-all
                         :exclude-normalize-keys exclude-normalize-keys
                         :unescape-unicode-escape-sequence unescape-unicode-escape-sequence))

(defun decode-json-as-alist-from-source (pathname &key junk-allowed
                                                  keywords-to-read
                                                  (keyword-normalizer #'alist-key-normalizer)
                                                  (normalize-all t)
                                                  exclude-normalize-keys
                                                  (unescape-unicode-escape-sequence t))
  "Decode the JSON file as an alist. Uses the default alist key normalizer.
   Normalize all is set to t such that nested keys would also be normalized."
  (jonathan.decode:parse (list-of-strings->string
                          (uiop/stream:read-file-lines pathname))
                         :as :alist :junk-allowed junk-allowed
                         :keywords-to-read keywords-to-read
                         :keyword-normalizer keyword-normalizer
                         :normalize-all normalize-all
                         :exclude-normalize-keys exclude-normalize-keys
                         :unescape-unicode-escape-sequence unescape-unicode-escape-sequence))

