(in-package :au-benchmark.base)

(export '(list-of-strings->string
          downcase upcase
          make-kw))

(defun list-of-strings->string (list &key (separator " "))
  "Turn a list of strings into a single string. The indidual strings are separated by sep"
  ;; check the input
  (assert (every #'identity (mapcar #'stringp list)))
  (assert (stringp separator))
  ;; concatenate the strings
  (mapconcat #'identity list separator))

(defun downcase (str)
  (format nil "~(~a~)" str))

(defun upcase (str)
  (format nil "~:@(~a~)" str))

(defun make-kw (name)
  "takes a string, e.g. \"test\" and turns it into a
   symbol interned in the keyword package, e.g. :test"
  (values (intern (string-upcase name) :keyword)))