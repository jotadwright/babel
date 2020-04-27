(in-package :cl-propbank)

;;;;;;;;;;;;;;;;
;;            ;;
;; Utilities  ;;
;;            ;;
;;;;;;;;;;;;;;;;

(defmacro pb-directory ()
  "Get actual pathname, before ASDF has redirected everything."
  `(pathname-directory ,(or *load-truename* *compile-file-truename*)))

(defun propbank-string->symbol (string)
  "Returns the name of the frame as a symbol."
  (let* ((string (cl-ppcre:regex-replace-all "\\(" (string-upcase string) ""))
         (string (cl-ppcre:regex-replace-all "\\)" (string-upcase string) ""))        
         (string (cl-ppcre:regex-replace-all "_" (string-upcase string) "-"))
         (string (cl-ppcre:regex-replace-all " " (string-upcase string) "-")))
  (make-symbol string)))
