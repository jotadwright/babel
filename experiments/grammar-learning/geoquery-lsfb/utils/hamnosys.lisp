(in-package :geoquery-lsfb)

(defun display-hamnosys (&key (absolute-P nil))
  "Makes css and css links that add hamnosys-webfont to the web-interface"
  (setf web-interface::*dispatch-table*
      (append web-interface::*dispatch-table* (list (web-interface::create-static-file-dispatcher-and-handler 
                                                     "/hamnosys.css" (babel-pathname 
                                                                      :directory '("systems" "web-interface" "hamnosys")
                                                                      :name "hamnosys" :type "css") "text/css"))))
  (if absolute-P
    (progn
      (define-css-link 'hamnosys.css absolute-P)
      (define-css 'main "ham {font-size: 10pt; font-family: hamnosysunicoderegular}"))
    (progn 
      (define-css-link 'hamnosys.css "/hamnosys.css")
      (define-css 'main "ham {font-size: 10pt; font-family: hamnosysunicoderegular}")))
  )

(display-hamnosys)

(defun make-fingerspelling (input &key (input-type 'gloss))
  "takes a gloss or geographical name as input (string) and returns its fingerspelled hamnosys form as a string"
  (loop with output = ""
        ;; read in the fingerspelling alphabet from the data folder
        with fingerspelling-alphabet =
          (jsonl->list-of-json-alists
           (concatenate
            'string
            *data-folder*
            "/fingerspelling-alphabet.jsonl"))
        ;; extract name to be fingerspelled from input
        with fingerspelling-name =
          (case input-type
            ;; if the input is a gloss, remove any prefixes
            ('gloss
             (remove
              #\:
              (second
               (split-sequence:split-sequence
                #\\
                (string input)))))
            ;; if the input is not a gloss, use it as is
            ('name
             input))

        ;; find the hamnosys character for each character of the input
        for character across fingerspelling-name
        for character-hamnosys =
          (loop for letter in fingerspelling-alphabet
                when (string=
                      (cdr
                       (assoc :letter letter))
                      (string-upcase
                       (string character)))
                  do 
                    (return
                     (cdr
                      (assoc :hamnosys letter))))
        ;; if the character is not the first in the string, add a comma between the previous and this character
        do (if (string=
                output
                "")
             (setf
              output
              (concatenate
               'string
               output
               character-hamnosys))
             (setf
              output
              (concatenate
               'string
               output
               (string
                (code-char  57514))
               character-hamnosys)))
        finally (return output)))

;(make-fingerspelling "niagara-falls" :input-type 'name)
;(concatenate 'string *data-folder* "/fingerspelling-alphabet.jsonl")