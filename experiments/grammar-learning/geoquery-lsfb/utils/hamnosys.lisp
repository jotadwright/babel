(in-package :geoquery-lsfb)

(defun make-fingerspelling (input &key (input-type 'gloss))
  "takes a gloss or geographical name as input (string) and returns its fingerspelled hamnosys form as a string"
  (loop with output = ""
        ;; read in the fingerspelling alphabet from the data folder
        with fingerspelling-alphabet =
          (jsonl->list-of-json-alists
           (merge-pathnames
            *data-folder*
            (babel-pathname
             :name "fingerspelling-alphabet"
             :type "jsonl")))
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

;(make-fingerspelling "nouveau-mexique" :input-type 'name)
