(in-package :slp)

(defparameter *hamnosys->sigml-intevory*
  (with-open-file (stream
                   (merge-pathnames (make-pathname :directory '(:relative "GeoQuery-LSFB")
                                                   :name "hamnosys-to-sigml-inventory"
                                                   :type "txt")
                                    *babel-corpora*)
                   :external-format :utf-8 :element-type 'cl:character) 
    (loop for line = (read-line stream nil)
          for splitted-line = (when line (split-sequence::split-sequence #\, line))
          while line
            collect `(,(parse-integer
                          (second splitted-line)
                          :radix 16)
                      ,(first splitted-line)))))

(defun find-sigml-name (hamnosys-character)
  "finds the sigml name of a hamnosys character"
  (first
   (cdr (assoc 
         (char-code hamnosys-character)
         *hamnosys->sigml-intevory*))))

(defun hamnosys->sigml (hamnosys)
  "transforms hamnosys string into sigml notation and returns
   this sigml as an xmls-node"
  (xmls::make-node
   :name "sigml"
   :children
   `(,(xmls::make-node
       :name "hns_sign"
       :children
       `(,(xmls::make-node
           :name "hamnosys_manual"
           :children
           (loop for character across hamnosys
                  collect
                    (xmls::make-node
                     :name (find-sigml-name character)))))))))

  
  