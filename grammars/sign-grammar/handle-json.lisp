(in-package :sign-grammar)

(defclass geoquery-item ()
  ((id
    :documentation "A unique id to refer to this geoquery-item"
    :initarg :id
    :accessor id
    :initform nil
    :type symbol)
   (eng
    :documentation "english form"
    :initarg :eng
    :accessor eng
    :initform nil
    :type string)
   (lsfb
    :documentation "lsfb form"
    :initarg :lsfb
    :accessor lsfb
    :initform nil
    :type list)
   (meaning
    :documentation "meaning of the geoquery-item"
    :initarg :meaning
    :accessor meaning
    :initform nil
    :type list)
   (variables
    :documentation "The list of variables in the form"
    :initarg :variables
    :accessor variables
    :initform nil
    :type list)
  (nr-of-variations
    :documentation "The nr of variations that exist for this geoquery item"
    :initarg :nr-of-variations
    :accessor nr-of-variations
    :initform 1)
  (queried-attribute
    :documentation "The type of queried attribute"
    :initarg :queried-attribute
    :accessor queried-attribute
    :initform nil
    :type symbol)
  (predicate-types
    :documentation "A list of all the types of predicates in the meaning"
    :initarg :predicate-types
    :accessor predicate-types
    :initform nil
    :type list)))


(defun load-data (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          for data = (when line (cl-json:decode-json-from-string line))
          while data
          collect data)))

(defun make-templated-item (json)
  (let ((split-meaning (split-sequence::split-sequence #\( (cdr (assoc :meaning json))))
        (meaning-variables '())
        (new-split-meaning '())
        (new-form nil)
        (keyword-list '("LARGEST" "POPULATION"  "FEWEST" "MOST" "COUNT" "?_" "ELEVATION" "DENSITY" "CAPITAL" "AREA" "PLACE" "SMALLEST" "MAJOR" "PLACEID" "CITYID" "SHORTEST" "LONGEST" "TRAVERSE" "ANSWER" "?C"  "COUNTRYID" "?A" "?D" "CITY" "LOC" "?B" "CONST" "STATEID" "HIGHEST" "?E" "?F" "HIGHER" "HIGH_POINT" "STATE" "NEXT_TO" "?G" "?H" "?I" "?J" "?K" "?L" "RIVERID" "LEN" "?M" "LOWEST" "?N" "SIZE" "?O" "?P" "RIVER" "LOW_POINT" "LOWER" "LAKE" "NOT" "USA" "SUM" "MOUNTAIN" "0")))
    (loop with change-form? = nil
          for item in split-meaning
          do (when (NOT (string= item ""))
                       (pushend (loop with new-list = '()
                             with split-meaning-item = (split-sequence::split-sequence #\space item)
                             with variable-counter = 1
                             for keyword in split-meaning-item
                             for cleaned-keyword = (remove #\) keyword)
                             do (when (NOT (string= cleaned-keyword ""))(if (member cleaned-keyword keyword-list :test #'string=)
                                                               (pushend keyword new-list)
                                                               (progn (pushend (concatenate 'string "?VARIABLE-PREDICATE-" (write-to-string variable-counter)) new-list)
                                                                 (pushend (cons (concatenate 'string "?VARIABLE-PREDICATE-" (write-to-string variable-counter)) keyword) meaning-variables)
                                                                 (incf variable-counter)
                                                                 (setf change-form? t)
                                                                 )))
                             finally (return (format nil "~{~A~^ ~}" new-list))) new-split-meaning ))
             (if change-form?
               (loop with form = (cdr (assoc :utterance json))
                     for variable in meaning-variables
                     for variable-name = (car variable)
                     for variable-value = (string-replace (cdr variable) "_" " ")
                     do (setf form (string-replace form variable-value variable-name))
                     finally (setf new-form form)
                     )
               (setf new-form (cdr (assoc :utterance json)))))
    `(,new-form ,(format nil "((~{~A~^(~}" new-split-meaning) ,meaning-variables)))


(defun compare-meaning (meaning geoquery-item)
  (string= meaning (meaning geoquery-item)))

(defun find-queried-attribute (meaning)
  (first (split-sequence::split-sequence #\space (fourth (split-sequence::split-sequence #\( meaning)))))

(defun find-predicate-types (meaning)
  (loop with scores = '()
        with output = '()
        with counter = 0
        with predicate-list = '("LARGEST" "POPULATION"  "FEWEST" "MOST" "COUNT" "ELEVATION" "DENSITY" "CAPITAL" "AREA" "PLACE" "SMALLEST" "MAJOR" "PLACEID" "CITYID" "SHORTEST" "LONGEST" "TRAVERSE" "COUNTRYID" "CITY" "LOC" "STATEID" "HIGHEST" "HIGHER" "HIGH_POINT" "STATE" "NEXT_TO"  "RIVERID" "LEN"  "LOWEST"  "SIZE" "RIVER" "LOW_POINT" "LOWER" "LAKE" "NOT" "SUM" "MOUNTAIN")
        for predicate in (split-sequence::split-sequence #\( meaning)
        for predicate-name = (first (split-sequence::split-sequence #\space predicate))
        do (when (member predicate-name predicate-list :test #'string=)
             (pushend predicate-name output))
           (unless (string= predicate "") (incf counter))
           (setf scores (loop with output-per-type = '()
                              for predicate in predicate-list
                              do (if (member predicate output :test #'string=)
                                   (pushend 1 output-per-type)
                                   (pushend 0 output-per-type))
                              finally (return output-per-type)))
           (pushend counter scores)
        finally (return scores)))

(defun compare-cdr (cons-1 cons-2)
  (string= (cdr cons-1) (cdr cons-2)))

(defun file->geoquery-items (file)
  (let ((data (load-data file))
        (output '()))
    (loop for item in data
          for templated-item = (make-templated-item item)
          for templated-form = (first templated-item)
          for templated-meaning = (second templated-item)
          for variables = (third templated-item)
          for existing-template-in-output = (find templated-meaning output :test #'compare-meaning)
          do (if existing-template-in-output
               (progn (setf variables (loop with new-variables = '()
                                            for variable in variables
                                            do (unless (member variable (variables existing-template-in-output) :test #'compare-cdr)
                                                 (pushend variable new-variables))
                                            finally (return new-variables)))
                 (when variables (setf (variables existing-template-in-output) (append (variables existing-template-in-output) variables))
                   (incf (nr-of-variations existing-template-in-output))))
               (pushend (make-instance 'geoquery-item
                                       :id (make-id)
                                       :eng templated-form
                                       :meaning templated-meaning
                                       :variables variables
                                       :queried-attribute (find-queried-attribute templated-meaning)
                                       :predicate-types (find-predicate-types templated-meaning)) output)))
    output))

(defun geoquery-items->csv (geoquery-items)
  (let ((pathname (babel-pathname :directory '("grammars" "sign-grammar" "data")
                         :name "geoquery"  :type "csv")))
  (with-open-file (stream pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :overwrite)
    (format stream "english form,meaning,variables,#variations,queried attribute,largest,population,fewest,most,count,elevation,density,capital,area,place,smallest,major,placeid,cityid,shortest,longest,traverse,countryid,city,loc,stateid,highest,higher,high_point,state,next-to,riverid,len,lowest,size,river,low_point,lower,lake,not,sum,mountain~%")
    (loop for item in geoquery-items
          do (format stream (concatenate 'string (format nil "~a,~a,~a,~a,~a," (eng item) (meaning item) (variables item) (nr-of-variations item) (queried-attribute item)) (format nil "~{~A~^,~}~%" (predicate-types item))))))))
        
;(geoquery-items->csv (file->geoquery-items "/Users/liesbetdevos/Projects/Corpora/geoquery/geoquery_en.jsonl"))