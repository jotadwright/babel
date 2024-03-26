(in-package :geoquery-lsfb)

(defparameter *default-output-json*
  (babel-pathname :directory '("grammars" "sign-grammar" "data" "LSFB-geoquery.json")))

(defparameter *input-path*
  (pathname "/Users/liesbetdevos/Projects/geoquery-sign/subset-1/annotations/"))

(defun xmls->meaning (xmls)
  (loop for child in (xmls:node-children xmls)
        for tier-id = (loop for attribute in (xmls:node-attrs child)
                            when (string= (first attribute) "TIER_ID")
                              do (return (read-from-string (second attribute))))
        when (eql tier-id 'MEANING)
          do (return (string-replace (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children child)))))))) "\"" ""))))

(defun xmls->english-form (xmls)
  (loop for child in (xmls:node-children xmls)
        for tier-id = (loop for attribute in (xmls:node-attrs child)
                            when (string= (first attribute) "TIER_ID")
                              do (return (read-from-string (second attribute))))
        when (eql tier-id 'TR-ENG)
          do (return (string-replace (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children child)))))))) "\"" ""))))


(defun make-json-dataset ()
  (let ((files (directory "/Users/liesbetdevos/Projects/geoquery-sign/subset-1/annotations/*.eaf")))
    (with-open-file (out-stream *default-output-json* :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format out-stream "\[")
      (loop for file in files
            for xmls = (read-elan file)
            for form = (xmls->predicates xmls)
            for meaning = (xmls->meaning xmls)
            for english-form = (xmls->english-form xmls)
            do (format out-stream "{\"form\":\"~A\", \"meaning-network\":\"~A\", \"english-form\":\"~A\"},~%" form meaning english-form)))))


              
      
;(make-json-dataset)