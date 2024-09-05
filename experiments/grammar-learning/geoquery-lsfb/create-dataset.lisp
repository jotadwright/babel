(in-package :geoquery-lsfb)

(defparameter *geoquery-ids*
  '("1" "2" "175" "188" "199" "255" "617" "10" "13" "21" "33" "34" "26" "27" "35" "37" "38" "42" "44" "45" "46" "47" "433" "437" "41" "598" "48" "52" "54" "61" "63" "80" "83" "92" "93" "105" "505" "516" "518" "519" "523" "527" "534" "538" "551" "67" "73" "78" "85" "94" "513" "526" "531" "550" "87" "97" "510" "515" "528" "533" "535" "537" "101" "113" "116" "120" "127" "124" "125" "130" "595" "154" "155" "198" "735" "870" "214" "215" "217" "222" "223" "224" "226" "233" "234" "238" "246" "265" "272" "278" "282" "283" "288" "296" "290" "291" "302" "303" "305" "307" "308" "313" "412" "417" "310" "311" "314" "420" "321" "325" "326" "330" "331" "333" "351" "352" "353" "346" "347" "355" "356" "357" "366" "374" "378" "379" "381" "382" "385" "400" "776" "777" "372" "386" "388" "397" "398" "403" "392" "401" "410" "414" "424" "442" "446" "447" "450" "456" "457" "464" "465" "466" "467" "469" "470" "471" "780" "472" "473" "474" "480" "779" "479" "485" "489" "654" "815" "495" "497" "502" "541" "545" "555" "557" "561" "562" "568" "570" "574" "572" "573" "578" "579" "590" "499" "597" "600" "619" "628" "631" "634" "800" "633" "637" "696" "697" "698" "699" "700" "701" "702" "705" "706" "707" "708" "709" "754" "806" "807" "808" "847" "848" "852" "854" "856" "858" "644" "646" "653" "816" "818" "648" "681" "649" "742" "743" "841" "875" "652" "660" "819" "683" "839" "728" "731" "736" "869" "872" "756" "795" "801" "809" "810" "817" "820" "823" "837" "843" "862" "876"))



(defun xmls->prolog (xmls)
  (loop for child in (xmls:node-children xmls)
        for tier-id = (loop for attribute in (xmls:node-attrs child)
                            when (string= (first attribute) "TIER_ID")
                              do (return (read-from-string (second attribute))))
        when (eql tier-id 'Prolog)
          do (return (string-replace (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children child)))))))) "\"" ""))))

(defun xmls->meaning (xmls)
  (loop for child in (xmls:node-children xmls)
        for tier-id = (loop for attribute in (xmls:node-attrs child)
                            when (string= (first attribute) "TIER_ID")
                              do (return (read-from-string (second attribute))))
        when (eql tier-id 'Predicates)
          do (return (string-replace (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children child)))))))) "\"" ""))))

(defun xmls->translation (xmls &key (language-tier-id "English translation"))
  (loop for child in (xmls:node-children xmls)
        for tier-id = (loop for attribute in (xmls:node-attrs child)
                            when (string= (first attribute) "TIER_ID")
                              do (return (second attribute)))
        when (string= tier-id language-tier-id)
          do (return (string-replace (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children (first (xmls:node-children child)))))))) "\"" ""))))

(defun make-pretty-predicates (predicates)
  (loop with output = ""
        for predicate in predicates
        for arguments = (loop with argument-string = ""
                              for argument in (rest predicate)
                              do (if (string= argument-string "")
                                   (setf argument-string (format nil "~a~a" argument-string argument))
                                   (setf argument-string (format nil "~a,~a" argument-string argument)))
                              finally (return argument-string))
        do (setf output (if (string= output "")
                          (format nil "~a~a(~a)" output (first predicate) arguments)
                          (format nil "~a,~a(~a)" output (first predicate) arguments)))
        finally (return output)))


(defun find-funql (xml id)
  (loop for example in (xmls:node-children xml)
        do (when (string= (second (first (xmls:node-attrs example))) id)
             (return (loop for child in (xmls:node-children example)
                           do (when (string= (second (first (xmls:node-attrs child))) "geo-funql")
                                (return (first (xmls:node-children child)))))))))
             
  
(find-funql (read-xml "/Users/liesbetdevos/Projects/geoquery-sign/original-geoquery.xml") "45")

(defun make-json-dataset (input-path output-path original-dataset-path)
  (let ((files (directory input-path))
        (list-of-ids '()))
    (with-open-file (out-stream output-path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (loop for file in files
          for xmls = (read-xml file)
          for file-name = (pathname-name file)
          for id = (third (split-sequence::split-sequence #\_ file-name))
          for lsfb-form = (make-pretty-predicates (xmls->hamnosyspredicates xmls))
          for english-form = (xmls->translation xmls :language-tier-id "English translation")
          for french-form = (xmls->translation xmls :language-tier-id "French Translation")
          for turkish-form = (xmls->translation xmls :language-tier-id "Turkish translation")
          for japanese-form = (xmls->translation xmls :language-tier-id "Japanese translation")
          for spanish-form = (xmls->translation xmls :language-tier-id "Spanish translation")
          ;for meaning = (xmls->meaning xmls)
          for prolog = (xmls->prolog xmls)
          for funql = (find-funql (read-xml original-dataset-path) id)
          do (when (and (member id *geoquery-ids* :test #'string=)
                        (NOT (member id list-of-ids :test #'string=)))
               (format out-stream "~a"(string-downcase (format nil "~a~%" (encode-json-alist-to-string
                                                                           `((lsfb . ,lsfb-form)
                                                                             (french . ,french-form)
                                                                             (english . ,english-form)
                                                                             (spanish . ,spanish-form)
                                                                             (turkish . ,turkish-form)
                                                                             (japanese . ,japanese-form)
                                                                             ;(meaning . ,meaning)
                                                                             (geo-prolog . ,prolog)
                                                                             (geo-funql . ,funql)
                                                                             (id . ,id)))))))
          (pushend id list-of-ids)))
    list-of-ids))

(defun compare-id-lists (list-1 list-2)
  (loop with output-list = list-2
        for id in list-1
        do (setf output-list (remove id output-list :test #'string=))
        finally (return output-list)))

;(compare-id-lists (make-json-dataset *input-path* *output-path*) *geoquery-ids*)

(defun string->predicates (string)
  (let ((split-string (split-sequence::split-sequence #\) string ))
        (output-list '()))
    (delete "" split-string :test #'string=)
    (loop for item in split-string
          for sublist = '()
          for cleaned-item = (string-replace item ":" "\\:")
          for split-predicate = (delete "" (split-sequence::split-sequence #\( cleaned-item) :test #'string=)
          for first-arg = (remove #\, (first split-predicate))
          for remaining-args = (split-sequence::split-sequence #\, (second split-predicate))
          do (pushend (read-from-string first-arg) sublist)
             (loop for remaining-arg in remaining-args
                   do (pushend (read-from-string remaining-arg) sublist))
             (pushend sublist output-list))
    output-list))


(defun read-dataset (dataset-path)
  (with-open-file (in-stream dataset-path :direction :input)
    (loop for line = (read-line in-stream nil)
          for json-alist = nil
          while line do (setf json-alist (decode-json-from-string line))
          (pprint (string->predicates(cdr (assoc :lsfb json-alist))))
                        ;(add-element (slp::represent-signs (string->predicates(cdr (assoc :lsfb json-alist)))))
          )))

;(read-dataset *output-path*)


      
      