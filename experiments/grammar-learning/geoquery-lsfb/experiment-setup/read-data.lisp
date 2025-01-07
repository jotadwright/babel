(in-package :geoquery-lsfb)

;;--------------------------------;;
;; reading data and preprocessing ;;
;;--------------------------------;;

;; splitting jsonl files into separate json-files + divide them over train and test set
;; train-test ratio: 80 train, 20 test

(defun make-id-list (jsonl-alists)
  (loop for alist in jsonl-alists
        for id
          = (parse-integer
             (cdr
              (assoc :id alist)))
        collect id))

(defun list-of-random-test-split-ids (id-list test-ratio)
  "Splits the list of all ids into a train and test split,
   following the indicated ratio. It returns a list containing
   the ids of examples in the test-set"
  (loop with test-split = '()
        with id-list-length
          = (length id-list)
        with test-size
          = (round
             (*
              id-list-length
              test-ratio))
        for i from 1 to test-size
        for random-id
          = (random-elt id-list)
        do (while
               (member
                random-id
                test-split)
             (setf
              random-id
              (random-elt
               id-list)))
           (push
            random-id
            test-split)
        finally
          (return test-split)))

(defun write-alist-to-file (alist directory)
  (let* ((alist-id
          (cdr
           (assoc :id alist)))
         (pathname
          (format
           nil
           "~a~a.json"
           directory
           alist-id)))
  (with-open-file
      (out-stream
       pathname
       :direction :output
       :if-exists :supersede
       :if-does-not-exist :create
       :external-format :utf-8
       :element-type 'cl:character)
    (format
     out-stream
     "{\"id\":\"~a\",\"type\":\"~a\",\"elan-ref\":\"~a\",\"english\":\"~a\",\"spanish\":\"~a\",\"turkish\":\"~a\",\"japanese\":\"~a\",\"german\":\"~a\",\"greek\":\"~a\",\"thai\":\"~a\",\"indonesian\":\"~a\",\"swedish\":\"~a\",\"chinese\":\"~a\",\"farsi\":\"~a\",\"french\":\"~a\",\"lsfb\":~a,\"geo-prolog\":\"~a\",\"geo-funql\":\"~a\",\"sql\":\"~a\"}~%"
     (cdr (assoc :id alist))
     (cdr (assoc :type alist))
     (cdr (assoc :elan-ref alist))
     (cdr (assoc :english alist))
     (cdr (assoc :spanish alist))
     (cdr (assoc :turkish alist))
     (cdr (assoc :japanese alist))
     (cdr (assoc :german alist))
     (cdr (assoc :greek alist))
     (cdr (assoc :thai alist))
     (cdr (assoc :indonesian alist))
     (cdr (assoc :swedish alist))
     (cdr (assoc :chinese alist))
     (cdr (assoc :farsi alist))
     (cdr (assoc :french alist))
     (format-as-json-lists
        (cdr (assoc :lsfb alist)))
     (cdr (assoc :geo-prolog alist))
     (cdr (assoc :geo-funql alist))
     (cdr (assoc :sql alist))))))

(defun split-train-and-test (json-dataset output-directory test-ratio)
  (loop with train-directory
          = (concatenate
             'string
             output-directory
             "/train/")
        with test-directory
          = (concatenate
             'string
             output-directory
             "/test/")
        with id-list
          = (make-id-list
             json-dataset)
        with test-ids
          = (list-of-random-test-split-ids
             id-list
             test-ratio)
        for alist in json-dataset
        for id
          = (parse-integer
             (cdr
              (assoc :id alist)))
        do (if (member
                id
                test-ids)
             (write-alist-to-file
              alist
              test-directory)
             (write-alist-to-file
              alist
              train-directory))
        finally
        (format
         t
         "~%train and test splits were created:~%* train ratio: ~a~%* test ratio: ~a~%* directory: ~a~%* test-set ids: ~a"
         (- 1 test-ratio)
         test-ratio
         output-directory
         test-ids)))
             
             
        
;(split-train-and-test *250-dataset-json* "/Users/liesbetdevos/Projects/GeoQuery-data/250" 0.2)

;(split-train-and-test *4500-dataset-json* "/Users/liesbetdevos/Projects/GeoQuery-data/4500" 0.2)

(defun preprocess-and-merge-english-json-files (directory outfile)
  (with-open-file
      (out-stream
       outfile
       :direction :output
       :if-exists :supersede
       :if-does-not-exist :create
       :external-format :utf-8
       :element-type 'cl:character)
    (loop with sorted-examples = (make-hash-table)
          for file in (directory directory)
          for json = (unless
                         (string=
                          (pathname-type file)
                          "DS_Store")
                       (first
                        (jsonl->list-of-json-alists file)))
          for utterance = (when json (cdr (assoc :english json)))
          for meaning = (when json (geo-prolog-to-predicates
                                    (cdr (assoc :geo-prolog json))))
          for template-examples = (when json (gethash (intern (cdr (assoc :elan-ref json))) sorted-examples))
          when json 
            do (setf (gethash (intern (cdr (assoc :elan-ref json))) sorted-examples)
                     (if template-examples
                       (append template-examples
                               (list
                                (format
                                nil
                                "{\"id\":\"~a\",\"utterance\":\"~a\",\"meaning\":\"~a\",\"elan-ref\":\"~a\"}~%"
                                (cdr (assoc :id json))
                                utterance
                                meaning
                                (cdr (assoc :elan-ref json)))))
                       (list (format
                              nil
                              "{\"id\":\"~a\",\"utterance\":\"~a\",\"meaning\":\"~a\",\"elan-ref\":\"~a\"}~%"
                              (cdr (assoc :id json))
                              utterance
                              meaning
                              (cdr (assoc :elan-ref json)))

              )))
          finally (loop with output = '()
                        with sorted-string-lists =
                          (loop for json-strings being the hash-values of sorted-examples
                                collect json-strings)
                        for sorted-string-list in sorted-string-lists
                        do (if output
                             (loop with counter = 0
                                   for list in output
                                   for ref-length = (length list)
                                   for sorted-string-list-length = (length sorted-string-list)
                                   do (if (> sorted-string-list-length ref-length)
                                        (if (= counter 0)
                                         (push sorted-string-list output)
                                         (utils::insert-after output (- counter 1) sorted-string-list))
                                       (incf counter)))
                             (push sorted-string-list output))
                            
                        finally (loop for output-list in output
                                      do (loop for output-string in output-list
                                               do (format
                                                   out-stream
                                                   output-string)))))))
                        

;(preprocess-and-merge-english-json-files "/Users/liesbetdevos/Projects/GeoQuery-data/4500/train/" "/Users/liesbetdevos/Documents/geoquery-english-train-4500.jsonl")


