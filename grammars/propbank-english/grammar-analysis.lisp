(in-package :propbank-english)


(defparameter *restored-grammar*
  (restore (babel-pathname :directory '("grammars" "propbank-english" "grammars")
                           :name "propbank-grammar-ontonotes-ewt-cleaned"
                           :type "fcg")))

(defun find-constructions-by-schema (schema cxn-inventory &key (collect-fn #'identity))
  "Returns the constructions of which the schema matches the given schema."
  (loop for cxn in (constructions-list cxn-inventory)
        for cxn-schema = (attr-val cxn :schema)
        if (and schema
                (unify schema cxn-schema))
        collect (funcall collect-fn cxn)))


(defun lex-items-for-schema (schema cxn-inventory)
  ""
  (let* ((gram-categories (find-constructions-by-schema schema cxn-inventory
                                                       :collect-fn #'(lambda (cxn)
                                                                       (attr-val cxn :gram-category))))
        (graph (type-hierarchies::graph (get-type-hierarchy cxn-inventory)))
        (lex-items-with-frequency (loop with lexical-items-with-frequency = (make-hash-table)
                                        for g in gram-categories
                                        for node-vector = (graph-utils::node-vector g graph :edge-type 'lex-gram)
                                        do (loop for lex-item being the hash-keys in node-vector using (hash-value frequency)
                                                 for lex-item-symbol = (graph-utils::lookup-node graph lex-item)
                                                 if (gethash lex-item-symbol lexical-items-with-frequency)
                                                 do (incf (gethash lex-item-symbol lexical-items-with-frequency) frequency)
                                                 else do (setf (gethash lex-item-symbol lexical-items-with-frequency) frequency))
                                        finally return lexical-items-with-frequency)))
    
    (loop for lex-item being the hash-keys in lex-items-with-frequency using (hash-value frequency)
          collect (cons lex-item frequency) into lex-items-with-frequency-list
          finally return (sort lex-items-with-frequency-list #'> :key #'cdr))))
          

(pprint (lex-items-for-schema '((:arg0  ?x)
                                (:v v)
                                (:arg1 ?y)
                                (:arg2 . pp\(to\)))
                              *restored-grammar*))


(pprint (lex-items-for-schema '((:arg0  ?x)
                                (:v v)
                                (:arg2 ?y)
                                (:arg1 . ?z))
                              *restored-grammar*))





(lex-items-for-schema '((:arg0  np)
                        (:v v)
                        (:arg1 np)
                        (:arg2 . pp\(to\)))
                      *restored-grammar*)


(pprint (lex-items-for-schema '((:arg1 ?x)
                                (:v v))
                              *restored-grammar*))



(pprint (find-constructions-by-schema '((:arg0  np)
                                        (:v v)
                                        (:arg1 np)
                                        (:arg2 . pp\(to\)))
                                      *restored-grammar*
                                      :collect-fn #'(lambda (cxn) (fcg::description cxn))))

(comprehend-and-extract-frames "The man made it work ." :cxn-inventory *restored-grammar*)


*saved-cipn*

(attr-val *saved-cxn* :schema)