(in-package :propbank-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;; Web service underlying the CCxG explorer ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CCxG explorer annotations are stored here:
*ccxg-explorer-annotations*

;; Loading PropBank annotations (only for data creation)

(load-propbank-annotations 'ewt :ignore-stored-data nil)
(load-propbank-annotations 'ontonotes :ignore-stored-data nil)

;; Creating CCxG explorer annotations
(make-ccxg-explorer-annotations-for-propbank-sentences :ontonotes
                                                       (append (train-split *ontonotes-annotations*)
                                                               (dev-split *ontonotes-annotations*)
                                                               (test-split *ontonotes-annotations*)))

(make-ccxg-explorer-annotations-for-propbank-sentences :ewt
                                                       (append (train-split *ewt-annotations*)
                                                               (dev-split *ewt-annotations*)
                                                               (test-split *ewt-annotations*)))







;; What patterns occur with explain.01 in which arg0, arg1 and arg2 are expressed?
(length (find-by-schema :ontonotes '(==p (==1 (:ROLE-TYPE arg0))
                                         (==1 (:ROLE-TYPE v)
                                              (:ROLESET explain.01))
                                         (==1 (:ROLE-TYPE arg2))
                                         (==1 (:ROLE-TYPE arg1)))))


;; Explain me this? Do such patterns occur?
(pprint (find-by-schema :ontonotes '((==1 (:ROLE-TYPE arg0))
                                     (==1 (:ROLE-TYPE v)
                                          (:ROLESET explain.01))
                                     (==1 (:ROLE-TYPE arg2))
                                     (==1 (:ROLE-TYPE arg1)))))








(cl-json:decode-json-from-string "null")

 (cl-json:encode-json-alist-to-string '((:schema
                                                                         ((:ROLE-TYPE . nil))
                                                                         ((:ROLE-TYPE . v)
                                                                          (:ROLESET . explain.01))
                                                                         ((:ROLE-TYPE . arg2))
                                                                         ((:ROLE-TYPE . arg1))))))
                               

                                 
                                 (((:ROLE-TYPE . arg0))
                                 ((:ROLE-TYPE . v)
                                  (:ROLESET . explain.01))
                                 ((:ROLE-TYPE . arg2))
                                 ((:ROLE-TYPE . arg1)))))




(pprint (find-by-schema :ontonotes '( (==1 (:ROLE-TYPE arg0))
                                      (==1 (:ROLE-TYPE v)
                                           (:ROLESET explain.01))
                                      (==1 (:ROLE-TYPE arg2)
                                           )
                                      (==1 (:ROLE-TYPE arg1)
                                           ))))



(defun get-posses ()
       (loop for annotation in (append (get-corpus :ontonotes) (get-corpus :ewt))
             for roles = (second (assoc :roles annotation))
             append (loop for role in roles
                          append (second (assoc :pos role)))
             into posses-list
             finally return (loop for group in (group-by posses-list #'identity)
                                  collect (cons (first group) (length group)))))

(setf *posses* (get-posses))

(pprint (sort *posses* #'> :key #'cdr))



(defun get-args ()
       (loop for annotation in (append (get-corpus :ontonotes) (get-corpus :ewt))
             for roles = (second (assoc :roles annotation))
             append (loop for role in roles
                          collect (second (assoc :role-type role)))
             into posses-list
             finally return (loop for group in (group-by posses-list #'identity)
                                  collect (cons (first group) (length group)))))

(setf *args* (get-args))

(pprint (sort *args* #'> :key #'cdr))


