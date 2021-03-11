(in-package :propbank-english)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;; Web service underlying de CCxG explorer ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CCxG explorer annotations are stored here:
*ccxg-explorer-annotations*

;; Loading PropBank annotations (only for data creation)

(load-propbank-annotations 'ewt :ignore-stored-data nil)
(load-propbank-annotations 'ontonotes :ignore-stored-data nil)

;; Creating CCxG explorer annoatations
(make-ccxg-explorer-annotations-for-propbank-sentences :ontonotes
                                                       (append (train-split *ontonotes-annotations*)
                                                               (dev-split *ontonotes-annotations*)
                                                               (test-split *ontonotes-annotations*)))

(make-ccxg-explorer-annotations-for-propbank-sentences :ewt
                                                       (append (train-split *ewt-annotations*)
                                                               (dev-split *ewt-annotations*)
                                                               (test-split *ewt-annotations*)))




(pprint (find-by-schema :ontonotes '(==p (==1 (:ROLE-TYPE arg0))
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
