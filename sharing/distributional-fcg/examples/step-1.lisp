(ql:quickload :propbank-grammar)
(ql:quickload :distributional-fcg)

(in-package :propbank-grammar)


;(setf nlp-tools::*penelope-host* "http://127.0.0.1:5000")
;(setf nlp-tools::*embedding-host* "http://127.0.0.1:5001")

(defun find-closest-match (lemma cxn-inventory)
  (let ((vector (second (nlp-tools:get-word-embedding (downcase lemma))))
        (lex-cxns (loop for cxn in (constructions-list cxn-inventory)
                        when (attr-val cxn :lex-category)
                          collect cxn)))
       (loop for cxn in lex-cxns
             for lemma = (downcase (attr-val cxn :lemma))
             for pointer =  (attr-val cxn :lemma-embedding-pointer)
             for cxn-vector = (cdr (assoc pointer (get-data (blackboard cxn-inventory) :cxn-token-embeddings)))
             for similarity = (cosine-similarity vector cxn-vector)
             collect (cons lemma similarity) into similarities
             finally (return (sort similarities #'> :key #'cdr)))))

(deactivate-all-monitors)

(progn 

  (setf *ontonotes-annotations-storage-file*
        (merge-pathnames (make-pathname :directory (cons :relative '("propbank-annotations"))
                                        :name "ontonotes-annotations"
                                        :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                         *babel-corpora*))

  (setf *ewt-annotations-storage-file* (merge-pathnames (make-pathname :directory (cons :relative '("propbank-annotations"))
                                                                       :name "ewt-annotations"
                                                                       :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                                        *babel-corpora*))



  (load-propbank-annotations 'ewt :ignore-stored-data nil)
  (load-propbank-annotations 'ontonotes :ignore-stored-data nil)

; *ewt-annotations*
; *ontonotes-annotations*


  (defparameter *training-configuration*
    `((:de-render-mode .  :de-render-constituents-dependents)
      (:node-tests :check-double-role-assignment)
      (:parse-goal-tests :no-valid-children :no-applicable-cxns)
      (:max-nr-of-nodes . 10)

      (:construction-inventory-processor-mode . :heuristic-search)
      (:search-algorithm . :best-first)   
      (:cxn-supplier-mode . :hashed-categorial-network)
    
      (:heuristics
       :nr-of-applied-cxns
       :nr-of-units-matched-x2 ;;nr-of-units-matched
       ) ;; edge-weight cannot be used, sometimes there are no neighbours
      ;;Additional heuristics: :prefer-local-bindings :frequency
    
      (:heuristic-value-mode . :sum-heuristics-and-parent)
      (:sort-cxns-before-application . nil)

      (:node-expansion-mode . :full-expansion)
      (:hash-mode . :hash-lemma)
    
      (:replace-when-equivalent . nil)
      (:learning-modes
       :core-roles)))

  (defparameter *propbank-ewt-ontonotes-learned-cxn-inventory* nil)


  (learn-distributional-propbank-grammar
   (append (train-split *ewt-annotations*)
           (train-split *ontonotes-annotations*))

   :excluded-rolesets '("be.01" "be.02" "be.03"
                        "do.01" "do.02" "do.04" "do.11" "do.12"
                        "have.01" "have.02" "have.03" "have.04" "have.05" "have.06" "have.07" "have.08" "have.09" "have.10" "have.11"
                        "get.03" "get.06" "get.24")
   :selected-rolesets nil
   :cxn-inventory '*propbank-ewt-ontonotes-learned-cxn-inventory*
   :fcg-configuration *training-configuration*)


  (cl-store:store *propbank-ewt-ontonotes-learned-cxn-inventory*
                  (merge-pathnames (make-pathname   :name "ewt-ontonotes-distributional-grammar-3-april"
                                                    :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                   *babel-corpora*))
  )



(defparameter *propbank-ewt-ontonotes-learned-cxn-inventory* (cl-store:restore
                                                              (merge-pathnames (make-pathname   :name "ewt-ontonotes-distributional-grammar-3-april"
                                                                                   :type #+lispworks "lw.store" #+ccl "ccl.store" #+sbcl "sbcl.store")
                                                                    *babel-corpora*)))


;; 0.9 & 0.8 geeft niets voor enchanting

(preprocessing-and-configs *propbank-ewt-ontonotes-learned-cxn-inventory* :step-1)

(activate-monitor trace-fcg)


;;; ENCHANTING  39 3306 6569 in dev-split ontonotes
;; enkel 39 kan met een hoge similarity geparsed worden, dus ga hiermee verder
;; closest match is 0.66 dus similarity naar beneden

(pprint (find-closest-match "enchanting" *propbank-ewt-ontonotes-learned-cxn-inventory*))
;; closest matches:  ("lovely" . 0.6675426) ("adorable" . 0.62586165) ("enjoyable" . 0.6111439) ("beautiful" . 0.610849) ("dazzling" . 0.59659225) ("terrifying" . 0.5627963) ("giddy" . 0.54182566)


(defparameter *solutions* nil)

(loop for i in (list 39 3306 6569)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 500 :n 1))
      do (push solutions *solutions*))

(defparameter *enchanting-solutions* nil)

;; enige hoop voor enchanting (maar duurt lang)
(loop for i in (list 0.6)
        do (set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold i)
           (loop for i in (list 39)
                 for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
                 for solutions = (multiple-value-list
                                  (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 1000 :n 1))
                 do (push solutions *enchanting-solutions*)))


;; EXHUME passive=problem?

(pprint (find-closest-match "exhume" *propbank-ewt-ontonotes-learned-cxn-inventory*))
;; closest matches: (("unearth" . 0.65681404) ("excavate" . 0.60981566) ("uncover" . 0.52135945) ("unseal" . 0.5033019) ("locate" . 0.49222195)

;; closest match is 0.65 so set threshold on 0.6
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.6)

;; make sentence simpler to test --> comprehension gives something but we don't get the unearth.01 and no arg-struct, maybe because of the passive? 
(defparameter *exhume*
  (comprehend-all "the airman is exhumed from his tomb" :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 500 :n 5))

;; test the full sentence:
(loop for i in (list 258)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 500 :n 5))
      do (push solutions *solutions*))


;; check whether arg struct and sense cxns that you expect are in this list: 
(pprint (graph-utils::neighbors
         (fcg::graph (categorial-network *propbank-ewt-ontonotes-learned-cxn-inventory*))
         'propbank-grammar::UNEARTH\(V\)-1 :return-ids? nil :edge-type 'lex-sense))



;; THATCH: 3427 ontonotes

(pprint (find-closest-match "thatch" *propbank-ewt-ontonotes-learned-cxn-inventory*))
; niet echt ideale opties:
; (("sand" . 0.42055455) ("bare" . 0.3922923) ("plaster" . 0.38972497) ("boo" . 0.38855404) ("bleached" . 0.38358036) ("tile" . 0.38284448)

(loop for i in (list 3427)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 500 :n 1))
      do (push solutions *solutions*))


(defparameter *scorch-solutions* nil)
(defparameter *misread-solutions* nil)
(defparameter *interject-solutions* nil)
(defparameter *enrage-solutions* nil)

(progn

"enrage" ;; ("infuriate" . 0.8419155) ("unnerve" . 0.77183545)
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.8)
(loop for i in (list 514 597 621 4901)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 1000 :n 1))
      do (push solutions *enrage-solutions*))
;; 621: "So I mean that right there it enraged me ."
;;  infuriate heeft toegepast, arg0 is it ipv that right there it, maar arg1 is correct me
;; veel andere cxns die toepassen, mss toch een selectie maken? vreemde dingen zoals way.01 op 'it', we als lexicale op me

"scorch" ;;("scald" . 0.5435439) ("burn" . 0.46978965) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.5)
(loop for i in (list 2517)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 1000 :n 1))
      do (push solutions *scorch-solutions*))

"misread" ;; ("misinterpret" . 0.6392812) ("misrepresent" . 0.532641)  ("misunderstand" . 0.52752597) !!!!!!!!!!!!!!!!!!!!! 
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.63)
(loop for i in (list 3595)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 1000 :n 1))
      do (push solutions *misread-solutions*))

;; zin is ook vreemd
"interject" ;; ("rephrase" . 0.6270519) ("shoehorn" . 0.5953462) ("browbeat" . 0.58865476) !!!!!!!
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.62)
(loop for i in (list 4710)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 1000 :n 1))
      do (push solutions *interject-solutions*))





;; slechte zin met typfouten
"sanctify" ;; ("glorify" . 0.63841355) ("exalt" . 0.6237715)  ("venerate" . 0.60114015)
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.6)
(loop for i in (list 14483)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 1000 :n 1))
      do (push solutions *sanctify-solutions*))


(defparameter *disrespect-solutions* nil)
;; deze is ook mogelijk, ook hier weer een hele hoop cxns die je liever niet wilt zien toepassen
"disrespect" ;; ("insult" . 0.6511977) ("insult" . 0.6511977) ("insult" . 0.6511977) ("disdain" . 0.6258957) ("shame" . 0.5817932) !!!!!!!!!!!!!!!!!
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.65)
(loop for i in (list 9829)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 1000 :n 1))
      do (push solutions *disrespect-solutions*))

;; start hier nu terug:

(progn 

(defparameter *colonize-solutions* nil) ;; geen frame toegepast
"colonize" ;; ("populate" . 0.64053935) ("invade" . 0.62584955) ????????????
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.62)
(loop for i in (list 3959)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 1000 :n 1))
      do (push solutions *colonize-solutions*))

(defparameter *encroach-solutions* nil) ;; geen oplossing
"encroach" ;; ("trample" . 0.73467267) ("intrude" . 0.7291419) ("infringe" . 0.6760854) !!!!!!!!!!!!!!!!!!!!!!!!!!
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.65)
(loop for i in (list 5271)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 1000 :n 1))
      do (push solutions *encroach-solutions*))


(defparameter *malign-solutions* nil) ;; geen oplossing
"malign" ;; ("defame" . 0.60829705) ("legitimize" . 0.57780046) ("belittle" . 0.5727067) ("counteract" . 0.56080646) !!!!!!!! 
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.55)
(loop for i in (list 7234)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 1000 :n 1))
      do (push solutions *malign-solutions*))

(defparameter *reorient-solutions* nil) ;; geen frame toegepast
"reorient" ;; ("refashion" . 0.7301874) ("refocus" . 0.6886717) ("prioritise" . 0.687688) !!!!!!!!!
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.67)
(loop for i in (list 8770)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 1000 :n 1))
      do (push solutions *reorient-solutions*))

(defparameter *recalibrate-solutions* nil) ;; geen oplossing
"recalibrate" ;; ("actualize" . 0.72000206) ("reexamine" . 0.698875) ("refashion" . 0.6781216) ("calibrate" . 0.6701613) ("re-evaluate" . 0.6686138) ("reformulate" . 0.66400815) ("readjust" . 0.66399026) !!!!!!!!!!!!!!!!!!!!
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.65)
(loop for i in (list 13581)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 2000 :n 1))
      do (push solutions *recalibrate-solutions*))

(defparameter *smear-solutions* nil) ;; geen frame toegepast
"smear" ;; ("discredit" . 0.5780037) ("discredit" . 0.5780037) ("intimidation" . 0.56651497) !!!!!!
(set-configuration *propbank-ewt-ontonotes-learned-cxn-inventory* :cosine-similarity-threshold 0.55)
(loop for i in (list 14295)
      for string = (sentence-string (nth i (dev-split *ontonotes-annotations*)))
      for solutions = (multiple-value-list (comprehend-all string :cxn-inventory *propbank-ewt-ontonotes-learned-cxn-inventory* :timeout 2000 :n 1))
      do (push solutions *smear-solutions*))

)




;(test-split *ontonotes-annotations*)

;(dev-split *ontonotes-annotations*)
;(train-split *ontonotes-annotations*)


(append (dev-split *ontonotes-annotations*)
        (dev-split *ewt-annotations*)
        (test-split *ontonotes-annotations*)
        (test-split *ewt-annotations*))

(append 
        (train-split *ontonotes-annotations*)
        (train-split *ewt-annotations*))

        
;; Verbs - base
(list 
"exhume"
"regurgitate"
"hypercontrol" ;; kent het niet
"overload" ;; nothing close
"befit" ;; ("horrify" . 0.6296152)
"carol" ;;("grace" . 0.5258521)
"snitch" ;;("whore" . 0.46164078)
"seep" ;;("evaporate" . 0.6504355)
"scorch" ;;("scald" . 0.5435439) ("burn" . 0.46978965) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
"thatch"
"shade" ;;("dark" . 0.62818176) geen verb
"parlay" ;; ("shoehorn" . 0.61323935) ("rediscover" . 0.6050489) ("capitalize" . 0.60027886)
"vouch" ;;  ("belittle" . 0.6281376) ("disparage" . 0.6125964) ("appraise" . 0.58917827)
"misread" ;; ("misinterpret" . 0.6392812) ("misrepresent" . 0.532641)  ("misunderstand" . 0.52752597) !!!!!!!!!!!!!!!!!!!!! 
"crease" ;;  ("corner" . 0.537145) ("corner" . 0.537145) ("slip" . 0.5066769)
"bob" ;;("pat" . 0.7019828) ("chuck" . 0.6832764)
"blacklist" ;; ("sanction" . 0.4679903) ("laundering" . 0.4608824)
"plagiarize" ;;  ("oversell" . 0.61144025) ("cadge" . 0.6086499)
"colonize" ;; ("populate" . 0.64053935) ("invade" . 0.62584955) ????????????
"warp" ;; ("thread" . 0.5406398) ("screw" . 0.50792265)
"ensnarl" ;; ("book-up" . 1) ("fan-out" . 1) ("appal" . 1)
"refit" ;; ("commissioning" . 0.57043314)  ("commence" . 0.50399346) ("renovation" . 0.5002251)
"interject" ;; ("rephrase" . 0.6270519) ("shoehorn" . 0.5953462) ("browbeat" . 0.58865476) !!!!!!!
"outrank" ;; ("lionize" . 0.64356345)  ("badmouth" . 0.6252574) ("coddle" . 0.62373925)
"overuse" ;;  ("misuse" . 0.59201086) ("aversion" . 0.5205285)
"enrage" ;; ("infuriate" . 0.8419155) ("unnerve" . 0.77183545) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
"ace" ;;  ("star" . 0.54893417) ("captain" . 0.476191) ("best" . 0.45831418)
"rebuke" ;; ("rebuff" . 0.7443863) ("denunciation" . 0.7237974) ("condemnation" . 0.7135689) ("reprimand" . 0.6364157) ("snub" . 0.5953908) !!!!!!!!
"encroach" ;; ("trample" . 0.73467267) ("intrude" . 0.7291419) ("infringe" . 0.6760854) !!!!!!!!!!!!!!!!!!!!!!!!!!
"queue" ;; ("waiting" . 0.54394526) ("jammed" . 0.49642164) ("throng" . 0.48987475)
"pine" ;; ("bark" . 0.59179956) ("sand" . 0.5741007) ("scrub" . 0.56282926)
"segment" ;; ("feature" . 0.58375234) ("feature" . 0.58375234) ("channel" . 0.5804177)
"beget"
"resubmit" ;; ("retract" . 0.5925255) ("modify" . 0.5902717)
"peal"
"muffle"
"reprove"
"disgorge"
"impart"
"authorizing"
"reallocate"
"overcrowd"
"insinuate" ;; ("detach" . 0.65514917) ("refashion" . 0.6465774) ("dissociate" . 0.63650477)
"proscribe"
"excise"
"parch"
"preapprove"
"retrace" ;; ("engrave" . 0.52410615) ("re-examine" . 0.5113046) ("punctuate" . 0.50553614)
"outdistance" ;; ("outshine" . 0.6328567) ("overtax" . 0.6227939) ("befuddle" . 0.5738537) ????????
"reprice" ;; ("re-evaluate" . 0.66487766) ("customise" . 0.66358245) ("re-examine" . 0.65398974)
"salve" ;; ("soothe" . 0.44506982) ("soothe" . 0.44506982) ("stab" . 0.43933278)
"bifurcate" ;; ("congeal" . 0.67334545) ("subsume" . 0.64883537) ("re-organize" . 0.62854296) ("vitiate" . 0.6146822) ("discomfit" . 0.6141841) ("balkanize" . 0.6081444)
"malign" ;; ("defame" . 0.60829705) ("legitimize" . 0.57780046) ("belittle" . 0.5727067) ("counteract" . 0.56080646) !!!!!!!! 
"heckle"
"rein"
"tether"
"snare" ;; ("trumpet" . 0.5134213) ("hook" . 0.5025609) ("fiddle" . 0.48108628) ("pedal" . 0.47960714)
"plunk"
"exhort" ;; ("implore" . 0.7961878) ("admonish" . 0.6548365) ("sensitize" . 0.6476492) ("beseech" . 0.632049) ("enlighten" . 0.62402404)
"reorient" ;; ("refashion" . 0.7301874) ("refocus" . 0.6886717) ("prioritise" . 0.687688) !!!!!!!!!
"overarch"  ;;doesnt know
"remake" 
"harness" ;; ("bike" . 0.5616593) ("bicycle" . 0.5611891) ("saddle" . 0.5010227)
"skirmish"
"pervade" ;; ("permeate" . 0.82651866) ("bedevil" . 0.7555317) ("preoccupy" . 0.69236976) ("enliven" . 0.6396377) ???????????
"deafen"
"disrespect" ;; ("insult" . 0.6511977) ("insult" . 0.6511977) ("insult" . 0.6511977) ("disdain" . 0.6258957) ("shame" . 0.5817932) !!!!!!!!!!!!!!!!!
"stake" ;; ("acquire" . 0.687273) ("acquisition" . 0.68335784)
"glimpse"  ;; ("curious" . 0.5981996) ("peek" . 0.57937706) ("startling" . 0.56215394) ("sight" . 0.5597508)
"recalibrate" ;; ("actualize" . 0.72000206) ("reexamine" . 0.698875) ("refashion" . 0.6781216) ("calibrate" . 0.6701613) ("re-evaluate" . 0.6686138) ("reformulate" . 0.66400815) ("readjust" . 0.66399026) !!!!!!!!!!!!!!!!!!!!
"tailgate" 
"lobotomize"
"incincerate" ;; doesnt know
"emblazon" ;; ("reword" . 0.7524176) ("denominate" . 0.71722985) ("contemporize" . 0.7012394)
"smear" ;; ("discredit" . 0.5780037) ("discredit" . 0.5780037) ("intimidation" . 0.56651497) !!!!!!
"desensitize" ;; ("traumatize" . 0.6343849) ("destigmatize" . 0.61453027) ("beguile" . 0.608633) ("misguide" . 0.59502745) !!!!!!!!!!!!
"brutalize" ;; ("oppress" . 0.75705654) ("manhandle" . 0.7140285) ("victimize" . 0.6918468) ("coddle" . 0.6580982) ??????????
"sanctify" ;; ("glorify" . 0.63841355) ("exalt" . 0.6237715)  ("venerate" . 0.60114015) !!!!!!!!!!!!!!!!!
"unsubscribe" ;;("cadge" . 0.5302635) ("log-in" . 0.51392174) ("expunge" . 0.50000155)
"authorise" ;; ("defame" . 0.60829705) ("legitimize" . 0.57780046) ("belittle" . 0.5727067)
"bulk" 
"tan"
)

;(pprint (find-closest-match "brutalize" *propbank-ewt-ontonotes-learned-cxn-inventory*))

(loop for verb in #|(list  "mummified" "enraged" "imbed" "joining" "wanted" "re-fix" "hunted" "thrilled" "becoming" "promising" "swinging" "dispatching" "communicating"  "hardening" "contested" "booming" "dedicated"  "noted" "pressing" "miscalculation" "welcoming" "snuck" "versed" "shaking" "fallen" "pre-set" "changed" "frustrating" "romanticized"  "persuasion" "polarized"  "recycled" "fragmentation" "rewarding"  "descending" "snaking" "ringing" "valuation" "mating" "propagandize" "filling"  "reclaimed" "beleaguered" "clouding" "manufactured" "hamstrung"  "fluctuating"  "batting"  "coordinated" "synchronized" "polished" "marching" "bashing" "leveraged" "selected" "geared"   "damaging" "authorized" "sighing" "brandish" "comforting" "politicized" "compelled" "suspending" "overarching" "alarming"  "preoccupied" "oversubscribed" "faltering" "unifying"  "chasten" "overstrain"   "pressed" "fed" "repositioning" "reconditioning"  "normalizing"  "gymnastic" "boasting" "been" "traipse" "tranquilizing" "tailgating"  "tilting" "laying"  "straining" "moaning"    "razed"  "beset" "panhandle"    "sunken"   "festering" "kidnapping" "unchanged"  "referenced" "re-wording" "referencing" "baiting"  "mated" "cooked" "plumbing"  "lurking"  "botched")|#
        (list 
         "exhume"
         "regurgitate"
         "hypercontrol" ;; kent het niet
         "overload" ;; nothing close
         "befit" ;; ("horrify" . 0.6296152)
         "carol" ;;("grace" . 0.5258521)
         "snitch" ;;("whore" . 0.46164078)
         "seep" ;;("evaporate" . 0.6504355)
         "scorch" ;;("scald" . 0.5435439) ("burn" . 0.46978965) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         "thatch"
         "shade" ;;("dark" . 0.62818176) geen verb
         "parlay" ;; ("shoehorn" . 0.61323935) ("rediscover" . 0.6050489) ("capitalize" . 0.60027886)
         "vouch" ;;  ("belittle" . 0.6281376) ("disparage" . 0.6125964) ("appraise" . 0.58917827)
         "misread" ;; ("misinterpret" . 0.6392812) ("misrepresent" . 0.532641)  ("misunderstand" . 0.52752597) !!!!!!!!!!!!!!!!!!!!! 
         "crease" ;;  ("corner" . 0.537145) ("corner" . 0.537145) ("slip" . 0.5066769)
         "bob" ;;("pat" . 0.7019828) ("chuck" . 0.6832764)
         "blacklist" ;; ("sanction" . 0.4679903) ("laundering" . 0.4608824)
         "plagiarize" ;;  ("oversell" . 0.61144025) ("cadge" . 0.6086499)
         "colonize" ;; ("populate" . 0.64053935) ("invade" . 0.62584955) ????????????
         "warp" ;; ("thread" . 0.5406398) ("screw" . 0.50792265)
         "ensnarl" ;; ("book-up" . 1) ("fan-out" . 1) ("appal" . 1)
         "refit" ;; ("commissioning" . 0.57043314)  ("commence" . 0.50399346) ("renovation" . 0.5002251)
         "interject" ;; ("rephrase" . 0.6270519) ("shoehorn" . 0.5953462) ("browbeat" . 0.58865476) !!!!!!!
         "outrank" ;; ("lionize" . 0.64356345)  ("badmouth" . 0.6252574) ("coddle" . 0.62373925)
         "overuse" ;;  ("misuse" . 0.59201086) ("aversion" . 0.5205285)
         "enrage" ;; ("infuriate" . 0.8419155) ("unnerve" . 0.77183545) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         "ace" ;;  ("star" . 0.54893417) ("captain" . 0.476191) ("best" . 0.45831418)
         "rebuke" ;; ("rebuff" . 0.7443863) ("denunciation" . 0.7237974) ("condemnation" . 0.7135689) ("reprimand" . 0.6364157) ("snub" . 0.5953908) !!!!!!!!
         "encroach" ;; ("trample" . 0.73467267) ("intrude" . 0.7291419) ("infringe" . 0.6760854) !!!!!!!!!!!!!!!!!!!!!!!!!!
         "queue" ;; ("waiting" . 0.54394526) ("jammed" . 0.49642164) ("throng" . 0.48987475)
         "pine" ;; ("bark" . 0.59179956) ("sand" . 0.5741007) ("scrub" . 0.56282926)
         "segment" ;; ("feature" . 0.58375234) ("feature" . 0.58375234) ("channel" . 0.5804177)
         "beget"
         "resubmit" ;; ("retract" . 0.5925255) ("modify" . 0.5902717)
         "peal"
         "muffle"
         "reprove"
         "disgorge"
         "impart"
         "authorizing"
         "reallocate"
         "overcrowd"
         "insinuate" ;; ("detach" . 0.65514917) ("refashion" . 0.6465774) ("dissociate" . 0.63650477)
         "proscribe"
         "excise"
         "parch"
         "preapprove"
         "retrace" ;; ("engrave" . 0.52410615) ("re-examine" . 0.5113046) ("punctuate" . 0.50553614)
         "outdistance" ;; ("outshine" . 0.6328567) ("overtax" . 0.6227939) ("befuddle" . 0.5738537) ????????
         "reprice" ;; ("re-evaluate" . 0.66487766) ("customise" . 0.66358245) ("re-examine" . 0.65398974)
         "salve" ;; ("soothe" . 0.44506982) ("soothe" . 0.44506982) ("stab" . 0.43933278)
         "bifurcate" ;; ("congeal" . 0.67334545) ("subsume" . 0.64883537) ("re-organize" . 0.62854296) ("vitiate" . 0.6146822) ("discomfit" . 0.6141841) ("balkanize" . 0.6081444)
         "malign" ;; ("defame" . 0.60829705) ("legitimize" . 0.57780046) ("belittle" . 0.5727067) ("counteract" . 0.56080646) !!!!!!!! 
         "heckle"
         "rein"
         "tether"
         "snare" ;; ("trumpet" . 0.5134213) ("hook" . 0.5025609) ("fiddle" . 0.48108628) ("pedal" . 0.47960714)
         "plunk"
         "exhort" ;; ("implore" . 0.7961878) ("admonish" . 0.6548365) ("sensitize" . 0.6476492) ("beseech" . 0.632049) ("enlighten" . 0.62402404)
         "reorient" ;; ("refashion" . 0.7301874) ("refocus" . 0.6886717) ("prioritise" . 0.687688) !!!!!!!!!
         "overarch"  ;;doesnt know
         "remake" 
         "harness" ;; ("bike" . 0.5616593) ("bicycle" . 0.5611891) ("saddle" . 0.5010227)
         "skirmish"
         "pervade" ;; ("permeate" . 0.82651866) ("bedevil" . 0.7555317) ("preoccupy" . 0.69236976) ("enliven" . 0.6396377) ???????????
         "deafen"
         "disrespect" ;; ("insult" . 0.6511977) ("insult" . 0.6511977) ("insult" . 0.6511977) ("disdain" . 0.6258957) ("shame" . 0.5817932) !!!!!!!!!!!!!!!!!
         "stake" ;; ("acquire" . 0.687273) ("acquisition" . 0.68335784)
         "glimpse"  ;; ("curious" . 0.5981996) ("peek" . 0.57937706) ("startling" . 0.56215394) ("sight" . 0.5597508)
         "recalibrate" ;; ("actualize" . 0.72000206) ("reexamine" . 0.698875) ("refashion" . 0.6781216) ("calibrate" . 0.6701613) ("re-evaluate" . 0.6686138) ("reformulate" . 0.66400815) ("readjust" . 0.66399026) !!!!!!!!!!!!!!!!!!!!
         "tailgate" 
         "lobotomize"
         "incincerate" ;; doesnt know
         "emblazon" ;; ("reword" . 0.7524176) ("denominate" . 0.71722985) ("contemporize" . 0.7012394)
         "smear" ;; ("discredit" . 0.5780037) ("discredit" . 0.5780037) ("intimidation" . 0.56651497) !!!!!!
         "desensitize" ;; ("traumatize" . 0.6343849) ("destigmatize" . 0.61453027) ("beguile" . 0.608633) ("misguide" . 0.59502745) !!!!!!!!!!!!
         "brutalize" ;; ("oppress" . 0.75705654) ("manhandle" . 0.7140285) ("victimize" . 0.6918468) ("coddle" . 0.6580982) ??????????
         "sanctify" ;; ("glorify" . 0.63841355) ("exalt" . 0.6237715)  ("venerate" . 0.60114015) !!!!!!!!!!!!!!!!!
         "unsubscribe" ;;("cadge" . 0.5302635) ("log-in" . 0.51392174) ("expunge" . 0.50000155)
         "authorise" ;; ("defame" . 0.60829705) ("legitimize" . 0.57780046) ("belittle" . 0.5727067)
         "bulk" 
         "tan"
         )
      for matches = (find-closest-match verb *propbank-ewt-ontonotes-learned-cxn-inventory*)
      if (and (> (cdr (first matches)) 0.8)
              (not (= (cdr (first matches)) 1)))
        do (print (cons verb (first-n 10 matches)))
      else
        do (print verb))


;; verbs other

"enchanting"
"mummified"
"enraged"
"imbed"
"joining"
"wanted"
"re-fix"
"hunted"
"thrilled"
"becoming"
"promising"
"swinging" "dispatching" "communicating"  "hardening" "contested" "booming" "dedicated"  "noted" "pressing" "miscalculation" "welcoming" "snuck" "versed" "shaking"
"fallen" "pre-set" "changed" "frustrating" "romanticized"  "persuasion" "polarized"  "recycled" "fragmentation" "rewarding"  "descending" "snaking" "ringing" "valuation" "mating" "propagandize" "filling"  "reclaimed" "beleaguered" "clouding" "manufactured" "hamstrung"  "fluctuating"  "batting"  "coordinated" "synchronized" "polished" "marching" "bashing" "leveraged" "selected" "geared"   "damaging" "authorized" "sighing" "brandish" "comforting" "politicized" "compelled" "suspending" "overarching" "alarming"  "preoccupied" "oversubscribed" "faltering" "unifying"  "chasten" "overstrain"   "pressed" "fed" "repositioning" "reconditioning"  "normalizing"  "gymnastic" "boasting" "been" "traipse" "tranquilizing" "tailgating"  "tilting" "laying"  "straining" "moaning"    "razed"  "beset" "panhandle"    "sunken"   "festering" "kidnapping" "unchanged"  "referenced" "re-wording" "referencing" "baiting"  "mated" "cooked" "plumbing"  "lurking"  "botched"

;;weird / adj? / nouns
"filibuster" ;closest cosine: override
"coo"
"pooh"
"wine"
"truck" 
"rockslide" "mudslide" "blip" "fawn" "class" "single" "rarefy" "retard" "tank" "malnutrition" "excision" "spackle" "bat" "while" "carpet"
"blab" "bumble" "dupe" "backflip" "cream" "toot" "propagation" "angle" "elevation" "gun" "scarred" "scapegoat" "vaccination" "supple" "posse"  "predictable"  "image" "annotation" "retransmission"  "realisation" "apt" "two" "questionsand"   "premature"  "weird" "egg"   "affliction" "red"  "norm" "genuine"  "scarce"  "covert" "bird"  "poop" "illegal" "advice" "offensive" "uncomfortable" "key" "outrageous" "has"    "inefficient" "ineffective" "inexpensive" "dumpy"




;; typo's: 
"gaurd" "wittle" "+approache" "+appreciate" "threate" "beatify" "cros" "partie" "misjudgment" "blipe" "-considering" "cle" "swinge" "innure" "empoy" "developiong" "aspecte" "undrstood" "maime" "influnce" "findin" "soe" "se" "goin" "amplifiae" "ruder" "bitchin"

;; capitalised:
"Trying" "Improvised" "Stated" "Diversification" "ENDED" "Appropriations" "Fun" "Bonding" "Cheerleading" "Tip" "Thanks" "Fix" "Cary" "CALL" "Look" "Hop" "Accommodation" "Pricing" "Managing" "Reverting" "Diffusion" "Model" "Love" "Great"  "ARISE" "TREATING" "WAR" "UPHELD" "TRIAL"  "Competes" "PORTING" "Touches" "EXCHANGE" "MIX" "Manifestation" "DM" "Sale"
"Stung" "Test" "DIALING" "Trick" "Processes" "Simulation" "Deferred" "Encryption" "Photo" "Inscriptions" "Breakdown" "Commemoration" "Invention" "Pledge" "Aquire" "Globalisation" "Closes" "Lure" "Deregulation" "Park" "Rent" "Fire" "Guarantee" "Ringing" "Offer" "Ignore"

















#|(length (list "enchanting" "exhume" "wittle" "mummified" "gaurd" "enraged" "filibuster" "Trying" "Improvised" "imbed" "+approache" "+appreciate" "regurgitate" "coo" "threate" "hypercontrol" "joining" "pooh" "wanted" "re-fix" "hunted" "thrilled" "becoming" "overload" "promising" "befit" "carol" "swinging" "partie" "seep" "beatify" "misjudgment" "cros" "scorch" "Love" "blipe" "snitch" "dispatching" "communicating" "-considering" "wine" "hardening" "contested" "booming" "thatch" "shade" "dedicated" "Stated" "noted" "pressing" "parlay" "miscalculation" "vouch" "misread" "Offer" "welcoming" "crease" "snuck" "truck" "bob" "versed" "blacklist" "plagiarize" "cle" "rockslide" "colonize" "shaking" "warp" "mudslide" "fallen" "pre-set" "blip" "ensnarl" "changed" "frustrating" "refit" "romanticized" "Stung" "persuasion" "interject" "outrank" "overuse" "enrage" "ace" "Test" "polarized" "WAR" "UPHELD" "TRIAL" "rebuke" "fawn" "recycled" "class" "single" "rarefy" "encroach" "queue" "pine" "segment" "beget" "Deregulation" "fragmentation" "rewarding" "resubmit" "peal" "descending" "snaking" "muffle" "retard" "Ringing" "ringing" "valuation" "mating" "tank" "reprove" "Lure" "disgorge" "impart" "authorizing" "reallocate" "Competes" "PORTING" "Touches" "EXCHANGE" "overcrowd" "malnutrition" "insinuate" "propagandize" "proscribe" "Appropriations" "filling" "excise" "excision" "reclaimed" "parch" "Closes" "beleaguered" "preapprove" "retrace" "DIALING" "clouding" "spackle" "manufactured" "outdistance" "reprice" "ENDED" "hamstrung" "Diversification" "fluctuating" "bat" "salve" "batting" "bifurcate" "malign" "while" "coordinated" "synchronized" "heckle" "ARISE" "TREATING" "polished" "carpet" "Park" "marching" "rein" "bashing" "leveraged" "selected" "geared" "tether" "Rent" "swinge" "snare" "plunk" "damaging" "authorized" "sighing" "brandish" "comforting" "blab" "bumble" "dupe" "backflip" "politicized" "exhort" "Ignore" "compelled" "reorient" "overarch" "remake" "suspending" "overarching" "harness" "alarming" "skirmish" "pervade" "cream" "preoccupied" "oversubscribed" "faltering" "unifying" "chasten" "overstrain" "toot" "deafen" "pressed" "fed" "repositioning" "reconditioning" "propagation" "Inscriptions" "disrespect" "normalizing" "stake" "gymnastic" "boasting" "been" "traipse" "glimpse" "tranquilizing" "Commemoration" "recalibrate" "Manifestation" "tailgating" "angle" "tailgate" "tilting" "laying" "elevation" "straining" "moaning" "Globalisation" "bitchin" "lobotomize" "incincerate" "Breakdown" "emblazon" "gun" "innure" "smear" "scarred" "scapegoat" "desensitize" "brutalize" "sanctify" "razed" "Trick" "beset" "panhandle" "Fire" "empoy" "sunken" "vaccination" "Invention" "supple" "festering" "kidnapping" "posse" "predictable" "developiong" "image" "unchanged" "Aquire" "annotation" "retransmission" "Guarantee" "referenced" "re-wording" "Pledge" "realisation" "apt" "two" "questionsand" "Sale" "referencing" "Pricing" "Managing" "Reverting" "Diffusion" "Model" "premature" "Processes" "Simulation" "Deferred" "weird" "egg" "DM" "unsubscribe" "aspecte" "affliction" "red" "undrstood" "norm" "maime" "baiting" "genuine" "Encryption" "scarce" "Photo" "Tip" "authorise" "influnce" "bulk" "covert" "bird" "MIX" "poop" "illegal" "advice" "findin" "soe" "offensive" "mated" "se" "goin" "Cary" "Hop" "uncomfortable" "key" "outrageous" "has" "Fun" "Bonding" "Cheerleading" "inefficient" "ineffective" "cooked" "inexpensive" "amplifiae" "Accommodation" "ruder" "Thanks" "Fix" "plumbing" "dumpy" "lurking" "Look" "Great" "CALL" "botched" "tan"))|#




    