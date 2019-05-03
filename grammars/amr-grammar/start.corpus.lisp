;(ql:quickload :amr-grammar)

(in-package :amr-grammar)

(activate-monitor trace-fcg)

(comprehend "investor") ;; works
(equivalent-amr-predicate-networks (comprehend "investor")
            '((PERSON P) (INVEST-01 I) (:ARG0-OF P I)))

(comprehend "Zintan") ;; works
(equivalent-amr-predicate-networks (comprehend "Zintan")
            '((CITY C) (NAME N) (:NAME C N) (:OP1 N "Zintan")))

(comprehend "bond investor") ;; works
(equivalent-amr-predicate-networks (comprehend "bond investor")
           '((PERSON P) (INVEST-01 I) (BOND B) (:ARG0-OF P I) (:ARG1 I B)))

(comprehend "small investor") ;; works 
(equivalent-amr-predicate-networks (comprehend "small investor")
           '((PERSON P) (INVEST-01 I) (SMALL S) (:ARG0-OF P I) (:MANNER I S)))

(comprehend "atomic bomb") ;; works
(equivalent-amr-predicate-networks (comprehend "atomic bomb")
            '((BOMB B) (ATOM A) (:MOD B A)))

(comprehend "atom bomb") ;; works
(equivalent-amr-predicate-networks (comprehend "atom bomb")
            '((BOMB B) (ATOM A) (:MOD B A)))

(comprehend "Mollie Brown") ;; works
(equivalent-amr-predicate-networks (comprehend "Mollie Brown")
            '((PERSON P) (NAME N) (:NAME P N) (:OP1 N "Mollie") (:OP2 N "Brown")))

(comprehend "President Obama") ;; works
(equivalent-amr-predicate-networks (comprehend "President Obama")
           '((PRESIDENT P) (NAME N) (:NAME P N) (:OP1 N "Obama")))

(comprehend "history teacher") ;; works
(equivalent-amr-predicate-networks (comprehend "history teacher")
            '((PERSON P) (TEACH-01 T) (HISTORY H) (:ARG0-OF P T) (:ARG1 T H)))

(comprehend "history professor") ;; works
(equivalent-amr-predicate-networks (comprehend "history professor")
            '((PERSON P) (TEACH-01 T) (HISTORY H) (:ARG0-OF P T) (:ARG1 T H)))

(comprehend "Obama , the president") ;; works
(equivalent-amr-predicate-networks (comprehend "Obama , the president")
           '((PRESIDENT P) (NAME N) (:NAME P N) (:OP1 N "Obama")))

(comprehend-all "the attractive spy") ;; works
(equivalent-amr-predicate-networks (comprehend "the attractive spy")
           '((SPY S) (ATTRACT-01 A) (:ARG0-OF S A)))

(comprehend "an edible sandwich") ;; works
(equivalent-amr-predicate-networks (comprehend "an edible sandwich")
           '((SANDWICH S) (EAT-01 E) (POSSIBLE P) (:ARG1-OF S E) (:DOMAIN-OF E P)))

(comprehend-all "a taxable fund") ;; works
(equivalent-amr-predicate-networks (comprehend "a taxable fund")
            '((FUND F) (TAX-01 T) (:ARG1-OF F T)))

(comprehend "the boy cannot go") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy cannot go")
            '((POSSIBLE P) (GO-01 G) (BOY B) (:DOMAIN P G) (:POLARITY P -) (:ARG0 G B)))

(comprehend "what the girl opined") ;; works
(equivalent-amr-predicate-networks (comprehend "what the girl opined")
            '((THING T) (OPINE-01 O) (GIRL G) (:ARG1-OF T O) (:ARG0 O G)))

(comprehend "the girl 's opinion") ;; works
(equivalent-amr-predicate-networks (comprehend "the girl 's opinion")
             '((THING T) (OPINE-01 O) (GIRL G) (:ARG1-OF T O) (:ARG0 O G)))

(comprehend "the marble is white") ;; works
(equivalent-amr-predicate-networks (comprehend "the marble is white")
             '((WHITE W) (MARBLE M) (:DOMAIN W M)))

(comprehend "pleasing girls is tough") ;; works
(equivalent-amr-predicate-networks (comprehend "pleasing girls is tough")
            '((TOUGH T) (PLEASE-01 P) (GIRL G) (:DOMAIN T P) (:ARG1 P G)))

(comprehend "the boy works hard") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy works hard")
            '((WORK-01 W) (BOY B) (HARD H) (:ARG0 W B) (:MANNER W H)))

(comprehend "the soldier feared battle") ;; works
(equivalent-amr-predicate-networks (comprehend  "the soldier feared battle")
            '((FEAR-01 F) (SOLDIER S) (BATTLE-01 B) (:ARG0 F S) (:ARG1 F B)))

(comprehend "the comment is inappropriate") ;; works
(equivalent-amr-predicate-networks (comprehend "the comment is inappropriate")
             '((APPROPRIATE A) (COMMENT C) (:DOMAIN A C) (:POLARITY A -)))

(comprehend "the opinion of the girl") ;; works
(equivalent-amr-predicate-networks (comprehend "the opinion of the girl")
            '((THING T) (OPINE-01 O) (GIRL G) (:ARG1-OF T O) (:ARG0 O G)))

(comprehend "Mollie Brown , who slew orcs") ;; always works. The schema is not good. problem with vp.
(equivalent-amr-predicate-networks (comprehend "Mollie Brown , who slew orcs")
       '((PERSON P) (NAME N) (SLAY-01 S) (ORC O) (:NAME P N) (:ARG0-OF P S) (:OP1 N "Mollie") (:OP2 N "Brown") (:ARG1 S O)))

(comprehend "the orc-slaying Mollie Brown") ;; works
(equivalent-amr-predicate-networks (comprehend "the orc-slaying Mollie Brown")
           '((PERSON P) (NAME N) (SLAY-01 S) (ORC O) (:NAME P N) (:ARG0-OF P S) (:OP1 N "Mollie") (:OP2 N "Brown") (:ARG1 S O)))

(comprehend "the woman is a lawyer") ;; works
(equivalent-amr-predicate-networks (comprehend "the woman is a lawyer")
            '((LAWYER L) (WOMAN W) (:DOMAIN L W)))

(comprehend "the boy wants to go") ;;  works
(equivalent-amr-predicate-networks (comprehend "the boy wants to go")
           '((WANT-01 W) (BOY B) (GO-01 G) (:ARG0 W B) (:ARG1 W G) (:ARG0 G B)))

(comprehend "the college boy who sang") ;; works
(equivalent-amr-predicate-networks (comprehend "the college boy who sang")
           '((BOY B) (SING-01 S) (COLLEGE C) (:ARG0-OF B S) (:SOURCE B C)))

(comprehend "the number of pandas increased") ;; works
(equivalent-amr-predicate-networks (comprehend "the number of pandas increased")
            '((INCREASE-01 I) (NUMBER N) (PANDA P) (:ARG1 I N) (:QUANT-OF N P)))

(comprehend "the boy did not go") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy did not go")
            '((GO-01 G) (BOY B) (:ARG0 G B) (:POLARITY G -)))

(comprehend "the boy must not go") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy must not go")
           '((OBLIGATE-01 P) (GO-01 G) (BOY B) (:ARG2 P G) (:ARG0 G B) (:POLARITY G -)))

(comprehend "what did the girl find ?") ;; works
(equivalent-amr-predicate-networks (comprehend "what did the girl find ?")
           '((FIND-01 F) (GIRL G) (AMR-UNKNOWN A) (:ARG0 F G) (:ARG1 F A)))

(comprehend "the girl adjusted the machine") ;; works 
(equivalent-amr-predicate-networks (comprehend "the girl adjusted the machine") 
             '((ADJUST-01 A) (GIRL G) (MACHINE M) (:ARG0 A G) (:ARG1 A M)))

(comprehend "the judge saw the explosion") ;; works 
(equivalent-amr-predicate-networks (comprehend "the judge saw the explosion")
              '((SEE-01 S) (JUDGE J) (EXPLODE-01 E) (:ARG0 S J) (:ARG1 S E)))

(comprehend "the judge read the proposal") ;; works 
(equivalent-amr-predicate-networks (comprehend "the judge read the proposal")
             '((READ-01 R) (JUDGE J) (THING T) (:ARG0 R J) (:ARG1 R T)))

(comprehend "girls are tough to please") ;; alwyas works but schema not perfect.
(equivalent-amr-predicate-networks (comprehend "girls are tough to please")
            '((TOUGH T) (PLEASE-01 P) (GIRL G) (:DOMAIN T P) (:ARG1 P G)))

(comprehend "the nation defaulted in June") ;; works
(equivalent-amr-predicate-networks (comprehend "the nation defaulted in June")
            '((DEFAULT-01 D) (NATION N) (DATE-ENTITY D2) (:ARG1 D N) (:TIME D D2) (:MONTH D2 6)))

(comprehend "the city of Zintan") ;; works 
(equivalent-amr-predicate-networks (comprehend  "the city of Zintan")
            '((CITY C) (NAME N) (:NAME C N) (:OP1 N "Zintan")))

(comprehend "the comment is not appropriate") ;; works
(equivalent-amr-predicate-networks (comprehend "the comment is not appropriate")
           '((APPROPRIATE A) (COMMENT C) (:DOMAIN A C) (:POLARITY A -)))

(comprehend "the marble in the jar") ;; works  
(equivalent-amr-predicate-networks (comprehend "the marble in the jar")
           '((MARBLE M) (JAR J) (:LOCATION M J)))

(comprehend "Dutch publishing group Elsevier N.V.") ;; works
(equivalent-amr-predicate-networks (comprehend "Dutch publishing group Elsevier N.V.")
           '((GROUP G) (NAME N) (COUNTRY C) (NAME N2) (PUBLISH-01 P) (:NAME G N) (:MOD G C) (:ARG0-OF G P) (:OP1 N "Elsevier") (:OP2 N "N.V.") (:NAME C N2) (:OP1 N2 "Netherlands")))

(comprehend "Elsevier N.V. , the Dutch publishing group") ;;  works
(equivalent-amr-predicate-networks (comprehend "Elsevier N.V. , the Dutch publishing group")
           '((GROUP G) (NAME N) (COUNTRY C) (NAME N2) (PUBLISH-01 P) (:NAME G N) (:MOD G C) (:ARG0-OF G P) (:OP1 N "Elsevier") (:OP2 N "N.V.") (:NAME C N2) (:OP1 N2 "Netherlands")))

(comprehend "the boy destroyed the room") ;; works
(equivalent-amr-predicate-networks (comprehend  "the boy destroyed the room")
           '((DESTROY-01 D) (BOY B) (ROOM R) (:ARG0 D B) (:ARG1 D R)))

(comprehend "the boy need not go") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy need not go")
           '((OBLIGATE-01 P) (GO-01 G) (BOY B) (:ARG2 P G) (:POLARITY P -) (:ARG0 G B)))

(comprehend "the nation defaulted after the war") ;; works 
(equivalent-amr-predicate-networks (comprehend "the nation defaulted after the war")
            '((DEFAULT-01 D) (NATION N) (AFTER A) (WAR-01 W) (:ARG1 D N) (:TIME D A) (:OP1 A W)))

(comprehend "it is tough to please girls") ;; works but schema not perfect
(equivalent-amr-predicate-networks (comprehend "it is tough to please girls")
           '((TOUGH T) (PLEASE-01 P) (GIRL G) (:DOMAIN T P) (:ARG1 P G)))

(comprehend "the boy from the college sang") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy from the college sang")
           '((BOY B) (SING-01 S) (COLLEGE C) (:ARG0-OF B S) (:SOURCE B C)))

(comprehend "whose toy did the girl find ? ") ;; works
(equivalent-amr-predicate-networks (comprehend "whose toy did the girl find ? ")
           '((FIND-01 F) (GIRL G) (TOY T) (AMR-UNKNOWN A) (:ARG0 F G) (:ARG1 F T) (:POSS T A)))

(comprehend "the boy looked the answer up") ;; works but schema not always correct
(equivalent-amr-predicate-networks (comprehend "the boy looked the answer up")
           '((LOOK-05 L) (BOY B) (ANSWER A) (:ARG0 L B) (:ARG1 L A)))

(comprehend "the boy looked up the answer") ;; works but schema not always correct
(equivalent-amr-predicate-networks (comprehend "the boy looked up the answer")
           '((LOOK-05 L) (BOY B) (ANSWER A) (:ARG0 L B) (:ARG1 L A)))

(comprehend "the boy is a hard worker") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy is a hard worker")
            '((WORK-01 W) (BOY B) (HARD H) (:ARG0 W B) (:MANNER W H)))

(comprehend "the soldier was afraid of battle") ;; works
(equivalent-amr-predicate-networks (comprehend "the soldier was afraid of battle")
            '((FEAR-01 F) (SOLDIER S) (BATTLE-01 B) (:ARG0 F S) (:ARG1 F B)))

(comprehend "yesterday 's marble in the non-jar") ;; works
(equivalent-amr-predicate-networks (comprehend "yesterday 's marble in the non-jar")
           '((MARBLE M) (JAR J) (YESTERDAY Y) (:LOCATION M J) (:TIME M Y) (:POLARITY J -)))

(comprehend "the girl made adjustments to the machine") ;;  works but schema not perfect
(equivalent-amr-predicate-networks (comprehend "the girl made adjustments to the machine")
            '((ADJUST-01 A) (GIRL G) (MACHINE M) (:ARG0 A G) (:ARG1 A M)))

(comprehend "the soldier had a fear of battle") ;;  works
(equivalent-amr-predicate-networks (comprehend "the soldier had a fear of battle")
             '((FEAR-01 F) (SOLDIER S) (BATTLE-01 B) (:ARG0 F S) (:ARG1 F B)))

(comprehend "the boy doesn't have to go") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy doesn't have to go") 
             '((OBLIGATE-01 P) (GO-01 G) (BOY B) (:ARG2 P G) (:POLARITY P -) (:ARG0 G B)))

(comprehend "where did the girl find the boy ? ") ;; works
(equivalent-amr-predicate-networks (comprehend "where did the girl find the boy ? ")
              '((FIND-01 F) (GIRL G) (BOY B) (AMR-UNKNOWN A) (:ARG0 F G) (:ARG1 F B) (:LOCATION F A)))

(comprehend  "the spy who is attractive to women") ;; works
(equivalent-amr-predicate-networks (comprehend  "the spy who is attractive to women")
             '((SPY S) (ATTRACT-01 A) (WOMAN W) (:ARG0-OF S A) (:ARG1 A W)))

(comprehend "the boy has responsibility for the work") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy has responsibility for the work")
              '((RESPONSIBLE-41 R) (BOY B) (WORK W) (:ARG1 R B) (:ARG2 R W)))

(comprehend "the boy is responsible for the work") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy is responsible for the work")
              '((RESPONSIBLE-41 R) (BOY B) (WORK W) (:ARG1 R B) (:ARG2 R W)))

(comprehend "the man was sued in the case") ;; works
(equivalent-amr-predicate-networks (comprehend "the man was sued in the case")
               '((SUE-01 S) (MAN M) (CASE C) (:ARG1 S M) (:PREP-IN S C)))

(comprehend  "the boy 's destruction of the room") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy 's destruction of the room")
               '((DESTROY-01 D) (BOY B) (ROOM R) (:ARG0 D B) (:ARG1 D R)))

(comprehend  "the boy isn't obliged to go") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy isn't obliged to go")
               '((OBLIGATE-01 P) (GO-01 G) (BOY B) (:ARG2 P G) (:POLARITY P -) (:ARG0 G B)))

(comprehend  "it's obligatory that the boy not go") ;; works
(equivalent-amr-predicate-networks (comprehend  "it's obligatory that the boy not go")
            '((OBLIGATE-01 P) (GO-01 G) (BOY B) (:ARG2 P G) (:ARG0 G B) (:POLARITY G -)))

(comprehend  "the man 's description of the mission : disaster") ;; works
(equivalent-amr-predicate-networks (comprehend  "the man 's description of the mission : disaster")
           '((DESCRIBE-01 D) (MAN M) (MISSION M2) (DISASTER D) (:ARG0 D M) (:ARG1 D M2) (:ARG2 D D)))

(comprehend  "the man described the mission as a disaster") ;; works
(equivalent-amr-predicate-networks (comprehend "the man described the mission as a disaster")
           '((DESCRIBE-01 D) (MAN M) (MISSION M2) (DISASTER D) (:ARG0 D M) (:ARG1 D M2) (:ARG2 D D)))

(comprehend  "the destruction of the room by the boy") ;; works
(equivalent-amr-predicate-networks (comprehend "the destruction of the room by the boy")
           '((DESTROY-01 D) (BOY B) (ROOM R) (:ARG0 D B) (:ARG1 D R)))

(comprehend "the marble was not in the jar yesterday") ;; works
(equivalent-amr-predicate-networks (comprehend "the marble was not in the jar yesterday")
           '((BE-LOCATED-AT-91 B) (MARBLE M) (JAR J) (YESTERDAY Y) (:ARG1 B M) (:ARG2 B J) (:POLARITY B -) (:TIME B Y)))

(comprehend "the boy thinks the team won't win") ;; works 
(equivalent-amr-predicate-networks (comprehend "the boy thinks the team won't win")
         '((THINK-01 T-1) (BOY B) (WIN-01 W) (TEAM T-2) (:ARG0 T-1 B) (:ARG1 T-1 W) (:ARG0 W T-2) (:POLARITY W -)))

(comprehend "it's not possible for the boy to go") ;; works
(equivalent-amr-predicate-networks (comprehend "it's not possible for the boy to go")
        '((POSSIBLE P) (GO-01 G) (BOY B) (:DOMAIN P G) (:POLARITY P -) (:ARG0 G B)))

(comprehend "it's possible for the boy not to go") ;; need to exclude prepositional-phrase ?
(equivalent-amr-predicate-networks (comprehend "it's possible for the boy not to go")
       '((POSSIBLE P) (GO-01 G) (BOY B) (:DOMAIN P G) (:ARG0 G B) (:POLARITY G -)))

(comprehend "the boy doesn't think the team will win") ;; works
(equivalent-amr-predicate-networks (comprehend "the boy doesn't think the team will win")
      '((THINK-01 T-1) (BOY B) (WIN-01 W) (TEAM T-2) (:ARG0 T-1 B) (:ARG1 T-1 W) (:ARG0 W T-2) (:POLARITY W -))) ;; see meaning for the report, add cxn and then discussion, so two possible solution
   ;; per avere vecchia construzione cambia positive +

(comprehend "as the man described it, the mission was a disaster") ;; how to exclude disaster patient transitive and yes mission ? 
(equivalent-amr-predicate-networks (comprehend "as the man described it, the mission was a disaster")
    '((DESCRIBE-01 D-1) (MAN M) (MISSION M2) (DISASTER D-2) (:ARG0 D-1 M) (:ARG1 D-1 M2) (:ARG2 D-1 D-2)))

(comprehend "the soldier hummed to the girl as she walked to town") ;; arg0 w g ?
(equivalent-amr-predicate-networks (comprehend "the soldier hummed to the girl as she walked to town")
     '((HUM-02 S) (SOLDIER S2) (GIRL G) (WALK-01 W) (TOWN T) (:ARG0 S S2) (:BENEFICIARY S G) (:TIME S W) (:ARG0 W G) (:DESTINATION W T)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 75 sentences


(evaluate-amr-grammar)

