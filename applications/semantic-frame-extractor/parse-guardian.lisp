;(ql:quickload :frame-extractor)
(in-package :frame-extractor)

(defun pie-comprehend-log (utterance &key (cxn-inventory *fcg-constructions*) (silent nil))
  "Utility function to comprehend an utterance and extract the frames in one go.
   Returns both a frame-set and the last cip-node."
  (multiple-value-bind (meaning cipn) (comprehend utterance :cxn-inventory cxn-inventory :silent silent)
    (values cipn (run-pie cipn))))

(defun get-sentences-from-json (path)
  (with-open-file (s path)
    (loop while (peek-char t s nil nil)
          collect (json:decode-json s) into docs
          finally (return docs))))

(defun log-parsing-output-into-json-file (frame-evoking-elem-list)
  "Parses sentences from the Guardian training-corpus that contain the specified frame-evoking-elems.
   Encodes the resulting frame-sets into json-format and writes them into 'frame-extractor-output.json' file."
  (let* ((sentence-objs (get-sentences-from-json (babel-pathname :directory '(:up "Corpora" "Guardian") :name "100-causation-frame-annotations" :type "json")))
         (sentences (loop for sent in sentence-objs
                          when (intersection
                                (mapcar #'cdr (mapcar (lambda (x) (assoc :frame-evoking-element x)) (cdr (assoc :frame-elements sent))))
                                frame-evoking-elem-list :test #'string=)
                          collect (cdr (assoc :sentence sent)) into sentences
                          finally (return sentences))))
      (loop for sent in sentences
            for (last-cipn raw-frame-set) = (multiple-value-list (pie-comprehend-log (string-trim '(#\Space #\Backspace #\Linefeed #\Page #\Return) sent) :silent t))
            collect (encode-json-alist-to-string `((:sentence . ,sent)
                                                   (:frame-elements . ,(loop for frame in (pie::entities raw-frame-set)
                                                                             collect `((:frame-evoking-element . ,(pie::frame-evoking-element frame))
                                                                                       (:cause . ,(cause frame))
                                                                                       (:effect . ,(effect frame)))))
                                                   (:applied-cxns . ,(mapcar #'name (applied-constructions last-cipn))))) into results
            finally (with-open-file (out (babel-pathname :directory '(:up "corpora" "Guardian") :name "frame-extractor-output" :type "json")
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
                      (loop for result in results
                            do (progn
                                 (format out result)
                                 (format out  "~%")))))))

;(activate-monitor trace-fcg)
;(log-parsing-output-into-json-file '("due to"))


;;; Spacy or grammar?

(pie-comprehend "But you might need to know this: one such report published by the Institute of Development Studies in the UK predicts a whopping 20% to 60% rise in food prices by 2050, depending on the type of food, largely due to declining yields brought upon us by climate change.") ;NOT working, spacy places prepositional phrase outside and generally - what do we want?
(pie-comprehend "The stalling is due to very weak prevailing winds, which are failing to steer the storm off to sea, allowing it to spin around and wobble back and forth.") ;NOT working, spacy places participial clause separately
(pie-comprehend "The first frame is a bit mouldy due to some damp at the front of the hive.") ;NOT working, "due" under "mouldy" and not "is"
(pie-comprehend "Yorkshire and Humberside are the regions most affected due to the combination of high social vulnerabilities and high likelihoods of flooding, the JRF report shows.") ;NOT working, spacy places "due to Y" under "affected"
(pie-comprehend "But the rich world still accounted for the majority of the carbon footprint of consumption due to the goods it imports from China and other developing economies.") ;NOT working, spacy places "due to Y" under "majority" and not under the event "accounted"
(pie-comprehend "The Lancet estimated that China suffers 1.2 million premature deaths due to fossil fuel pollution.") ;NOT working, spacy places "due to" under "deathes" and not "suffers"
(pie-comprehend "UK waters are also not exempt from the global trend of ocean acidifiation due to higher levels of dissolved CO2.") ;NOT working, spacy places "due to" under "exempt" and not "trend" or "acidifiation"
(pie-comprehend "Energy-intensive industries, such as iron, steel and cement manufacture, have become more efficient over time due to new equipment and better re-use of waste heat.") ;NOT working, spacy incorrect
(pie-comprehend "The stalling is due to very weak prevailing winds, which are failing to steer the storm off to sea, allowing it to spin around and wobble back and forth.") ;NOT working, spacy does not include "allowing"


;;; WORKING

(pie-comprehend "Nevertheless, it seems to me that our collective failure to tackle climate change is not just due to political deadlock or insufficient knowledge.")
(pie-comprehend "There is no doubt whatsoever that the planet is warming, and it is primarily due to increased carbon dioxide in the atmosphere from burning of fossil fuels.")
(pie-comprehend "If it doesn't, the investors eventually get their money back (and they keep the interest).With the growing number of natural disasters due to climate change, the sums spent by governments on catastrophe management have risen to unprecedented levels.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Incorrectly parsed sentences with "cause", due to spacy errors:

;(pie-comprehend "Global warming is \"very likely\" to have been caused by human activity, the most authoritative global scientific body studying climate change said in a report today.") ; NOT working, spacy does not like these quotation marks

;(pie-comprehend "As so many other polls have shown consistently, the majority of Australians believe climate change is happening and is caused by human activity.") ;NOT working, ellipsis not resoluted by spacy.

;(pie-comprehend "The great advantage that climate change has over other pressing issues is that the gases that cause it are measured down to the last gram.") ;NOT recognising "it", spacy tree incorrect

;(pie-comprehend "It has been devastated by a combination of a long drought caused by a strong El NiÃ±o weather cycle and climate change.") ;NOT working, spacy incorrect

;(pie-comprehend "Back in 1984, journalists reported from Ethiopia about a famine of biblical proportions caused by widespread drought.") ;NOT working, incorrect effect

;(pie-comprehend "But the ASA said the ad implied that the vehicle's emission rate was low in relation to all vehicles and that readers were likely to understand that the car caused little or no harm to the environment.The watchdog concluded that the ads were likely to mislead and banned the ads.") ;NOT working, spacy excludes prepositional modifiers

;(pie-comprehend "Last year, Hurricane Felix caused widespread devastation to Nicaragua's coffee plantations.") ;NOT working, spacy excludes prepositional modifiers

;(pie-comprehend "On top of the upheaval caused by the drive to boost productivity, mountain biodiversity must now withstand climate change.") ;NOT working, spacy excludes prepositional modifiers

;(pie-comprehend "With an immense scientific consensus that manmade greenhouse gases cause climate change, there is pressure to reduce carbon emissions, but little sign that governments can reach a binding agreement to cut back sufficiently.") ;NOT working completely, spacy tree incorrect

;(pie-comprehend "In 2001, the body - which brings together 2,500 scientists from more than 30 countries - said global warming was only likely, or 66% probable, to have been caused by humans.") ;NOT working, spacy incorrect

