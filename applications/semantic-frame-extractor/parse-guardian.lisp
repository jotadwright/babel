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

(defun log-parsing-output-into-json-file (target-frame-evoking-elements)
  "Parses sentences from the Guardian training-corpus that contain the specified frame-evoking-elems.
   Encodes the resulting frame-sets into json-format and writes them into 'frame-extractor-output.json' file."
  (let* ((sentence-objs (get-sentences-from-json
                         (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                         :name "111-causation-frame-annotations" :type "json")))
         (sentences (loop for sentence-object in (first sentence-objs)
                          for frame-elements-in-sentence = (rest (assoc :frame-elements sentence-object))
                          for frame-evoking-elements-in-sentence = (loop for frame-elts in frame-elements-in-sentence
                                                                         collect (rest (assoc :frame-evoking-element frame-elts)))
                          when (intersection frame-evoking-elements-in-sentence
                                             target-frame-evoking-elements :test #'string=)
                          collect (rest (assoc :sentence sentence-object)) into sentences
                          finally (return sentences))))
      (loop for sent in sentences
            for (last-cipn raw-frame-set) = (multiple-value-list
                                             (pie-comprehend-log (string-trim '(#\Space #\Backspace #\Linefeed #\Page #\Return) sent) :silent t))
            collect (encode-json-alist-to-string `((:sentence . ,sent)
                                                   (:frame-elements . ,(loop for frame in (pie::entities raw-frame-set)
                                                                             collect `((:frame-evoking-element . ,(pie::frame-evoking-element frame))
                                                                                       (:cause . ,(cause frame))
                                                                                       (:effect . ,(effect frame)))))
                                                   (:applied-cxns . ,(mapcar #'name (applied-constructions last-cipn))))) into results
            finally (with-open-file (out (babel-pathname :directory '("applications" "semantic-frame-extractor" "data")
                                                         :name "frame-extractor-output" :type "json")
                                         :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
                      (loop for result in results
                            do (progn
                                 (format out result)
                                 (format out  "~%")))))))

;(activate-monitor trace-fcg)
;(log-parsing-output-into-json-file '("lead to"))

;(load (babel-pathname :directory '("applications" "semantic-frame-extractor") :name "evaluate-guardian-annotations" :type "lisp"))
;(evaluate-grammar-output-for-evoking-elem '("lead to"))

(pie-comprehend "The Senate is holding hearings this month that could lead to further legislation")
(pie-comprehend "And public opposition has led to hundreds of coal plants closed or blocked by the Sierra Club and its allies.")

;; wrong analysis by Spacy:
(pie-comprehend "Rural-dwellers who are unemployed, young or old, or disabled are increasingly stuck in their homes and unable to access essential services because of savage cuts to public transport in the countryside, MPs have warned.")
(pie-comprehend "Shell warned environmentalists and ethical investors yesterday that failure to exploit tar sands and other unconventional oil products would worsen climate change because it would lead to the world burning even more carbon-heavy coal.")

;;; TODO, sometimes spacy a bit incorrect or ambiguous

(pie-comprehend "In every case the line is already quite steep due to the hundreds of billions of tons of carbon pollution humans have dumped into the atmosphere thus far.") ;only "quite steep" included but also spacy incorrect for the cause
(pie-comprehend "But you might need to know this: one such report published by the Institute of Development Studies in the UK predicts a whopping 20% to 60% rise in food prices by 2050, depending on the type of food, largely due to declining yields brought upon us by climate change.") ;NOT working, statement-frame ("predict") included in spacy parsing
(pie-comprehend "Marmots and martens in the Americas are getting bigger off of longer growing seasons produce more foodstuffs, while the alpine chipmunks of Yellowstone National Park have actually seen the shape of their skulls change due to climate pressure.") ;NOT working or annotation needs to change?
(pie-comprehend "The Lancet estimated that China suffers 1.2 million premature deaths due to fossil fuel pollution.") ;NOT working, spacy places "due to" under "deaths" and not "suffers"
(pie-comprehend "Yorkshire and Humberside are the regions most affected due to the combination of high social vulnerabilities and high likelihoods of flooding, the JRF report shows.") ;NOT working, spacy places "due to Y" under "affected" under "regions"


;;; NOT working, Spacy incorrect

(pie-comprehend "But the rich world still accounted for the majority of the carbon footprint of consumption due to the goods it imports from China and other developing economies.") ;NOT working, spacy places "due to Y" under "majority" and not under the event "accounted"
(pie-comprehend "UK waters are also not exempt from the global trend of ocean acidifiation due to higher levels of dissolved CO2.") ;NOT working, spacy places "due to" under "exempt" and not "trend" or "acidifiation"
(pie-comprehend "Energy-intensive industries, such as iron, steel and cement manufacture, have become more efficient over time due to new equipment and better re-use of waste heat.") ;NOT working, spacy incorrect


;;; X-event-due-to-Y -> WORKING, but not completely because discontinuous or simply superfluous subunits are rendered

(pie-comprehend "Canada faced fresh calls to shut down its commercial seal hunt on Thursday, following new evidence that death rates among seal pups had dramatically increased due to thinning winter sea ice.")
(pie-comprehend "Indeed, due to the rise of the freezing line, the snow-rain limit is moving to a higher elevation.")
(pie-comprehend "The company has seven reactors out of action due to unexpected or routine repairs but Coley said five would return before the end of December.")
(pie-comprehend "The Malaysian minister of defence said on Twitter he had fallen ill due to the haze and warned Malaysians to stay indoors.")


;;; WORKING

(pie-comprehend "Property damage due to weather extremes has risen sharply in North America, including the Caribbean, over the last 30 years, the report said.")
(pie-comprehend "A stronger greenhouse effect due to higher carbon dioxide levels in the atmosphere may be one explanation.")
(pie-comprehend "That difference is due to the long-term, human-caused global warming trend.")
(pie-comprehend "Part of this can be attributed to the carbon tax and RET, but a far greater portion is due to other factors.")
(pie-comprehend "A rise towards the 10% EU-wide target by 2020 has stalled due to the environmental concerns.")
(pie-comprehend "A decline in the size of some species of fish in the North Sea could be due to a rise in water temperatures, according to research.")
(pie-comprehend "Derwin said he experienced a windfall due to the post-election stock market rally.")
(pie-comprehend "Almost all of the fatalities last year, 93%, were due to weather-related events.")
(pie-comprehend "Income inequality between countries has also been falling, mainly due to the rapid growth in large developing countries.")
(pie-comprehend "The first frame is a bit mouldy due to some damp at the front of the hive.")
(pie-comprehend "The stalling is due to very weak prevailing winds, which are failing to steer the storm off to sea, allowing it to spin around and wobble back and forth.")
(pie-comprehend "Nevertheless, it seems to me that our collective failure to tackle climate change is not just due to political deadlock or insufficient knowledge.")
(pie-comprehend "There is no doubt whatsoever that the planet is warming, and it is primarily due to increased carbon dioxide in the atmosphere from burning of fossil fuels.")
(pie-comprehend "If it doesn't, the investors eventually get their money back (and they keep the interest).With the growing number of natural disasters due to climate change, the sums spent by governments on catastrophe management have risen to unprecedented levels.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Incorrectly parsed sentences with "cause", due to spacy errors:

;(pie-comprehend "Global warming is \"very likely\" to have been caused by human activity, the most authoritative global scientific body studying climate change said in a report today.") ; NOT working, spacy does not like these quotation marks

;(pie-comprehend "It has been devastated by a combination of a long drought caused by a strong El NiÃ±o weather cycle and climate change.") ;NOT working, spacy incorrect

;(pie-comprehend "On top of the upheaval caused by the drive to boost productivity, mountain biodiversity must now withstand climate change.") ;NOT working, spacy excludes prepositional modifiers

;(pie-comprehend "Back in 1984, journalists reported from Ethiopia about a famine of biblical proportions caused by widespread drought.") ;NOT working, incorrect effect

;(pie-comprehend "With an immense scientific consensus that manmade greenhouse gases cause climate change, there is pressure to reduce carbon emissions, but little sign that governments can reach a binding agreement to cut back sufficiently.") ;NOT working completely, spacy tree incorrect

;(pie-comprehend "In 2001, the body - which brings together 2,500 scientists from more than 30 countries - said global warming was only likely, or 66% probable, to have been caused by humans.") ;NOT working, spacy incorrect

;(pie-comprehend "Last year, Hurricane Felix caused widespread devastation to Nicaragua's coffee plantations.") ;NOT working, spacy excludes prepositional modifiers

;(pie-comprehend "As so many other polls have shown consistently, the majority of Australians believe climate change is happening and is caused by human activity.") ;NOT working, ellipsis not resoluted by spacy.

;(pie-comprehend "But the ASA said the ad implied that the vehicle's emission rate was low in relation to all vehicles and that readers were likely to understand that the car caused little or no harm to the environment.The watchdog concluded that the ads were likely to mislead and banned the ads.") ;NOT working, spacy excludes prepositional modifiers

;(pie-comprehend "The great advantage that climate change has over other pressing issues is that the gases that cause it are measured down to the last gram.") ;NOT recognising "it", spacy tree incorrect


