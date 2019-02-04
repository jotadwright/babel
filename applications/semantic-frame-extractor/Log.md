# Semantic Frame extractor log

## Causation-frame annotations
#### 12.10.2018
In total, 174 Guardian article sentences are annotated with their corresponding causation-frames. Frames consist of three slots, a *frame-evoking-element* (currently only "cause"), a *cause* and an *effect*. Following [framenet's definition of causation](https://framenet2.icsi.berkeley.edu/fnReports/data/frameIndex.xml?frame=Causation), each sentence contains at least one of the following *frame-evoking-elements*:

* cause.v
* because of.prep
* because.c
* give rise.v
* lead to.v
* due to.prep
* result in.v

At the moment, pronominal resolutions are not taken into consideration and negations are not treated specially.

For grammar engineering and general evaluation purposes, the annotations are divided into a training set (`111-causation-frame-annotations.json`) and a test set (`63-causation-frame-annotations.json`).

## Incremental evaluation results for sentences with "due to"

#### 2019-02-04
A simple wordlevel evaluation results in 17 out of 30 correct sentences, 71 out of 90 correct slot fillers and overall 607 out of 692 correctly annotated words.

#### 2019-01-16
Adding the <span style="font-variant:small-caps;">subj-due-to-Y</span> cxn results in 2 more correctly parsed sentences.

#### 2018-12-28
Parsing the new training set sentences (a total of 30 now) results in 65 out of 90 correct slot fillers, 15 out of 30 completely correct sentences and 79.59% correct characters per slot filler.

#### 2018-12-11
Via substring-matching in the evaluation, it is now possible to calculate the ratio of correct characters per frame slot filler. The current average correctness of the training set lies at 82.38% correct characters per slot filler. This metric allows to assign a correctness of 95.96% to sentences, such as "Indeed, due to the rise of the freezing line, the snow-rain limit is moving to a higher elevation.", in which all but one of the slot fillers are correctly extracted, with the latter additionally containing some superfluous words. Naive string-matching would assign these sentences a correctness of only 67%, ignoring the correct slot filler parts.

#### 2018-12-10
Adding the <span style="font-variant:small-caps;">predicative-adj-due-to-Y</span> cxn results in one more correct sentence. 

#### 2018-12-09
Reverting back to the original rendering function, which theoretically allows for discontinuous frame elements to be rendered. Made possible via a small adaptation, rendering now happens via unit boundaries and not the 'bag of words' approach. This change decreases correctness to 42 slot fillers and 9 correctly parsed sentences, as the new rendering technique sometimes catches too many units. Thus a more fine-grained evaluation technique would help, accounting for the correct substrings.

#### 2018-12-06
Improving the annotations yields one more correctly parsed sentence.

#### 2018-12-02
Trying to render discontinuous frame slot fillers now poses a problem. Tweaking the rendering function to only render until a unit whose referent is another frame slot filler is found, prevents this error and results in 2 more correct sentences, thus 10 out of 19 sentences are parsed correctly and 44 out of 57 slot fillers are found. Still, whenever the target slot filler appears after another filler, it cannot be rendered. Thus the effect-slot in sentence "Indeed, due to the rise of the freezing line, the snow-rain limit is moving to a higher elevation." cannot be found.

#### 2018-11-29
<span style="font-variant:small-caps;">X-event-due-to-Y</span> cxns have been added to cover sentences in which the effect is a whole event (involving e.g. subject, object and predicate). Also in this case, two versions are needed to account for both spacy interpretations. Correctness rises to 8 out of 19 sentences.

#### 2018-11-27
For sentences containing "X is due to Y", where X is an NP, three slightly different <span style="font-variant:small-caps;">X-is-due-to-Y</span> cxns have been created, given the different possibilities of spacy positioning the units for "due", "to" and the "cause". Moreover, noun phrases involving the prepositional postmodifier "of" are covered by the <span style="font-variant:small-caps;">X1-of-X2-due-to-Y</span> cxn. These first changes result in 14 out of 57 correct slot fillers and 4 correctly parsed sentences out of 19.

## Test set result for sentences with "cause"

#### 2018-12-11
The more fine-grained evaluation results in 92.44% correct characters per frame slot filler.

#### 2018-11-24
Parsing the unknown test set sentences, containing "cause" as frame-evoking-element, results in 14 out of 20 correctly parsed sentences and 54 out of 60 correct slot fillers. 🎉

## Incremental evaluation results for sentences with "cause"

#### 2018-12-11
The more fine-grained evaluation results in 92.97% correct characters per frame slot filler.

#### 2018-11-24
Making the passive transitive modular as well, catches one more correct slot filler.

#### 2018-11-22
The <span style="font-variant:small-caps;">perfect-infinitive-passive-cxn</span>, evoking the causation frame for "to have been caused", covers one more sentence. Making the grammar more modular by dividing the cxn for active transitive into one for the agent and one for the theme allows to parse two more slot fillers correctly. Adapting the <span style="font-variant:small-caps;">active-transitive-actor-theme-cxn-subject-parataxis</span> to requiring a coordinating conjunction prevents it from sometimes interfering with the parsing of sentences containing subordinating conjunctions, such as "as". These changes result in 100 out of 111 correct slot fillers and 26 out of 36 correctly parsed sentences.

#### 2018-11-18
Uncommenting the too lenient <span style="font-variant:small-caps;">meta-causation=cause-cxn</span> and introducing the <span style="font-variant:small-caps;">causative-to-cxn</span>, which covers structures such as "X causes Y to Z", results in two more correct slot fillers and 23 correct sentences.

#### 2018-11-16
A minor improvement of the slot-filler clean-up leads to 21 out of 36 correctly parsed sentences.

#### 2018-11-07
Cleaning up annotations (i.e. downcasing, removing punctuation) before measuring the similarity results in ~88 correct slot fillers and 19 correctly parsed sentences.

#### 2018-10-29
The adapted <span style="font-variant:small-caps;">X-caused-by-Y-cxn</span> now restricts the dependency-features of the "caused"-unit. Consequently, the sentence "It deleted references to studies showing that global warming is caused by manmade emissions" works without interference, while four other sentences now need other cxns to fill their frame slots. The change results in ~77 correct slot fillers and 12 correct sentences.

#### 2018-10-28
Frame slot rendering is adapted, it now leaves out all those units (and subunits of those) which have another frame slot as their referent. This adaptation results in ~80 correct slot fillers and 12 correctly parsed sentences.

#### 2018-10-22
After the insertion of an explicit "by"-unit into the <span style="font-variant:small-caps;">passive-transitive-cxn</span>, correctness rises to ~73 out of 111, one more sentence is parsed correctly.

#### 2018-10-19
Very first, naive string matching as similarity measure results in ~71 found slot fillers (out of 111). Of these, 37 are the trivial *frame-evoking-element* fillers, while 34 are non-trivial correctly parsed slot fillers. On the sentence level, 9 out of 36 are parsed completely correctly.

Vanja Cangalovic, Katrien Beuls, Paul Van Eecke
