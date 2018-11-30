# Semantic Frame extractor log

## Causation-frame annotations
#### 12.10.2018
In total, 150 Guardian article sentences are annotated with their corresponding causation-frames. Frames consist of three slots, a *frame-evoking-element* (currently only "cause"), a *cause* and an *effect*. Following [framenet's definition of causation](https://framenet2.icsi.berkeley.edu/fnReports/data/frameIndex.xml?frame=Causation), each sentence contains at least one of the following *frame-evoking-elements*:
* cause.v
* because of.prep
* because.c
* give rise.v
* lead to.v
* due to.prep
* result in.v

At the moment, pronominal resolutions are not taken into consideration and negations are not treated specially.

For grammar engineering and general evaluation purposes, the annotations are divided into a training set (`100-causation-frame-annotations.json`) and a test set (`50-causation-frame-annotations.json`).

## Test set result for sentences with "cause"

#### 2018-11-24
Parsing the unknown test set sentences, containing "cause" as frame-evoking-element, results in 14 out of 20 correctly parsed sentences and 54 out of 60 correct slot fillers. 🎉

## Incremental evaluation results for sentences with "cause"

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

## Incremental evaluation results for sentences with "due to"

#### 2018-11-27
For sentences containing "X is due to Y", where X is an NP, three slightly different <span style="font-variant:small-caps;">X-is-due-to-Y</span> cxns have been created, given the different possibilities of spacy positioning the units for "due", "to" and the "cause". Moreover, noun phrases involving the prepositional postmodifier "of" are covered by the <span style="font-variant:small-caps;">X1-of-X2-due-to-Y</span> cxn. These first changes result in 14 out of 57 correct slot fillers and 4 correctly parsed sentences out of 19.

Vanja Cangalovic, Katrien Beuls, Paul Van Eecke
