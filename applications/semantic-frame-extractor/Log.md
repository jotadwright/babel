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

## Incremental evaluation results for sentences with "cause"

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
