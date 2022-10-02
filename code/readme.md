# Code

### Instructions
This folder contains the code used to create post-processed derivatives, statistics, and figures.


### Project Notes

**Hypotheses:**

_Prediction 1A_

When sublexical characteristics are held constant in a primed reading paradigm, individuals appear to process positive words faster than negative.  As a result, this positivity bias -- which is ostensibly caused by the larger network activation of positive words -- would impact reading speed and error rate in valenced passages, with faster reading speeds and fewer errors in positive passages than negative passages.  Likewise, when word frequency, and more particularly valence/frequency interactions are taken into account, valence is found to have a monotonic effect on lexical decision and speeded naming response times, with the fastest responses for positive words, followed by neutral words, and finally with the slowest responses for negative words.  However, these valence effects may not be strong enough to impact reading performance in the real world, where myriad other factors exert pressures.  Therefore, performance may not differ when reading positive and negative passages, provided that the passages are matched on non-neutrality.
* **A**: no difference in reading speed/disfluency between positive and negative passages
* **B**: positivity bias: faster speeds and fewer disfluencies on positive passages relative to negative

_Prediction 1B_

If good mood and bad mood lead to differing adaptive strategies  (appetitive/assimilative/creative for good mood, aversive/accommodative/non-creative for bad mood), participant mood state may moderate the relationship between passage valence and reading aloud performance, with faster reading speeds and fewer errors in passages that more closely match the participant's mood state.  This effect may be heightened in positive mood; that is, participants in more positive mood may perform better on positive passages than all other conditions, including negative passages read in negative mood state.  This effect may also be curbed in negative mood, such that negative mood leads to similar performance on positive and negative passages.  It is also possible that the more "creative" strategies deployed in good mood could lead to faster performance but a dampened concern over errors such that good mood results in faster speeds across valence conditions but a higher probability of articulatory error.
* **A**: mood-congruency: faster speeds and fewer disfluencies on passages that better match mood state
* **B**: mood-congruency + positivity advantage: best performance on positive passages in good mood
* **C**: negativity dampening: similar performance in negative mood on positive and negative passages
* **D**: creativity trap: faster speed but more disfluencies in positive mood

    **Independent Variables**
    - variable 1: valence rating of passage-portion, calculated using the "emotional tone" rating from [LIWC](https://liwc.wpengine.com/) {continuous, higher numbers represent more positive tone}
    - variable 2: whether passage-portion was pre- or post-switch {categorical, treated as binary}
    - variables 3+ (1B only): participant measures on initial questionnaires (BMIS, etc.) {continuous}

    **Dependent Variables**
    - speed of reading aloud: syllables (required to read the text, not necessarily as produced) / second {continuous}
    - percentage of disfluent syllables: number of disfluent syllables (repeated syllable, pre-syllable pause or interjection, and/or mispronounced syllable) / syllables required to read the text aloud {continuous}

My explicit prediction only involves the BMIS measurement, but we may want to explore how scores on emotion regulation and state/trait anxiety measures might impact the model.

_Prediction 2_

Sass et al. (2012) found an interesting pattern in valence priming: positive words are highly effective in priming other positive words, but negative words don't really "prime" anything (that is, RT on lexical decision for target words is similar for both related (negative) and unrelated (positive) targets following a negative prime).  More interesting yet, when they compared RTs for positive-prime>negative-target against negative-prime>positive-target, participants actually performed better in the **latter**.  If positive words activate a larger semantic network whereas compensatory mechanisms prevent such extensive network activation following exposure to negative words, one would expect a greater likelihood of disfluency at a positive>negative switch than a negative>positive switch.  Alternately, if shifting between valence contexts is akin to task-switching, the surprisal associated with the conflicting valence would be expected to impede performance in either direction, but particularly when hitting a valenced word contradictory to one's current mood state.
* **A**: positive activation and negative compensation: fewer disfluencies in negative>positive switches than reverse
* **B**: general surprisal: similar disfluency rates positive>negative and negative>positive
* **C**: mood-incongruent surprisal: enhanced disfluency rates when hitting switch that conflicts with current mood state

    **Independent Variable**
    - variable 1: type of switch (pos>neg or neg>pos) {categorical}
    - variables 2: participant measures on initial questionnaires (BMIS, etc.) {continuous}

    **Dependent Variable**<br/>
    percentage of disfluent syllables across three factors:

    | pre-switch group| switch group | post-switch group|
    |:-: | :-: | :-: |
    | 7 - 6 - 5 - 4 - 3 | 2 - 1 - switch - 1 - 2 | 3 - 4 - 5 - 6 - 7 |

    1. within the switch group (two words prior to the switch, the switch itself, and two words following the switch)
    2. in the word group prior to the switch (the five words prior to the switch group)
    3. in the post-switch word group (the five words that following the switch group)

    *Note 1*: I feel that syllables form a logical 'unit' in which to measure disfluency.  However, as semantic integration is required to prepare articulation during reading aloud, I propose that we slice on the word level for analyzing disfluency around the switch.<br/>
    *Note 2*: As a starting point, I selected groups of five words; this may need to be revised.  Some prior research has looked into how far ahead people begin semantic and phonological encoding (so they understand an upcoming word and prepare to articulate it). For instance, if a participant is articulating three words prior to the switch word, but they have already seen the switch word and begun to integrate it semantically (and assuming this does actually affect their performance in reading aloud), they might stumble three words before the actual switch word. Alternately, they might recruit all the necessary resources to articulate the switch correctly...and then stumble after they successfully make it past the switch word. My to-read list includes [this article](https://www.frontiersin.org/articles/10.3389/fpsyg.2015.01432/full), which may provide better guidance.

I do not have an explicit prediction on how emotion regulation abilities and state/trait anxiety measures might impact performance, but we may also want to run exploratory analyses that include these measures.

_Prediction 3_

[PLACEHOLDER FOR DETAIL ON POST-ERROR BEHAVIOR PREDICTION]

_LDT Replication Analysis_

The data can be analyzed five ways:
1. using the valence ratings from ANEW
2. using the valence ratings from extended ANEW (same as Kousta et al. (2009))
3. using the valence ratings from extended ANEW but correcting for valence x frequency interactions (similar to Kuperman et al. (2014))
4. using the valence ratings from Warriner et al. (2013)
5. using the valence ratings from the Glasgow Norms