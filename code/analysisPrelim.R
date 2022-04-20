# Preliminary Analyses: readAloud-valence-alpha
# Author: Jessica M. Alexander
# Last Updated: 2022-04-19

### GENERAL EFFECT OF PASSAGE VALENCE ON READING BEHAVIOR (BETWEEN-SUBJECT)
### Hypothesis 1A:
##### A: no difference in reading speed/disfluency between positive and negative passages
##### B: positivity bias: faster speeds and fewer disfluencies on positive passages relative to negative
#### Exploratory: acoustic behavioral measure of pitch

## independent variables
## liwcScore : emotional tone rating from LIWC for the first half of the passage (continuous)
## PosOrNeg : indication of whether the first passage half is positive or negative (categorical: pos2neg, neg2pos)

## dependent variables
## syllPerSec : syllables coded for the passage half / length of reading in seconds (continuous)
## percDisfluent : number of syllables produced with some kind of disfluency / syllables coded for the passage half (continuous)
## vocalPitch : pitch rating from Praat for the passage half (continuous)


###### categorical independent variables
# syllPerSec ~ PosOrNeg
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=syllPerSec)) + geom_boxplot()
# model
t.test(DATA[PosOrNeg=="pos2neg"]$syllPerSec, DATA[PosOrNeg=="neg2pos"]$syllPerSec, alternative="two-sided", paired=FALSE, conf.level=0.95)

# percDisfluent ~ PosOrNeg
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=percDisfluent)) + geom_boxplot()
# model
t.test(DATA[PosOrNeg=="pos2neg"]$percDisfluent, DATA[PosOrNeg=="neg2pos"]$percDisfluent, alternative="two-sided", paired=FALSE, conf.level=0.95)

# vocalPitch ~ PosOrNeg
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=vocalPitch)) + geom_boxplot()
# model
t.test(DATA[PosOrNeg=="pos2neg"]$vocalPitch, DATA[PosOrNeg=="neg2pos"]$vocalPitch, alternative="two-sided", paired=FALSE, conf.level=0.95)

###### continuous independent variables
# syllPerSec ~ liwcScore x PreOrPost
# plot
ggplot(data=DATA, aes(x=liwcScore, y=syllPerSec)) + geom_point() + geom_smooth(method="lm", se=FALSE)
# model
lm(syllPerSec ~ liwcScore, data=DATA)
# individual t-tests TBD

# percDisfluent ~ liwcScore x PreOrPost
# plot
ggplot(data=DATA, aes(x=liwcScore, y=percDisfluent)) + geom_point() + geom_smooth(method="lm", se=FALSE)
# model
lm(percDisfluent ~ liwcScore, data=DATA)
# individual t-tests TBD

# vocalPitch ~ liwcScore x PreOrPost
# plot
ggplot(data=DATA, aes(x=liwcScore, y=vocalPitch)) + geom_point() + geom_smooth(method="lm", se=FALSE)
# model
lm(vocalPitch ~ liwcScore, data=DATA)
# individual t-tests TBD


### EFFECT OF PASSAGE VALENCE AND MOOD CONGRUENCY ON READING BEHAVIOR (WITHIN-SUBJECT)
### Hypothesis 1B:
##### A: mood-congruency: faster speeds and fewer disfluencies on passages that better match mood state
##### B: mood-congruency + positivity advantage: best performance on positive passages in good mood
##### C: negativity dampening: similar performance in negative mood on positive and negative passages
##### D: creativity trap: faster speed but more disfluencies in positive mood
#### Exploratory: acoustic behavioral measure of pitch

## independent variables
## liwcScore : emotional tone rating from LIWC for the passage half (continuous)
## PosOrNeg : indication of whether the passage half is positive or negative (categorical: pos2neg, neg2pos)
## bmis_scrdVal : participant mood score (continuous)
## SubjectMood : indication of whether participant mood falls above/below the midmark on bmis_scrdVal (categorical: posMood, negMood)

## dependent variables
## syllPerSec : syllables coded for the passage half / length of reading in seconds (continuous)
## percDisfluent : number of syllables produced with some kind of disfluency / syllables coded for the passage half (continuous)
## vocalPitch : pitch rating from Praat for the passage half (continuous)

###### categorical independent variables
# syllPerSec ~ PosOrNeg x SubjectMood
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=syllPerSec, fill=SubjectMood)) + geom_boxplot()
# model
aov(syllPerSec ~ PosOrNeg + SubjectMood, data=DATA)
# post-hoc comparisons TBD

# percDisfluent ~ PosOrNeg x SubjectMood
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=percDisfluent, fill=SubjectMood)) + geom_boxplot()
# model
aov(percDisfluent ~ PosOrNeg + SubjectMood, data=DATA)
# post-hoc comparisons TBD

# vocalPitch ~ PosOrNeg x SubjectMood
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=vocalPitch, fill=SubjectMood)) + geom_boxplot()
# model
aov(vocalPitch ~ PosOrNeg + SubjectMood, data=DATA)
# post-hoc comparisons TBD


###### continuous independent variables
# syllPerSec ~ liwcScore x bmis_scrdVal
# plot
ggplot(data=DATA, aes(x=liwcScore, y=bmis_scrdVal, color=syllPerSec)) + geom_point() + geom_smooth(method="lm", se=FALSE) + scale_color_viridis()
# model
lm(syllPerSec ~ liwcScore + bmis_scrdVal + liwcScore:bmis_scrdVal + 0, data=DATA)
# individual t-tests TBD

# percDisfluent ~ liwcScore x bmis_scrdVal
# plot
ggplot(data=DATA, aes(x=liwcScore, y=bmis_scrdVal, color=percDisfluent)) + geom_point() + geom_smooth(method="lm", se=FALSE) + scale_color_viridis()
# model
lm(percDisfluent ~ liwcScore + bmis_scrdVal + liwcScore:bmis_scrdVal + 0, data=DATA)
# individual t-tests TBD

# vocalPitch ~ liwcScore x bmis_scrdVal
# plot
ggplot(data=DATA, aes(x=liwcScore, y=bmis_scrdVal, color=vocalPitch)) + geom_point() + geom_smooth(method="lm", se=FALSE) + scale_color_viridis()
# model
lm(vocalPitch ~ liwcScore + bmis_scrdVal + liwcScore:bmis_scrdVal + 0, data=DATA)
# individual t-tests TBD


### Hypothesis 2:
##### A: positive activation and negative compensation: fewer disfluencies in negative>positive switches than reverse
##### B: general surprisal: similar disfluency rates positive>negative and negative>positive
##### C: mood-incongruent surprisal: enhanced disfluency rates when hitting switch that conflicts with current mood state

## independent variables
## PosOrNeg : indication of the directionality of the passage switch (categorical: pos2neg, neg2pos)
## SubjectMood : indication of whether participant mood falls above/below the midmark on bmis_scrdVal (categorical: posMood, negMood)

## dependent variables
## syllPerSec : syllables coded for the switch group / length of reading in seconds (continuous)
## percDisfluent : number of syllables produced with some kind of disfluency / syllables coded for the switch group (continuous)
## vocalPitch : pitch rating from Praat for the switch group (continuous)

# straight comparison of switches
#syllPerSec
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=syllPerSec)) + geom_boxplot()
# model
t.test(DATA[PosOrNeg=="pos2neg"]$syllPerSec, DATA[PosOrNeg=="neg2pos"]$syllPerSec, alternative="two-sided", paired=FALSE, conf.level=0.95)

#percDisfluent
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=percDisfluent)) + geom_boxplot()
# model
t.test(DATA[PosOrNeg=="pos2neg"]$percDisfluent, DATA[PosOrNeg=="neg2pos"]$percDisfluent, alternative="two-sided", paired=FALSE, conf.level=0.95)

#vocalPitch
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=vocalPitch)) + geom_boxplot()
# model
t.test(DATA[PosOrNeg=="pos2neg"]$vocalPitch, DATA[PosOrNeg=="neg2pos"]$vocalPitch, alternative="two-sided", paired=FALSE, conf.level=0.95)


# mood-congruency on switches
#syllPerSec
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=syllPerSec, fill=SubjectMood)) + geom_boxplot()
# model
aov(syllPerSec ~ PosOrNeg + SubjectMood, data=DATA)
# post-hoc comparisons TBD

#percDisfluent
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=percDisfluent, fill=SubjectMood)) + geom_boxplot()
# model
aov(percDisfluent ~ PosOrNeg + SubjectMood, data=DATA)
# post-hoc comparisons TBD

#vocalPitch
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=vocalPitch, fill=SubjectMood)) + geom_boxplot()
# model
aov(vocalPitch ~ PosOrNeg + SubjectMood, data=DATA)
# post-hoc comparisons TBD


### GENERAL EFFECT OF PASSAGE VALENCE ON READING BEHAVIOR (BETWEEN-SUBJECT)
### Hypothesis 3:
##### A: PES and PIA will be accentuated in positive passages versus negative passages.
##### B: RT and accuracy following errors will be slower/higher than the associated post-correct measures.

## independent variables
## TBD

## dependent variables
## TBD