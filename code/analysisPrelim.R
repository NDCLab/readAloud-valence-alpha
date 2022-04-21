# Preliminary Analyses: readAloud-valence-alpha
# Author: Jessica M. Alexander
# Last Updated: 2022-04-21

### SETTING UP
library(ggplot2)


### GENERAL EFFECT OF PASSAGE VALENCE ON READING BEHAVIOR (BETWEEN-SUBJECT)
### Hypothesis 1A:
##### A: no difference in reading speed/disfluency between positive and negative passages
##### B: positivity bias: faster speeds and fewer disfluencies on positive passages relative to negative
#### Exploratory: acoustic behavioral measure of pitch

## independent variables
## liwcScore : emotional tone rating from LIWC for the first half of the passage (continuous)
## warrinerAvg : average of lexical valence scores from Warriner et al. 2013 (continuous)
## PosOrNeg : indication of whether the first passage half is positive or negative (categorical: pos2neg, neg2pos)

## dependent variables
## syllPerSec_firstHalf : syllables coded for the first passage half / length of reading in seconds (continuous)
## percDisfluent_firstHalf : number of syllables produced with some kind of disfluency / syllables coded for the first passage half (continuous)
## vocalPitch_firstHalf : pitch rating from Praat for the first passage half (continuous)


###### categorical independent variables
# syllPerSec ~ PosOrNeg
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=syllPerSec_firstHalf)) + geom_boxplot()
# model
t.test(DATA[DATA$PosOrNeg=="pos2neg",]$syllPerSec_firstHalf, DATA[DATA$PosOrNeg=="neg2pos",]$syllPerSec_firstHalf, alternative="two.sided", paired=FALSE, conf.level=0.95)

# percDisfluent ~ PosOrNeg
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=percDisfluent_firstHalf)) + geom_boxplot()
# model
t.test(DATA[DATA$PosOrNeg=="pos2neg",]$percDisfluent_firstHalf, DATA[DATA$PosOrNeg=="neg2pos",]$percDisfluent_firstHalf, alternative="two.sided", paired=FALSE, conf.level=0.95)

# vocalPitch ~ PosOrNeg
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=vocalPitch_firstHalf)) + geom_boxplot()
# model
t.test(DATA[DATA$PosOrNeg=="pos2neg",]$vocalPitch_firstHalf, DATA[DATA$PosOrNeg=="neg2pos",]$vocalPitch_firstHalf, alternative="two.sided", paired=FALSE, conf.level=0.95)

###### continuous independent variables (LIWC)
# syllPerSec ~ liwcScore x PreOrPost
# plot
ggplot(data=DATA, aes(x=liwcScore, y=syllPerSec_firstHalf)) + geom_point() + geom_smooth(method="lm", se=FALSE)
# model
lm(syllPerSec_firstHalf ~ liwcScore, data=DATA)
# individual t-tests TBD

# percDisfluent ~ liwcScore x PreOrPost
# plot
ggplot(data=DATA, aes(x=liwcScore, y=percDisfluent_firstHalf)) + geom_point() + geom_smooth(method="lm", se=FALSE)
# model
lm(percDisfluent_firstHalf ~ liwcScore, data=DATA)
# individual t-tests TBD

# vocalPitch ~ liwcScore x PreOrPost
# plot
ggplot(data=DATA, aes(x=liwcScore, y=vocalPitch_firstHalf)) + geom_point() + geom_smooth(method="lm", se=FALSE)
# model
lm(vocalPitch_firstHalf ~ liwcScore, data=DATA)
# individual t-tests TBD

###### continuous independent variables (Warriner)
# syllPerSec ~ warrinerAvg x PreOrPost
# plot
ggplot(data=DATA, aes(x=warrinerAvg, y=syllPerSec_firstHalf)) + geom_point() + geom_smooth(method="lm", se=FALSE)
# model
lm(syllPerSec_firstHalf ~ warrinerAvg, data=DATA)
# individual t-tests TBD

# percDisfluent ~ warrinerAvg x PreOrPost
# plot
ggplot(data=DATA, aes(x=warrinerAvg, y=percDisfluent_firstHalf)) + geom_point() + geom_smooth(method="lm", se=FALSE)
# model
lm(percDisfluent_firstHalf ~ warrinerAvg, data=DATA)
# individual t-tests TBD

# vocalPitch ~ warrinerAvg x PreOrPost
# plot
ggplot(data=DATA, aes(x=warrinerAvg, y=vocalPitch_firstHalf)) + geom_point() + geom_smooth(method="lm", se=FALSE)
# model
lm(vocalPitch_firstHalf ~ warrinerAvg, data=DATA)
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
## warrinerAvg : average of lexical valence scores from Warriner et al. 2013 (continuous)
## PosOrNeg : indication of whether the passage half is positive or negative (categorical: pos2neg, neg2pos)
## bmis_scrdVal : participant mood score (continuous)
## subMood : indication of whether participant mood falls above/below the midmark on bmis_scrdVal (categorical: posMood, negMood)

## dependent variables
## syllPerSec_firstHalf : syllables coded for the first passage half / length of reading in seconds (continuous)
## percDisfluent_firstHalf : number of syllables produced with some kind of disfluency / syllables coded for the first passage half (continuous)
## vocalPitch_firstHalf : pitch rating from Praat for the first passage half (continuous)

###### categorical independent variables
# syllPerSec ~ PosOrNeg x subMood
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=syllPerSec_firstHalf, fill=subMood)) + geom_boxplot()
# model
aov(syllPerSec_firstHalf ~ PosOrNeg + subMood, data=DATA)
# post-hoc comparisons TBD

# percDisfluent ~ PosOrNeg x subMood
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=percDisfluent_firstHalf, fill=subMood)) + geom_boxplot()
# model
aov(percDisfluent_firstHalf ~ PosOrNeg + subMood, data=DATA)
# post-hoc comparisons TBD

# vocalPitch ~ PosOrNeg x subMood
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=vocalPitch_firstHalf, fill=subMood)) + geom_boxplot()
# model
aov(vocalPitch_firstHalf ~ PosOrNeg + subMood, data=DATA)
# post-hoc comparisons TBD


###### continuous independent variables (LIWC)
# syllPerSec ~ liwcScore x bmis_scrdVal
# plot
ggplot(data=DATA, aes(x=liwcScore, y=bmis_scrdVal, color=syllPerSec_firstHalf)) + geom_point() + geom_smooth(method="lm", se=FALSE) + scale_color_viridis()
# model
lm(syllPerSec_firstHalf ~ liwcScore + bmis_scrdVal + liwcScore:bmis_scrdVal + 0, data=DATA)
# individual t-tests TBD

# percDisfluent ~ liwcScore x bmis_scrdVal
# plot
ggplot(data=DATA, aes(x=liwcScore, y=bmis_scrdVal, color=percDisfluent_firstHalf)) + geom_point() + geom_smooth(method="lm", se=FALSE) + scale_color_viridis()
# model
lm(percDisfluent_firstHalf ~ liwcScore + bmis_scrdVal + liwcScore:bmis_scrdVal + 0, data=DATA)
# individual t-tests TBD

# vocalPitch ~ liwcScore x bmis_scrdVal
# plot
ggplot(data=DATA, aes(x=liwcScore, y=bmis_scrdVal, color=vocalPitch_firstHalf)) + geom_point() + geom_smooth(method="lm", se=FALSE) + scale_color_viridis()
# model
lm(vocalPitch_firstHalf ~ liwcScore + bmis_scrdVal + liwcScore:bmis_scrdVal + 0, data=DATA)
# individual t-tests TBD

###### continuous independent variables (Warriner)
# syllPerSec ~ warrinerAvg x bmis_scrdVal
# plot
ggplot(data=DATA, aes(x=warrinerAvg, y=bmis_scrdVal, color=syllPerSec_firstHalf)) + geom_point() + geom_smooth(method="lm", se=FALSE) + scale_color_viridis()
# model
lm(syllPerSec_firstHalf ~ warrinerAvg + bmis_scrdVal + warrinerAvg:bmis_scrdVal + 0, data=DATA)
# individual t-tests TBD

# percDisfluent ~ warrinerAvg x bmis_scrdVal
# plot
ggplot(data=DATA, aes(x=warrinerAvg, y=bmis_scrdVal, color=percDisfluent_firstHalf)) + geom_point() + geom_smooth(method="lm", se=FALSE) + scale_color_viridis()
# model
lm(percDisfluent_firstHalf ~ warrinerAvg + bmis_scrdVal + warrinerAvg:bmis_scrdVal + 0, data=DATA)
# individual t-tests TBD

# vocalPitch ~ warrinerAvg x bmis_scrdVal
# plot
ggplot(data=DATA, aes(x=warrinerAvg, y=bmis_scrdVal, color=vocalPitch_firstHalf)) + geom_point() + geom_smooth(method="lm", se=FALSE) + scale_color_viridis()
# model
lm(vocalPitch_firstHalf ~ warrinerAvg + bmis_scrdVal + warrinerAvg:bmis_scrdVal + 0, data=DATA)
# individual t-tests TBD


### Hypothesis 2:
##### A: positive activation and negative compensation: fewer disfluencies in negative>positive switches than reverse
##### B: general surprisal: similar disfluency rates positive>negative and negative>positive
##### C: mood-incongruent surprisal: enhanced disfluency rates when hitting switch that conflicts with current mood state

## independent variables
## PosOrNeg : indication of the directionality of the passage switch (categorical: pos2neg, neg2pos)
## subMood : indication of whether participant mood falls above/below the midmark on bmis_scrdVal (categorical: posMood, negMood)

## dependent variables
## syllPerSec_switch : syllables coded for the switch group / length of reading in seconds (continuous)
## percDisfluent_switch : number of syllables produced with some kind of disfluency / syllables coded for the switch group (continuous)
## vocalPitch_switch : pitch rating from Praat for the switch group (continuous)

# straight comparison of pos2neg and neg2pos switch groups
#syllPerSec
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=syllPerSec_switch)) + geom_boxplot()
# model
t.test(DATA[DATA$PosOrNeg=="pos2neg",]$syllPerSec_switch, DATA[DATA$PosOrNeg=="neg2pos",]$syllPerSec_switch, alternative="two.sided", paired=FALSE, conf.level=0.95)

#percDisfluent
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=percDisfluent_switch)) + geom_boxplot()
# model
t.test(DATA[DATA$PosOrNeg=="pos2neg",]$percDisfluent_switch, DATA[DATA$PosOrNeg=="neg2pos",]$percDisfluent_switch, alternative="two.sided", paired=FALSE, conf.level=0.95)

#vocalPitch
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=vocalPitch_switch)) + geom_boxplot()
# model
t.test(DATA[DATA$PosOrNeg=="pos2neg",]$vocalPitch_switch, DATA[DATA$PosOrNeg=="neg2pos",]$vocalPitch_switch, alternative="two.sided", paired=FALSE, conf.level=0.95)


# mood-congruency on switches
#syllPerSec
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=syllPerSec_switch, fill=subMood)) + geom_boxplot()
# model
aov(syllPerSec_switch ~ PosOrNeg + subMood, data=DATA)
# post-hoc comparisons TBD

#percDisfluent
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=percDisfluent_switch, fill=subMood)) + geom_boxplot()
# model
aov(percDisfluent_switch ~ PosOrNeg + subMood, data=DATA)
# post-hoc comparisons TBD

#vocalPitch
# plot
ggplot(data=DATA, aes(x=PosOrNeg, y=vocalPitch_switch, fill=subMood)) + geom_boxplot()
# model
aov(vocalPitch_switch ~ PosOrNeg + subMood, data=DATA)
# post-hoc comparisons TBD


### GENERAL EFFECT OF PASSAGE VALENCE ON READING BEHAVIOR (BETWEEN-SUBJECT)
### Hypothesis 3:
##### A: PES and PIA will be accentuated in positive passages versus negative passages.
##### B: RT and accuracy following errors will be slower/higher than the associated post-correct measures.

## independent variables
## TBD

## dependent variables
## TBD