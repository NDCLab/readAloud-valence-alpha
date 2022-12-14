# readAloud-valence-alpha Reading Task Analyses
# Author: Jessica M. Alexander
# Last Updated: 2022-12-13

### SECTION 1: SETTING UP
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(interactions)
library(gridExtra)
library(grid)
library(cowplot)
library(colorspace)
library(colorblindr)

#set up date for output file naming
today <- Sys.Date()
today <- format(today, "%Y%m%d")

#set up directories for input/output data
data <- '/Users/jalexand/github/readAloud-valence-alpha/derivatives/readAloudData_20221212.csv'
out_path <- '/Users/jalexand/github/readAloud-valence-alpha/products/paper-readAloud/results/'

#read in data
df <- read.csv(data)

#organize participant demographic variables
df$sex <- as.factor(df$sex)
df$pronouns <- as.factor(df$pronouns)
df$ethnic <- as.factor(df$ethnic)
df$socclass <- as.factor(df$socclass)

#extract age and sex stats
summary(df$age)
sd(df$age)
summary(df$sex)/19
summary(df$sex)/19/sum(summary(df$sex)/19)
summary(df$pronouns)/19
summary(df$pronouns)/19/sum(summary(df$sex)/19)
summary(df$ethnic)/19
summary(df$ethnic)/19/sum(summary(df$sex)/19)
summary(df$socclass)/19
summary(df$socclass)/19/sum(summary(df$sex)/19)

#remove participants whose challenge question accuracy below 50%
dfTrim <- df
dfTrim <- dfTrim %>%
  group_by(id) %>%
  mutate(challengeAvgSub = mean(challengeACC)) %>%
  ungroup

dfTrim <- subset(dfTrim, challengeAvgSub>0.5)
length(unique(df$id)) - length(unique(dfTrim$id)) #number of participants removed

#calculate average accuracy
mean(dfTrim$challengeAvgSub)
sd(dfTrim$challengeAvgSub)


### SECTION 2: INITIAL DATA TRIMMING
passage_no_before_trimming <- nrow(dfTrim)
dfTrim <- dfTrim[-c(which(is.na(dfTrim$timeFirst))),] #remove passages for which first half timing could not be calculated
dfTrim <- dfTrim[-c(which(is.na(dfTrim$timeSecond))),] #remove passages for which second half timing could not be calculated

dfTrim[, "speedDeltaOutlier"] <- NA
for(m in 1:nrow(dfTrim)){
  subId <- dfTrim$id[m]
  subDf <- subset(dfTrim, dfTrim$id==subId)
  avgSpeedDelta <- mean(subDf$speedDelta, na.rm=TRUE)
  sdSpeedDelta <- sd(subDf$speedDelta, na.rm=TRUE)
  threeAboveDelta <- avgSpeedDelta + (3 * sdSpeedDelta)
  threeBelowDelta <- avgSpeedDelta - (3 * sdSpeedDelta)
  if(dfTrim$speedDelta[m] >= threeAboveDelta | dfTrim$speedDelta[m] <= threeBelowDelta){
    dfTrim$speedDeltaOutlier[m] <- TRUE
  }else{dfTrim$speedDeltaOutlier[m] <- FALSE}
}
dfTrim <- subset(dfTrim, dfTrim$speedDeltaOutlier==FALSE) #remove passages with speed delta ±3 SDs (based on post-trimming averages)

passage_no_after_trim1 <- nrow(dfTrim)
passage_no_before_trimming - passage_no_after_trim1 #number of passages trimmed
(passage_no_before_trimming - passage_no_after_trim1) / passage_no_before_trimming #percentage of passages trimmed


### SECTION 3: SPEED
#trim passages
dfTrim_speed <- dfTrim
dfTrim_speed <- subset(dfTrim_speed, dfTrim_speed$challengeACC==1) #remove passages with incorrect answer on challenge question

dfTrim_speed[, "speedOutlier"] <- NA
for(p in 1:nrow(dfTrim_speed)){
  subId <- dfTrim_speed$id[p]
  subDf <- subset(dfTrim_speed, dfTrim_speed$id==subId)
  meanSpeed <- mean(c(dfTrim_speed$speedFirst, dfTrim_speed$speedSecond), na.rm=TRUE)
  sdSpeed <- sd(c(dfTrim_speed$speedFirst, dfTrim_speed$speedSecond), na.rm=TRUE)
  threeAbove <- meanSpeed + (3 * sdSpeed)
  threeBelow <- meanSpeed - (3 * sdSpeed)
  if(dfTrim_speed$speedFirst[p] >= threeAbove | dfTrim_speed$speedSecond[p] >= threeAbove |
     dfTrim_speed$speedFirst[p] <= threeBelow | dfTrim_speed$speedSecond[p] <= threeBelow){dfTrim_speed$speedOutlier[p] <- TRUE}
  else{dfTrim_speed$speedOutlier[p] <- FALSE}
}
dfTrim_speed <- subset(dfTrim_speed, dfTrim_speed$speedOutlier==FALSE) #remove passages with speed ±3 SDs from average

passage_no_after_trim2 <- nrow(dfTrim_speed)
passage_no_before_trimming - passage_no_after_trim2 #number of passages trimmed
(passage_no_before_trimming - passage_no_after_trim2) / passage_no_before_trimming #percentage of passages trimmed

#transition data to long format
speedDat <- data.frame(matrix(ncol=12, nrow=0))
colnames(speedDat) <- c("passage", "switchType", "id", "position", "speed", "freq", "val", "sex", "pronouns", "age", "ethnic", "socclass")
for(j in 1:nrow(dfTrim_speed)){
  passage <- dfTrim_speed$passage[j]
  id <- dfTrim_speed$id[j]
  sex <- as.character(dfTrim_speed$sex[j])
  pronouns <- as.character(dfTrim_speed$pronouns[j])
  age <- dfTrim_speed$age[j]
  ethnic <- as.character(dfTrim_speed$ethnic[j])
  socclass <- as.character(dfTrim_speed$socclass[j])
  switchType <- dfTrim_speed$switch[j]
  speedFirst <- dfTrim_speed$speedFirst[j]
  speedSecond <- dfTrim_speed$speedSecond[j]
  freqFirst <- dfTrim_speed$freqFirst[j]
  freqSecond <- dfTrim_speed$freqSecond[j]
  valFirst <- dfTrim_speed$valFirst[j]
  valSecond <- dfTrim_speed$valSecond[j]
  speedDat[nrow(speedDat) + 1,] <-c(passage, switchType, id, "preswitch", speedFirst, freqFirst, valFirst, sex, pronouns, age, ethnic, socclass)
  speedDat[nrow(speedDat) + 1,] <-c(passage, switchType, id, "postswitch", speedSecond, freqSecond, valSecond, sex, pronouns, age, ethnic, socclass)
}

#organize data types
speedDat$speed <- as.numeric(speedDat$speed)
speedDat$freq <- as.numeric(speedDat$freq)
speedDat$val <- as.numeric(speedDat$val)
speedDat$position <- as.factor(speedDat$position)
speedDat$switchType <- as.factor(speedDat$switchType)
speedDat$sex <- as.factor(speedDat$sex)
speedDat$pronouns <- as.factor(speedDat$pronouns)
speedDat$age <- as.numeric(speedDat$age)
speedDat$ethnic <- as.factor(speedDat$ethnic)
speedDat$socclass <- as.factor(speedDat$socclass)

#modify contrasts for categorical predictor
contrasts(speedDat$position) <- contr.sum(2) #preswitch position: -1, postswitch position: +1

#center continuous predictors
speedDat$freq_gmc <- speedDat$freq - mean(speedDat$freq)
speedDat$val_gmc <- speedDat$val - mean(speedDat$val)

#extract age and sex stats
speedDatStats <- subset(speedDat, !duplicated(speedDat$id))
summary(speedDatStats$age)
sd(speedDatStats$age)
summary(speedDatStats$sex)
summary(speedDatStats$sex)/sum(summary(speedDatStats$sex))
summary(speedDatStats$pronouns)
summary(speedDatStats$pronouns)/sum(summary(speedDatStats$sex))
summary(speedDatStats$ethnic)
summary(speedDatStats$ethnic)/sum(summary(speedDatStats$sex))
summary(speedDatStats$socclass)
summary(speedDatStats$socclass)/sum(summary(speedDatStats$sex))

#model
modelReadSpeed <- lmerTest::lmer(speed ~ position * freq_gmc * val_gmc + (1|id) + (1|passage), data=speedDat, REML=TRUE)
summary(modelReadSpeed)
confint(modelReadSpeed, method="boot", oldNames=FALSE)

#create df and model for Johnson-Neyman intervals, which requires numeric (not factor) predictors
speedDatJN <- speedDat
speedDatJN$posi <- as.numeric(speedDatJN$position=='postswitch')
for(a in 1:nrow(speedDatJN)){
  if(speedDatJN$posi[a]==0){speedDatJN$posi[a] <- -1}
} #adjust negative to -1 and positive to 1
modelReadSpeedJN <- lmerTest::lmer(speed ~ posi * freq_gmc * val_gmc + (1|id) + (1|passage), data=speedDatJN, REML=TRUE)

#johnson-neyman: frequency x position
sim_slopes(modelReadSpeedJN, pred = freq_gmc, modx = posi, jnplot = TRUE, cond.int=TRUE)
sim_slopes(modelReadSpeedJN, pred = posi, modx = freq_gmc, jnplot = TRUE, cond.int=TRUE)

#johnson-neyman: frequency x valence
sim_slopes(modelReadSpeedJN, pred = freq_gmc, modx = val_gmc, jnplot = TRUE, cond.int=TRUE)
sim_slopes(modelReadSpeedJN, pred = val_gmc, modx = freq_gmc, jnplot = TRUE, cond.int=TRUE)

#plots
#fig3a
figure3a <- interact_plot(modelReadSpeed, pred = freq_gmc, modx = position,
  plot.points = TRUE, jitter=0, point.alpha=0.1, colors=c("#999933", "#882255"), interval=TRUE, vary.lty=FALSE,
  main.title = "Effect of Word Frequency and\nPassage Position on Reading Speed",
  x.label="Average Log Frequency (centered)", y.label="Reading Speed (syllables per second)",
  legend.main = "Passage Position", modx.labels = c("Postswitch", "Preswitch")) +
  theme_light() + theme(legend.position = c(0.82, 0.19), legend.background = element_rect(fill = NA)) +
  coord_cartesian(xlim=c(-0.305, 0.405), ylim=c(1.985, 5.55)) +
  labs(tag = "A")

cvd_grid(figure3a) #test contrasts

#fig4
#bug in interactions package was incorrectly displaying +1SD val_gmc slope (+0.55) as slightly negative:
#interact_plot(modelReadSpeed, pred = freq_gmc, modx = val_gmc, plot.points = TRUE, jitter=0.01,
#              point.alpha=0.1, colors=c("#004488", "#DDAA33", "#BB5566"), interval=TRUE, vary.lty=FALSE,
#              main.title = "Effect of Lexical Valence and\nWord Frequency on Reading Speed",
#              x.label="Average Log Frequency (centered)", y.label="Reading Speed (syllables per second)", legend.main = "Valence")
#therefore re-built in base ggplot:
fig4_data <- sim_slopes(modelReadSpeedJN, pred = freq_gmc, modx = val_gmc, jnplot = TRUE, cond.int=TRUE)

vec <- seq(min(speedDat$freq_gmc), max(speedDat$freq_gmc), 0.001)
dfp <- data.frame(matrix(ncol=3, nrow=length(vec)))
colnames(dfp) <- c("freq_gmc", "speed", "val_gmc")
dfp[,1] <- vec
dfp[,2] <- fig4_data$slopes[3,2] * vec +fig4_data$ints[3,2]
dfp[,3] <- rep(1, length(vec))

dfn <- data.frame(matrix(ncol=3, nrow=length(vec)))
colnames(dfn) <- c("freq_gmc", "speed", "val_gmc")
dfn[,1] <- vec
dfn[,2] <- fig4_data$slopes[1,2] * vec +fig4_data$ints[1,2]
dfn[,3] <- rep(1, length(vec))

speedDat_fig4 <- speedDat
speedDat_fig4$valCat <- NA
for(x in 1:nrow(speedDat_fig4)){
  if(speedDat_fig4$val_gmc[x] > 0){speedDat_fig4$valCat[x] <- "Positive (+1 SD)"}
  else{speedDat_fig4$valCat[x] <- "Negative (-1 SD)"}
}


figure4 <- ggplot(data=speedDat_fig4, aes(x=freq_gmc, y=speed, color=valCat)) + geom_jitter(alpha=0.1, show.legend=FALSE) +
  coord_cartesian(xlim=c(-0.305, 0.405), ylim=c(1.985, 5.55)) +
  scale_color_manual(name = "Passage Valence",
                     values = c("Negative (-1 SD)" = "#202A44", "Positive (+1 SD)" = "#E1AD01")) +
  guides(color = guide_legend(override.aes = list(alpha=1))) +
  geom_segment(aes(x=min(freq_gmc), xend=max(freq_gmc),
                   y=(fig4_data$slopes[3,2] * min(freq_gmc) + fig4_data$ints[3,2]),
                   yend=(fig4_data$slopes[3,2] * max(freq_gmc) + fig4_data$ints[3,2]), color=valCat), size=1, linetype=1, show.legend=TRUE) +
  geom_ribbon(data=dfp, aes(ymin=(speed - fig4_data$slopes[3,3]), ymax=(speed + fig4_data$slopes[3,3])), color=NA, fill="#E1AD01", alpha=0.3) +
  geom_segment(aes(x=min(freq_gmc), xend=max(freq_gmc),
                   y=(fig4_data$slopes[1,2] * min(freq_gmc) + fig4_data$ints[1,2]),
                   yend=(fig4_data$slopes[1,2] * max(freq_gmc) + fig4_data$ints[1,2])), color="#202A44", size=1, linetype=1) +
  geom_ribbon(data=dfn, aes(ymin=(speed - fig4_data$slopes[1,3]), ymax=(speed + fig4_data$slopes[1,3])), color=NA, fill="#202A44", alpha=0.3) +
  theme_light() + theme(legend.position = c(0.82, 0.19), legend.background = element_rect(fill = NA)) +
  labs(x="Average Log Frequency (centered)",
       y="Reading Speed (syllables per second)",
       title="Effect of Lexical Valence and\nWord Frequency on Reading Speed")

ggsave(paste(out_path, "figure4", "_", today, ".png", sep="", collapse=NULL))
cvd_grid(figure4) #test contrasts

### SECTION 4: CHALLENGE QUESTION ACCURACY
dfTrim_acc <- dfTrim

#transition data to long format
challengeDat <- data.frame(matrix(ncol=13, nrow=0))
colnames(challengeDat) <- c("passage", "switchType", "id", "age", "sex", "pronouns", "ethnic", "socclass", "position", "challengeACC", "challengeSource", "freq", "val")
for(j in 1:nrow(dfTrim_acc)){
  passage <- dfTrim_acc$passage[j]
  id <- dfTrim_acc$id[j]
  age <- dfTrim_acc$age[j]
  sex <- as.character(dfTrim_acc$sex[j])
  pronouns <- as.character(dfTrim_acc$pronouns[j])
  ethnic <- as.character(dfTrim_acc$ethnic[j])
  socclass <- as.character(dfTrim_acc$socclass[j])
  challengeACC <- dfTrim_acc$challengeACC[j]
  questionVal <- dfTrim_acc$questionVal[j]
  switchType <- dfTrim_acc$switch[j]
  valFirstCat <- substr(switchType, 1, 3)
  valSecondCat <- substr(switchType, 5, 7)
  challengeFirst <- questionVal==valFirstCat
  challengeSecond <- questionVal==valSecondCat
  speedFirst <- dfTrim_acc$speedFirst[j]
  speedSecond <- dfTrim_acc$speedSecond[j]
  freqFirst <- dfTrim_acc$freqFirst[j]
  freqSecond <- dfTrim_acc$freqSecond[j]
  valFirst <- dfTrim_acc$valFirst[j]
  valSecond <- dfTrim_acc$valSecond[j]
  challengeDat[nrow(challengeDat) + 1,] <-c(passage, switchType, id, age, sex, pronouns, ethnic, socclass, "preswitch", challengeACC, challengeFirst, freqFirst, valFirst)
  challengeDat[nrow(challengeDat) + 1,] <-c(passage, switchType, id, age, sex, pronouns, ethnic, socclass, "postswitch", challengeACC, challengeSecond, freqSecond, valSecond)
}

#organize data types
challengeDat$challengeACC <- as.numeric(challengeDat$challengeACC)
challengeDat$freq <- as.numeric(challengeDat$freq)
challengeDat$val <- as.numeric(challengeDat$val)
challengeDat$position <- as.factor(challengeDat$position)
challengeDat$switchType <- as.factor(challengeDat$switchType)
challengeDat$age <- as.numeric(challengeDat$age)
challengeDat$sex <- as.factor(challengeDat$sex)
challengeDat$pronouns <- as.factor(challengeDat$pronouns)
challengeDat$ethnic <- as.factor(challengeDat$ethnic)
challengeDat$socclass <- as.factor(challengeDat$socclass)

#trim passages
challengeDat <- subset(challengeDat, challengeDat$challengeSource==TRUE) #limit to passage halves that included the challenge question answer

#center continuous predictors
challengeDat$freq_gmc <- challengeDat$freq - mean(challengeDat$freq)
challengeDat$val_gmc <- challengeDat$val - mean(challengeDat$val)

#modify contrasts for dependent variables
contrasts(challengeDat$position) <- contr.sum(2) #preswitch position: -1, postswitch position: +1

#extract age and sex stats (qc check)
challengeDatStats <- subset(challengeDat, !duplicated(challengeDat$id))
summary(challengeDatStats$age)
summary(challengeDatStats$sex)
summary(challengeDatStats$pronouns)
summary(challengeDatStats$ethnic)
summary(challengeDatStats$socclass)

#model
modelReadAcc <- glmer(challengeACC ~ position * val_gmc * freq_gmc + (1|id) + (1|passage), data=challengeDat, family="binomial")
summary(modelReadAcc)
confint(modelReadAcc, method="boot", oldNames=FALSE)

#plots
figure3b <- interact_plot(modelReadAcc, pred = freq_gmc, modx = position,
  plot.points = TRUE, jitter=0.01, point.alpha=0.1, colors=c("#999933", "#882255"), interval=TRUE, vary.lty=FALSE,
  main.title = "Effect of Word Frequency and Passage\nPosition on Comprehension Question Accuracy",
  x.label="Average Log Frequency (centered)", y.label="Comprehension Question Accuracy (%)",
  legend.main = "Passage Position", modx.labels = c("Postswitch", "Preswitch")) +
  theme_light() + theme(legend.position = c(0.81, 0.19), legend.background = element_rect(fill = NA)) +
  labs(tag = "B")

cvd_grid(figure3b) #test contrasts

interact_plot(modelReadAcc, pred = val_gmc, modx = position)

#create df and model for Johnson-Neyman intervals, which requires numeric (not factor) predictors
challengeDatJN <- challengeDat
challengeDatJN$posi <- as.numeric(challengeDatJN$position=='postswitch')
for(a in 1:nrow(challengeDatJN)){
  if(challengeDatJN$posi[a]==0){challengeDatJN$posi[a] <- -1}
} #adjust negative to -1 and positive to 1
modelReadAccJN <- glmer(challengeACC ~ posi * val_gmc * freq_gmc + (1|id) + (1|passage), data=challengeDatJN, family="binomial")

#johnson-neyman: position x frequency
sim_slopes(modelReadAccJN, pred = posi, modx = freq_gmc, jnplot = TRUE, cond.int=TRUE)
sim_slopes(modelReadAccJN, pred = freq_gmc, modx = posi, jnplot = TRUE, cond.int=TRUE)

#position x valence
sim_slopes(modelReadAccJN, pred = posi, modx = val_gmc, jnplot = TRUE, cond.int=TRUE)
sim_slopes(modelReadAccJN, pred = val_gmc, modx = posi, jnplot = TRUE, cond.int=TRUE)


#output combo plot
comboPlot <- grid.arrange(figure3a, figure3b, ncol=2)
ggsave(paste(out_path, "figure3", "_", today, ".png", sep="", collapse=NULL),
       arrangeGrob(comboPlot))