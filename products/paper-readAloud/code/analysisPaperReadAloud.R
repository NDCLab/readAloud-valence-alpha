# readAloud-valence-alpha Reading Task Analyses
# Author: Jessica M. Alexander
# Last Updated: 2023-06-30

# INPUTS
# data/df: behavioral data, for each participant on each passage, with relevant participant information and trial-level stimulus information

# OUTPUTS
# modelReadSpeed: main model of reading speed, including confidence intervals, Johnson-Neyman intervals, and visual plot
# supplementary models including individual differences predictors (sex, age, depression, anxiety, and state affect)


### SECTION 1: SETTING UP
library(dplyr)
library(lme4)
library(lmerTest)
library(interactions)

#visualization tools
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)
library(colorspace)
library(colorblindr)

#set up date for output file naming
today <- Sys.Date()
today <- format(today, "%Y%m%d")

#set up directories for input/output data
data <- '/Users/jalexand/github/readAloud-valence-alpha/derivatives/readAloudData_20230615.csv'
out_path <- '/Users/jalexand/github/readAloud-valence-alpha/products/paper-readAloud/results/'

#read in data
df <- read.csv(data)

#organize participant demographic variables
df$sex <- as.factor(df$sex)
df$pronouns <- as.factor(df$pronouns)
df$ethnic <- as.factor(df$ethnic)
df$socclass <- as.factor(df$socclass)

#extract demo stats
summary(df$age)
sd(df$age)
summary(df$sex)/19
summary(df$sex)/19 / (nrow(df)/19)
summary(df$pronouns)/19
summary(df$pronouns)/19 / (nrow(df)/19)
summary(df$ethnic)/19
summary(df$ethnic)/19 / (nrow(df)/19)
summary(df$socclass)/19
summary(df$socclass)/19 / (nrow(df)/19)

#remove participants whose challenge question accuracy was below 50% (chance = 25%)
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

#identify passages with outlier differences between speed of reading first and second half (word)
dfTrim[, "speedWordDeltaOutlier"] <- NA
for(m in 1:nrow(dfTrim)){
  subId <- dfTrim$id[m]
  subDf <- subset(dfTrim, dfTrim$id==subId)
  avgSpeedWordDelta <- mean(subDf$speedWordDelta, na.rm=TRUE)
  sdSpeedWordDelta <- sd(subDf$speedWordDelta, na.rm=TRUE)
  threeAboveDelta <- avgSpeedWordDelta + (3 * sdSpeedWordDelta)
  threeBelowDelta <- avgSpeedWordDelta - (3 * sdSpeedWordDelta)
  if(dfTrim$speedWordDelta[m] >= threeAboveDelta | dfTrim$speedWordDelta[m] <= threeBelowDelta){
    dfTrim$speedWordDeltaOutlier[m] <- TRUE
  }else{dfTrim$speedWordDeltaOutlier[m] <- FALSE}
}
dfTrim <- subset(dfTrim, dfTrim$speedWordDeltaOutlier==FALSE) #remove passages with speed delta ±3 SDs (based on post-trimming averages)

passage_no_after_trim1 <- nrow(dfTrim)
passage_no_before_trimming - passage_no_after_trim1 #number of passages trimmed
(passage_no_before_trimming - passage_no_after_trim1) / passage_no_before_trimming #percentage of passages trimmed


### SECTION 3: MAIN MODEL (SPEED)
#trim passages
dfTrim_speed <- dfTrim
dfTrim_speed <- subset(dfTrim_speed, dfTrim_speed$challengeACC==1) #remove passages with incorrect answer on challenge question

dfTrim_speed[, "speedWordOutlier"] <- NA
for(p in 1:nrow(dfTrim_speed)){
  subId <- dfTrim_speed$id[p]
  subDf <- subset(dfTrim_speed, dfTrim_speed$id==subId)
  meanSpeed <- mean(c(dfTrim_speed$speedWordFirst, dfTrim_speed$speedWordSecond), na.rm=TRUE)
  sdSpeed <- sd(c(dfTrim_speed$speedWordFirst, dfTrim_speed$speedWordSecond), na.rm=TRUE)
  threeAbove <- meanSpeed + (3 * sdSpeed)
  threeBelow <- meanSpeed - (3 * sdSpeed)
  if(dfTrim_speed$speedWordFirst[p] >= threeAbove | dfTrim_speed$speedWordSecond[p] >= threeAbove |
     dfTrim_speed$speedWordFirst[p] <= threeBelow | dfTrim_speed$speedWordSecond[p] <= threeBelow){dfTrim_speed$speedWordOutlier[p] <- TRUE}
  else{dfTrim_speed$speedWordOutlier[p] <- FALSE}
}
dfTrim_speed <- subset(dfTrim_speed, dfTrim_speed$speedWordOutlier==FALSE) #remove passages with speed ±3 SDs from average

passage_no_after_trim2 <- nrow(dfTrim_speed)
passage_no_before_trimming - passage_no_after_trim2 #number of passages trimmed
(passage_no_before_trimming - passage_no_after_trim2) / passage_no_before_trimming #percentage of passages trimmed

#transition data to long format
speedDat <- data.frame(matrix(ncol=15, nrow=0))
colnames(speedDat) <- c("passage", "id",
                        "position", "speed", "freq", "val",
                        "sex", "pronouns", "age", "ethnic", "socclass",
                        "panasPA", "panasNA", "phq8", "scaaredTotal")
for(j in 1:nrow(dfTrim_speed)){
  passage <- dfTrim_speed$passage[j]
  id <- dfTrim_speed$id[j]
  sex <- as.character(dfTrim_speed$sex[j])
  pronouns <- as.character(dfTrim_speed$pronouns[j])
  age <- dfTrim_speed$age[j]
  ethnic <- as.character(dfTrim_speed$ethnic[j])
  socclass <- as.character(dfTrim_speed$socclass[j])
  panasPA <- dfTrim_speed$panasPA[j]
  panasNA <- dfTrim_speed$panasNA[j]
  phq8 <- dfTrim_speed$phq8[j]
  scaaredTotal <- dfTrim_speed$scaaredTotal[j]
  speedFirst <- dfTrim_speed$speedWordFirst[j]
  speedSecond <- dfTrim_speed$speedWordSecond[j]
  freqFirst <- dfTrim_speed$freqFirst[j]
  freqSecond <- dfTrim_speed$freqSecond[j]
  valFirst <- dfTrim_speed$valFirst[j]
  valSecond <- dfTrim_speed$valSecond[j]
  speedDat[nrow(speedDat) + 1,] <-c(passage, id, "preswitch", speedFirst, freqFirst, valFirst, sex, pronouns, age, ethnic, socclass, panasPA, panasNA, phq8, scaaredTotal)
  speedDat[nrow(speedDat) + 1,] <-c(passage, id, "postswitch", speedSecond, freqSecond, valSecond, sex, pronouns, age, ethnic, socclass, panasPA, panasNA, phq8, scaaredTotal)
}

#organize data types
speedDat$speed <- as.numeric(speedDat$speed)
speedDat$freq <- as.numeric(speedDat$freq)
speedDat$val <- as.numeric(speedDat$val)
speedDat$position <- as.factor(speedDat$position)
speedDat$sex <- as.factor(speedDat$sex)
speedDat$pronouns <- as.factor(speedDat$pronouns)
speedDat$age <- as.numeric(speedDat$age)
speedDat$ethnic <- as.factor(speedDat$ethnic)
speedDat$socclass <- as.factor(speedDat$socclass)
speedDat$phq8 <- as.numeric(speedDat$phq8)
speedDat$scaaredTotal <- as.numeric(speedDat$scaaredTotal)

#modify contrasts for categorical predictors
contrasts(speedDat$position) <- contr.sum(2) #preswitch position: -1, postswitch position: +1
contrasts(speedDat$sex) <- contr.sum(2) #male: -1, female: +1

#center continuous predictors
speedDat$freq_gmc <- speedDat$freq - mean(speedDat$freq)
speedDat$val_gmc <- speedDat$val - mean(speedDat$val)
speedDat$age_gmc <- speedDat$age - mean(speedDat$age)
speedDat$phq8_gmc <- speedDat$phq8 - mean(speedDat$phq8)
speedDat$scaaredTotal_gmc <- speedDat$scaaredTotal - mean(speedDat$scaaredTotal)

#extract demo stats
speedDatStats <- subset(speedDat, !duplicated(speedDat$id))
summary(speedDatStats$age)
sd(speedDatStats$age)
summary(speedDatStats$sex)
summary(speedDatStats$sex) / length(unique(speedDatStats$id))
summary(speedDatStats$pronouns)
summary(speedDatStats$pronouns) / length(unique(speedDatStats$id))
summary(speedDatStats$ethnic)
summary(speedDatStats$ethnic) / length(unique(speedDatStats$id))
summary(speedDatStats$socclass)
summary(speedDatStats$socclass) / length(unique(speedDatStats$id))

#model
modelReadSpeed <- lmerTest::lmer(speed ~ position * freq_gmc * val_gmc + (1|id) + (1|passage), data=speedDat, REML=TRUE)
summary(modelReadSpeed)
confint(modelReadSpeed, method="boot", oldNames=FALSE)

#explore interaction
interact_plot(modelReadSpeed, pred = freq_gmc, modx = val_gmc, plot.points = TRUE, jitter=0, point.alpha=0.1)
interact_plot(modelReadSpeed, pred = val_gmc, modx = freq_gmc, plot.points = TRUE, jitter=0, point.alpha=0.1)

#Johnson-Neyman intervals
#create df and model (JN requires numeric (not factor) predictors)
speedDatJN <- speedDat
speedDatJN$posi <- as.numeric(speedDatJN$position=='postswitch')
for(a in 1:nrow(speedDatJN)){
  if(speedDatJN$posi[a]==0){speedDatJN$posi[a] <- -1}
} #adjust negative to -1 and positive to 1
modelReadSpeedJN <- lmerTest::lmer(speed ~ posi * freq_gmc * val_gmc + (1|id) + (1|passage), data=speedDatJN, REML=TRUE)
summary(modelReadSpeedJN) #check identical to main model

#johnson-neyman: frequency x valence
sim_slopes(modelReadSpeedJN, pred = freq_gmc, modx = val_gmc, jnplot = TRUE, cond.int=TRUE)
sim_slopes(modelReadSpeedJN, pred = val_gmc, modx = freq_gmc, jnplot = TRUE, cond.int=TRUE)

#plot: fig3
fig3_data <- sim_slopes(modelReadSpeedJN, pred = freq_gmc, modx = val_gmc, jnplot = TRUE, cond.int=TRUE)

vec <- seq(min(speedDat$freq_gmc), max(speedDat$freq_gmc), 0.001)
dfp <- data.frame(matrix(ncol=3, nrow=length(vec)))
colnames(dfp) <- c("freq_gmc", "speed", "val_gmc")
dfp[,1] <- vec
dfp[,2] <- fig3_data$slopes[3,2] * vec +fig3_data$ints[3,2]
dfp[,3] <- rep(1, length(vec))

dfn <- data.frame(matrix(ncol=3, nrow=length(vec)))
colnames(dfn) <- c("freq_gmc", "speed", "val_gmc")
dfn[,1] <- vec
dfn[,2] <- fig3_data$slopes[1,2] * vec +fig3_data$ints[1,2]
dfn[,3] <- rep(1, length(vec))

speedDat_fig3 <- speedDat
speedDat_fig3$valCat <- NA
for(x in 1:nrow(speedDat_fig3)){
  if(speedDat_fig3$val_gmc[x] > 0){speedDat_fig3$valCat[x] <- "More Positive (+1 SD)"}
  else{speedDat_fig3$valCat[x] <- "More Negative (-1 SD)"}
}

figure3 <- ggplot(data=speedDat_fig3, aes(x=freq_gmc, y=speed, color=valCat)) + geom_jitter(alpha=0.1, show.legend=FALSE) +
  coord_cartesian(xlim=c(-0.38, 0.42), ylim=c(1.3, 3.7)) +
  scale_color_manual(name = "Relative Passage Valence",
                     values = c("More Negative (-1 SD)" = "#202A44", "More Positive (+1 SD)" = "#E1AD01")) +
  guides(color = guide_legend(override.aes = list(alpha=1))) +
  geom_segment(aes(x=min(freq_gmc), xend=max(freq_gmc),
                   y=(fig3_data$slopes[3,2] * min(freq_gmc) + fig3_data$ints[3,2]),
                   yend=(fig3_data$slopes[3,2] * max(freq_gmc) + fig3_data$ints[3,2]), color=valCat), size=1, linetype=1, show.legend=TRUE) +
  geom_ribbon(data=dfp, aes(ymin=(speed - fig3_data$slopes[3,3]), ymax=(speed + fig3_data$slopes[3,3])), color=NA, fill="#E1AD01", alpha=0.3) +
  geom_segment(aes(x=min(freq_gmc), xend=max(freq_gmc),
                   y=(fig3_data$slopes[1,2] * min(freq_gmc) + fig3_data$ints[1,2]),
                   yend=(fig3_data$slopes[1,2] * max(freq_gmc) + fig3_data$ints[1,2])), color="#202A44", size=1, linetype=1) +
  geom_ribbon(data=dfn, aes(ymin=(speed - fig3_data$slopes[1,3]), ymax=(speed + fig3_data$slopes[1,3])), color=NA, fill="#202A44", alpha=0.3) +
  theme_light() + theme(legend.position = c(0.75, 0.15), legend.background = element_rect(fill = NA)) +
  labs(x="Average Log Frequency (centered)",
       y="Reading Speed (words per second)",
       title="Effect of Lexical Valence and\nWord Frequency on Reading Speed")

ggsave(paste(out_path, "figure3", "_", today, ".png", sep="", collapse=NULL))
cvd_grid(figure3) #test contrasts


### SECTION 4: INDIVIDUAL DIFFERENCES
#sex
modelReadSpeedSEX <- lmerTest::lmer(speed ~ position + freq_gmc + val_gmc +
                                   position:freq_gmc + position:val_gmc + freq_gmc:val_gmc +
                                   position:freq_gmc:val_gmc +
                                   sex + sex:val_gmc +
                                   (1|id) + (1|passage), data=speedDat, REML=TRUE)
summary(modelReadSpeedSEX)

#age
modelReadSpeedAGE <- lmerTest::lmer(speed ~ position + freq_gmc + val_gmc +
                                      position:freq_gmc + position:val_gmc + freq_gmc:val_gmc +
                                      position:freq_gmc:val_gmc +
                                      age_gmc + age_gmc:val_gmc +
                                      (1|id) + (1|passage), data=speedDat, REML=TRUE)
summary(modelReadSpeedAGE)

#depression (phq8)
modelReadSpeedPHQ <- lmerTest::lmer(speed ~ position + freq_gmc + val_gmc +
                                      position:freq_gmc + position:val_gmc + freq_gmc:val_gmc +
                                      position:freq_gmc:val_gmc +
                                      phq8_gmc + phq8_gmc:val_gmc +
                                      (1|id) + (1|passage), data=speedDat, REML=TRUE)
summary(modelReadSpeedPHQ)

#anxiety (scaaredTotal)
modelReadSpeedSCAARED <- lmerTest::lmer(speed ~ position + freq_gmc + val_gmc +
                                      position:freq_gmc + position:val_gmc + freq_gmc:val_gmc +
                                      position:freq_gmc:val_gmc +
                                      scaaredTotal_gmc + scaaredTotal_gmc:val_gmc +
                                      (1|id) + (1|passage), data=speedDat, REML=TRUE)
summary(modelReadSpeedSCAARED)

#PANAS {subset of participants}
#create dataframe
speedDat2 <- speedDat
speedDat2 <- subset(speedDat2, !is.na(speedDat2$panasPA))
speedDat2$panasPA <- as.numeric(speedDat2$panasPA)
speedDat2$panasNA <- as.numeric(speedDat2$panasNA)
speedDat2$panasPA_gmc <- speedDat2$panasPA - mean(speedDat2$panasPA)
speedDat2$panasNA_gmc <- speedDat2$panasNA - mean(speedDat2$panasNA)

#extract demo stats
speedDat2Stats <- subset(speedDat2, !duplicated(speedDat2$id))
summary(speedDat2Stats$age)
sd(speedDat2Stats$age)
summary(speedDat2Stats$sex)
summary(speedDat2Stats$sex) / length(unique(speedDat2Stats$id))
summary(speedDat2Stats$pronouns)
summary(speedDat2Stats$pronouns) / length(unique(speedDat2Stats$id))
summary(speedDat2Stats$ethnic)
summary(speedDat2Stats$ethnic) / length(unique(speedDat2Stats$id))
summary(speedDat2Stats$socclass)
summary(speedDat2Stats$socclass) / length(unique(speedDat2Stats$id))

#positive affect (panas-pa)
modelReadSpeedPANASPA <- lmerTest::lmer(speed ~ position + freq_gmc + val_gmc +
                                          position:freq_gmc + position:val_gmc + freq_gmc:val_gmc +
                                          position:freq_gmc:val_gmc +
                                          panasPA_gmc + panasPA_gmc:val_gmc +
                                          (1|id) + (1|passage), data=speedDat2, REML=TRUE)
summary(modelReadSpeedPANASPA)

#negative affect (panas-na)
modelReadSpeedPANASNA <- lmerTest::lmer(speed ~ position + freq_gmc + val_gmc +
                                          position:freq_gmc + position:val_gmc + freq_gmc:val_gmc +
                                          position:freq_gmc:val_gmc +
                                          panasNA_gmc + panasNA_gmc:val_gmc +
                                          (1|id) + (1|passage), data=speedDat2, REML=TRUE)
summary(modelReadSpeedPANASNA)
