# readAloud-valence-alpha Reading Task Analyses
# Author: Jessica M. Alexander
# Last Updated: 2022-10-01

### SECTION 1: SETTING UP
library(ggplot2)
library(ggbeeswarm)
library(readxl)
library(lme4)
library(lmerTest)
library(interactions)
library(ggstance)

#set up date for output file naming
today <- Sys.Date()
today <- format(today, "%Y%m%d")

#set up directories for input/output data
main_dataset <- '/Users/jalexand/github/readAloud-valence-dataset/'
main_analyses <- '/Users/jalexand/github/readAloud-valence-alpha/'
out_path <- '/Users/jalexand/github/readAloud-valence-alpha/products/poster-neurolang2022/results/'

data <- paste(main_dataset, 'derivatives/preprocessed/valence-timing/timingpitch_subject-by-passage_2022-09-09.csv', sep="", collapse=NULL)
subdat_path <- paste(main_dataset,'derivatives/preprocessed/readAloud_subject-level_summary_20220812.csv', sep="", collapse=NULL)
passdat_path <- paste(main_dataset,'derivatives/preprocessed/readAloud_passage-level_summary_20220812.csv', sep="", collapse=NULL)
passchar_path <- paste(main_dataset,'materials/readAloud-ldt/stimuli/readAloud/readAloud-stimuli_characteristics.xlsx', sep="", collapse=NULL)
readDat_path <- paste(main_analyses, 'derivatives/analysisStimuli_readDat_20221001.csv', sep="", collapse=NULL)
redcap_path <- paste(main_dataset,'derivatives/preprocessed/202201v0readAloudval_SCRD_2022-06-20_1019.csv', sep="", collapse=NULL)

### SECTION 2: READ IN PASSAGE-LEVEL DATA TO DF AND TRIM PARTICIPANTS AND TRIALS
df <- read.csv(data)
passchar <- read_xlsx(passchar_path, sheet='passages', na='#') #characteristics of the passage stimuli
redcap <- read.csv(redcap_path, na.strings='NA') #participant questionnaire responses
subdat <- read.csv(subdat_path, na.strings='NA') #subject-level, overall challenge question accuracy
passdat <- read.csv(passdat_path, na.strings='NA', check.names=FALSE) #passage level accuracy for each subject
passdat$passage <- c("dams", "flying", "bats", "broccoli", "realty", "bees", "dogshow", "dolphins", "icefishing",
                     "cars", "vegas", "sun", "caramel", "congo", "antarctica", "depression", "skunkowl", "grizzly",
                     "mantis", "dentist") #rename passages with short-name
readDat <- read.csv(readDat_path, na.strings='N') #passage-level characteristics from analysisStimuli.R

#consolidate ethnicity affiliation question from redcap
for(i in 1:nrow(redcap)){
  if(redcap$demo_b_ethnic_s1_r1_e1___1[i]==1){redcap$ethnic[i] <- 'AI'} #american indian/alaska native
  else if(redcap$demo_b_ethnic_s1_r1_e1___2[i]==1){redcap$ethnic[i] <- 'A'} #asian
  else if(redcap$demo_b_ethnic_s1_r1_e1___3[i]==1){redcap$ethnic[i] <- 'AA'} #african american
  else if(redcap$demo_b_ethnic_s1_r1_e1___4[i]==1){redcap$ethnic[i] <- 'LX'} #hispanic/latinx
  else if(redcap$demo_b_ethnic_s1_r1_e1___5[i]==1){redcap$ethnic[i] <- 'ME'} #middle eastern
  else if(redcap$demo_b_ethnic_s1_r1_e1___6[i]==1){redcap$ethnic[i] <- 'PI'} #pacific islander
  else if(redcap$demo_b_ethnic_s1_r1_e1___7[i]==1){redcap$ethnic[i] <- 'W'} #white
  else if(redcap$demo_b_ethnic_s1_r1_e1___8[i]==1){redcap$ethnic[i] <- 'O'} #other
  else{redcap$ethnic[i] <- 'UND'} #undisclosed
}

#build trial-level (i.e., passage-level) df of desired variables
for(i in 1:nrow(df)){
  subject <- df$id[i]
  passage <- df$passage[i]
  timeFirst <- df$timeFirst[i] #time spent reading first passage half
  timeSecond <- df$timeSecond[i] #time spent reading second passage half
  lenSyllPos <- passchar$lenSYLLpos[match(df$passage[i], passchar$passage)] #number of syllables in positive passage half
  lenSyllNeg <- passchar$lenSYLLneg[match(df$passage[i], passchar$passage)] #number of syllables in negative passage half
  df$overallACC[i] <- subdat$challengeACC[match(df$id[i], subdat$id)] #overall subject challenge question accuracy
  df$challengeACC[i] <- passdat[match(passage, passdat$passage), match(subject, colnames(passdat))] #passage-specific challenge question accuracy for subject
  df$mood[i] <- redcap$bmis_scrdVal_s1_r1_e1[match(df$id[i], redcap$record_id)] #participant valence score from BMIS questionnaire
  df$pronouns[i] <- redcap$demo_b_pronouns_s1_r1_e1[match(df$id[i], redcap$record_id)]   #participant preferred pronouns
  if(redcap$demo_b_sex_s1_r1_e1[match(df$id[i], redcap$record_id)]==1){df$sex[i] <- "male"}else{df$sex[i] <- "female"}   #participant biological sex
  df$yob[i] <- redcap$demo_b_yob_s1_r1_e1[match(df$id[i], redcap$record_id)] #participant year of birth
  df$ethnic[i] <- redcap$ethnic[match(df$id[i], redcap$record_id)] #participant ethnic group affiliation
  df$eng[i] <- redcap$demo_b_eng_s1_r1_e1[match(df$id[i], redcap$record_id)] #participant monolingualism
  df$langhis[i] <- redcap$demo_b_langhis_s1_r1_e1[match(df$id[i], redcap$record_id)] #participant language history
  df$ageen[i] <- redcap$demo_b_ageen_s1_r1_e1[match(df$id[i], redcap$record_id)] #participant age of English acquisition
  df$profen[i] <- redcap$demo_b_profen_s1_r1_e1[match(df$id[i], redcap$record_id)] #participant English proficiency
  df$commdis[i] <- sum(redcap$demo_b_comdiskid_s1_r1_e1[match(df$id[i], redcap$record_id)],
                       redcap$demo_b_comdisteen_s1_r1_e1[match(df$id[i], redcap$record_id)],
                       redcap$demo_b_comdisad_s1_r1_e[match(df$id[i], redcap$record_id)]) #participant communication disorder history
  df$stai5_scrdS[i] <- redcap$stai5_scrdS_s1_r1_e1[match(df$id[i], redcap$record_id)] #participant state anxiety score from STAI5 questionnaire
  switch <- passchar$switchType[match(df$passage[i], passchar$passage)] #store passage switch type as local variable
  df$switch[i] <- switch #passage switch type (pos2neg or neg2pos)
  df$questionVal[i] <- passchar$questionVal[match(df$passage[i], passchar$passage)] #whether challenge question came from positive or negative passage half
  df$lenSyllPos[i] <- lenSyllPos #number of syllables in positive passage portion
  df$lenSyllNeg[i] <- lenSyllNeg #number of syllables in negative passage portion
  if(switch=='pos2neg'){
    df$speedFirst[i] <- lenSyllPos/timeFirst
    df$speedSecond[i] <- lenSyllNeg/timeSecond
  }else{
    df$speedFirst[i] <- lenSyllNeg/timeFirst
    df$speedSecond[i] <- lenSyllPos/timeSecond
  } #calculate syllables per second
  df$freqFirst[i] <- readDat$avgFreq[which(readDat$passage==passage & readDat$position=='pre')] #averaged lexical frequency for first half
  df$freqSecond[i] <- readDat$avgFreq[which(readDat$passage==passage & readDat$position=='post')] #averaged lexical frequency for second half
  df$magFirst[i] <- readDat$avgMag[which(readDat$passage==passage & readDat$position=='pre')] #averaged lexical valence magnitude for first half
  df$magSecond[i] <- readDat$avgMag[which(readDat$passage==passage & readDat$position=='post')] #averaged lexical valence magnitude for second half
  df$valFirst[i] <- readDat$valenceWARAvg[which(readDat$passage==passage & readDat$position=='pre')] #averaged lexical valence for first half
  df$valSecond[i] <- readDat$valenceWARAvg[which(readDat$passage==passage & readDat$position=='post')] #averaged lexical valence for second half
}

#add delta between first and second half reading speeds
df$speedDelta <- abs(df$speedFirst - df$speedSecond)

#organize participant demographic variables
df$sex <- as.factor(df$sex)
df$age <- 2022 - as.numeric(df$yob)
df$ethnic <- as.factor(df$ethnic)

#remove participants based on challenge question accuracy and language history
dfTrim <- df
dfTrim <- subset(dfTrim, dfTrim$eng==0 | dfTrim$langhis %in% c(1,3) | dfTrim$ageen<=6) #keep monolingual English OR natively bilingual OR learned English before age 6
#sum(dfTrim$profen>3, na.rm=TRUE) #one remaining subject (150060) rates own English proficiency as not "elementary" or "not proficient", but reads fluidly and achieved 80% accuracy on challenge questions, so not excluded
dfTrim <- subset(dfTrim, dfTrim$commdis==0) #remove if diagnosed with communication disorder
dfTrim <- subset(dfTrim, overallACC>=0.6) #remove if challenge question accuracy below 60%

#extract age and sex stats
summary(dfTrim$age)
sd(dfTrim$age)
summary(dfTrim$sex)/20
summary(dfTrim$ethnic)/20/sum(summary(dfTrim$sex)/20)

#remove passages
dfTrim <- dfTrim[-c(which(is.na(dfTrim$timeFirst))),] #remove passages for which first half timing could not be calculated
dfTrim <- dfTrim[-c(which(is.na(dfTrim$timeSecond))),] #remove passages for which second half timing could not be calculated
dfTrim <- subset(dfTrim, dfTrim$challengeACC==1) #remove passages with incorrect answer on challenge question
dfTrim <- subset(dfTrim, !(dfTrim$passage=='broccoli')) #remove broccoli, which had a typo in the last sentence

#remove passages with speed delta >3 SDs (based on post-trimming averages)
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

dfTrim <- subset(dfTrim, dfTrim$speedDeltaOutlier==FALSE)


### SECTION 3: TRANSITION DATA TO LONG FORMAT
timePitchDat <- data.frame(matrix(ncol=15, nrow=0))
colnames(timePitchDat) <- c("passage", "switchType", "id", "position", "valenceCat", "speed", "pitch",
                            "freq", "mag", "val", "sex", "age", "ethnic", "mood", "stai5_scrdS")
for(j in 1:nrow(dfTrim)){
  passage <- dfTrim$passage[j]
  id <- dfTrim$id[j]
  sex <- as.character(dfTrim$sex[j])
  age <- dfTrim$age[j]
  ethnic <- as.character(dfTrim$ethnic[j])
  mood <- dfTrim$mood[j]
  stai5_scrdS <- dfTrim$stai5_scrdS[j]
  switchType <- dfTrim$switch[j]
  valFirstCat <- substr(switchType, 1, 3)
  valSecondCat <- substr(switchType, 5, 7)
  speedFirst <- dfTrim$speedFirst[j]
  speedSecond <- dfTrim$speedSecond[j]
  pitchFirst <- dfTrim$pitchMeanFirst[j]
  pitchSecond <- dfTrim$pitchMeanSecond[j]
  freqFirst <- dfTrim$freqFirst[j]
  freqSecond <- dfTrim$freqSecond[j]
  magFirst <- dfTrim$magFirst[j]
  magSecond <- dfTrim$magSecond[j]
  valFirst <- dfTrim$valFirst[j]
  valSecond <- dfTrim$valSecond[j]
  timePitchDat[nrow(timePitchDat) + 1,] <-c(passage, switchType, id, "preswitch", valFirstCat, speedFirst, pitchFirst, freqFirst, magFirst, valFirst, sex, age, ethnic, mood, stai5_scrdS)
  timePitchDat[nrow(timePitchDat) + 1,] <-c(passage, switchType, id, "postswitch", valSecondCat, speedSecond, pitchSecond, freqSecond, magSecond, valSecond, sex, age, ethnic, mood, stai5_scrdS)
}

#organize data types
timePitchDat$speed <- as.numeric(timePitchDat$speed)
timePitchDat$pitch <- as.numeric(timePitchDat$pitch)
timePitchDat$freq <- as.numeric(timePitchDat$freq)
timePitchDat$mag <- as.numeric(timePitchDat$mag)
timePitchDat$val <- as.numeric(timePitchDat$val)
timePitchDat$mood <- as.numeric(timePitchDat$mood)
timePitchDat$stai5_scrdS <- as.numeric(timePitchDat$stai5_scrdS)
timePitchDat$valenceCat <- as.factor(timePitchDat$valenceCat)
timePitchDat$position <- as.factor(timePitchDat$position)
timePitchDat$switchType <- as.factor(timePitchDat$switchType)
timePitchDat$sex <- as.factor(timePitchDat$sex)
timePitchDat$age <- as.numeric(timePitchDat$age)
timePitchDat$ethnic <- as.factor(timePitchDat$ethnic)

#modify contrasts for dependent variables
contrasts(timePitchDat$valenceCat) <- rev(contr.sum(2)) #negative valence: -1, positive valence: +1
contrasts(timePitchDat$position) <- contr.sum(2) #preswitch position: -1, postswitch position: +1


### SECTION 4: SPEED
#remove passages with speed delta >3 SDs
timeDat <- timePitchDat
for(p in 1:nrow(timeDat)){
  subId <- timeDat$id[p]
  subDf <- subset(timeDat, timeDat$id==subId)
  meanSpeed <- mean(timeDat$speed, na.rm=TRUE)
  sdSpeed <- sd(timeDat$speed, na.rm=TRUE)
  threeAbove <- meanSpeed + (3 * sdSpeed)
  threeBelow <- meanSpeed - (3 * sdSpeed)
  if(timeDat$speed[p] >= threeAbove | timeDat$speed[p] <= threeBelow){timeDat$speedOutlier[p] <- TRUE}
  else{timeDat$speedOutlier[p] <- FALSE}
}

oddrows <- timeDat[seq(1, nrow(timeDat), 2),]
evenrows <- timeDat[seq(2, nrow(timeDat), 2),]
speedMatch <- data.frame(cbind(oddrows$speedOutlier, evenrows$speedOutlier))
colnames(speedMatch) <- c("oddrows", "evenrows")
speedMatch$match <- (speedMatch$oddrows==TRUE | speedMatch$evenrows==TRUE)
outlierRows <- which(speedMatch$match==TRUE)
outlierPairs <- c()
for(row in 1:length(outlierRows)){
  second <- outlierRows[row] * 2
  first <- second - 1
  outlierPairs <- c(outlierPairs, first, second)
}

timeDatTrim <- timeDat[-outlierPairs,]

#alternate analysis with female participants only (given that only using female readers for pitch analysis)
#timeDatTrim <- subset(timeDatTrim, timeDatTrim$sex=="female")

#center continuous passage predictors
timeDatTrim$freq_gmc <- timeDatTrim$freq - mean(timeDatTrim$freq)
timeDatTrim$mag_gmc <- timeDatTrim$mag - mean(timeDatTrim$mag)
timeDatTrim$val_gmc <- timeDatTrim$val - mean(timeDatTrim$val)


#MAIN VISUAL PLOT
timeDatTrimPlot <- timeDatTrim
levels(timeDatTrimPlot$position) <- list(Preswitch = "preswitch", Postswitch = "postswitch")
pd1 <- position_dodge(0.2)
pd2 <- position_dodge(1.2)
ggplot(timeDatTrimPlot) + aes(x=position, y=speed, fill=valenceCat, color=freq_gmc) +
              geom_beeswarm(cex=1.5, size=1.0, dodge.width=1.0) +
              geom_boxplot(width = 0.25, position=pd1, alpha=0.9, outlier.size=1.0) +
              theme_classic() + theme(plot.title = element_text(hjust = 0.5, size= 20)) +
              ggtitle('Reading Speed by Valence and Position') +
              ylab('Speed (syllables per second)') +
              xlab('Position of Passage Half') +
              scale_color_gradient(low="#e76e5b", high="#fccd25", name="Word Frequency") +
              scale_fill_manual(values = c("#c5407e", "#6e00a8"), name="Valence", labels=c("Negative", "Positive"))

#SIMPLE LINEAR MODEL
modelA <- lm(speed ~ valenceCat * position, data=timeDatTrim)
summary(modelA)

modelA.t1 <- t.test(timeDatTrim$speed[which(timeDatTrim$valenceCat=='pos' & timeDatTrim$position=='preswitch')],
       timeDatTrim$speed[which(timeDatTrim$valenceCat=='neg' & timeDatTrim$position=='preswitch')]) #non-significant
modelA.t2 <- t.test(timeDatTrim$speed[which(timeDatTrim$valenceCat=='pos' & timeDatTrim$position=='postswitch')],
       timeDatTrim$speed[which(timeDatTrim$valenceCat=='neg' & timeDatTrim$position=='postswitch')])  #significant: valence in postswitch
modelA.t3 <- t.test(timeDatTrim$speed[which(timeDatTrim$valenceCat=='pos' & timeDatTrim$position=='preswitch')],
       timeDatTrim$speed[which(timeDatTrim$valenceCat=='pos' & timeDatTrim$position=='postswitch')])  #non-significant
modelA.t4 <- t.test(timeDatTrim$speed[which(timeDatTrim$valenceCat=='neg' & timeDatTrim$position=='preswitch')],
       timeDatTrim$speed[which(timeDatTrim$valenceCat=='neg' & timeDatTrim$position=='postswitch')])  #significant: negative in pre/post

modelA.pvals <- c(modelA.t1$p.value, modelA.t2$p.value, modelA.t3$p.value, modelA.t4$p.value)
format(p.adjust(modelA.pvals, method='fdr'), scientific=FALSE)


#MIXED EFFECTS
modelReadSpeed <- lmerTest::lmer(speed ~ position * freq_gmc * val_gmc + (1|id) + (1|passage), data=timeDatTrim, REML=FALSE)
summary(modelReadSpeed)

interact_plot(modelReadSpeed, pred = freq_gmc, modx = position,
              plot.points = TRUE, jitter=0.01, point.alpha=0.1, colors=c("#dd5e66", "#0d0887"), interval=TRUE, vary.lty=FALSE,
              main.title = "Effect of Word Frequency\nand Passage Position\non Reading Speed",
              x.label="Average Log Frequency (centered)", y.label="Reading Speed (syllables per second)",
              legend.main = "Passage Position", modx.labels = c("Postswitch", "Preswitch")) +
              theme(plot.title = element_text(color="black", hjust = 0.5, size= 20), strip.text = element_text(color="black"),
              legend.title = element_text(color="black"), legend.text = element_text(color="black"),
              legend.position = c(0.82, 0.15), legend.background = element_rect(fill="white", color="black"),
              axis.title.x = element_text(color="black"), axis.title.y = element_text(color="black"),
              axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black")) +
              annotate("text", label="*significant frequency × position (pre/post) interaction\n(p = 0.0000000000000002)", x=-0.1, y=1.5, size=3.5)
#ggsave(paste(out_path, "plot1", "_", today, ".png", sep="", collapse=NULL))

interact_plot(modelReadSpeed, pred = freq_gmc, modx = val_gmc, plot.points = TRUE, jitter=0.01,
              point.alpha=0.1, colors=c("#f79044", "#dd5e66", "#0d0887"), interval=TRUE, vary.lty=FALSE,
              main.title = "Effect of Lexical Valence\nand Word Frequency\non Reading Speed",
              x.label="Average Log Frequency (centered)", y.label="Reading Speed (syllables per second)", legend.main = "Valence") +
              theme(plot.title = element_text(color="black", hjust = 0.5, size= 20),
              legend.title = element_text(color="black"), legend.text = element_text(color="black"),
              legend.position = c(0.8, 0.17), legend.background = element_rect(fill="white", color="black"),
              axis.title.x = element_text(color="black"), axis.title.y = element_text(color="black"),
              axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black")) +
  annotate("text", label="*significant valence × frequency interaction\n(p = 0.000000000000000233)", x=-0.1, y=1.9, size=3.5)
#ggsave(paste(out_path, "plot2", "_", today, ".png", sep="", collapse=NULL))

#create df and model for Johnson-Neyman intervals, which requires numeric (not factor) predictors
timeDatTrimJN <- timeDatTrim
timeDatTrimJN$posi <- as.numeric(timeDatTrimJN$position=='postswitch')
for(a in 1:nrow(timeDatTrimJN)){
  if(timeDatTrimJN$posi[a]==0){timeDatTrimJN$posi[a] <- -1}
} #adjust negative to -1 and positive to 1
modelReadSpeedJN <- lmerTest::lmer(speed ~ posi * freq_gmc * val_gmc + (1|id) + (1|passage), data=timeDatTrimJN, REML=FALSE)

#frequency x position
johnson_neyman(modelReadSpeedJN, pred = freq_gmc, modx = posi, control.fdr=TRUE)
sim_margins(modelReadSpeedJN, pred = freq_gmc, modx = posi, cond.int=TRUE)
sim_slopes(modelReadSpeedJN, pred = freq_gmc, modx = posi, jnplot = TRUE, cond.int=TRUE)
sim_slopes(modelReadSpeedJN, pred = posi, modx = freq_gmc, jnplot = TRUE, cond.int=TRUE)

#frequency x valence
sim_slopes(modelReadSpeedJN, pred = freq_gmc, modx = val_gmc, jnplot = TRUE, cond.int=TRUE)
sim_slopes(modelReadSpeedJN, pred = val_gmc, modx = freq_gmc, jnplot = TRUE, cond.int=TRUE)


#STATE MEASURES OF INDIVIDUAL DIFFERENCE
#check for potentially invalid mood ratings (none found)
bmis <- subset(redcap, redcap$bmis_s1_r1_e1_complete==2)
cols <- as.character(expression(record_id, bmis_i1_s1_r1_e1, bmis_i2_s1_r1_e1, bmis_i3_s1_r1_e1, bmis_i4_s1_r1_e1, bmis_i5_s1_r1_e1, bmis_i6_s1_r1_e1, bmis_i7_s1_r1_e1, bmis_i8_s1_r1_e1, bmis_i9_s1_r1_e1, bmis_i10_s1_r1_e1, bmis_i11_s1_r1_e1, bmis_i12_s1_r1_e1, bmis_i13_s1_r1_e1, bmis_i14_s1_r1_e1, bmis_i15_s1_r1_e1, bmis_i16_s1_r1_e1, bmis_i17_s1_r1_e1))
bmis <- select(bmis, all_of(cols))
for(sub in 1:nrow(bmis)){
  bmis$one[sub] <- sum(bmis[sub,2:16]==1)
  bmis$two[sub] <- sum(bmis[sub,2:16]==2)
  bmis$three[sub] <- sum(bmis[sub,2:16]==3)
  bmis$four[sub] <- sum(bmis[sub,2:16]==4)
}

#exclude participants who did not complete BMIS/STAI5 questionnaires in same window as reading task
noMoodList <- c(150019L, 150060L, 150067L, 150074L, 150075L, 150083L, 150116L, 150128L, 150131L, 150252L, 150299L)
timeDatTrimMood<- timeDatTrim[!(timeDatTrim$id %in% noMoodList),]
timeDatTrimSTAIS <- subset(timeDatTrimMood, !(is.na(timeDatTrimMood$stai5_scrdS))) #stai5 added after start of data collection

#extract age and sex stats
staisSubs <- unique(timeDatTrimSTAIS$id)
dfStaisSubs <- subset(df, df$id %in% staisSubs)
summary(dfStaisSubs$age)
summary(dfStaisSubs$sex)/20
summary(dfStaisSubs$ethnic)/20

#center stai5
staisMeanDf <- subset(timeDatTrimSTAIS, !duplicated(timeDatTrimSTAIS$id))
staisMean <- mean(staisMeanDf$stai5_scrdS)
timeDatTrimSTAIS$stais_gmc <- timeDatTrimSTAIS$stai5_scrdS - staisMean

#re-center continuous passage predictors
timeDatTrimSTAIS$freq_gmc <- timeDatTrimSTAIS$freq - mean(timeDatTrimSTAIS$freq)
timeDatTrimSTAIS$mag_gmc <- timeDatTrimSTAIS$mag - mean(timeDatTrimSTAIS$mag)
timeDatTrimSTAIS$val_gmc <- timeDatTrimSTAIS$val - mean(timeDatTrimSTAIS$val)

#stais: model and interactions
modelStaisVal <- lmerTest::lmer(speed ~ position * freq_gmc * val_gmc * stais_gmc + (1|id) + (1|passage), data=timeDatTrimSTAIS, REML=FALSE)
summary(modelStaisVal)


### SECTION 5: PITCH
#remove participants whose audio prevents pitch measurements
noPitchList <- c(150017L, 150056L, 150086L, 150088L, 150262L)
pitchDat <- timePitchDat[!(timePitchDat$id %in% noPitchList),]

#extract age and sex stats
pitchDatStats <- subset(pitchDat, !duplicated(pitchDat$id))
summary(pitchDatStats$age)
summary(pitchDatStats$sex)
summary(pitchDatStats$ethnic)

#remove passages with pitch delta >3 SDs
for(p in 1:nrow(pitchDat)){
  subId <- pitchDat$id[p]
  subDf <- subset(pitchDat, pitchDat$id==subId)
  meanPitch <- mean(pitchDat$pitch, na.rm=TRUE)
  sdPitch <- sd(pitchDat$pitch, na.rm=TRUE)
  threeAbove <- meanPitch + (3 * sdPitch)
  threeBelow <- meanPitch - (3 * sdPitch)
  if(pitchDat$pitch[p] >= threeAbove | pitchDat$pitch[p] <= threeBelow){pitchDat$pitchOutlier[p] <- TRUE}
  else{pitchDat$pitchOutlier[p] <- FALSE}
}

oddrows <- pitchDat[seq(1, nrow(pitchDat), 2),]
evenrows <- pitchDat[seq(2, nrow(pitchDat), 2),]
pitchMatch <- data.frame(cbind(oddrows$pitchOutlier, evenrows$pitchOutlier))
colnames(pitchMatch) <- c("oddrows", "evenrows")
pitchMatch$match <- (pitchMatch$oddrows==TRUE | pitchMatch$evenrows==TRUE)
outlierRows <- which(pitchMatch$match==TRUE)
outlierPairs <- c()
for(row in 1:length(outlierRows)){
  second <- outlierRows[row] * 2
  first <- second - 1
  outlierPairs <- c(outlierPairs, first, second)
}

pitchDatTrim <- pitchDat[-outlierPairs,]

#simple plot
ggplot(data=pitchDat, aes(x=position, y=pitch, fill=valenceCat)) + geom_boxplot() + facet_wrap(~sex)

#subset to female participants and center continuous predictors
pitchDatTrimF <- pitchDatTrim[which(pitchDatTrim$sex=='female'),]
pitchDatTrimF$freq_gmc <- pitchDatTrimF$freq - mean(pitchDatTrimF$freq)
pitchDatTrimF$mag_gmc <- pitchDatTrimF$mag - mean(pitchDatTrimF$mag)
pitchDatTrimF$val_gmc <- pitchDatTrimF$val - mean(pitchDatTrimF$val)

#extract age and sex stats
pitchDatFStats <- subset(pitchDatTrimF, !duplicated(pitchDatTrimF$id))
summary(pitchDatFStats$age)
summary(pitchDatFStats$sex)
summary(pitchDatFStats$ethnic)

#mixed effects model + plots
modelPitchFemale <- lmerTest::lmer(pitch ~ val_gmc * position * freq_gmc + (1|id) + (1|passage), data=pitchDatTrimF, REML=FALSE)
summary(modelPitchFemale)

pitchDatTrimFPlot <- pitchDatTrimF
levels(pitchDatTrimFPlot$position) <- list(Preswitch = "preswitch", Postswitch = "postswitch")
ggplot(pitchDatTrimFPlot) + aes(x=position, y=pitch, fill=position, color=position) +
  geom_beeswarm(cex=1.5, size=1.0, dodge.width=1.0, alpha=0.5, show.legend = FALSE) +
  geom_boxplot(width = 0.25, alpha=1, outlier.size=1.0, color="black", show.legend = FALSE) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5, size= 20, face="bold"),
                          plot.subtitle = element_text(hjust = 0.5, size= 11)) +
  ggtitle('Effect of Passage Position\non Vocal Pitch') +
  labs(subtitle = '(Female Participants Only)') +
  ylab('Average Vocal Pitch (Hz)') +
  xlab('Position of Passage Half') +
  scale_fill_manual(values = c("#c5407e", "#6e00a8")) +
  scale_color_manual(values = c("#c5407e", "#6e00a8"))
#ggsave(paste(out_path, "plot3", "_", today, ".png", sep="", collapse=NULL))

### SECTION 6: CHALLENGE QUESTIONS
subs <- unique(dfTrim$id)

challengeDat <- data.frame(matrix(ncol=10, nrow=0))
colnames(challengeDat) <- c("passage", "switchType", "id", "position", "challengeACC", "challengeSource", "valenceCat", "freq", "mag", "val")
for(j in 1:nrow(df)){
  passage <- df$passage[j]
  id <- df$id[j]
  challengeACC <- df$challengeACC[j]
  questionVal <- df$questionVal[j]
  switchType <- df$switch[j]
  valFirstCat <- substr(switchType, 1, 3)
  valSecondCat <- substr(switchType, 5, 7)
  challengeFirst <- questionVal==valFirstCat
  challengeSecond <- questionVal==valSecondCat
  speedFirst <- df$speedFirst[j]
  speedSecond <- df$speedSecond[j]
  freqFirst <- df$freqFirst[j]
  freqSecond <- df$freqSecond[j]
  magFirst <- df$magFirst[j]
  magSecond <- df$magSecond[j]
  valFirst <- df$valFirst[j]
  valSecond <- df$valSecond[j]
  challengeDat[nrow(challengeDat) + 1,] <-c(passage, switchType, id, "preswitch", challengeACC, challengeFirst, valFirstCat, freqFirst, magFirst, valFirst)
  challengeDat[nrow(challengeDat) + 1,] <-c(passage, switchType, id, "postswitch", challengeACC, challengeSecond, valSecondCat, freqSecond, magSecond, valSecond)
}

#organize data types
challengeDat$challengeACC <- as.numeric(challengeDat$challengeACC)
challengeDat$freq <- as.numeric(challengeDat$freq)
challengeDat$mag <- as.numeric(challengeDat$mag)
challengeDat$val <- as.numeric(challengeDat$val)
challengeDat$valenceCat <- as.factor(challengeDat$valenceCat)
challengeDat$position <- as.factor(challengeDat$position)
challengeDat$switchType <- as.factor(challengeDat$switchType)

#trim passages and participants
challengeDat <- subset(challengeDat, challengeDat$challengeSource==TRUE) #limit to passage halves that included the challenge question answer
challengeDat <- subset(challengeDat, !(challengeDat$passage=="broccoli")) #remove broccoli, which had a typo in the last sentence
challengeDat <- subset(challengeDat, challengeDat$id %in% subs) #limit to participants included in main analysis

#center continuous predictors
challengeDat$freq_gmc <- challengeDat$freq - mean(challengeDat$freq)
challengeDat$mag_gmc <- challengeDat$mag - mean(challengeDat$mag)
challengeDat$val_gmc <- challengeDat$val - mean(challengeDat$val)

#modify contrasts for dependent variables
contrasts(challengeDat$position) <- contr.sum(2) #preswitch position: -1, postswitch position: +1

#mixed effects model
modelReadAcc <- glmer(challengeACC ~ position * val_gmc * freq_gmc + (1|id) + (1|passage), data=challengeDat, family="binomial")
summary(modelReadAcc)

interact_plot(modelReadAcc, pred = freq_gmc, modx = position,
              plot.points = TRUE, jitter=0.01, point.alpha=0.1, colors=c("#dd5e66", "#0d0887"), interval=TRUE, vary.lty=FALSE,
              main.title = "Effect of Word Frequency\nand Passage Position\non Content Question Accuracy",
              x.label="Average Log Frequency (centered)", y.label="Challenge Accuracy",
              legend.main = "Passage Position", modx.labels = c("Postswitch", "Preswitch")) +
              theme(plot.title = element_text(color="black", hjust = 0.5, size= 20), strip.text = element_text(color="black"),
              legend.position = c(0.75, 0.3), legend.background = element_rect(fill="white", color="black"),
              legend.title = element_text(color="black"), legend.text = element_text(color="black"),
              axis.title.x = element_text(color="black"), axis.title.y = element_text(color="black"),
              axis.text.x = element_text(color="black"), axis.text.y = element_text(color="black")) +
  annotate("text", label="*significant accuracy × frequency interaction\n(p = 0.00381)", x=-0.1, y=0.1, size=3.5)
#ggsave(paste(out_path, "plot4", "_", today, ".png", sep="", collapse=NULL))

interact_plot(modelReadAcc, pred = val_gmc, modx = position)

#create df and model for Johnson-Neyman intervals, which requires numeric (not factor) predictors
challengeDatJN <- challengeDat
challengeDatJN$posi <- as.numeric(challengeDatJN$position=='postswitch')
for(a in 1:nrow(challengeDatJN)){
  if(challengeDatJN$posi[a]==0){challengeDatJN$posi[a] <- -1}
} #adjust negative to -1 and positive to 1
modelReadAccJN <- glmer(challengeACC ~ posi * val_gmc * freq_gmc + (1|id) + (1|passage), data=challengeDatJN, family="binomial")

#position x frequency
sim_slopes(modelReadAccJN, pred = posi, modx = freq_gmc, jnplot = TRUE, cond.int=TRUE)
sim_slopes(modelReadAccJN, pred = freq_gmc, modx = posi, jnplot = TRUE, cond.int=TRUE)

#position x valence
sim_slopes(modelReadAccJN, pred = posi, modx = val_gmc, jnplot = TRUE, cond.int=TRUE)
sim_slopes(modelReadAccJN, pred = val_gmc, modx = posi, jnplot = TRUE, cond.int=TRUE)