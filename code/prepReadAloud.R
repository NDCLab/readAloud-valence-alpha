# readAloud-valence-alpha Reading Task Analysis Preparation
# Author: Jessica M. Alexander
# Last Updated: 2022-11-20

### SECTION 1: SETTING UP
library(readxl)

#set up date for output file naming
today <- Sys.Date()
today <- format(today, "%Y%m%d")

#set up directories for input/output data
main_dataset <- '/Users/jalexand/github/readAloud-valence-dataset/'
main_analyses <- '/Users/jalexand/github/readAloud-valence-alpha/'
out_path <- '/Users/jalexand/github/readAloud-valence-alpha/derivatives/'

data <- paste(main_dataset, 'derivatives/preprocessed/valence-timing/timingpitch_subject-by-passage_2022-09-09.csv', sep="", collapse=NULL)
passdat_path <- paste(main_dataset,'derivatives/preprocessed/readAloud_passage-level_summary_20220812.csv', sep="", collapse=NULL)
passchar_path <- paste(main_dataset,'materials/readAloud-ldt/stimuli/readAloud/readAloud-stimuli_characteristics.xlsx', sep="", collapse=NULL)
readDat_path <- paste(main_dataset, 'derivatives/analysisStimuli_readDat_20221120.csv', sep="", collapse=NULL)
redcap_path <- paste(main_dataset,'derivatives/preprocessed/202201v0readAloudval_SCRD_2022-06-20_1019.csv', sep="", collapse=NULL)


### SECTION 2: READ IN PASSAGE-LEVEL DATA TO DF
df <- read.csv(data)
passchar <- read_xlsx(passchar_path, sheet='passages', na='#') #characteristics of the passage stimuli
redcap <- read.csv(redcap_path, na.strings='NA') #participant questionnaire responses
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

#add ids for three participants who met inclusion criteria but were not timestamped due to low challenge accuracy
not_timestamped <- c(150222, 150252, 150257)
for(sub in not_timestamped){
  tempdf <- data.frame(matrix(nrow=20, ncol=ncol(df)))
  colnames(tempdf) <- colnames(df)
  tempdf[,1] <- rep(sub, 20)
  tempdf[,11] <- df$passage[1:20]
  df <- rbind(df, tempdf)
}

#build trial-level (i.e., passage-level) df of desired variables
for(i in 1:nrow(df)){
  subject <- df$id[i]
  passage <- df$passage[i]
  timeFirst <- df$timeFirst[i] #time spent reading first passage half
  timeSecond <- df$timeSecond[i] #time spent reading second passage half
  lenSyllPos <- passchar$lenSYLLpos[match(df$passage[i], passchar$passage)] #number of syllables in positive passage half
  lenSyllNeg <- passchar$lenSYLLneg[match(df$passage[i], passchar$passage)] #number of syllables in negative passage half
  df$challengeACC[i] <- passdat[match(passage, passdat$passage), match(subject, colnames(passdat))] #passage-specific challenge question accuracy for subject
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
  df$valFirst[i] <- readDat$valenceWARAvg[which(readDat$passage==passage & readDat$position=='pre')] #averaged lexical valence for first half
  df$valSecond[i] <- readDat$valenceWARAvg[which(readDat$passage==passage & readDat$position=='post')] #averaged lexical valence for second half
}

#add delta between first and second half reading speeds
df$speedDelta <- abs(df$speedFirst - df$speedSecond)

#organize participant demographic variables
df$sex <- as.factor(df$sex)
df$age <- 2022 - as.numeric(df$yob)
df$ethnic <- as.factor(df$ethnic)


### SECTION 3: TRIM PARTICIPANTS WHO DID NOT MEET EXCLUSION CRITERIA
dfTrim <- df
dfTrim <- subset(dfTrim, dfTrim$eng==0 | dfTrim$langhis %in% c(1,3) | dfTrim$ageen<=6) #keep monolingual English OR natively bilingual OR learned English before age 6
#sum(dfTrim$profen>3, na.rm=TRUE) #one remaining subject (150060) rates own English proficiency as not "elementary" or "not proficient", but reads fluidly and achieved 80% accuracy on challenge questions, so not excluded
dfTrim <- subset(dfTrim, dfTrim$commdis==0) #remove if diagnosed with communication disorder

#extract age and sex stats
summary(dfTrim$age)
sd(dfTrim$age)
summary(dfTrim$sex)/20
summary(dfTrim$ethnic)/20
summary(dfTrim$ethnic)/20/sum(summary(dfTrim$sex)/20)


### SECTION 4: TRIM PASSAGES DUE TO EXPERIMENTER ERROR
#remove broccoli passage from all participants due to typo in the last sentence
dfTrim <- subset(dfTrim, !(dfTrim$passage=='broccoli'))


### SECTION 5: OUTPUT DATAFRAME
write.csv(dfTrim, paste(out_path, "readAloudData_", today, ".csv", sep="", collapse=NULL))