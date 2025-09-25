# readAloud-valence-alpha Reading Task Analysis Preparation
# Author: Jessica M. Alexander
# Last Updated: 2024-02-01

# INPUTS
# data/df: behavioral data, for each participant on each passage, incl. time spent reading (each half) and F0 mean/sd (each half)
# accDat: comprehension question accuracy (0/1) for each participant for each passage
# readDat: stimuli characteristics from each passage half
# redcap: participant data, incl. demographics and responses + scored factors for questionnaires:
    # TRAIT SURVEYS
      # phq8 (patient health questionnaire): phq8_scrdTotal (depression scale total)
      # scaared (screen for adult anxiety disorders): scaared_b_scrdTotal (total anxiety)
    # STATE SURVEY
      # panas (positive and negative affect schedule, "now" {subset of participants}):
                                                                                # panasnow_scrdPA (positive affect);
                                                                                # panasnow_scrdNA (negative affect)
# OUTPUTS
# dfTrim: for each passage half, for each participant, details on:
  # timestamps: start, switch, end
  # raw participant behavior: time spent reading, reading pitch (avg and sd), comprehension question accuracy
  # passage half characteristics: passage switch type, length (syllable and word), average syllables per word, word frequency, lexical valence, source of comprehension question
  # participant data: demographics, language history, mood and mood disorder scores
  # calculated participant behavior: reading speed (syllables/second and words/second), delta in speed between halves of same passage

### SECTION 1: SETTING UP
library(readxl)

#set up date for output file naming
today <- Sys.Date()
today <- format(today, "%Y%m%d")

#set up directories for input/output
main_dataset <- '/Users/jalexand/github/readAloud-valence-dataset/'
main_analyses <- '/Users/jalexand/github/readAloud-valence-alpha/'
out_path <- '/Users/jalexand/github/readAloud-valence-alpha/derivatives/'

#load input files
data <- paste(main_dataset, 'derivatives/preprocessed/valence-timing/timingpitch_subject-by-passage_2022-09-09.csv', sep="", collapse=NULL)
accDat_path <- paste(main_dataset,'derivatives/preprocessed/readAloud_passage-level_summary_20220812.csv', sep="", collapse=NULL)
readDat_path <- paste(main_dataset, 'derivatives/analysisStimuli_readDat_20230614.csv', sep="", collapse=NULL)
redcap_path <- paste(main_dataset,'derivatives/preprocessed/202201v0readAloudval_SCRD_2022-06-20_1019.csv', sep="", collapse=NULL)
agedat_path <- paste(main_dataset,'derivatives/preprocessed/202201v0readAloudval_SCRD_2022-06-20_1019_ageonly.csv', sep="", collapse=NULL)

df <- read.csv(data)
redcap <- read.csv(redcap_path, na.strings='NA') #participant questionnaire responses
agedat <- read.csv(agedat_path, na.strings='NA') #participant age information
readDat <- read.csv(readDat_path, na.strings='N') #passage-level characteristics from analysisStimuli.R
accDat <- read.csv(accDat_path, na.strings='NA', check.names=FALSE) #passage level accuracy for each subject
accDat$passage <- c("dams", "flying", "bats", "broccoli", "realty", "bees", "dogshow", "dolphins", "icefishing",
                     "cars", "vegas", "sun", "caramel", "congo", "antarctica", "depression", "skunkowl", "grizzly",
                     "mantis", "dentist")        #rename passages with short-name

#add ids for three participants who met inclusion criteria but were not timestamped due to low challenge accuracy
not_timestamped <- c(150222, 150252, 150257)
for(sub in not_timestamped){
  tempdf <- data.frame(matrix(nrow=20, ncol=ncol(df)))
  colnames(tempdf) <- colnames(df)
  tempdf[,1] <- rep(sub, 20)
  tempdf[,11] <- df$passage[1:20]
  df <- rbind(df, tempdf)
}

### SECTION 2: BUILD DEMOGRAPHIC DATA DF
demoDat <- redcap[,c(1,5)]
#biological sex: replace numerical values with text description
for(a in 1:nrow(redcap)){
  if(is.na(redcap$demo_b_sex_s1_r1_e1[a])){demoDat$sex[a] <- 'undisclosed'}
  else if(redcap$demo_b_sex_s1_r1_e1[a]==1){demoDat$sex[a] <- 'male'}
  else if(redcap$demo_b_sex_s1_r1_e1[a]==2){demoDat$sex[a] <- 'female'}
  else if(redcap$demo_b_sex_s1_r1_e1[a]==3){demoDat$sex[a] <- 'intersex'}
  else if(redcap$demo_b_sex_s1_r1_e1[a]==4){demoDat$sex[a] <- 'other'}
  else if(redcap$demo_b_sex_s1_r1_e1[a]==5){demoDat$sex[a] <- 'unknown'}
  else{demoDat$sex[a] <- 'undisclosed'}
}

#preferred pronouns: replace numerical values with text description
for(b in 1:nrow(redcap)){
  if(is.na(redcap$demo_b_pronouns_s1_r1_e1[b])){demoDat$pron[b] <- 'undisclosed'}
  else if(redcap$demo_b_pronouns_s1_r1_e1[b]==1){demoDat$pron[b] <- 'she/her'}
  else if(redcap$demo_b_pronouns_s1_r1_e1[b]==2){demoDat$pron[b] <- 'he/him'}
  else if(redcap$demo_b_pronouns_s1_r1_e1[b]==3){demoDat$pron[b] <- 'they/them'}
  else if(redcap$demo_b_pronouns_s1_r1_e1[b]==5){demoDat$pron[b] <- 'other'}
  else{demoDat$pron[b] <- 'undisclosed'}
}

#race/ethnicity affiliation: map to text description
redcap$numEthResp <- rowSums(redcap[,c("demo_b_ethnic_s1_r1_e1___1", "demo_b_ethnic_s1_r1_e1___2", "demo_b_ethnic_s1_r1_e1___3",
                                       "demo_b_ethnic_s1_r1_e1___4", "demo_b_ethnic_s1_r1_e1___5", "demo_b_ethnic_s1_r1_e1___6",
                                       "demo_b_ethnic_s1_r1_e1___7", "demo_b_ethnic_s1_r1_e1___8")]) #sum across columns to identify participants identifying as multiracial
for(c in 1:nrow(redcap)){
  if(redcap$numEthResp[c]>1){demoDat$ethnic[c] <- 'M'} #multiple options selected
  else if(redcap$demo_b_ethnic_s1_r1_e1___1[c]==1){demoDat$ethnic[c] <- 'AI'} #american indian/alaska native
  else if(redcap$demo_b_ethnic_s1_r1_e1___2[c]==1){demoDat$ethnic[c] <- 'A'} #asian
  else if(redcap$demo_b_ethnic_s1_r1_e1___3[c]==1){demoDat$ethnic[c] <- 'AA'} #african american
  else if(redcap$demo_b_ethnic_s1_r1_e1___4[c]==1){demoDat$ethnic[c] <- 'LX'} #hispanic/latinx
  else if(redcap$demo_b_ethnic_s1_r1_e1___5[c]==1){demoDat$ethnic[c] <- 'ME'} #middle eastern
  else if(redcap$demo_b_ethnic_s1_r1_e1___6[c]==1){demoDat$ethnic[c] <- 'PI'} #pacific islander
  else if(redcap$demo_b_ethnic_s1_r1_e1___7[c]==1){demoDat$ethnic[c] <- 'W'} #white
  else if(redcap$demo_b_ethnic_s1_r1_e1___8[c]==1){demoDat$ethnic[c] <- 'O'} #other
  else{demoDat$ethnic[c] <- 'UND'} #undisclosed
}

#social class affiliation: replace numerical values with text description
for(d in 1:nrow(redcap)){
  if(is.na(redcap$demo_b_socclass_s1_r1_e1[d])){demoDat$socclass[d] <- 'undisclosed'}
  else if(redcap$demo_b_socclass_s1_r1_e1[d]==1){demoDat$socclass[d] <- 'poor'}
  else if(redcap$demo_b_socclass_s1_r1_e1[d]==2){demoDat$socclass[d] <- 'working'}
  else if(redcap$demo_b_socclass_s1_r1_e1[d]==3){demoDat$socclass[d] <- 'middle'}
  else if(redcap$demo_b_socclass_s1_r1_e1[d]==4){demoDat$socclass[d] <- 'affluent'}
  else{demoDat$socclass[d] <- 'undisclosed'}
}

#communication disorders diagnoses: sum across childhood, adolescence, and adulthood
for(e in 1:nrow(redcap)){
  demoDat$commdis[e] <- sum(redcap$demo_b_comdiskid_s1_r1_e1[e],
                            redcap$demo_b_comdisteen_s1_r1_e1[e],
                            redcap$demo_b_comdisad_s1_r1_e[e])
}

#language history: transfer directly
for(f in 1:nrow(redcap)){
  demoDat$eng[f] <- redcap$demo_b_eng_s1_r1_e1[match(demoDat$record_id[f], redcap$record_id)] #participant monolingualism
  demoDat$langhis[f] <- redcap$demo_b_langhis_s1_r1_e1[match(demoDat$record_id[f], redcap$record_id)] #participant language history
  demoDat$ageen[f] <- redcap$demo_b_ageen_s1_r1_e1[match(demoDat$record_id[f], redcap$record_id)] #participant age of English acquisition
  demoDat$profen[f] <- redcap$demo_b_profen_s1_r1_e1[match(demoDat$record_id[f], redcap$record_id)] #participant English proficiency
}

#mood and mood disorders: transfer directly
for(g in 1:nrow(redcap)){
  demoDat$panasPA[g] <- redcap$panasnow_scrdPA[match(demoDat$record_id[g], redcap$record_id)] #panas positive affect
  demoDat$panasNA[g] <- redcap$panasnow_scrdNA[match(demoDat$record_id[g], redcap$record_id)] #panas negative affect
  demoDat$phq8[g] <- redcap$phq8_scrdTotal[match(demoDat$record_id[g], redcap$record_id)] #phq8 depression scale
  demoDat$scaaredTotal[g] <- redcap$scaared_b_scrdTotal[match(demoDat$record_id[g], redcap$record_id)] #scaared total anxiety
}

#age: pull from separate file
for(h in 1:nrow(demoDat)){
  demoDat$age[h] <- agedat$info_age_s1_r1_e1[match(demoDat$record_id[h], agedat$record_id)]
}


### SECTION 3: BUILD TRIAL-LEVEL DF (ADD DEMODAT, READDAT, and ACCDAT to DF)
for(i in 1:nrow(df)){
  subject <- df$id[i] #extract subject number for matching
  passage <- df$passage[i] #extract passage name for matching
  timeFirst <- df$timeFirst[i] #extract time spent reading first passage half for later calculations
  timeSecond <- df$timeSecond[i] #extract time spent reading second passage half for later calculations
  
  #extract passage half characteristics from readDat
  df$switchType[i] <- readDat$switchType[which(readDat$passage==passage)][1] #direction of passage switch
  df$lenSyllFirst[i] <- readDat$lengthSyll[which(readDat$passage==passage & readDat$position=='pre')] #number of syllables in first passage half
  df$lenSyllSecond[i] <- readDat$lengthSyll[which(readDat$passage==passage & readDat$position=='post')] #number of syllables in second passage half
  df$lenWordFirst[i] <- readDat$lengthWord[which(readDat$passage==passage & readDat$position=='pre')] #number of words in first passage half
  df$lenWordSecond[i] <- readDat$lengthWord[which(readDat$passage==passage & readDat$position=='post')] #number of words in second passage half
  df$avgSyllPerWordFirst[i] <- readDat$avgSyllPerWord[which(readDat$passage==passage & readDat$position=='pre')] #averaged syllables per word in first passage half
  df$avgSyllPerWordSecond[i] <- readDat$avgSyllPerWord[which(readDat$passage==passage & readDat$position=='post')] #averaged syllables per word in second passage half
  df$freqFirst[i] <- readDat$avgFreq[which(readDat$passage==passage & readDat$position=='pre')] #averaged lexical frequency for first passage half
  df$freqSecond[i] <- readDat$avgFreq[which(readDat$passage==passage & readDat$position=='post')] #averaged lexical frequency for second passage half
  df$valFirst[i] <- readDat$avgVal[which(readDat$passage==passage & readDat$position=='pre')] #averaged lexical valence for first passage half
  df$valSecond[i] <- readDat$avgVal[which(readDat$passage==passage & readDat$position=='post')] #averaged lexical valence for second passage half
  df$questionHalfFirst[i] <- readDat$questionHalf[which(readDat$passage==passage & readDat$position=='pre')] #whether challenge question drawn from first passage half
  df$questionHalfSecond[i] <- readDat$questionHalf[which(readDat$passage==passage & readDat$position=='post')] #whether challenge question drawn from second passage half
  
  #extract participant accuracy from accDat
  df$challengeACC[i] <- accDat[match(passage, accDat$passage), as.character(subject)] #passage-specific challenge question accuracy for subject
  
  #extract participant demographics from demoDat
  df$sex[i] <- demoDat$sex[match(df$id[i], demoDat$record_id)]   #participant biological sex
  df$pronouns[i] <- demoDat$pron[match(df$id[i], demoDat$record_id)]   #participant preferred pronouns
  df$age[i] <- demoDat$age[match(df$id[i], demoDat$record_id)] #participant age
  df$ethnic[i] <- demoDat$ethnic[match(df$id[i], demoDat$record_id)] #participant ethnic group affiliation
  df$socclass[i] <- demoDat$socclass[match(df$id[i], demoDat$record_id)]   #participant social class identification
  df$eng[i] <- demoDat$eng[match(df$id[i], demoDat$record_id)] #participant multilingualism (0=EN only, 1=EN+another)
  df$langhis[i] <- demoDat$langhis[match(df$id[i], demoDat$record_id)] #participant language learning history (1=EN first, 2=other first, 3=EN+other same, 4=something else)
  df$ageen[i] <- demoDat$ageen[match(df$id[i], demoDat$record_id)] #participant age of English acquisition (if not L1)
  df$profen[i] <- demoDat$profen[match(df$id[i], demoDat$record_id)] #participant English proficiency (1=Native, 2=Advanced, 3=Intermediate, 4=Elementary, 5=Not proficient)
  df$commdis[i] <- demoDat$commdis[match(df$id[i], demoDat$record_id)] #participant communication disorder history (0=none, 1+=diagnoses to review)
  df$panasPA[i] <- demoDat$panasPA[match(df$id[i], demoDat$record_id)] #participant positive affect
  df$panasNA[i] <- demoDat$panasNA[match(df$id[i], demoDat$record_id)] #participant negative affect
  df$phq8[i] <- demoDat$phq8[match(df$id[i], demoDat$record_id)] #participant depression
  df$scaaredTotal[i] <- demoDat$scaaredTotal[match(df$id[i], demoDat$record_id)] #participant overall anxiety
  }

#calculate speed (in both syllables and words per second)
df$speedSyllFirst <- df$lenSyllFirst/df$timeFirst
df$speedSyllSecond <- df$lenSyllSecond/df$timeSecond
df$speedWordFirst <- df$lenWordFirst/df$timeFirst
df$speedWordSecond <- df$lenWordSecond/df$timeSecond

#add delta between first and second half reading speeds
df$speedSyllDelta <- abs(df$speedSyllFirst - df$speedSyllSecond)
df$speedWordDelta <- abs(df$speedWordFirst - df$speedWordSecond)

#organize participant demographic variables
df$sex <- as.factor(df$sex)
df$pronouns <- as.factor(df$pronouns)
df$ethnic <- as.factor(df$ethnic)
df$socclass <- as.factor(df$socclass)


### SECTION 3: TRIM PARTICIPANTS WHO DID NOT MEET INCLUSION CRITERIA
dfTrim <- df
dfTrim <- subset(dfTrim, dfTrim$eng==0 | dfTrim$langhis %in% c(1,3) | dfTrim$ageen<=6) #keep monolingual English OR natively bilingual OR learned English before age 6
#sum(dfTrim$profen>3, na.rm=TRUE)/20 #one remaining subject (150060) rates own English proficiency as not "elementary" or "not proficient", but reads fluidly and achieved 80% accuracy on challenge questions, so not excluded
dfTrim <- subset(dfTrim, dfTrim$commdis==0) #remove if diagnosed with any communication disorder

#extract age and sex stats
summary(dfTrim$age) #age range and mean
sd(dfTrim$age) #age standard deviation
summary(dfTrim$sex)/20 #number of participants by sex
summary(dfTrim$sex)/20 / (nrow(dfTrim)/20) #percentage of participants by sex
summary(dfTrim$pronouns)/20 #number of participants by preferred pronoun
summary(dfTrim$pronouns)/20 / (nrow(dfTrim)/20) #percentage of participants by preferred pronoun
summary(dfTrim$ethnic)/20 #number of participants by ethnic affiliation
summary(dfTrim$ethnic)/20 / (nrow(dfTrim)/20) #percentage of participants by ethnic affiliation
summary(dfTrim$socclass)/20 #number of participants by social class affiliation
summary(dfTrim$socclass)/20 / (nrow(dfTrim)/20) #percentage of participants by social class affiliation


### SECTION 4: TRIM PASSAGES DUE TO EXPERIMENTER ERROR
#remove broccoli passage from all participants due to typo in the last sentence
dfTrim <- subset(dfTrim, !(dfTrim$passage=='broccoli'))


### SECTION 5: OUTPUT DATAFRAME
write.csv(dfTrim, paste(out_path, "readAloudData_", today, ".csv", sep="", collapse=NULL))