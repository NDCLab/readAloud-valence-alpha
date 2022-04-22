# Preliminary Analyses: readAloud-valence-alpha
# Author: Jessica M. Alexander
# Last Updated: 2022-04-22

### SECTION 1: SETTING UP
library(readxl)

#set up date for output file naming
today <- Sys.Date()
today <- format(today, "%Y%m%d")

#set up directories for input/output data
#hpc
#passagechar_file <- '/home/data/NDClab/datasets/readAloud-valence-dataset/materials/readAloud-ldt/stimuli/readAloud/readAloud-stimuli_characteristics.xlsx'
#redcap_file <- '/home/data/NDClab/datasets/readAloud-valence-dataset/sourcedata/checked/redcap/202201v0readAloudval_DATA_2022-04-20_0854.csv'
#challengeAcc_file <- '/home/data/NDClab/datasets/readAloud-valence-dataset/derivatives/preprocessed/readAloud_subject-level_summary_20220422.csv'
#timing_file <- '/home/data/NDClab/datasets/readAloud-valence-dataset/derivatives/preprocessed/timing_subject-by-passage_20220422.csv'
#pitch_file <- '/home/data/NDClab/datasets/readAloud-valence-dataset/derivatives/preprocessed/pitch_subject-by-passage_20220422.csv''
#disfluencies_file <- '/home/data/NDClab/datasets/readAloud-valence-dataset/derivatives/preprocessed/disfluencies_subject-by-passage_20220422.csv'
#out_path <- '/home/data/NDClab/datasets/readAloud-valence-alpha/derivatives/'
#local
passagechar_file <- '/Users/jalexand/github/readAloud-valence-dataset/materials/readAloud-ldt/stimuli/readAloud/readAloud-stimuli_characteristics.xlsx'
redcap_file <- '/Users/jalexand/github/readAloud-valence-dataset/sourcedata/checked/redcap/202201v0readAloudval_DATA_2022-04-20_0854.csv'
challengeAcc_file <- '/Users/jalexand/github/readAloud-valence-dataset/derivatives/preprocessed/readAloud_subject-level_summary_20220422.csv'
timing_file <- '/Users/jalexand/github/readAloud-valence-dataset/derivatives/preprocessed/timing_subject-by-passage_20220422.csv'
pitch_file <- '/Users/jalexand/github/readAloud-valence-dataset/derivatives/preprocessed/pitch_subject-by-passage_20220422.csv'
disfluencies_file <- '/Users/jalexand/github/readAloud-valence-dataset/derivatives/preprocessed/disfluencies_subject-by-passage_20220422.csv'
out_path <- '/Users/jalexand/github/readAloud-valence-alpha/derivatives/'

#load data files
passageDat <- read_xlsx(passagechar_file, sheet="passages")
redcapDat <- read.csv(redcap_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))
challengeDat <- read.csv(challengeAcc_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))
timingDat <- read.csv(timing_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))
pitchDat <- read.csv(pitch_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))
disfluenciesDat <- read.csv(disfluencies_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))

#create dataframes for storing output data and define output file name
prelimPassageSummaryDat <- data.frame(matrix(ncol=31, nrow=0))
colnames(prelimPassageSummaryDat) <- c("id",
                                       "demo_b_sex",
                                       "demo_b_eng",
                                       "demo_b_langhis",
                                       "demo_b_ageen",
                                       "demo_b_diagkidnow",
                                       "demo_b_diateennow",
                                       "demo_b_diagadnow",
                                       "bmis_scrdVal",
                                       "subMood",
                                       "phq8_scrdTotal",
                                       "challengeAcc",
                                       "passage",
                                       "PosOrNeg",
                                       "liwcScore",
                                       "warrinerAvg",
                                       "syllPerSec_firstHalf",
                                       "syllPerSec_prePREswitch",
                                       "syllPerSec_preswitch",
                                       "syllPerSec_switch",
                                       "syllPerSec_postswitch",
                                       "vocalPitch_firstHalf",
                                       "vocalPitch_prePREswitch",
                                       "vocalPitch_preswitch",
                                       "vocalPitch_switch",
                                       "vocalPitch_postswitch",
                                       "percDisfluent_firstHalf",
                                       "percDisfluent_prePREswitch",
                                       "percDisfluent_preswitch",
                                       "percDisfluent_switch",
                                       "percDisfluent_postswitch")
prelimPassage_out <- paste("postprocess_subject-by-passage_", today, ".csv", sep="", collapse=NULL)

prelimSubjectSummaryDat <- data.frame(matrix(ncol=30, nrow=0))
colnames(prelimSubjectSummaryDat) <- colnames(prelimPassageSummaryDat)[-13] #drop only 'passage' as collapsing across passages
prelimSubject_out <- paste("postprocess_subject-by-valence_", today, ".csv", sep="", collapse=NULL)

### SECTION 2: SELECT PARTICIPANTS TO INCLUDE IN OUTPUT FILE
#manual selection for inclusion for debugging/prelim analyses
subs <- c(150004, 150005)
#automatic exclusion--currently in debugging
#subs <- c()
#for (a in 1:nrow(challengeDat)){
#  id <- challengeDat$id[a]
#  redcap_row <- match(id, redcapDat$record_id)
#  bmis_timestamp <- as.numeric(as.POSIXct(redcapDat$bmis_s1_r1_e1_timestamp[redcap_row]))
#  readaloud_timestamp <- as.numeric(as.POSIXct(redcapDat$readaloudvalo_s1_r1_e1_timestamp[redcap_row]))
#  taskdelay <- (readaloud_timestamp - bmis_timestamp)/60 #gives number of minutes btwn completing bmis questionnaire and completing study tasks
#  
#  langhis <- redcapDat$demo_b_langhis_s1_r1_e1[redcap_row]
#  diagkidnow <- redcapDat$demo_b_diagkidnow_s1_r1_e1[redcap_row]
#  diateennow <- redcapDat$demo_b_diateennow_s1_r1_e1[redcap_row]
#  diagadnow <- redcapDat$demo_b_diagadnow_s1_r1_e1[redcap_row]
#  phq8score <- redcapDat$phq8_scrdTotal_s1_r1_e1[redcap_row]
#  
#  challengeacc <- challengeDat$challengeAccuracy[match(id, challengeDat$id)]
#  
#  if(taskdelay<180 &
#  (langhis==1 | langhis==3) &
#  is.na(diagkidnow) & is.na(diateennow) & is.na(diagadnow) &
#  phq8score<10 &
#  challengeacc>0.7){
#    subs <- c(subs, id)
#  } else {
#    next
#  }
#}



### SECTION 3: START PARTICIPANT LOOP AND READ IN PARTICIPANT DATA
for(i in 1:length(subs)){
  id <- subs[i]
  redcap_row <- match(id, redcapDat$record_id)
  
  demo_b_sex <- redcapDat$demo_b_sex_s1_r1_e1[redcap_row]
  demo_b_eng <- redcapDat$demo_b_eng_s1_r1_e1[redcap_row]
  demo_b_langhis <- redcapDat$demo_b_langhis_s1_r1_e1[redcap_row]
  demo_b_ageen <- redcapDat$demo_b_ageen_s1_r1_e1[redcap_row]
  demo_b_diagkidnow <- redcapDat$demo_b_diagkidnow_s1_r1_e1[redcap_row]
  demo_b_diateennow <- redcapDat$demo_b_diateennow_s1_r1_e1[redcap_row]
  demo_b_diagadnow <- redcapDat$demo_b_diagadnow_s1_r1_e1[redcap_row]
  
  bmis_scrdVal <- redcapDat$bmis_scrdVal_s1_r1_e1[redcap_row]
  if (bmis_scrdVal<=40){subMood <- "negMood"} else {subMood <- "posMood"}
  
  phq8_scrdTotal <- redcapDat$phq8_scrdTotal_s1_r1_e1[redcap_row]
  
  challengeAcc <- challengeDat[match(id, challengeDat$id),2]
  
  ### SECTION 4: TRIM PREPROCESSED FILES DOWN TO SUBJECT ID
  timingDatTrim <- timingDat[timingDat$id==id,]
  pitchDatTrim <- pitchDat[timingDat$id==id,]
  disfluenciesDatTrim <- disfluenciesDat[timingDat$id==id,]
  
  ### SECTION 5: START PASSAGE LOOP AND READ IN PASSAGE CHARACTERISTICS AND BEHAVIORAL DATA
  for (j in 1:nrow(timingDatTrim)){
    passage <- timingDatTrim$passage[j]
    passagechars_row <- match(passage, passageDat$passage)
    PosOrNeg <- passageDat$switchType[passagechars_row]
    if (PosOrNeg == "pos2neg"){
      liwcScore <- passageDat$emoTonePOS[passagechars_row]
      warrinerAvg <- passageDat$posAvgWAR[passagechars_row]
    } else {
      liwcScore <- passageDat$emoToneNEG[passagechars_row]
      warrinerAvg <- passageDat$negAvgWAR[passagechars_row]
    }
    
    passage_timing_row <- match(passage, timingDatTrim$passage)
    syllPerSec_firstHalf <- timingDatTrim$syllPerSec_firstHalf[passage_timing_row]
    syllPerSec_prePREswitch <- timingDatTrim$syllPerSec_prePREswitch[passage_timing_row]
    syllPerSec_preswitch <- timingDatTrim$syllPerSec_preswitch[passage_timing_row]
    syllPerSec_switch <- timingDatTrim$syllPerSec_switch[passage_timing_row]
    syllPerSec_postswitch <- timingDatTrim$syllPerSec_postswitch[passage_timing_row]
    
    passage_pitch_row <- match(passage, pitchDatTrim$passage)
    vocalPitch_firstHalf <- pitchDatTrim$vocalPitch_firstHalf[passage_pitch_row]
    vocalPitch_prePREswitch <- pitchDatTrim$vocalPitch_prePREswitch[passage_pitch_row]
    vocalPitch_preswitch <- pitchDatTrim$vocalPitch_preswitch[passage_pitch_row]
    vocalPitch_switch <- pitchDatTrim$vocalPitch_switch[passage_pitch_row]
    vocalPitch_postswitch <- pitchDatTrim$vocalPitch_postswitch[passage_pitch_row]
    
    passage_disfluences_row <- match(passage, disfluenciesDatTrim$passage)
    percDisfluent_firstHalf <- disfluenciesDatTrim$percDisfluent_firstHalf[passage_disfluences_row]
    percDisfluent_prePREswitch <- disfluenciesDatTrim$percDisfluent_prePREswitch[passage_disfluences_row]
    percDisfluent_preswitch <- disfluenciesDatTrim$percDisfluent_preswitch[passage_disfluences_row]
    percDisfluent_switch <- disfluenciesDatTrim$percDisfluent_switch[passage_disfluences_row]
    percDisfluent_postswitch <- disfluenciesDatTrim$percDisfluent_postswitch[passage_disfluences_row]
    
    ### SECTION 6: STORE OUTPUT DATA IN SUMMARY MATRIX
    prelimPassageSummaryDat[nrow(prelimPassageSummaryDat) + 1,] <-c(id,
                                                                    demo_b_sex,
                                                                    demo_b_eng,
                                                                    demo_b_langhis,
                                                                    demo_b_ageen,
                                                                    demo_b_diagkidnow,
                                                                    demo_b_diateennow,
                                                                    demo_b_diagadnow,
                                                                    bmis_scrdVal,
                                                                    subMood,
                                                                    phq8_scrdTotal,
                                                                    challengeAcc,
                                                                    passage,
                                                                    PosOrNeg,
                                                                    liwcScore,
                                                                    warrinerAvg,
                                                                    syllPerSec_firstHalf,
                                                                    syllPerSec_prePREswitch,
                                                                    syllPerSec_preswitch,
                                                                    syllPerSec_switch,
                                                                    syllPerSec_postswitch,
                                                                    vocalPitch_firstHalf,
                                                                    vocalPitch_prePREswitch,
                                                                    vocalPitch_preswitch,
                                                                    vocalPitch_switch,
                                                                    vocalPitch_postswitch,
                                                                    percDisfluent_firstHalf,
                                                                    percDisfluent_prePREswitch,
                                                                    percDisfluent_preswitch,
                                                                    percDisfluent_switch,
                                                                    percDisfluent_postswitch)
  }
}


### SECTION 7: LOOP BACK OVER SUBJECTS TO COLLAPSE prelimPassageSummaryDat ACROSS PASSAGES
for(k in 1:length(subs)){
  subjectDat <- prelimPassageSummaryDat[prelimPassageSummaryDat$id==subs[k],]
  
  #loop over switch types to extract means for each
  switchTypes <- c("pos2neg", "neg2pos")
  for(n in 1:length(switchTypes)){
    subjectDatVal <- subjectDat[subjectDat$PosOrNeg==switchTypes[n],]
    
    id <- subs[k]
    demo_b_sex <- subjectDatVal$demo_b_sex[1]
    demo_b_eng <- subjectDatVal$demo_b_eng[1]
    demo_b_langhis <- subjectDatVal$demo_b_langhis[1]
    demo_b_ageen <- subjectDatVal$demo_b_ageen[1]
    demo_b_diagkidnow <- subjectDatVal$demo_b_diagkidnow[1]
    demo_b_diateennow <- subjectDatVal$demo_b_diateennow[1]
    demo_b_diagadnow <- subjectDatVal$demo_b_diagadnow[1]
    bmis_scrdVal <- subjectDatVal$bmis_scrdVal[1]
    subMood <- subjectDatVal$subMood[1]
    phq8_scrdTotal <- subjectDatVal$phq8_scrdTotal[1]
    challengeAcc <- subjectDatVal$challengeAcc[1]
    PosOrNeg <- switchTypes[n]
    liwcScore <- mean(as.numeric(subjectDatVal$liwcScore))
    warrinerAvg <- mean(as.numeric(subjectDatVal$warrinerAvg))
    syllPerSec_firstHalf <- mean(as.numeric(subjectDatVal$syllPerSec_firstHalf))
    syllPerSec_prePREswitch <- mean(as.numeric(subjectDatVal$syllPerSec_prePREswitch))
    syllPerSec_preswitch <- mean(as.numeric(subjectDatVal$syllPerSec_preswitch))
    syllPerSec_switch <- mean(as.numeric(subjectDatVal$syllPerSec_switch))
    syllPerSec_postswitch <- mean(as.numeric(subjectDatVal$syllPerSec_postswitch))
    vocalPitch_firstHalf <- mean(as.numeric(subjectDatVal$vocalPitch_firstHalf))
    vocalPitch_prePREswitch <- mean(as.numeric(subjectDatVal$vocalPitch_prePREswitch))
    vocalPitch_preswitch <- mean(as.numeric(subjectDatVal$vocalPitch_preswitch))
    vocalPitch_switch <- mean(as.numeric(subjectDatVal$vocalPitch_switch))
    vocalPitch_postswitch <- mean(as.numeric(subjectDatVal$vocalPitch_postswitch))
    percDisfluent_firstHalf <- mean(as.numeric(subjectDatVal$percDisfluent_firstHalf))
    percDisfluent_prePREswitch <- mean(as.numeric(subjectDatVal$percDisfluent_prePREswitch))
    percDisfluent_preswitch <- mean(as.numeric(subjectDatVal$percDisfluent_preswitch))
    percDisfluent_switch <- mean(as.numeric(subjectDatVal$percDisfluent_switch))
    percDisfluent_postswitch <- mean(as.numeric(subjectDatVal$percDisfluent_postswitch))
    
    #store output data in summary matrix
    prelimSubjectSummaryDat[nrow(prelimSubjectSummaryDat) + 1,] <-c(id,
                                                                    demo_b_sex,
                                                                    demo_b_eng,
                                                                    demo_b_langhis,
                                                                    demo_b_ageen,
                                                                    demo_b_diagkidnow,
                                                                    demo_b_diateennow,
                                                                    demo_b_diagadnow,
                                                                    bmis_scrdVal,
                                                                    subMood,
                                                                    phq8_scrdTotal,
                                                                    challengeAcc,
                                                                    PosOrNeg,
                                                                    liwcScore,
                                                                    warrinerAvg,
                                                                    syllPerSec_firstHalf,
                                                                    syllPerSec_prePREswitch,
                                                                    syllPerSec_preswitch,
                                                                    syllPerSec_switch,
                                                                    syllPerSec_postswitch,
                                                                    vocalPitch_firstHalf,
                                                                    vocalPitch_prePREswitch,
                                                                    vocalPitch_preswitch,
                                                                    vocalPitch_switch,
                                                                    vocalPitch_postswitch,
                                                                    percDisfluent_firstHalf,
                                                                    percDisfluent_prePREswitch,
                                                                    percDisfluent_preswitch,
                                                                    percDisfluent_switch,
                                                                    percDisfluent_postswitch)
    
  }
}

### SECTION 8: OUTPUT DATA
#write the extracted summary scores to CSV
write.csv(prelimPassageSummaryDat, paste(out_path, prelimPassage_out, sep = "", collapse = NULL), row.names=FALSE)
write.csv(prelimSubjectSummaryDat, paste(out_path, prelimSubject_out, sep = "", collapse = NULL), row.names=FALSE)