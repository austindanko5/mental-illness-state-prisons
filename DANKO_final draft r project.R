setwd("~/Documents/Penn Masters/CRIM 602")

#install.packages("sunburstR")
#install.packages("plotly")
library(sunburstR)
library(tidyverse)

load("~/Documents/Penn Masters/CRIM 602/bjs state prison data 2016/DS0002/37692-0002-Data.rda")

#examine
da37692.0002[1:3,]

var.lookup <- attributes(da37692.0002)$variable.labels
var.lookup[1:3]
grep("suppressed", var.lookup, value=TRUE)
#many variables suppressed - not included/restricted

#find variables interested in
grep("mental", var.lookup, value=TRUE)
#find types of disorders - grep(disorder) to get more narrow results
grep("disorder", var.lookup, value=TRUE)
#upcoded vs. original?
#looks like anything i would be interested in is coded MH__ - examine grep(MH)
grep("MH", var.lookup, value=TRUE)
#V1178 starts mental health variables (flag column)


#VARIABLES
#signs of mental illness
#V1205 - currently receiving pro help
#V1204 - received pro help since being admitted to prison
#V1203 - currently taking meds for mental health
#V1202 - taken meds for mental health since being admitted to prison
#V1201 - taking meds for MH at time of arrest
#V1200 - hosp due to MH 12 months before arrest (yes/no)
#V1199 - hosp due to ment/soc/emo health (not including overnight stays for drug/alc use)

#TYPES (original)
#V1198 - other mental illness
#V1197 - diagnosed personality disorder
#V1196 - diagnosed anxiety disorder
#V115 - diagnosed ptsd
#V1194 - diagnosed schizo or other psychotic 
#V1193 - diagnosed depressive
#V1192 - diagnosed manic depression, mania, bipolar
####V1185-1191 - same but upcoded ?????

#see example of responses and variable type
attributes(da37692.0002$V1192)

################################################################
####DISTRIBUTION OF TYPES OF MENTAL ILLNESS IN STATE PRISONS####
################################################################
#types of mental illness 
diagnoses <- data.frame(MANICDEP_MANIA_BIPO = da37692.0002$V1192,
                        DEPRESSIVE = da37692.0002$V1193,
                        SCHIZ_PSYCHOTIC = da37692.0002$V1194,
                        PTSD = da37692.0002$V1195,
                        ANXIETY = da37692.0002$V1196,
                        PERSONALITY_DIS = da37692.0002$V1197,
                        OTHER_MI = da37692.0002$V1198,
                        stringsAsFactors=FALSE)
diagnoses[1:3,]

#clean - from file attached in data/codebook/etc zip
#label values table in case i need it later
lbls <- sort(levels(diagnoses$MANICDEP_MANIA_BIPO))
lbls
#gsub to convert to just numbers
diagnoses$MANICDEP_MANIA_BIPO <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1", 
                                                 diagnoses$MANICDEP_MANIA_BIPO))
diagnoses$DEPRESSIVE <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1", 
                                       diagnoses$DEPRESSIVE))
diagnoses$SCHIZ_PSYCHOTIC <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1", 
                                            diagnoses$SCHIZ_PSYCHOTIC))
diagnoses$PTSD <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1", diagnoses$PTSD))
diagnoses$ANXIETY <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1",
                                    diagnoses$ANXIETY))
diagnoses$PERSONALITY_DIS <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1", 
                                            diagnoses$PERSONALITY_DIS))
diagnoses$OTHER_MI <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1", 
                                    diagnoses$OTHER_MI))
head(diagnoses)

#change 2 (no) to 0 (no) to make analysis easier
diagnoses$MANICDEP_MANIA_BIPO <- as.numeric(gsub("2", "0", diagnoses$MANICDEP_MANIA_BIPO))
diagnoses$DEPRESSIVE <- as.numeric(gsub("2", "0", diagnoses$DEPRESSIVE))
diagnoses$SCHIZ_PSYCHOTIC <- as.numeric(gsub("2", "0", diagnoses$SCHIZ_PSYCHOTIC))
diagnoses$PTSD <- as.numeric(gsub("2", "0", diagnoses$PTSD))
diagnoses$ANXIETY <- as.numeric(gsub("2", "0", diagnoses$ANXIETY))
diagnoses$PERSONALITY_DIS <- as.numeric(gsub("2", "0", diagnoses$PERSONALITY_DIS))
diagnoses$OTHER_MI <- as.numeric(gsub("2", "0", diagnoses$OTHER_MI))
head(diagnoses)

diagnoses <- diagnoses %>%
  mutate(MItype = NA)

diagnoses$MItype <- ifelse(diagnoses$MANICDEP_MANIA_BIPO == 1, 
                           'Manic or Bipolar Disorder', diagnoses$MItype)
diagnoses$MItype <- ifelse(diagnoses$DEPRESSIVE == 1, 
                           'Depressive Disorder', diagnoses$MItype)
diagnoses$MItype <- ifelse(diagnoses$SCHIZ_PSYCHOTIC == 1, 
                           'Schizophrenic or Other Psychotic Disorder', diagnoses$MItype)
diagnoses$MItype <- ifelse(diagnoses$PTSD == 1, 'PTSD', diagnoses$MItype)
diagnoses$MItype <- ifelse(diagnoses$ANXIETY == 1, 
                           'Anxiety Disorder', diagnoses$MItype)
diagnoses$MItype <- ifelse(diagnoses$PERSONALITY_DIS == 1, 
                           'Personality Disorder', diagnoses$MItype)
diagnoses$MItype <- ifelse(diagnoses$OTHER_MI == 1, 
                           'Other Mental Illness', diagnoses$MItype)
head(diagnoses)
#create "none" and "combo" columns for people who have never been diagnosed or with comorbidities 
diagnoses$MItype <- ifelse(rowSums(diagnoses[,1:7])==0, 
                           'No Diagnosis', diagnoses$MItype)
diagnoses$MItype <- ifelse(rowSums(diagnoses[,1:7])>1, 
                           'Combo of Diagnoses', diagnoses$MItype)
#dist of MItypes
table(diagnoses$MItype)

#create ever-diagnosed column
diagnoses <- diagnoses %>%
  mutate(everdiagnosed = NA)

diagnoses$everdiagnosed <- ifelse(diagnoses$MItype == 'No Diagnosis', 
                                  'Never Diagnosed', diagnoses$everdiagnosed)
diagnoses$everdiagnosed <- ifelse(diagnoses$MItype != 'No Diagnosis', 
                                  'Diagnosed Mental Illness', diagnoses$everdiagnosed)
#dist of diagnosed mental illness among state prisoners
table(diagnoses$everdiagnosed)


#Do same for signs of MI
################################################################
####PREVALENCE OF MENTAL ILLNESS IN STATE PRISONS####
################################################################
#signs of having mental illness - copy paste from creation of mentill frame
signsofMI <- data.frame(EVER_HOSP = da37692.0002$V1199,
                    HOSP_LAST_12 = da37692.0002$V1200,
                    MEDS_ARST = da37692.0002$V1201,
                    MEDS_PRISON = da37692.0002$V1202,
                    MEDS_CURRENT = da37692.0002$V1203,
                    PROF_HELP_PRIS = da37692.0002$V1204,
                    PROF_HELP_CURRENT = da37692.0002$V1205,
                    stringsAsFactors=FALSE)
signsofMI[1:3,]

#clean - from file attached in data/codebook/etc zip
#gsub to convert to just numbers
signsofMI$EVER_HOSP <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1", 
                                       signsofMI$EVER_HOSP))
signsofMI$HOSP_LAST_12 <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1",
                                          signsofMI$HOSP_LAST_12))
signsofMI$MEDS_ARST <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1",
                                       signsofMI$MEDS_ARST))
signsofMI$MEDS_PRISON <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1",
                                         signsofMI$MEDS_PRISON))
signsofMI$MEDS_CURRENT <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1", 
                                          signsofMI$MEDS_CURRENT))
signsofMI$PROF_HELP_PRIS <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1", 
                                            signsofMI$PROF_HELP_PRIS))
signsofMI$PROF_HELP_CURRENT <- as.numeric(gsub("^\\(([0-9])\\).*$", "\\1", 
                                     signsofMI$PROF_HELP_CURRENT))
head(signsofMI)

#change 2 (no) to 0 (no) to make analysis easier
signsofMI$EVER_HOSP <- as.numeric(gsub("2", "0", signsofMI$EVER_HOSP))
signsofMI$HOSP_LAST_12 <- as.numeric(gsub("2", "0", signsofMI$HOSP_LAST_12))
signsofMI$MEDS_ARST <- as.numeric(gsub("2", "0", signsofMI$MEDS_ARST))
signsofMI$MEDS_PRISON <- as.numeric(gsub("2", "0", signsofMI$MEDS_PRISON))
signsofMI$MEDS_CURRENT <- as.numeric(gsub("2", "0", signsofMI$MEDS_CURRENT))
signsofMI$PROF_HELP_PRIS <- as.numeric(gsub("2", "0", signsofMI$PROF_HELP_PRIS))
signsofMI$PROF_HELP_CURRENT <- as.numeric(gsub("2", "0", signsofMI$PROF_HELP_CURRENT))
head(signsofMI)

signsofMI$EVER_HOSP <- gsub("0", "Never Been Hospitalized for MI", signsofMI$EVER_HOSP)
signsofMI$EVER_HOSP <- gsub("1", "Has Been Hospitalized for MI", signsofMI$EVER_HOSP)
table(signsofMI$EVER_HOSP)

#ever taken meds for MI column
signsofMI <- signsofMI %>%
  mutate(MEDS_EVER = NA)

signsofMI$MEDS_EVER <- ifelse(c(signsofMI$MEDS_ARST == 1 | 
                                             signsofMI$MEDS_PRISON == 1 | 
                                             signsofMI$MEDS_PRISON == 1), 
                                         'Has Taken Meds for MI', signsofMI$MEDS_EVER)
signsofMI$MEDS_EVER <- ifelse(c(signsofMI$MEDS_ARST == 0 & 
                                             signsofMI$MEDS_PRISON == 0 & 
                                             signsofMI$MEDS_PRISON == 0), 
                                         'Never Taken Meds for MI', signsofMI$MEDS_EVER)
table(signsofMI$MEDS_EVER)

#have they received help/meds for their mi in prison?
signsofMI <- signsofMI %>%
  mutate(TREATMENT_PRISON = NA)

signsofMI$TREATMENT_PRISON <- ifelse(c(signsofMI$MEDS_PRISON == 1 | 
                                         signsofMI$PROF_HELP_PRIS == 1), 
                              'Taken Meds or Received Professional Help in Prison', 
                              signsofMI$TREATMENT_PRISON)
signsofMI$TREATMENT_PRISON <- ifelse(c(signsofMI$MEDS_PRISON == 0 & 
                                  signsofMI$PROF_HELP_PRIS == 0), 
                              'Have Not Taken Meds or Received Professional Help in Prison', 
                              signsofMI$TREATMENT_PRISON)
table(signsofMI$TREATMENT_PRISON)

#########################################################
####COMBINE DF WITH TYPES AND SIGNS OF MENTAL ILLNESS####
#########################################################
mentill <- cbind(diagnoses, signsofMI)
head(mentill)

#NOW SUNBURST CHART
# for graphic purposes - inner circle 100% to make graph more readable
mentill <- mentill %>%
  mutate(prison = NA)
mentill$prison <- "State Prison"

#looking at whether people have MH issues and the dist of types of mental illnesses in prisons 
mentillsun <- mentill %>%
  select(prison, everdiagnosed, MItype, MEDS_EVER, EVER_HOSP) %>%
  # remove dash within dplyr pipe
  mutate(
    path = paste(prison, everdiagnosed, MItype, MEDS_EVER, EVER_HOSP, 
                 sep = "-")) %>%
  mutate(
    V2 = 1
  )
head(mentillsun)
sunburst(data = data.frame(xtabs(V2~path, mentillsun)), 
         legend = TRUE, 
         percent=TRUE,
         count=TRUE,
         colors = c("salmon", "lavender", "lemonchiffon", "skyblue", "purple"))

#Have people with MI received help while in prison?
#fix values I am adding
mentill$MEDS_PRISON <- gsub("0", "Has Not Taken Meds for MI in Prison", mentill$MEDS_PRISON)
mentill$MEDS_PRISON <- gsub("1", "Has Taken Meds for MI in Prison", mentill$MEDS_PRISON)

mentill$PROF_HELP_PRIS <- gsub("0", "Has Not Received Professional Help for MI in Prison", 
                               mentill$PROF_HELP_PRIS)
mentill$PROF_HELP_PRIS <- gsub("1", "Has Received Professional Help for MI in Prison", 
                               mentill$PROF_HELP_PRIS)

#new sunburst plot
mentillsun2 <- mentill %>%
  select(everdiagnosed, MItype, TREATMENT_PRISON, MEDS_PRISON, PROF_HELP_PRIS) %>%
  # remove dash within dplyr pipe
  filter(mentill$everdiagnosed=='Diagnosed Mental Illness') %>%
  mutate(
    path = paste(everdiagnosed, MItype, TREATMENT_PRISON, MEDS_PRISON, PROF_HELP_PRIS,
                 sep = "-")) %>%
  mutate(
    V2 = 1
  )

#widen frame so path is not covered
sunburst(data = data.frame(xtabs(V2~path, mentillsun2)), 
         legend = TRUE, 
         percent=TRUE,
         count=TRUE,
         colors = c("salmon", "wheat", "violet", "turquoise", "thistle"))



