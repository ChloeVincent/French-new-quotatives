library(tidyverse)

# Input files: ELAN export (see directed research report) <transcription> -> ELAN -> tab-delimited files
# need a lit of speakers, with labels (for plotting) and whether they are interviewers (see speechDuration script)

source(file= "~/Desktop/FrenchNewQuotatives/loadData.R")
source(file= "~/Desktop/FrenchNewQuotatives/speechDuration.R")

# MPF Analysis ####
#mpfData <- loadMPFData()
#df.filesDurMPF <- getSpeechDurations(mpfData$allRec, mpfData$allSpeakers)
####

#CFPP Analysis####
cfppData <- loadCFPPData()
df.filesDurCFPP <- getSpeechDurations(cfppData$allRec, cfppData$allSpeakers)

# if needed, go back to MA Directed Research MPFCFPPAnalysis.R for all plots and stats done.

df.filesDurCFPP %>% ggplot (aes (x= durationTot, y=speakerCode))+geom_bar(stat="identity")+geom_vline(aes(xintercept = 300))

df <- df.filesDurCFPP%>%merge(cfppData$speakerInfo, by=c("File", "speakerCode", "label"))%>%
  mutate(QPH = nb.quotatives * 3600 / durationTot)%>%
  filter(durationTot > 300)


df %>% ggplot(aes(x=Age, y=QPH, color=LocationGroup))+geom_point()+geom_smooth(se=FALSE, method = "lm")+geom_text(label=df$speakerCode)

## add more points !

