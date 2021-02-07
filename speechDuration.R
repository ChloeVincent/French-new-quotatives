
##########
# This getSpeechDurations function returns a dataframe containing, 
# for each speakers in each file of the corpus : 
#   the number of turns taken
#   the total duration of speech 
#   the mean and median duration of each turn
#   the duration of the recording for this file
#   the percentage of speech for this speaker in this recording
#   whether the speaker is the interviewer
#   the percentage of speech of the interviewer in this recording
######

#####
# INPUT
# df.allRec is obtained from the ELAN export: 
# df.allRec <- read.delim(path.CFPPexportFromELAN, quote="") #equivalent to import dataset with option 'Quote' to 'None'
#
# df.allSpeakers is a dataframe containing (speakerCode | label | isInt)
# the list of speakers as they appear in the transcripts
# (this is used to filter the speakers we are interested in)
# 
# the short labels to avoid plotting long names
#
# whether the speaker is the interviewer
######


getSpeechDurations <- function(df.allRec, df.allSpeakers){
  # construct filenameDuration
  filenameDuration = aggregate(list(duration.File = df.allRec$End.Time),by=list(File = df.allRec$File),max)

  # build the new dataframe (one line per speaker per file)
  df.filesDur <- df.allRec %>% pivot_longer(any_of(df.allSpeakers$speakerCode), names_to = "speakerCode", values_to = "turnUtterance")%>%
    filter(turnUtterance != "")%>%
    select(End.Time, Duration, File, speakerCode)
  
  meanDuration<- function(x,y){df.filesDur%>%filter(speakerCode==x, File == y)%>%pull(Duration)%>% mean()}
  totalDuration<-function(x, y){df.filesDur%>%filter(speakerCode==x, File == y)%>%pull(Duration)%>% sum()}
  medDuration<-function(x,y){df.filesDur%>%filter(speakerCode==x, File == y)%>%pull(Duration)%>% median()}
  countTurns<-function(x,y){length(df.filesDur$speakerCode[df.filesDur$speakerCode == x &  df.filesDur$File == y])}
  
  
  df.filesDur <- df.filesDur%>% select(File, speakerCode)%>%unique()%>%
    mutate(durationMean = mapply(meanDuration, speakerCode, File),
           nb.turns = mapply(countTurns,speakerCode, File),
           durationMed = mapply(medDuration, speakerCode, File),
           durationTot = mapply(totalDuration, speakerCode, File))%>%
    merge(filenameDuration)%>%   # add total duration of interview (per file)
    mutate(speechPercentage = durationTot*100 /duration.File) %>%# add percentage of speech per speaker per file
    merge(df.allSpeakers) # add speakers informations
  
  # add percentage of speech of the interviewer
  df.filesDur <- df.filesDur %>% merge(df.filesDur%>%
                                         filter(isInt)%>%
                                         select(File, speechPercentage)%>%
                                         rename(intperc = speechPercentage), all.x=TRUE)
  
  # if there is no interviewer, percentage of speech is 0
  df.filesDur[which(is.na(df.filesDur[,"intperc"])),"intperc"] <- 0

  
  return(df.filesDur)
}
