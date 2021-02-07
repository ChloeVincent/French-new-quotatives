
#common steps
path.plots <- "Desktop/FrenchNewQuotatives/plots/"


loadMPFData <- function(){
  
  # DEFINITION OF FILE NAMES FOR IMPORT ####
  #MPF
  path.coding_sheet_MPF <- "~/Desktop/FrenchNewQuotatives/data/coding sheet MPF-clean.xlsx"
  path.MPFexportFromELAN <-"~/Desktop/MA_Linguistics/DirectedResearch/Rproject/allRecExport.txt"
  
  #FOR EXPORTS
  path.filesDurMPF <- "Desktop/FrenchNewQuotatives/data/filesDurMPF.csv"
  saveplot.MPF <- function(name){
    ggsave(paste(path.plots,name,"_MPF.png", sep=""),width = 4, height = 4)
  }
  
  # Calculate the amount of speech per interviewer per file ####
  #########
  # MPF
  interviewerCode <- c("ENQ", "INT")
  allSpeakers <- append(interviewerCode, paste("SP", 1:93, sep=""))
  df.allSpeakers <- data.frame(speakerCode = allSpeakers, label = allSpeakers)
  df.allSpeakers$isInt <- df.allSpeakers$label %in% interviewerCode
  #The same interviewer is sometimes coded as ENQ, other times as INT
  # note that there are other speakers interrupting the conversations sometimes like teachers or parents, but those intervention are rare and not part of the analysis.
  
  # import data of all the recordings in R
  df.allRec <- read.delim(path.MPFexportFromELAN, quote="") #equivalent to import dataset with option 'Quote' to 'None'
  
  return(list(allRec = df.allRec, allSpeakers = df.allSpeakers))
}

loadCFPPData <- function(){
  
  # DEFINITION OF FILE NAMES FOR IMPORT ####
  #CFPP
  path.allCFPPspeakersInfo <- "~/Desktop/FrenchNewQuotatives/data/allSpeakers.csv"
  path.CFPPexportFromELAN <- "~/Desktop/MA_Linguistics/DirectedResearch/DataCFPP/ELANexport/allCFPPexport.txt"
  path.interviewers_CFPP <- "~/Desktop/FrenchNewQuotatives/data/interviewers.csv"
  
  #FOR EXPORTS
  path.filesDurCFPP <- "Desktop/FrenchNewQuotatives/data/filesDurCFPP.csv"
  saveplot.CFPP <- function(name){
    ggsave(paste(path.plots,name,"_CFPP.png", sep=""), width = 4, height = 4)
  }
  
  # Calculate the amount of speech per interviewer per file ####
  ##########
  # CFPP
  cfppSpeakerInfo <- read.csv(path.allCFPPspeakersInfo)
  cfppSpeakers <- cfppSpeakerInfo %>% select(speakerCode, label)%>% mutate(isInt =FALSE, 
                                                                           label = as.character(label))
  interviewers <- read_csv(path.interviewers_CFPP) %>% select(speakerCode, label)%>%mutate(isInt = TRUE)
  df.allSpeakers <- bind_rows(cfppSpeakers,interviewers)
  
  # import data of all the recordings in R
  df.allRec <- read.delim(path.CFPPexportFromELAN, quote="") #equivalent to import dataset with option 'Quote' to 'None'
  
  # to plot only the files that we coded for:
  listOfCodedFiles <- cfppSpeakerInfo%>%
                          filter(!is.na(nb.quotatives))%>%
                          select(File)%>%unique
  df.allRec <- subset(df.allRec, File %in% listOfCodedFiles$File)
  
  return(list(allRec = df.allRec, allSpeakers = df.allSpeakers, speakerInfo = cfppSpeakerInfo))
  
}
