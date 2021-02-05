
# DEFINITION OF FILE NAMES FOR IMPORT ####
#MPF
path.coding_sheet_MPF <- "Desktop/MA_Linguistics/DirectedResearch/Rproject/coding sheet MPF-clean.xlsx"
path.MPFexportFromELAN <-"~/Desktop/MA_Linguistics/DirectedResearch/Rproject/allRecExport.txt"
#CFPP
path.allCFPPspeakersInfo <- "Desktop/MA_Linguistics/DirectedResearch/DataCFPP/allSpeakers.csv"
path.CFPPexportFromELAN <- "~/Desktop/MA_Linguistics/DirectedResearch/DataCFPP/ELANexport/allCFPPexport.txt"
path.cfppSpeakers <- "~/Desktop/MA_Linguistics/DirectedResearch/DataCFPP/ELANexport/speakerNames.csv"
path.coding_sheet_CFPP <- "Desktop/MA_Linguistics/DirectedResearch/Rproject/coding_sheet_CFPP.csv"

#FOR EXPORTS
path.filesDurMPF <- "Desktop/MA_Linguistics/DirectedResearch/Rproject/filesDurMPF.csv"
path.filesDurCFPP <- "Desktop/MA_Linguistics/DirectedResearch/Rproject/filesDurCFPP.csv"
path.plots <- "Desktop/MA_Linguistics/DirectedResearch/Rproject/plots/"
saveplot.MPF <- function(name){
  ggsave(paste(path.plots,name,"_MPF.png", sep=""),width = 4, height = 4)
  }
saveplot.CFPP <- function(name){
  ggsave(paste(path.plots,name,"_CFPP.png", sep=""), width = 4, height = 4)
}

# Calculate the amount of speech per interviewer per file ####


# MPF
exportFromELANpath <-path.MPFexportFromELAN
interviewerCode <- c("ENQ", "INT")
allSpeakers <- append(interviewerCode, paste("SP", 1:93, sep=""))
speakerLabels <- allSpeakers
#The same interviewer is sometimes coded as ENQ, other times as INT
# note that there are other speakers interrupting the conversations sometimes like teachers or parents, but those intervention are rare and not part of the analysis.

# import data of all the recordings in R
df.allRec <- read.delim(exportFromELANpathMPF, quote="") #equivalent to import dataset with option 'Quote' to 'None'

# CFPP
cfppSpeakers <- read.csv(path.cfppSpeakers)
allSpeakers <- cfppSpeakers$speaker
interviewerCode <- cfppSpeakers$speaker[cfppSpeakers$interviewer != "no"]
speakerLabels <- cfppSpeakers$label
###
# import data of all the recordings in R
df.allRec <- read.delim(path.CFPPexportFromELAN, quote="") #equivalent to import dataset with option 'Quote' to 'None'

# to plot only the files that we coded for:
coding_sheet_CFPP <- read.csv(path.coding_sheet_CFPP)
coding_sheet_CFPP$fileName <- paste(coding_sheet_CFPP$fileName, ".eaf", sep = "")
df.allRec <- subset(df.allRec, File %in% unique(coding_sheet_CFPP$fileName))


####
# other speakers were also eliminated from the analysis (see process section)



# construct filenameDuration
filenameDuration = aggregate(list(Duration = df.allRec$End.Time),by=list(File = df.allRec$File),max)

#### create file dataframe ###
# note: There might be a more elegant way to do this in R. I am still learning...

# create lists for later calculations
fileName <- c()
speakerCode <-c()
nb.turns<-c()
durationMean<-c()
durationMed<-c()
durationTot<-c()

#for each file in the export from ELAN, calculate the duration of speech for each speaker
for (file in filenameDuration$File) {
  for(name in allSpeakers){
    if(name %in% colnames(df.allRec)){
      # each speaker has their own column, we count only the lines when they speak
      speakerSub <- subset(df.allRec, df.allRec[name] != "" & df.allRec["File"] == file)
      # consider only the speakers that are actually saying something in this file
      if(length(speakerSub$Begin.Time)>0){
        #update the new data frame columns
        fileName <- append(fileName, file)
        speakerCode <- append(speakerCode, name) #changed newName to name, do I need this?
        
        # get oll the durations of each of the turn
        turnDurations <- speakerSub$Duration
        
        nb.turns <- append(nb.turns, length(turnDurations)) # nb of turns per speaker
        durationTot <- append(durationTot, sum(turnDurations)) # total duration of speech (per speaker per file)
        durationMean <- append(durationMean, mean(turnDurations)) # mean duration of turn
        durationMed <- append(durationMed, median(turnDurations)) # median duration of turn
      }
    }
  }
}

# build the new dataframe (one line per speaker per file)
df.filesDur <- data.frame(fileName, speakerCode, nb.turns, durationTot, durationMean, durationMed)
# add total duration of interview (per file)
df.filesDur <-merge(df.filesDur, filenameDuration, by.x="fileName", by.y="File")
names(df.filesDur)[names(df.filesDur) == 'Duration'] <- 'duration.File'

# add percentage of speech per speaker per file
df.filesDur$speechPercentage <- df.filesDur$durationTot *100 / df.filesDur$duration.File

# get the interviewer percentage of speech per file and add to the dataframe
df.filesDur$isInt <- df.filesDur$speakerCode %in% interviewerCode
interviewerDurPerFil <- df.filesDur[which(df.filesDur$isInt), c("fileName", "speechPercentage")]
colnames(interviewerDurPerFil) <- c("fileName", "intperc")
df.filesDur <-merge(df.filesDur, interviewerDurPerFil, all.x = TRUE)
df.filesDur[which(is.na(df.filesDur[,"intperc"])),"intperc"] <- 0


# Plots ####
library(ggplot2)
library(plyr)
# next section provide saved version of df.filesDur !
df.filesDur <- df.filesDurCFPP
df.filesDur <- df.filesDurMPF

df.filesDur$speakerCode.label <- mapvalues(df.filesDur$speakerCode,
                                           as.vector(allSpeakers),
                                           as.vector(speakerLabels))

# for each files, plot speakers time of speech
p.bar<-ggplot(data=df.filesDur, aes(x=speakerCode.label, y=speechPercentage, fill=isInt)) + geom_bar(stat="identity") +facet_wrap(.~fileName, scales = "free") + coord_flip() +guides(fill=FALSE)
p.bar

saveplot.MPF("bar_timeSpeech")



####### Compare to inner quotatives ########

# MPF ####
#so not to have to rebuild the whole thing which takes time
df.filesDurMPF <- df.filesDur
write.csv(df.filesDurMPF,path.filesDurMPF, row.names = FALSE)
df.filesDurMPF <- read.csv(path.filesDurMPF)

library(readxl)
# Read the coding sheet modified externally to add file names and speaker anonymous codes
coding_sheet_MPF <- read_excel(path.coding_sheet_MPF)

# look at all quotatives and compares new and old (not really innovative)
xt.quotPerSkeaker <- xtabs(~speakerCode+fileName+Variant, data = coding_sheet_MPF)
coding_sheet_MPFall <-merge(coding_sheet_MPF, xt.quotPerSkeaker)
names(coding_sheet_MPFall)[names(coding_sheet_MPFall) == 'Freq'] <- 'nb.quotatives'

# merge file durations per speaker and coding sheet and only keep 1 line per speaker
colToKeep <- c("YESNOinterviewer","fileName","speakerCode","Content","Variant","Age", "nb.quotatives")
coding_sheet <- data.frame(coding_sheet_MPFall[colToKeep])
df.MPF.all <- unique(merge(coding_sheet, df.filesDurMPF))

#add column is.new (everything that is not "demander", "dire" or "se dire", which are almost exclusively the ones found in older speakers narratives)
df.MPF.all$is.new <- ! df.MPF.all$Variant %in% c('DE','DI','SD')

# add frequency column 
df.MPF.all$quotPerc <- df.MPF.all$nb.quotatives / df.MPF.all$speechPercentage
#number of quotatives per hour of speech
df.MPF.all$quotPerc <- df.MPF.all$nb.quotatives *3600 / df.MPF.all$durationTot

# mean median
mean(df.MPF.all$quotPerc)
median(df.MPF.all$quotPerc)

hist(df.MPF.all$quotPerc, breaks = 50)


# PLOTS and significance tests
# quotatives percentage depending on interviewer amount of speech
ggplot(df.MPF.all, aes(x=intperc, y=quotPerc, color=is.new, shape=YESNOinterviewer)) + geom_point() +geom_smooth(method=lm, se=TRUE) +guides(shape=FALSE)

lm.all <- lm(quotPerc~intperc, data=df.MPF.all)
summary(lm.all) #not significant

# box plots of quotperc for new vs old quotatives and presence or absence of interviewer 
ggplot(df.MPF.all, aes(x=is.new, y=quotPerc)) + geom_boxplot()+facet_wrap(.~YESNOinterviewer)

lm.all.isnew <- lm(quotPerc~YESNOinterviewer*is.new, data=df.MPF.all)
summary(lm.all.isnew) # yesno and interaction significant

#focus on interviewer presence
df.MPF.withInt <- subset(df.MPF.all, YESNOinterviewer == "YES")
lm.int.isnew <- lm(quotPerc~intperc*is.new, data = df.MPF.withInt)
summary(lm.int.isnew) #not significant

#simplify dont look at is new
lm.int <- lm(quotPerc~intperc, data = df.MPF.withInt)
summary(lm.int) #not significant BUT CLOSE


# same but FOCUS on THOUGHTS
# add a column for the number of thought quotatives by speaker
#xt.code <- xtabs(~speakerCode, data = df.merged) #if all quotatives are needed
coding_sheet_ThoughtMPF <- subset(coding_sheet_MPF, coding_sheet_MPF$Content == "T")
xt.toughtsPerSkeaker <- xtabs(~speakerCode+fileName, data = coding_sheet_ThoughtMPF)
coding_sheet_ThoughtMPF <-merge(coding_sheet_ThoughtMPF, xt.toughtsPerSkeaker)
names(coding_sheet_ThoughtMPF)[names(coding_sheet_ThoughtMPF) == 'Freq'] <- 'nb.quotatives'

# merge file durations per speaker and coding sheet and only keep 1 line per speaker
colToKeep <- c("YESNOinterviewer","fileName","speakerCode","Content","Age", "nb.quotatives")
coding_sheet <- data.frame(coding_sheet_ThoughtMPF[colToKeep])
df.MPF <- unique(merge(coding_sheet, df.filesDurMPF))

# IF I want to have all speakers, even those with no quotatives:
df.MPF.with0 <- unique(merge(coding_sheet, df.filesDurMPF, all.y = TRUE))
df.MPF.with0 <- subset(df.MPF.with0, durationTot > 600) #remove under 10 minutes, only 5 thought quot, but many almost dont talk.
#missing columnsTODO
df.MPF.with0$nb.quotatives[which(is.na(df.MPF.with0$nb.quotatives))] <- 0
# FI

# add frequency column 
df.MPF$quotPerc <- df.MPF$nb.quotatives / df.MPF$speechPercentage
#number of quotatives per hour of speech
df.MPF$quotPerc <- df.MPF$nb.quotatives *3600 / df.MPF$durationTot
df.MPF.with0$quotPerc <- df.MPF.with0$nb.quotatives *3600 / df.MPF.with0$durationTot

# mean median
mean(df.MPF$quotPerc)
median(df.MPF$quotPerc)
mean(df.MPF.with0$quotPerc)
median(df.MPF.with0$quotPerc)

hist(df.MPF$quotPerc, breaks = 10)


# PLOTS and significance tests
# quotatives percentage depending on interviewer amount of speech
plot(df.MPF$intperc, df.MPF$nb.quotatives)
plot(df.MPF$intperc, df.MPF$quotPerc)

ggplot(df.MPF, aes(x=intperc, y=quotPerc, color=YESNOinterviewer)) + geom_point() +geom_smooth(method=lm, se=FALSE)
saveplot.MPF("qph_intperc")

lm.thought <- lm(quotPerc~intperc, data = df.MPF)
summary(lm.thought) #SIGNIFICANT

# box plots of quotperc for presence or absence of interviewer 
ggplot(df.MPF, aes(x=YESNOinterviewer, y=quotPerc)) + geom_boxplot()
saveplot.MPF("qph_Yesno")

lm.t.yesno <- lm(quotPerc~YESNOinterviewer, data = df.MPF )
summary(lm.t.yesno) # SIGNIFICANT

#anova(lm.thought, lm.t.yesno, test="Chisq")

df.MPFyes <- subset(df.MPF, YESNOinterviewer == "YES")
ggplot(df.MPFyes, aes(x=intperc, y=quotPerc)) + geom_point()+geom_smooth(method=lm, se=FALSE)
lm.MPFyes <- lm(quotPerc~intperc, data = df.MPFyes)
summary(lm.MPFyes)

#age
ggplot(df.MPF, aes(x=Age, y=quotPerc, color=YESNOinterviewer)) + geom_point() +geom_smooth(method=lm, se=FALSE)

lm.ageMPF <- lm(quotPerc~Age*YESNOinterviewer, data = df.MPF )
summary(lm.ageMPF) # SIGNIFICANT

saveplot.MPF("qph_AgeYesno")

# CFPP ####
#so not to have to rebuild the whole thing which takes time
df.filesDurCFPP <- df.filesDur
write.csv(df.filesDurCFPP,path.filesDurCFPP, row.names = FALSE)
df.filesDurCFPP <- read.csv(path.filesDurCFPP)

speakersInfo <- read.csv(path.allCFPPspeakersInfo)

# merge durations per file and speaker, and coding sheet
df.CFPP <- merge(coding_sheet_CFPP, df.filesDurCFPP, all.x=TRUE)
df.CFPP <- merge(speakersInfo, df.CFPP, all.y = TRUE)

# add frequency column 
df.CFPP$quotPerc <- df.CFPP$nb.quotatives / df.CFPP$speechPercentage
#number of quotatives per hour of speech
df.CFPP$quotPerc <- df.CFPP$nb.quotatives *3600 / df.CFPP$durationTot

# mean median
mean(df.CFPP$quotPerc)
median(df.CFPP$quotPerc)
hist(df.CFPP$quotPerc, breaks = 10)


# IF I want to have all speakers, without those no quotatives:
df.CFPP.without0 <- subset(df.CFPP, quotPerc !=0) #remove zeros 
# mean median
mean(df.CFPP.without0$quotPerc)
median(df.CFPP.without0$quotPerc)
hist(df.CFPP.without0$quotPerc, breaks = 10)

# PLOTS and significance tests
# quotperc function of AGE
ggplot(df.CFPP, aes(x=Age, y=quotPerc)) + geom_point(aes(color = interviewer))+geom_smooth(method=lm, se=FALSE)
saveplot.CFPP("qphAge")

lm.age <- lm(quotPerc~Age, data = df.CFPP)
summary(lm.age) # NS 0.3

# age as categories
df.CFPP$AgeGroup <- "Young"
df.CFPP[which(df.CFPP$Age > 65), "AgeGroup"] <- "Old"
mean(subset(df.CFPP, AgeGroup == "Young" & quotPerc != 0)$quotPerc)

ggplot(df.CFPP, aes(x=AgeGroup, y=quotPerc)) + geom_boxplot()

saveplot.CFPP("agegroup")
lm.agegroup <- lm(quotPerc~AgeGroup, data = df.CFPP)
summary(lm.agegroup) # NS 0.14

# quotPerc function of Age and location and total duration of speech per speaker
ggplot(df.CFPP, aes(x=Age, y=quotPerc, color = LocationGroup)) + geom_point(aes(size=durationTot))+geom_smooth(method=lm, se=FALSE)

lm.ageLocation <- lm(quotPerc~Age+LocationGroup, data = df.CFPP)
summary(lm.ageLocation) # NS

#age categories+ location
ggplot(df.CFPP, aes(x=AgeGroup, y=quotPerc)) + geom_boxplot()+ facet_wrap(.~LocationGroup)
lm.ageGroupLoc <- lm(quotPerc~AgeGroup+LocationGroup, data = df.CFPP)
summary(lm.ageGroupLoc) # NS
saveplot.CFPP("ageGroupLoc")

# add number of interviewees
ggplot(df.CFPP, aes(x=Age, y=quotPerc, color = LocationGroup)) + geom_point()+geom_smooth(method=lm, se=FALSE)
saveplot.CFPP("qphAgeLocation")
ggplot(df.CFPP, aes(x=Age, y=quotPerc, color = LocationGroup)) + geom_point(aes(size=durationTot, shape = as.factor(interviewees)))+geom_smooth(method=lm, se=FALSE) +scale_size(range = c(1, 5))+geom_text(label=df.CFPP$speakerCode)
saveplot.CFPP("qphAgeLocationDetail")

#replace number of interviewees by interviewer
ggplot(df.CFPP, aes(x=Age, y=quotPerc, color = LocationGroup)) + geom_point(aes(shape = interviewer))+geom_smooth(method=lm, se=FALSE)

#only Sonia
df.Sonia <- subset(df.CFPP, interviewer =="Sonia")
lm.sonia <- lm(quotPerc~Age, data = df.Sonia)
summary(lm.sonia) # NS

lm.sonia.2 <- lm(quotPerc~Age*intperc, data = df.Sonia)
summary(lm.sonia.2) # NS
#ggplot(df.Sonia, aes(x=Age, y=quotPerc)) + geom_point(aes(size = intperc))+geom_smooth(method=lm, se=FALSE)
#ggplot(df.Sonia, aes(x=intperc, y=quotPerc)) + geom_point(aes(size = Age))+geom_smooth(method=lm, se=FALSE)


#INTPERC
# quotatives percentage depending on interviewer amount of speech
ggplot(df.CFPP, aes(x=intperc, y=quotPerc)) +geom_smooth(method=lm, se=FALSE) + geom_point(aes(color = interviewer))
saveplot.CFPP("qph_intperc")

lm.intperc <- lm(quotPerc~intperc, data = df.CFPP)
summary(lm.intperc) # NS but close (10%)

ggplot(df.CFPP, aes(x=intperc, y=quotPerc, color = AgeGroup)) +geom_smooth(method=lm, se=FALSE) + geom_point()
saveplot.CFPP("qph_intpercAge")

lm.intperc <- lm(quotPerc~intperc*AgeGroup, data = df.CFPP)
summary(lm.intperc) # NS except AgeGroup Young

# do interviewer speak more or less depending on the location
ggplot(df.CFPP, aes(x=Location, y=intperc, fill=interviewer)) + geom_bar(stat="summary", fun="mean", position="dodge")+ coord_flip()

ggplot(df.CFPP, aes(x=LocationGroup, y=intperc)) +stat_summary(aes(fill=interviewer), fun=mean, geom = "bar", position = "dodge")+stat_summary(aes(label=round(..y..,2)), fun=mean, geom = "text")+ coord_flip()
mean(unique(df.CFPP$intperc))
saveplot.CFPP("bar_locIntperc")

# does the amount of speech of the interviewer influence the amount of quotatives depending of location
#geom_text added to be able to understand who is who and move forward
ggplot(df.CFPP, aes(x=intperc, y=quotPerc, color=LocationGroup))+geom_point()+geom_smooth(method = lm, se=FALSE) #+geom_text(label=df.CFPP$speakerCode)
saveplot.CFPP("qph_intpercLoc")
lm.int.loc <- lm(quotPerc~intperc*LocationGroup, data=df.CFPP)
summary(lm.int.loc) #intperc and locations significant, interaction close


#age categories+ location
ggplot(df.CFPP, aes(x=AgeGroup, y=quotPerc)) + geom_boxplot()+ facet_wrap(.~LocationGroup)
lm.ageGroupLoc <- lm(quotPerc~AgeGroup+LocationGroup, data = df.CFPP)
summary(lm.ageGroupLoc) # NS

#focus on Sonia (because Mat interviewed Marie-Hélène Matera far from other banlieue pattern)
ggplot(df.Sonia, aes(x=intperc, y=quotPerc, color=LocationGroup))+geom_point(aes(shape=as.factor(interviewees)))+geom_smooth(method = lm, se=FALSE)+geom_text(label=df.Sonia$speakerCode)
lm.sonia.loc <- lm(quotPerc~intperc*LocationGroup, data=df.Sonia)
summary(lm.sonia.loc) # SIGNIFICANT but enough data?
#does not mean that in banlieue, intperc influence quotPerc, but maybe in banlieue quotperc~style? influence intperc?
# == it is significantly not random that quot is dependant on intperc, location have an influence and depending on the location the influence is different 


# relation between age and intperc? and location?
ggplot(df.Sonia, aes(x=intperc, y=as.numeric(Age), color=LocationGroup))+geom_point(aes(shape=as.factor(interviewees)))+geom_smooth(method = lm, se=FALSE)+geom_text(label=df.Sonia$speakerCode)
lm.sonia.intAgeLoc <- lm(quotPerc~as.numeric(Age)*LocationGroup, data=df.Sonia)
summary(lm.sonia.intAgeLoc) # NS

lm.sonia.intAgeLoc <- lm(quotPerc~as.numeric(Age)*LocationGroup, data=df.Sonia)
summary(lm.sonia.intAgeLoc) # NS

# relation between intperc and location?
ggplot(df.Sonia, aes(x=FirstName, y=intperc, color=LocationGroup))+geom_point(aes(shape=as.factor(interviewees)))+geom_text(label=df.Sonia$speakerCode)+facet_grid(.~LocationGroup, scales = "free")
lm.sonia.intAgeLoc <- lm(quotPerc~as.numeric(Age)*LocationGroup, data=df.Sonia)
summary(lm.sonia.intAgeLoc) # NS


# Significance tests ####

library(lmerTest) # for running anova on lmer models and giving p values
library(lme4) # for lmer

# should probably do some anova testing but maybe no time, anyway we are aware that this is not very statistically significant




# Merge TOUGHTS! both for plotting
#could add the year and see if if fits together?
df.CFPP$corpus <- "CFPP"
df.CFPP$YESNOinterviewer <- "YES"
df.MPF$corpus <- "MPF"
common_cols <- intersect(colnames(df.CFPP), colnames(df.MPF))
df.merged <- rbind(df.CFPP[, common_cols], df.MPF[, common_cols])

#focus on yes interviewer
df.yes <- subset(df.merged, YESNOinterviewer == "YES")
ggplot(df.yes, aes(x=intperc, y=quotPerc, color=corpus)) + geom_point() +geom_smooth(method=lm, se=TRUE)

lm.yes.intperc <- lm(quotPerc~intperc, data = df.yes)
summary(lm.yes.intperc) # NS

lm.yes.intpercCorpus <- lm(quotPerc~intperc*corpus, data = df.yes)
summary(lm.yes.intpercCorpus) # significant?

#
ggplot(df.merged, aes(x=speechPercentage, y=quotPerc, color=YESNOinterviewer, shape=corpus)) + geom_point()+geom_smooth(method=lm, se=FALSE)

#Age
ggplot(df.merged, aes(x=Age, y=quotPerc, color=YESNOinterviewer, shape=corpus)) + geom_point()+geom_smooth(method=lm, se=FALSE)
ggplot(df.merged, aes(x=Age, y=nb.quotatives, color=YESNOinterviewer, shape=corpus)) + geom_point()+geom_smooth(method=lm, se=FALSE)

glm2.age = glm(quotPerc~Age*corpus, data = df.merged)
summary(glm2.age)


ggplot(df.merged, aes(x=Age, y=quotPerc)) + geom_point(aes(color=YESNOinterviewer, shape=corpus))+geom_smooth(method=lm, se=FALSE)

lm2.age = lm(quotPerc~Age, data = df.merged)
summary(lm2.age) #significant

#yes age
ggplot(df.yes, aes(x=Age, y=quotPerc)) + geom_point(aes(shape=corpus))+geom_smooth(method=lm, se=FALSE)

lm2.yesage = lm(quotPerc~Age, data = df.yes)
summary(lm2.yesage) #significantish




###### print speaker by file for the MPF corpus #####
# to fill the fileName column in coding sheet, when there is one file only for a speaker, easy!
for (speaker in allSpeakers){
  print(paste(speaker, df.filesDur[which(speakerCode == speaker), "fileName"]))
}
