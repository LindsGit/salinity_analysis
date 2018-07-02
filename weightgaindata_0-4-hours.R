library(tidyr)
library(psych)
library(ggplot2)
  #get the file
  pathString <- readline("Paste path from windows explorer: ") # takes the path paste from windows and converts to r format
  pathString <- gsub("\\\\", "/", pathString)
  setwd(pathString)
  
  FileNameString <- "weightgaindata_0-4-hours.csv"
  IsoDat <- read.csv(file = FileNameString, header = TRUE)
  IsoDat$salinity <- sprintf("%d%%_Salinity", IsoDat$salinity)
  IsoDat$salinity <- factor(IsoDat$salinity)
  IsoDat$subject <- factor(IsoDat$subject)
  #convert to long time format
  LongIsoDat <- gather(IsoDat,"time","weight",X0min:X4hrs,factor_key = TRUE)
  str(LongIsoDat)
  LongIsoDat.Table<-describe(LongIsoDat)
  LongIsoDat.Table
  
  LongIsoDat.BarGraph<-ggplot(LongIsoDat, aes(time, weight, fill=time)) +
    geom_bar(stat="summary", fun.y="mean", position = "dodge") + 
    #scale_y_continuous(breaks = seq(0, 101, 10), limits =c(0,101)) +
    facet_grid(.~salinity) +
    xlab("time") + ylab("weight") +
    scale_fill_brewer(palette="Dark2") +
    theme(legend.position="none")
  LongIsoDat.BarGraph
  LongIsoDat.LineGraph <- ggplot(LongIsoDat, aes(x = time, y = weight, group=salinity, shape=salinity)) +
    geom_line(stat="summary", fun.y="mean") +
    geom_point(stat="summary", fun.y="mean")
  LongIsoDat.LineGraph
  
  #--------------------#
  FileNameString <- "changeData.csv"
  IsoChangeDat <- read.csv(file = FileNameString, header = TRUE)
  IsoChangeDat$group <- factor(IsoChangeDat$group, ordered = TRUE)
  IsoChangeDat$subject <- factor(IsoChangeDat$subject, ordered = FALSE)
  #plot
  IsoChangeDat.BarGraph<-ggplot(IsoChangeDat, aes(group, weightChange._4hr, fill=group)) +
    geom_bar(stat="summary", fun.y="mean", position = "dodge") + 
    #scale_y_continuous(breaks = seq(0, 101, 10), limits =c(0,101)) +
    #facet_grid(.~group) +
    xlab("Salinity (%)") + ylab("Weight change 0-4hrs") +
    scale_fill_brewer(palette="Dark2") +
    theme(legend.position="none")
  IsoChangeDat.BarGraph
  #descriptives
  IsoChangeDat.descriptives <- describeBy(IsoChangeDat$weightChange._4hr,IsoChangeDat$group,mat= TRUE)
  IsoChangeDat.descriptives
  # IsoChangeDat.descriptives <- knitr::kable(IsoChangeDat.descriptives)
  IsoChangeDat.model <- aov(weightChange._4hr ~ group, data = IsoChangeDat)
  summary(IsoChangeDat.model)
  #setup contrasts  - this only seems to run a number of contrasts = df
  levels(IsoChangeDat$group)
  c1 <- c(0, 0, 1, -1)
  c2 <- c(0, 1, 0, -1)
  c3 <- c(1, 0, 0, -1)
  contrastMat <- cbind(c1,c2,c3)
  contrasts(IsoChangeDat$group) <- contrastMat
  IsoChangeDat.contrastModel <- aov(weightChange._4hr ~ group, data = IsoChangeDat)
  summary(IsoChangeDat.contrastModel)
  summary.aov(IsoChangeDat.contrastModel, split=list(group=list("50% vs. 100%"=1, "25% vs 100%" = 2, "0% vs 100%"=3))) 
  
  #aov_car approach
  #IsoChangeDat.model2 <- aov_car(weightChange._4hr ~ group + Error(subject/group), data=IsoChangeDat)
  #summary(IsoChangeDat.model2)

#linear model approach
  library(car)
  model = lm(weightChange._4hr ~ group, data = IsoChangeDat)
  anova(model,type = "II")
  summary(model)
  levels(IsoChangeDat$group)
  leastsquare = lsmeans(model, "group")
  contrastsIso = list(  "50vs100" =  c(0, 0, 1, -1),
                        "25vs100" =  c(0, 1, 0, -1),
                        "0vs100"  =  c(1, 0, 0, -1),
                        "0vs50"  =  c(1, 0, -1, 0))
  contrastTable <- contrast(leastsquare,contrastsIso)
  contrastTable
  