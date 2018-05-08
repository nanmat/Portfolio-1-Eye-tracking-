pacman::p_load(readr,groupdata2,ggplot2,tidyverse,plyr,lmer)
setwd('~/OneDrive/4 Semester/Computational models/2018 - Eye tracking')

#Loading the files
log1=read_csv("PupilsLogs/logfile_1_2_f.csv")
log2=read_csv("PupilsLogs/logfile_2_1_f.csv")
log3=read_csv("PupilsLogs/logfile_3_2_f.csv")
log4=read_csv("PupilsLogs/logfile_4_1_F.csv")
log5=read_csv("PupilsLogs/logfile_5_2_m.csv")
log6=read_csv("PupilsLogs/logfile_6_1_m.csv")

SamplesV1=read.csv('SamplesV1.csv')
FixationsV1=read.csv('FixationsV1.csv')
SaccadesV1=read.csv('SaccadesV1.csv')

#Renaming the columns
colnames(log1)[1] = paste("Trial")
colnames(log2)[1] = paste("Trial")
colnames(log3)[1] = paste("Trial")
colnames(log4)[1] = paste("Trial")
colnames(log5)[1] = paste("Trial")
colnames(log6)[1] = paste("Trial")

colnames(log1)[2] = paste("ParticipantID")
colnames(log2)[2] = paste("ParticipantID")
colnames(log3)[2] = paste("ParticipantID")
colnames(log4)[2] = paste("ParticipantID")
colnames(log5)[2] = paste("ParticipantID")
colnames(log6)[2] = paste("ParticipantID")

#Binding all logfiles together
LogAll = rbind(log1, log2, log3,log4,log5,log6)

#Adding a number to Trial because python and r count differently
LogAll$Trial = LogAll$Trial + 1

# To create a variable according to gender of the actor
for (file in 1:nrow(LogAll)){
  if (grepl("m", LogAll$video[file])){
    LogAll$GenderActor[file] = "Male"}
  if (grepl("f", LogAll$video[file])){
    LogAll$GenderActor[file] = "Female"}
}

# To create a variable according to the orientation
for (file in 1:nrow(LogAll)){
  if (grepl("dir", LogAll$video[file])){
    LogAll$Orientation[file] = "Directed"}
  if (grepl("div", LogAll$video[file])){
    LogAll$Orientation[file] = "Diverted"}
}

# To create a variable according to ostensiveness
for (file in 1:nrow(LogAll)){
  if (grepl("+", LogAll$video[file])){
    LogAll$Ostensiveness[file] = "1"}
  if (grepl("-", LogAll$video[file])){
    LogAll$Ostensiveness[file] = "0"}
}

#To merge the logfiles from the video with the data from the eye-tracking
Fixationsmerge = merge(FixationsV1, LogAll, all = TRUE)
Samplesmerge = merge(SamplesV1, LogAll, all = TRUE)
Saccadesmerge = merge(SaccadesV1, LogAll, all = TRUE)


#Adding a column telling which condition the visual search paradigm had. 
Fixationsmerge$SearchType[Fixationsmerge$SearchOrder == "1" & Fixationsmerge$Trial < 6] = "Star"
Fixationsmerge$SearchType[Fixationsmerge$SearchOrder == "1" & Fixationsmerge$Trial > 5] = "Count"
Fixationsmerge$SearchType[Fixationsmerge$SearchOrder == "2" & Fixationsmerge$Trial < 6] = "Count"
Fixationsmerge$SearchType[Fixationsmerge$SearchOrder == "2" & Fixationsmerge$Trial > 5] = "Star"

Samplesmerge$SearchType[Samplesmerge$SearchOrder == "1" & Samplesmerge$Trial < 6] = "Star"
Samplesmerge$SearchType[Samplesmerge$SearchOrder == "1" & Samplesmerge$Trial > 5] = "Count"
Samplesmerge$SearchType[Samplesmerge$SearchOrder == "2" & Samplesmerge$Trial < 6] = "Count"
Samplesmerge$SearchType[Samplesmerge$SearchOrder == "2" & Samplesmerge$Trial > 5] = "Star"

Saccadesmerge$SearchType[Saccadesmerge$SearchOrder == "1" & Saccadesmerge$Trial < 6] = "Star"
Saccadesmerge$SearchType[Saccadesmerge$SearchOrder == "1" & Saccadesmerge$Trial > 5] = "Count"
Saccadesmerge$SearchType[Saccadesmerge$SearchOrder == "2" & Saccadesmerge$Trial < 6] = "Count"
Saccadesmerge$SearchType[Saccadesmerge$SearchOrder == "2" & Saccadesmerge$Trial > 5] = "Star"

#Write csv
write.csv(Fixationsmerge, file = "Fixationsmerge.csv")
write.csv(Samplesmerge, file = "Samplesmerge.csv")
write.csv(Saccadesmerge, file = "Saccadesmerge.csv")

Fixation=read.csv('Fixationsmerge.csv')
Sample=read.csv('Samplesmerge.csv')
Saccade=read.csv('Saccadesmerge.csv')

library(lmerTest)
#Searchtype and Trial is crucial, search order can be left out, fixations can also be left out. 
#Interaction because the two types of tasks vary and you can get better by practice in the star and not
#really better in the counting. 
model1 = glmer(Duration ~ SearchType * Trial + (1 + SearchType * Trial | ParticipantID), data = Fixation)
summary(model1)

model2 = lmer(Duration ~ SearchType + Trial + (1 + SearchType + Trial | ParticipantID), data = Fixation)
summary(model2)

model3 = lmer(Duration ~ SearchType + (1 + Trial | ParticipantID), data = Fixation)
summary(model3)

library(tidyverse)
library(merTools)
library(caret)
library(Metrics)
library(ggplot2)

visualsearch = subset(Fixation, Task == "VisualSearch")
visualsearch$ParticipantID = as.numeric(as.factor(as.character(visualsearch$ParticipantID)))
folds = createFolds(unique(visualsearch$ParticipantID),3)

train_RMSE = NULL
test_RMSE = NULL
n = 1

for (fold in folds) {
  #train2 is everything but not fold, in first loop fold 1, in second loop fold 2. ! means all but not the following
  train = subset(visualsearch, ! (ParticipantID %in% fold))
  #test2 is every other fold than the fold in train2
  test = subset(visualsearch, (ParticipantID %in% fold))
  #The model we want to test
  train_model = lmer(Duration ~ SearchType * Trial + (1 + SearchType * Trial | ParticipantID), data = train, REML = FALSE)
  #Test the model on train data, seeing how well the model predicts train data
  train_RMSE[n] = Metrics::rmse(train$Duration, predict(train_model, train, allow.new.levels = TRUE))
  #Test the model on test data, seeing how well the model predicts test data - which is not a part of the model
  pred = predict(train_model, test, allow.new.levels = TRUE)
  test_RMSE[n] = Metrics::rmse(test$Duration, pred)
  n = n+1
}
#Reporting the mean of how well the model predicts. 
mean(test_RMSE)
mean(train_RMSE)

#The model with main effects:
train_RMSE2 = NULL
test_RMSE2 = NULL
n = 1

for (fold in folds) {
  #train2 is everything but not fold, in first loop fold 1, in second loop fold 2. ! means all but not the following
  train2 = subset(visualsearch, ! (ParticipantID %in% fold))
  #test2 is every other fold than the fold in train2
  test2 = subset(visualsearch, (ParticipantID %in% fold))
  #The model we want to test
  train_model2 = lmer(Duration ~ SearchType + Trial + (1 + SearchType + Trial | ParticipantID), data = train2, REML = FALSE)
  #Test the model on train data, seeing how well the model predicts train data
  train_RMSE2[n] = Metrics::rmse(train2$Duration, predict(train_model2, train2, allow.new.levels = TRUE))
  #Test the model on test data, seeing how well the model predicts test data - which is not a part of the model
  pred = predict(train_model2, test2, allow.new.levels = TRUE)
  test_RMSE2[n] = Metrics::rmse(test2$Duration, pred)
  n = n+1
}
#Reporting the mean of how well the model predicts. 
mean(test_RMSE2)
mean(train_RMSE2)



#Crossvalidating the simplest model
train_RMSE3 = NULL
test_RMSE3 = NULL
n = 1

for (fold in folds) {
  #train2 is everything but not fold, in first loop fold 1, in second loop fold 2. ! means all but not the following
  train3 = subset(visualsearch, ! (ParticipantID %in% fold))
  #test2 is every other fold than the fold in train2
  test3 = subset(visualsearch, (ParticipantID %in% fold))
  #The model we want to test
  train_model3 = lmer(Duration ~ Trial + (1 + Trial | ParticipantID), data = train3, REML = FALSE)
  #Test the model on train data, seeing how well the model predicts train data
  train_RMSE3[n] = Metrics::rmse(train3$Duration, predict(train_model3, train3, allow.new.levels = TRUE))
  #Test the model on test data, seeing how well the model predicts test data - which is not a part of the model
  pred = predict(train_model3, test3, allow.new.levels = TRUE)
  test_RMSE3[n] = Metrics::rmse(test3$Duration, pred)
  n = n+1
}
#Reporting the mean of how well the model predicts. 
mean(test_RMSE3)
mean(train_RMSE3)


#PUPILSIZE
m1 = lmer(PupilSize ~ Ostensiveness * Orientation * ( TrialTime + TrialTime^2 + TrialTime^3) + 
            GenderActor * ParticipantGender  * ( TrialTime + TrialTime^2 + TrialTime^3) + Trial + 
            (1 + Ostensiveness + Orientation | ParticipantID), data = Sample)

library(jpeg)
library(grid)
#Density
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
img <- readJPEG('eyetrackingscripts/Foraging/ng150ws.jpg')
g <- rasterGrob(img, interpolate=TRUE)


ggplot(subset(Fixation, Task == "VisualSearch" & ParticipantID == "3_1_f1" & Trial == 8), aes(x = PositionX, y = 1081-PositionY)) +
  xlim(0,1920) +
  ylim(0,1080) +
  annotation_custom(g, xmin = -Inf, xmax = Inf, ymin = -0, ymax = 1080) + 
  stat_density2d(geom="raster", aes(fill=..density.., alpha=sqrt(sqrt(..density..))), contour = FALSE, n =1000) +
  scale_alpha(range=c(0.1,0.6)) + scale_fill_gradientn(colours = jet.colors(10), trans = "sqrt")

#Scanpath
img1 <- readJPEG('eyetrackingscripts/Foraging/ng150ws.jpg')
g1 <- rasterGrob(img1, interpolate=TRUE)

x = subset(Fixation, Task == "VisualSearch" & ParticipantID == "3_1_f1" & Trial == 8)
x = x[order(x$Fixation),]
ggplot(x, aes(x = PositionX, y = 1081-PositionY)) +
  annotation_custom(g1, xmin = -Inf, xmax = Inf, ymin = -0, ymax = 1080) +
  geom_point(size = x$Duration/100, alpha = 1, color = "cyan3") + 
  geom_path(size = 1, alpha = 0.3, color = "cyan") + 
  geom_text(aes(label = Fixation, size = 5), color = "gray100") 
  

#Social engagement and gender, growth plots pupil size
ggplot(subset(Sample, Task == "SocialEngagement"), aes(x = TrialTime, y = PupilSize, colour = Ostensiveness)) + 
  geom_smooth() +
  facet_grid(~Orientation) +
  xlim(0,6000)






