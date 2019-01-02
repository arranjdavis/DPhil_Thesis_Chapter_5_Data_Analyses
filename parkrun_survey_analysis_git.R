#clean environment
rm(list= ls())

#set current working directory
setwd("/Users/arrandavis/Desktop/R")

library(lubridate)
library(coda)
library(languageR)
library(lme4)
library(lmerTest)
library(e1071)
library(dplyr)
library(ggplot2)

#data set
mydata <- read.csv("parkrun_survey_data_8Sep2018.csv")

################################################################################################################################################

### GET SURVEY RESPONSE TIMES ###

#combine columns for date and time survey was sent

#combine columns
mydata <- transform(mydata, newcol=paste(date_ymd, survey_send, sep="  "))

#rename column
names(mydata)[names(mydata) == 'newcol'] <- 'survey_received'

#convert character string to time
Sys.setenv(TZ='Europe/London') #updates system time zone (perhaps unnecessary)
mydata$survey_received <- as.POSIXct(strptime(mydata$survey_received, "%Y-%m-%d %H:%M:%S"))
mydata$survey_end <- as.POSIXct(strptime(mydata$survey_end, "%Y-%m-%d %H:%M:%S"))
mydata$survey_begin <- as.POSIXct(strptime(mydata$survey_begin, "%Y-%m-%d %H:%M:%S"))

### TIME TO COMPLETE SURVEY (SURVEY RECIEVED TO SURVEY SUBMITTED) ###

#in minutes
mydata$time_to_respond = as.numeric(mydata$survey_end - mydata$survey_received)
mean(mydata$time_to_respond)
median(mydata$time_to_respond)
sd(mydata$time_to_respond)
range(mydata$time_to_respond)
skewness(mydata$time_to_respond)

#in hours
mydata$time_to_respond_hr = mydata$time_to_respond / 60
mean(mydata$time_to_respond_hr)
median(mydata$time_to_respond_hr)
sd(mydata$time_to_respond_hr)
range(mydata$time_to_respond_hr)
skewness(mydata$time_to_respond_hr)

#create a histogram
hist(mydata$time_to_respond_hr, breaks = 167, main = " ", xlab = "Hours", xaxt = 'n')
axis(side=1, at=seq(0,168,24), labels=seq(0,168,24))

#give percentage of participants that took less than one hour to respond
(1 - (table(mydata$time_to_respond < 60)[1] / sum(table(mydata$time_to_respond < 60)))) * 100

#total number of participants that took less than one hour to respond
nrow(mydata) - table(mydata$time_to_respond < 60)[1]
#percentage
(nrow(mydata) - table(mydata$time_to_respond < 60)[1]) / nrow(mydata)

#give how many people responded within two hours
nrow(subset(mydata, time_to_respond < (2*60)))
#percentage
(nrow(mydata) - table(mydata$time_to_respond < (2*60))[1]) / nrow(mydata)

#give how many people responded within one day
nrow(subset(mydata, time_to_respond < (24*60)))
#percentage
(nrow(mydata) - table(mydata$time_to_respond < (24*60))[1]) / nrow(mydata)

#give how many people responded within two days
nrow(subset(mydata, time_to_respond < (48*60)))
#percentage
(nrow(mydata) - table(mydata$time_to_respond < (48*60))[1]) / nrow(mydata)

#give how many people responded within four days
nrow(subset(mydata, time_to_respond < (96*60)))
#percentage
(nrow(mydata) - table(mydata$time_to_respond < (96*60))[1]) / nrow(mydata)

### TIME TAKEN TO FILL OUT SURVEY (SURVEY BEGIN TO SURVEY END) ###

mydata$time_to_fill_out = as.numeric((mydata$survey_end- mydata$survey_begin) / 60)

#in minutes
mean(mydata$time_to_fill_out)
sd(mydata$time_to_fill_out)
range(mydata$time_to_fill_out)
median(mydata$time_to_fill_out)

#get percent of responses that took less than 5 minutes (survey was designed to take 5 minutes)
(1 - (table(mydata$time_to_fill_out < 5)[1] / sum(table(mydata$time_to_fill_out < 10)))) * 100

#less than 10 minutes
(1 - (table(mydata$time_to_fill_out < 10)[1] / sum(table(mydata$time_to_fill_out < 5)))) * 100

#get the percentage that took more than half an hour
(table(mydata$time_to_fill_out < 30)[1] / sum(table(mydata$time_to_fill_out < 30))) * 100

#remove big outliers
table(mydata$time_to_fill_out)
q = subset(mydata, time_to_fill_out < 30)

#create a histogram
hist(q$time_to_fill_out, breaks = 30, main = " ", xlab = "Minutes", xaxt = 'n')
axis(side=1, at=seq(0,35,5), labels=seq(0,35,5))

### GET THE NUMBER OF RESPONSES PER PARTICIPANT ###

total_survey_responses = mydata %>%

  #sumarise unique locations attended by each parkrunner
  group_by(Athlete_ID) %>% 
  summarize(n = n_distinct(survey_end))

table(total_survey_responses$n)

#descriptives for number of responses per participant
mean(total_survey_responses$n)
sd(total_survey_responses$n)
range(total_survey_responses$n)
median(total_survey_responses$n)

### MORE DESCRIPTIVES ###

#get information on gender

#subset one row per participant
one_run_per <- mydata %>% group_by(Athlete_ID) %>% sample_n(1)

#percent of female participants
table(one_run_per$gender)[2] / (sum(table(one_run_per$gender)))

#percentage of total responses filled out by females
table(mydata$gender)[2] / (sum(table(mydata$gender)))

### CREATE AGE CATEGORIES ###

#create age categories by taking the midpoint of the categories provided by parkrun: (18-19)(20-24)(25-29)(30-34)(35-39)(40-44)(45-49)(50-54)(55-59)(60-64)(65-69)(70-74)(75-79)
mydata$age_integer <- ifelse(mydata$age == "1", 18.5,
                              ifelse(mydata$age == "2", 22,
                                     ifelse(mydata$age == "3", 27,
                                            ifelse(mydata$age == "4", 32,
                                                   ifelse(mydata$age == "5", 37,
                                                          ifelse(mydata$age == "6", 42,
                                                                 ifelse(mydata$age == "7", 47,
                                                                        ifelse(mydata$age == "8", 52,
                                                                               ifelse(mydata$age == "9", 57,
                                                                                      ifelse(mydata$age == "10", 62,
                                                                                             ifelse(mydata$age == "11", 67,
                                                                                                    ifelse(mydata$age == "12", 72,
                                                                                                           ifelse(mydata$age == "13", 77, NA)))))))))))))

#get the median age category
age_data <- mydata %>%

#sumarise unique locations attended by each parkrunner
group_by(Athlete_ID) %>% 
summarise(n = mean(age_integer))

#mean age
mean(age_data$n)
median(age_data$n)
sd(age_data$n)
range(age_data$n)

################################################################################################################################################

### DESCRIPTIVE STATISTICS OF SURVEY QUESTIONS AND VARIABLE TRANSFORMATIONS ###

#code pre-run sociality variables

#recode the just_before variable so that: 1 = not being social before the run and 2 = equals being social before the run
mydata$just_before2 <- ifelse(mydata$just_before == "1", 0, ifelse(mydata$just_before == "3", 0, ifelse(mydata$just_before == "2", 1, NA)))

#recode the came_with variable so that 1 = came alone, 2 = came with acquaintance, and 3 = a combined category of coming with friends/family (3) or friends/family AND acquaintances (4)
mydata$came_with <- ifelse(mydata$came_with == "1", 1, ifelse(mydata$came_with == "2", 2, ifelse(mydata$came_with == "3", 3, ifelse(mydata$came_with == "4", 3, NA))))

#recode the came_with variable so that 0 = came alone (1) or with acquaintances (2) and 1 = a combined category of coming with friends/family (3) or a friends/family/acquaintances mix (4 from above)
mydata$came_with2 <- ifelse(mydata$came_with == "1", 0, ifelse(mydata$came_with == "2", 0, ifelse(mydata$came_with == "3", 1, NA)))

#get descriptives on run type

#motivations for attending parkrun (1 = "to improve my ranking"; 2 = "to improve my time; 3 = "to run together with other people")
table(mydata$ranking_time_people)

#in percentages
(table(mydata$ranking_time_people)[1] / sum(table(mydata$ranking_time_people))) * 100
(table(mydata$ranking_time_people)[2] / sum(table(mydata$ranking_time_people))) * 100
(table(mydata$ranking_time_people)[3] / sum(table(mydata$ranking_time_people))) * 100

#who people ran with (1 = "on my own", 2 = "alongside one or more acquaintances", 3 = "alongside one or more friends/family members", 4 = "alongside a mix of acquaintances and friends/family members")
table(mydata$run_description)

#in percentages
(table(mydata$run_description)[1] / sum(table(mydata$run_description))) * 100
(table(mydata$run_description)[2] / sum(table(mydata$run_description))) * 100
(table(mydata$run_description)[3] / sum(table(mydata$run_description))) * 100
(table(mydata$run_description)[4] / sum(table(mydata$run_description))) * 100

#who participants came or met up with (1 = "nobody else"; 2 = "one or more acquaintances"; 3 = "one or more friends/family members or a mix of acquaintances and friends/family members")
table(mydata$came_with)
table(mydata$came_with2)

#in percentages
(table(mydata$came_with)[1] / sum(table(mydata$came_with))) * 100
(table(mydata$came_with)[2] / sum(table(mydata$came_with))) * 100
(table(mydata$came_with)[3] / sum(table(mydata$came_with))) * 100

#percentages for the recoded, binary variable
(table(mydata$came_with2)[1] / sum(table(mydata$came_with2))) * 100
(table(mydata$came_with2)[2] / sum(table(mydata$came_with2))) * 100

#pre-run sociality
table(mydata$just_before)
table(mydata$just_before2)

#in percentages
(table(mydata$just_before)[1] / sum(table(mydata$just_before))) * 100
(table(mydata$just_before)[2] / sum(table(mydata$just_before))) * 100
(table(mydata$just_before)[3] / sum(table(mydata$just_before))) * 100

#percentages for the recoded, binary variable
(table(mydata$just_before2)[1] / sum(table(mydata$just_before2))) * 100
(table(mydata$just_before2)[2] / sum(table(mydata$just_before2))) * 100

################################################################################################################################################

#CREATE PARKRUN COMMUNITY FACTOR ###

library("corpcor")
library("GPArotation")
library("psych")

#create a data frame of the variables of interest
cor.dat <- mydata[,c("support_felt", "community_inclusion")]

#create a matrix
community.matrix <- cor(cor.dat)

#Bartlett's test to see if PCA is appropriate (see Field et al., 2014; p. 775)
cortest.bartlett(cor.dat)

#do a KMO (see Field et al., 2014; p. 776)
KMO(cor.dat)

#determinant of correlation matrix (see Field et al., 2014; p. 777)
det(community.matrix)

#the PCA
PCA.mod <-principal(cor.dat, scores = TRUE)
PCA.mod #h2 is communalities, SS loadings are eigenvalues
PCA.mod.final <-principal(cor.dat, nfactors = 1, rotate = "oblimin", scores = TRUE)
PCA.mod.final

#Cronbach's alpha
alpha(cor.dat)

#add scores to the main dataset
parkrun_community_factor <- cbind(mydata, PCA.mod.final$scores)
mydata$parkrun_community_factor = parkrun_community_factor$PC1

################################################################################################################################################

### MAIN MULTILEVEL MODELS ON RUN TIMES ###

library(lmerTest)
library(MuMIn)

#log run times
mydata$time.lg = log(mydata$time)

#make binary predictors factors
mydata$came_with2 = as.factor(mydata$came_with2)
mydata$just_before2 = as.factor(mydata$just_before2)

#create variable that indicates whether or not participants had to slow down to run with their slower running partner (0 = did not slow down, 1 = slowed down)
mydata$slowed_down2 = as.factor(ifelse(mydata$slowed_spedup_natural == "2", 0, ifelse(mydata$slowed_spedup_natural == "1", 1, ifelse(mydata$slowed_spedup_natural == "3", 0, ifelse(mydata$slowed_spedup_natural == "4", 0, NA)))))
mydata$slowed_down2 = as.factor(mydata$slowed_down2)

#model for Hypothesis 1.1
hypothesis_1.1 = lmer(time.lg ~ 1 + came_with2 + exertion + slowed_down2 + (1 + came_with2 + exertion + slowed_down2 | Athlete_ID), data = mydata)
summary(hypothesis_1.1)
r.squaredGLMM(hypothesis_1.1)

#model for Hypothesis 1.2
hypothesis_1.2 = lmer(time.lg ~ 1 + parkrun_community_factor + exertion + slowed_down2 + (1 + parkrun_community_factor + exertion + slowed_down2 | Athlete_ID), data = mydata)
summary(hypothesis_1.2)
r.squaredGLMM(hypothesis_1.2)

#model for Hypothesis 1.3
hypothesis_1.3 = lmer(time.lg ~ 1 + just_before2 + exertion + slowed_down2 + (1 + just_before2 + exertion + slowed_down2 | Athlete_ID), data = mydata)
summary(hypothesis_1.3)
r.squaredGLMM(hypothesis_1.3)

################################################################################################################################################

### MAIN MULTILEVEL MODELS ON FATIGUE LEVELS ###

#model for Hypothesis 2.1
hypothesis_2.1 = lmer(fatigued ~ 1 + came_with2 + time.lg + (1 + came_with2 + time.lg | Athlete_ID), data = mydata)
summary(hypothesis_2.1)
r.squaredGLMM(hypothesis_2.1)

#model for Hypothesis 2.2 (unlogged run times used here because logged run times led the model to fail to converge)
hypothesis_2.2 = lmer(fatigued ~ 1 + parkrun_community_factor + time + (1 + parkrun_community_factor + time | Athlete_ID), data = mydata)
summary(hypothesis_2.2)
r.squaredGLMM(hypothesis_2.2)

#model for Hypothesis 2.3 (unlogged run times used here because logged run times led the model to fail to converge)
hypothesis_2.3 = lmer(fatigued ~ 1 + just_before2 + time + (1 + just_before2 + time | Athlete_ID), data = mydata)
summary(hypothesis_2.3)
r.squaredGLMM(hypothesis_2.3)

################################################################################################################################################

### MAIN MULTILEVEL MODELS ON ENJOYMENT LEVELS ###

#model for Hypothesis 3.1
hypothesis_3.1 = lmer(enjoyment ~ 1 + came_with2 + (1 + came_with2 | Athlete_ID), data = mydata)
summary(hypothesis_3.1)
r.squaredGLMM(hypothesis_3.1)

#model for Hypothesis 3.2
hypothesis_3.2 = lmer(enjoyment ~ 1 + parkrun_community_factor + (1 + parkrun_community_factor | Athlete_ID), data = mydata)
summary(hypothesis_3.2)
r.squaredGLMM(hypothesis_3.2)

#model for Hypothesis 3.3
hypothesis_3.3 = lmer(enjoyment ~ 1 + just_before2 + (1 + just_before2 | Athlete_ID), data = mydata)
summary(hypothesis_3.3)
r.squaredGLMM(hypothesis_3.3)

################################################################################################################################################

### MOTIVATION TO ATTEND PARKRUN AS MODERATOR OF SOCIAL SUPPORT EFFECTS ON RUN TIMES ###

#model for Hypothesis 4.1
hypothesis_4.1 = lmer(time.lg ~ 1 + came_with2*ranking_time_people + (1 + came_with2*ranking_time_people | Athlete_ID), data = mydata)
summary(hypothesis_4.1)
r.squaredGLMM(hypothesis_4.1)

#model for Hypothesis 4.2
hypothesis_4.2 = lmer(time.lg ~ 1 + parkrun_community_factor*ranking_time_people + (1 + parkrun_community_factor*ranking_time_people | Athlete_ID), data = mydata)
summary(hypothesis_4.2)
r.squaredGLMM(hypothesis_4.2)

#model for Hypothesis 4.3
hypothesis_4.3 = lmer(time.lg ~ 1 + just_before2*ranking_time_people + (1 + just_before2*ranking_time_people | Athlete_ID), data = mydata)
summary(hypothesis_4.3)
r.squaredGLMM(hypothesis_4.3)

################################################################################################################################################

### SPEEDING UP TO RUN WITH A FASTER RUNNING PARTNER AS MODERATOR OF SOCIAL SUPPORT EFFECTS ON FATIGUE LEVELS ###

#recode the slowed_spedup_natural variable so that 1 = speeding up to run with a faster running partner and 0 = every other option
#original question: 1 = "Today I slowed down for my running partner(s)", 2 = "Today I sped up for my running partner(s)", 3 = "Today my natural pace was pretty much the same as the pace of my running partner(s)", 4 = "Not applicable – I ran on my own."
mydata$slowed_spedup_natural2 <- as.factor(ifelse(mydata$slowed_spedup_natural == "2", 1, ifelse(mydata$slowed_spedup_natural == "1", 0, ifelse(mydata$slowed_spedup_natural == "3", 0, ifelse(mydata$slowed_spedup_natural == "4", 0, NA)))))

#model for Hypothesis 4.4 (more complex random effects structure fails to converge)
hypothesis_4.4 = lmer(fatigued ~ 1 + came_with2*slowed_spedup_natural2 + (1 + came_with2 + slowed_spedup_natural2 | Athlete_ID), data = mydata)
summary(hypothesis_4.4)
r.squaredGLMM(hypothesis_4.4)

#model for Hypothesis 4.5
hypothesis_4.5 = lmer(fatigued ~ 1 + parkrun_community_factor*slowed_spedup_natural2 + (1 + parkrun_community_factor*slowed_spedup_natural2 | Athlete_ID), data = mydata)
summary(hypothesis_4.5)
r.squaredGLMM(hypothesis_4.5)

#model for Hypothesis 4.6
hypothesis_4.6 = lmer(time.lg ~ 1 + just_before2*slowed_spedup_natural2 + (1 + just_before2*slowed_spedup_natural2 | Athlete_ID), data = mydata)
summary(hypothesis_4.6)
r.squaredGLMM(hypothesis_4.6)

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS ###

library(mediation)
set.seed(2014)
detach("package:lmerTest", unload=TRUE)

################################################################################################################################################

#make binary predictors integers (a requirement of the mediate package)
mydata$came_with2_int = as.integer(mydata$came_with2)
mydata$just_before2_int = as.integer(mydata$just_before2)
mydata$slowed_down2_int = as.integer(mydata$slowed_down2)

################################################################################################################################################

### HYPOTHESIS 5.1 ###

#mediation relationship: came_with2 -> how_energising -> time

med.fit <- lmer(how_energising ~ came_with2_int + slowed_down2_int + (1 + came_with2_int | Athlete_ID), data = mydata)
summary(med.fit)
r.squaredGLMM(med.fit)

out.fit <- lmer(time.lg ~ how_energising + came_with2_int + slowed_down2_int + (1 + came_with2_int| Athlete_ID), data = mydata) # more complex random effects structures don't converge
summary(out.fit)
r.squaredGLMM(out.fit)

med.out <- mediate(med.fit, out.fit, treat = "came_with2_int", mediator = "how_energising", covariates = "slowed_down2_int", sims = 1000, boot.ci.type = "bca")
summary(med.out)

################################################################################################################################################

### HYPOTHESIS 5.2 ###

#mediation relationship: parkrun_community_factor -> how_energising -> time

med.fit <- lmer(how_energising ~ parkrun_community_factor + slowed_down2_int + (1 + parkrun_community_factor | Athlete_ID), data = mydata)
summary(med.fit)
r.squaredGLMM(med.fit)

out.fit <- lmer(time.lg ~ how_energising + parkrun_community_factor + slowed_down2_int + (1 + parkrun_community_factor | Athlete_ID), data = mydata)
summary(out.fit)
r.squaredGLMM(out.fit)

med.out <- mediate(med.fit, out.fit, treat = "parkrun_community_factor", mediator = "how_energising", covariates = "slowed_down2_int", sims = 1000, boot.ci.type = "bca")
summary(med.out)

################################################################################################################################################

### HYPOTHESIS 5.3 ###

#mediation relationship: just_before2 -> how_energising -> time

med.fit <- lmer(how_energising ~ just_before2_int + slowed_down2_int +  (1 + just_before2_int | Athlete_ID), data = mydata)
summary(med.fit)
r.squaredGLMM(med.fit)

out.fit <- lmer(time.lg ~ how_energising + just_before2_int + slowed_down2_int + (1 + just_before2_int | Athlete_ID), data = mydata)
summary(out.fit)
r.squaredGLMM(out.fit)

med.out <- mediate(med.fit, out.fit, treat = "just_before2_int", mediator = "how_energising", covariates = "slowed_down2_int", sims = 1000, boot.ci.type = "bca")
summary(med.out)

################################################################################################################################################

### IMPROVEMENTS IN RUN TIMES ###

#indirect effect of (-0.002)
(mean(mydata$time) * 60 ) * .002

#indirect effect of (-0.007)
(mean(mydata$time) * 60 ) * .007

################################################################################################################################################

### DOES RESPONSE LAG PREDICT ANY OUTCOME-RELEVANT VARIABLES? ###

library(lmerTest)
library(lmerTest)
library(MuMIn)

#test whether response lag times predict responses to any of the survey questions
#time variable logged to facilitate model fit

plot(mydata$time_to_respond, mydata$fatigued)
time.fatigue = lmer(fatigued ~ 1 + log(time_to_respond) + (1 + log(time_to_respond) | Athlete_ID), data = mydata)
summary(time.fatigue)
r.squaredGLMM(time.fatigue)


plot(mydata$time_to_respond, mydata$how_energising)
time.energy = lmer(how_energising ~ 1 + log(time_to_respond) + (1 + log(time_to_respond) | Athlete_ID), data = mydata)
summary(time.energy)
r.squaredGLMM(time.energy)


plot(mydata$time_to_respond, mydata$enjoyment)
time.enjoyment = lmer(enjoyment ~ 1 + log(time_to_respond) + (1 + log(time_to_respond) | Athlete_ID), data = mydata)
summary(time.enjoyment)
r.squaredGLMM(time.enjoyment)

plot(mydata$time_to_respond, mydata$parkrun_community_factor)
time.pcc = lmer(parkrun_community_factor ~ 1 + log(time_to_respond) + (1 + log(time_to_respond) | Athlete_ID), data = mydata)
summary(time.pcc)
r.squaredGLMM(time.pcc)

################################################################################################################################################

### GRAPHS ###

#box plot of came_with2 and how_energised
ggplot(mydata_5resp, aes(x = came_with2, y = how_energising, grouping(came_with2))) +
  geom_boxplot()+
    xlab("Came or met up with family and/or friends?") +
      ylab("Felt energy (7-point Likert scale)") +
        scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) + 
          scale_y_continuous(breaks = pretty(mydata_5resp$how_energising, n = 7)) +
            theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))
  

#violin plot of came_with2 and how_energised
ggplot(mydata, aes(x = came_with2, y = how_energising, grouping(came_with2))) +
  geom_violin() + 
  geom_boxplot(width = 0.1) +
  xlab("Came or met up with family and/or friends?") +
  ylab("Felt energy (7-point Likert scale)") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) + 
  scale_y_continuous(breaks = pretty(mydata$how_energising, n = 7))+
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
        axis.title.x = element_text(margin = unit(c(9, 0, 0, 0), "mm")), axis.title.y = element_text(margin = unit(c(0, 9, 0, 0), "mm")), text = element_text(size = 33, family = "sans"))

### ### ### ### ### ### ### ### ###

#box plot of just_before2 (must be factor) and how_energised
ggplot(mydata_5resp, aes(x = just_before2, y = how_energising, grouping(just_before2))) +
  geom_boxplot()+
    xlab("Pre-run sociality?") +
      ylab("Felt energy (7-point Likert scale)") +
        scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) + 
          scale_y_continuous(breaks = pretty(mydata_5resp$how_energising, n = 7)) +
            theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2))

#violin plot of just_before2 and how_energised
ggplot(mydata, aes(x = just_before2, y = how_energising, grouping(just_before2))) +
  geom_violin() + 
  geom_boxplot(width = 0.1) +
  xlab("Pre-run sociality?") +
  ylab("Felt energy (7-point Likert scale)") +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) + 
  scale_y_continuous(breaks = pretty(mydata$how_energising, n = 7))+
  theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
        axis.title.x = element_text(margin = unit(c(9, 0, 0, 0), "mm")), axis.title.y = element_text(margin = unit(c(0, 9, 0, 0), "mm")), text = element_text(size = 33, family = "sans"))

### ### ### ### ### ### ### ### ###

#regression line
mod1 = lm(how_energising ~ 1 +  parkrun_community_factor, data = mydata)

#intercept
int = mod1$coefficients[1]
slope = mod1$coefficients[2]

#scatter plot of parkrun_community_factor and how_energising
ggplot(mydata, aes(x = parkrun_community_factor, y = how_energising)) +
  geom_point(alpha = 0.3) +    #this makes dots density-specific
    geom_abline(intercept = int, slope = slope) +
      xlab(expression(paste(italic("parkrun"), " community component"))) +
      #xlab("parkrun community component") +
        ylab("Felt energy 7-point Likert scale") +
          scale_y_continuous(breaks = pretty(mydata$how_energising, n = 7)) +
          theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
                panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
                axis.title.x = element_text(margin = unit(c(9, 0, 0, 0), "mm")), axis.title.y = element_text(margin = unit(c(0, 9, 0, 0), "mm")), text = element_text(size = 33, family = "sans"))

### ### ### ### ### ### ### ### ###

#regression line
mod2 = lm(time ~ 1 +  how_energising, data = mydata)

#intercept
int = mod2$coefficients[1]
slope = mod2$coefficients[2]

#scatter plot of how_energising and run times
ggplot(mydata, aes(x = how_energising, y = time)) +
  geom_point(alpha = 0.3) +    #this makes dots density-specific
    geom_abline(intercept = int, slope = slope) + #this is the lmer regression line of time on how_energising (with no x covariates and random slopes)
      xlab("Felt energy (7-point Likert scale)") +
        ylab("5 km run time (min)") +
          scale_x_continuous(breaks = pretty(mydata$how_energising, n = 7)) +
            theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),
                  panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2),
                  axis.title.x = element_text(margin = unit(c(9, 0, 0, 0), "mm")), axis.title.y = element_text(margin = unit(c(0, 9, 0, 0), "mm")), text = element_text(size = 33, family = "sans"))


################################################################################################################################################

### ASSUMPTION CHECKS (FROM SNIJDERS & BOSKER, 2012) - WHO PARTICIPANTS CAME OR MET UP WITH ON FATIGUE ###

###################################################################
###  ch10.r                                                     ###
###                                                             ###
###  This is an R script for producing examples in              ###
###  Chapter 10 of                                              ###
###  Snijders, Tom A.B., and Bosker, Roel J.                    ###
###  Multilevel Analysis:                                       ###
###  An Introduction to Basic and Advanced Multilevel Modeling, ###
###  second edition.                                            ###
###  London etc.: Sage Publishers, 2012                         ###
###                                                             ###
###  version April 4, 2014                                      ###
###                                                             ###
###################################################################

setwd("/Users/arrandavis/Desktop/University of Oxford/DPHIL/Study 2/Parkrun.1/Data.Merge1")

#rm(list = ls())

library(lubridate)
library(lme4)
library(lmerTest)
library(RColorBrewer)


### ### ###

###################################################################
###                                                             ###
### Within - group OLS residuals                                ###
###                                                             ###
###################################################################

#this will estimate seperate linear models for every level of the grouping variable

#lme4 is used for these checks
detach("package:nlme")
library(lme4)

#this will run a liner model at every level of the grouping factor that is present in the data set (so, a linear model will be run on every participants' data that is in the data set)
mydata$came_with2 = as.integer(mydata$came_with2)

#this keeps the parkrunner-level variables (even though they make no sense and will generate NA's since they don't vary, so that the compareFits function can be used)
olselist <- lmList(fatigued ~ 1 + came_with2 + time.lg | Athlete_ID, data = mydata)

#this runs the main model on the same subset of parkrunners
library(nlme)
summary(mod1 <- lme(fatigued ~ 1 + came_with2 + time.lg, random = ~ came_with2 | Athlete_ID, data = mydata)) #this fails to converge with time as a random effect

#The compareFits function compares the OLS estimates with the posterior means of the multilevel model. This is done only for the level-1 effects.
#The graph produced below will compare the variance of the OLS regression estimates from each parkrunner to the posterior means calculated in the multilevel model

comp.models <- compareFits(coef(olselist), coef(mod1))
plot(comp.models, ylab = "")

#the plots indicate that variabiliry for the OLS estimates is indeed larger, but less so for the came_with2 variable

#this will get the within-group residual d.f.
#first see how olselist is structured:
str(olselist, 1)

#it is a list of results for each of the participants
str(olselist[[1]], 1)

#the within-group residual d.f. can be obtained as follows (this is number of runs minus the number of parameters plus 1):

df_res <- sapply(olselist, function(x){x$df.residual})
table(df_res)

#to study the outliers for the effect of the came_with2 variable the plot of comp.models shows that the value 2 could be used as to seperate the outliers from the rest.
strong.came_with.effect <- which(abs(comp.models[,1,"came_with2"]) > 2) 

#What are the numbers in the table when this is printed? They are the number of the athlete number and the number of the model.

#check whether these observations come from relatively small groups - the average number of responses per participant is:
mean_responses = mean(table(mydata$Athlete_ID))

strong.came_with.table = df_res[strong.came_with.effect] #smaller than average, but larger groups are also included
mean(strong.came_with.table)

#this will give the percentage of these parkrunners who have a total number of runs larger than the average
counter = 0

for (i in strong.came_with.table) {
  
  if (mean_responses < i)
    
    counter = counter + 1
  
}

#percentage of these parkrunners who have fewer than the average amount of runs
1 - (counter / length(strong.came_with.table))

#the relatively small size of these groups may well account for their deviating observed effects of the came_with2 variable; 
#these outliers are thus not very bothering (Snijders & Bosker, 2012)

### ### ###

### TEST FOR HOMOSCEDASTICITY (NULL IS HOMOSCEDASTICITY, AND IT HAS A CHI-SQUARE DISTRIBUTION UNDER THE NULL) ###

# Example 10.1
#make a selection of the participants with a d.f. that is relatively large (Snijders & Bosker, 2012), in this case, at least 9 (gives 20 participants)
use <- df_res >= 9

#the number of such participants is:
sum(use)

#now get the within-group residual standard deviations
summary(olselist[[1]])$sigma

#make a plot of residual variance against residual d.f.:
sigma2_res <- sapply(olselist, function(x){(summary(x)$sigma)^2})
plot(df_res, sigma2_res) 

#there are some outliers for the very low residual d.f.

#Formula (10.3); "Used to detect heteroscedasticity in the form of between-group differences in level-one residual variance" (Snijders & Bosker, 2012; p. 159).

#test heteroscedasticity
ls_tot <- sum((df_res*log(sigma2_res))[use])/sum(df_res[use])
d <- (sqrt(0.5*df_res))*(log(sigma2_res) - ls_tot)
(H <- sum((d^2)[use]))

#the associated p-value is:
1-pchisq(H, sum(use)-1)
length(use)

#this will plot d, which is Gausian if there is level-one homoscedasticity
qqnorm(d[use])
qqline(d[use])

### ### ###

#Example 10.2
#the goal here is to inspect level-one residuals, step one is to plot the unstandardised OLS residuals against level-one explanatory variables to check for non-linearity
#step two is to make a normal probability plot of the standardised OLS residuals to check the assumption of a normal distribution

### PLOT UNSTANDARDISED RESIDUALS AGAINST LEVEL-ONE EXPLANATORY VARIABLES TO CHECK FOR NON-LINEARITY ###

#the within-group OLS residuals are obtained by:
resi <- residuals(olselist) 

#for the logged times variable
cols = brewer.pal(n=4,name="Set1")

with(mydata,plot(time.lg,resi, xlab = expression('Logged 5 km run times'), ylab = "Residual"))
with(mydata,lines(lowess(time.lg,resi), col=cols[2]))

#the lowess line indicates linearity

### MAKE A NORMAL PROBABILITY PLOT OF THE STANDARDISED OLS RESIDUALS TO CHECK THE ASSUMPTIONS OF A NORMAL DISTRIBUTION ###

#first define a function that gives the studentized OLS residuals for this model in a given data frame:
res_st5 <- function(x){
  rstudent(lm(fatigued ~ came_with2 + time.lg,
              data=x))
}

#compute within-participant studentized OLS residuals
resi_st <- by(mydata, mydata$Athlete_ID, res_st5)
rs <- unlist(resi_st)

#some of the residuals are NA or exactly 0.
sum(is.na(rs))
sum(rs[!is.na(rs)]==0)

#this is because of the presence of small groups (Snijders & Bosker, 2012)

#make a QQ plot with use the <use> vector defined above (to only include the parkrunners that have enough runs)
qqnorm(rs[use], main = "")
qqline(rs[use])

###################################################################
###                                                             ###
### Residuals at level two                                      ###
###                                                             ###
###################################################################

# Example 10.3
#the goal here is to inspect level-two residuals, step one is to plot the unstandardised level-two residuals against "relevant level-two variables" to check for non-linearity
#step one will be skipped, since there are no relevant level-two predictor variables
#step two is to make normal probability plots of standardised level-two residuals to check the assumption of a normal distribution, and
#step three is to plot the squared standardised level-two residuals as a function of level-two variables to check homoscedasticity

#lme4 is used for these checks
detach("package:nlme")
library(lme4)

#this is the model for which the level-two residuals will be inspected (this should be the full model run above)

#get the random effects for this model
re.mod103 <- ranef(hypothesis_2.1, condVar=TRUE, standard=TRUE)

#this will get the posterior means (taking out the columns of random slopes and intercepts for each participant):
postmean  <- re.mod103$Athlete_ID[,1] #first column is the intercept
postmean  <- re.mod103$Athlete_ID$`(Intercept)` #first column is the intercept (this is the same as above)
postslope.came_with <- re.mod103$Athlete_ID$came_with21
postslope.time <- re.mod103$Athlete_ID$time.lg

#this will get the posterior variances:
postmeanvar <-  attr(re.mod103$Athlete_ID,'postVar')[1,1,]
postslopevar.came_with <-  attr(re.mod103$Athlete_ID,'postVar')[2,2,] 
postslopevar.time <-  attr(re.mod103$Athlete_ID,'postVar')[3,3,]

#these are the comparative variances, cf. Section 4.8. in Snijders & Bosker (2012; p. 65)

#posterior means are computed by taking information from the overall mean and the group mean, which is influenced by the size and variability of the observations within the group
#posterior means, in other words, are the empirical Bayes' estiamtes of the random slopes and intercepts (Snijders & Bosker, 2012; p. 64)

#the parameters of the random part
VarCorr(hypothesis_2.1)

#diagnostic variance is calculated using 4.18 from Snijders & Bosker (2012; p. 65):
diagmeanvar  <- VarCorr(hypothesis_2.1)$Athlete_ID[1,1] - postmeanvar
diagslopevar.came_with <- VarCorr(hypothesis_2.1)$Athlete_ID[2,2] - postslopevar.came_with
diagslopevar.time <- VarCorr(hypothesis_2.1)$Athlete_ID[3,3] - postslopevar.time

### NORMAL PROBABILITY PLOTS OF STANDARDISED LEVEL-TWO RESIDUALS TO CHECK FOR LEVEL-TWO RESIDUAL NORMALITY ###

#make a plot like figure 10.6 (Snijders & Bosker, 2012; p. 166) for the intercepts and both the 'partner_relation.lg' and 'previous_runs.lg' variables:

#for the intercepts
postmean.stand <- postmean/sqrt(diagmeanvar)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#for the 'came_with2' variable
postmean.stand <- postslope.came_with/sqrt(diagslopevar.came_with)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

###################################################################
###                                                             ###
### Influence of level-two units                                ###
###                                                             ###
###################################################################

#(from this package: https://cran.r-project.org/web/packages/influence.ME/influence.ME.pdf)

library(influence.ME)

#check the influence of data points included in the model 
alt.est <- influence(hypothesis_2.1, group= "Athlete_ID")

#Cook's distances
cooks.distance.estex(alt.est)
range(cooks.distance.estex(alt.est))
mean(cooks.distance(alt.est))
sd(cooks.distance(alt.est))
hist(cooks.distance(alt.est), breaks = 100, main = "", xlab = "Cook's distance")

#percent change in parameter estimates when including and not including each level-two unit (i.e., participant)
pchange(alt.est)

influencers = cooks.distance.estex(alt.est)
p = length(influencers)

#this will give the percentage of participants with potentially problematic Cook's distances
count4 = 0
for (i in influencers) {
  #threshold for potentially problematic Cook's distances; either 4 / n where n in the number of level-two units (Nieuwenhuis et al., 2012) or 1 (Field et al., 2014)
  if (i > 1){
    print(i)
    count4 = count4 + 1
  }
}

#percentage of participants above threshold for potentially problematic Cook's distances; either 4 / n where n in the number of level-two units (Nieuwenhuis et al., 2012) or 1 (Field et al., 2014)
print(count4 / p)

#the sigtest function tests whether excluding a particular level-two unit changes the statistical significance (at 1.96) of a model’s predictor variables (Nieuwenhuis et al. 2012)
sig.test.dataframe = as.data.frame(sigtest(alt.est))

#came_with2 variable

#this give the range of t-values produced by interatively excluding the sampled parkrunners one at a time
range(sig.test.dataframe$came_with21.Altered.Teststat)

#does excluding this level-two unit change the significance of the variable?
table(sig.test.dataframe$came_with21.Changed.Sig)

#graph variation in FPRS t-statistic created by excluding level-two units one at a time
hist(sig.test.dataframe$came_with21.Altered.Teststat, breaks = 100, main = "", xlab = "t-statistic")

################################################################################################################################################

### ASSUMPTION CHECKS (FROM SNIJDERS & BOSKER, 2012) - WHO PARTICIPANTS CAME OR MET UP WITH ON ENJOYMENT ###

###################################################################
###                                                             ###
### Within - group OLS residuals                                ###
###                                                             ###
###################################################################

#this will run a linear model at every level of the grouping factor that is present in the data set (so, a linear model will be run on every participants' data that is in the data set)

#this needs to be an integer
mydata$came_with2 = as.integer(mydata$came_with2)

#(nlme cannot be loaded for this to run)
detach("package:lmerTest", unload=TRUE)
detach("package:lme4", unload=TRUE)
detach("package:nlme", unload=TRUE)
library(lme4)

#this will estimate seperate linear models for every level of the grouping variable 
olselist <- lmList(enjoyment ~ 1 + came_with2 | Athlete_ID, data = mydata)

#this runs the main model on the same subset of parkrunners
library(nlme)
summary(mod1 <- lme(enjoyment ~ 1 + came_with2, random = ~ came_with2 | Athlete_ID, data = mydata)) 

#The compareFits function compares the OLS estimates with the posterior means of the multilevel model. This is done only for the level-1 effects.
#The graph produced below will compare the variance of the OLS regression estimates from each parkrunner to the posterior means calculated in the multilevel model

comp.models <- compareFits(coef(olselist), coef(mod1))
plot(comp.models, ylab = "")

#the plots indicate that variabiliry for the OLS estimates is indeed larger, but less so for the came_with2 variable

#this will get the within-group residual d.f.
#first see how olselist is structured:
str(olselist, 1)

#it is a list of results for each of the participants
str(olselist[[1]], 1)

#the within-group residual d.f. can be obtained as follows (this is number of runs minus the number of parameters plus 1):

df_res <- sapply(olselist, function(x){x$df.residual})
table(df_res)

#to study the outliers for the effect of the came_with2 variable the plot of comp.models shows that the value 1 could be used as to seperate the outliers from the rest.
strong.came_with.effect <- which(abs(comp.models[,1,"came_with2"]) > 1) 

#check whether these observations come from relatively small groups - the average number of responses per participant is:
mean_responses = mean(table(mydata$Athlete_ID))

strong.came_with.table = df_res[strong.came_with.effect] #smaller than average, but larger groups are also included
mean(strong.came_with.table)

#this will give the percentage of these runners who have a total number of runs larger than the average
counter = 0

for (i in strong.came_with.table) {
  
  if (mean_responses < i)
    
    counter = counter + 1
  
}

#percentage of these participants who have fewer than the average amount of survey responses
1 - (counter / length(strong.came_with.table))

#the relatively small size of these groups may well account for their deviating observed effects of the came_with2 variable; 
#these outliers are thus not very bothering (Snijders & Bosker, 2012)

### TEST FOR HOMOSCEDASTICITY (NULL IS HOMOSCEDASTICITY, AND IT HAS A CHI-SQUARE DISTRIBUTION UNDER THE NULL) ###

#Example 10.1
#make a selection of the participants with a d.f. that is relatively large (Snijders & Bosker, 2012), in this case, at least 9 (gives 20 participants)
use <- df_res >= 9

#the number of such participants is:
sum(use)

#now get the within-group residual standard deviations
summary(olselist[[1]])$sigma

#make a plot of residual variance against residual d.f.:
sigma2_res <- sapply(olselist, function(x){(summary(x)$sigma)^2})
plot(df_res, sigma2_res) 

#there are some outliers for the very low residual d.f.

#Formula (10.3); "Used to detect heteroscedasticity in the form of between-group differences in level-one residual variance" (Snijders & Bosker, 2012; p. 159).

#test heteroscedasticity
ls_tot <- sum((df_res*log(sigma2_res))[use])/sum(df_res[use])
d <- (sqrt(0.5*df_res))*(log(sigma2_res) - ls_tot)
(H <- sum((d^2)[use]))

#the associated p-value is:
1-pchisq(H, sum(use)-1)
length(use)

#this will plot d, which is Gausian if there is level-one homoscedasticity
qqnorm(d[use])
qqline(d[use])

### ### ###

#Example 10.2
#the goal here is to inspect level-one residuals, 
#step one is to plot the unstandardised OLS residuals against level-one explanatory variables to check for non-linearity (there are none here)
#step two is to make a normal probability plot of the standardised OLS residuals to check the assumption of a normal distribution

### MAKE A NORMAL PROBABILITY PLOT OF THE STANDARDISED OLS RESIDUALS TO CHECK THE ASSUMPTIONS OF A NORMAL DISTRIBUTION ###

#first define a function that gives the studentized OLS residuals for this model in a given data frame:
res_st5 <- function(x){
  rstudent(lm(fatigued ~ came_with2 + time.lg,
              data=x))
}

#compute within-participant studentized OLS residuals
resi_st <- by(mydata, mydata$Athlete_ID, res_st5)
rs <- unlist(resi_st)

#some of the residuals are NA or exactly 0.
sum(is.na(rs))
sum(rs[!is.na(rs)]==0)

#this is because of the presence of small groups (Snijders & Bosker, 2012)

#make a QQ plot with use the <use> vector defined above (to only include the parkrunners that have enough runs)
qqnorm(rs[use], main = "")
qqline(rs[use])

###################################################################
###                                                             ###
### Residuals at level two                                      ###
###                                                             ###
###################################################################

#Example 10.3
#the goal here is to inspect level-two residuals
#step one is to plot the unstandardised level-two residuals against "relevant level-two variables" to check for non-linearity
#step one will be skipped, since there are no relevant level-two predictor variables
#step two is to make normal probability plots of standardised level-two residuals to check the assumption of a normal distribution, and
#step three is to plot the squared standardised level-two residuals as a function of level-two variables to check homoscedasticity
#step three will be skipped, since there are no relevant level-two predictor variables

#lme4 is used for these checks
detach("package:nlme")
library(lme4)

#this is the model for which the level-two residuals will be inspected (this should be the full model run above)

#get the random effects for this model
re.mod103 <- ranef(hypothesis_3.1, condVar=TRUE, standard=TRUE)

#this will get the posterior means (taking out the columns of random slopes and intercepts for each participant):
postmean  <- re.mod103$Athlete_ID[,1] #first column is the intercept
postmean  <- re.mod103$Athlete_ID$`(Intercept)` #first column is the intercept (this is the same as above)
postslope.came_with <- re.mod103$Athlete_ID$came_with2

#this will get the posterior variances:
postmeanvar <-  attr(re.mod103$Athlete_ID,'postVar')[1,1,]
postslopevar.came_with <-  attr(re.mod103$Athlete_ID,'postVar')[2,2,] 

#these are the comparative variances, cf. Section 4.8. in Snijders & Bosker (2012; p. 65)

#posterior means are computed by taking information from the overall mean and the group mean, which is influenced by the size and variability of the observations within the group
#posterior means, in other words, are the empirical Bayes' estiamtes of the random slopes and intercepts (Snijders & Bosker, 2012; p. 64)

#the parameters of the random part
VarCorr(hypothesis_3.1)

#diagnostic variance is calculated using 4.18 from Snijders & Bosker (2012; p. 65):
diagmeanvar  <- VarCorr(hypothesis_2.1)$Athlete_ID[1,1] - postmeanvar
diagslopevar.came_with <- VarCorr(hypothesis_2.1)$Athlete_ID[2,2] - postslopevar.came_with

### NORMAL PROBABILITY PLOTS OF STANDARDISED LEVEL-TWO RESIDUALS TO CHECK FOR LEVEL-TWO RESIDUAL NORMALITY ###

#make a plot like figure 10.6 (Snijders & Bosker, 2012; p. 166) for the intercepts and both the 'partner_relation.lg' and 'previous_runs.lg' variables:

#for the intercepts
postmean.stand <- postmean/sqrt(diagmeanvar)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#for the 'came_with2' variable
postmean.stand <- postslope.came_with/sqrt(diagslopevar.came_with)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

###################################################################
###                                                             ###
### Influence of level-two units                                ###
###                                                             ###
###################################################################

#(from this package: https://cran.r-project.org/web/packages/influence.ME/influence.ME.pdf)

library(influence.ME)

#check the influence of data points included in the model
alt.est <- influence(hypothesis_3.1, group= "Athlete_ID")

#Cook's distances
cooks.distance.estex(alt.est)
range(cooks.distance.estex(alt.est))
mean(cooks.distance(alt.est))
sd(cooks.distance(alt.est))
hist(cooks.distance(alt.est), breaks = 100, main = "", xlab = "Cook's distance")

################################################################################################################################################

### ASSUMPTION CHECKS (FROM SNIJDERS & BOSKER, 2012) - PARKRUN COMMUNITY COMPONENT ON ENJOYMENT ###

###################################################################
###                                                             ###
### Within - group OLS residuals                                ###
###                                                             ###
###################################################################

#this will run a liner model at every level of the grouping factor that is present in the data set (so, a linear model will be run on every participants' data that is in the data set)

#(nlme cannot be loaded for this to run)
detach("package:lmerTest", unload=TRUE)
detach("package:lme4", unload=TRUE)
detach("package:nlme", unload=TRUE)
library(lme4)

#rename variable
mydata$Community = mydata$parkrun_community_factor

#these participants are extreme outliers; excluding them helps with plot interpretation
mydata.sub = subset(mydata, Athlete_ID != "A687538" & Athlete_ID != "A1463656")

#this will estimate seperate linear models for every level of the grouping variable 
olselist <- lmList(enjoyment ~ 1 + Community | Athlete_ID, data = mydata.sub)

#this runs the main model on the same subset of parkrunners
library(nlme)
summary(mod1 <- lme(enjoyment ~ 1 + Community, random = ~ Community | Athlete_ID, data = mydata.sub)) 

#The compareFits function compares the OLS estimates with the posterior means of the multilevel model. This is done only for the level-1 effects.
#The graph produced below will compare the variance of the OLS regression estimates from each parkrunner to the posterior means calculated in the multilevel model

comp.models <- compareFits(coef(olselist), coef(mod1))
plot(comp.models, ylab = "")

#the plots indicate that variabiliry for the OLS estimates is indeed larger, but less so for the came_with2 variable

#the within-group residual d.f. can be obtained as follows (this is number of runs minus the number of parameters plus 1):

df_res <- sapply(olselist, function(x){x$df.residual})

#to study the outliers for the effect of the Community variable the plot of comp.models shows that the value 1 could be used as to seperate the outliers from the rest.
strong.community.effect <- which(abs(comp.models[,1,"Community"]) > 1) 

#check whether these observations come from relatively small groups - the average number of responses per participant is:
mean_responses = mean(table(mydata.sub$Athlete_ID))

strong.community.effect.table = df_res[strong.community.effect] #smaller than average, but larger groups are also included
mean(strong.community.effect)

#this will give the percentage of these participants who have a total number of survey responses larger than the average
counter = 0

for (i in strong.community.effect.table) {
  
  if (mean_responses < i)
    
    counter = counter + 1
  
}

#percentage of these participants who have fewer than the average amount of survey responses
1 - (counter / length(strong.community.effect.table))

#the relatively small size of these groups may well account for their deviating observed effects of the came_with2 variable; 
#these outliers are thus not very bothering (Snijders & Bosker, 2012)

### TEST FOR HOMOSCEDASTICITY (NULL IS HOMOSCEDASTICITY, AND IT HAS A CHI-SQUARE DISTRIBUTION UNDER THE NULL) ###

#Example 10.1
#make a selection of the participants with a d.f. that is relatively large (Snijders & Bosker, 2012), in this case, at least 101
use <- df_res >= 10

#the number of such participants is:
sum(use)

#now get the within-group residual standard deviations
summary(olselist[[1]])$sigma

#make a plot of residual variance against residual d.f.:
sigma2_res <- sapply(olselist, function(x){(summary(x)$sigma)^2})
plot(df_res, sigma2_res) 

#there are some outliers for the very low residual d.f.

#Formula (10.3); "Used to detect heteroscedasticity in the form of between-group differences in level-one residual variance" (Snijders & Bosker, 2012; p. 159).

#test heteroscedasticity
ls_tot <- sum((df_res*log(sigma2_res))[use])/sum(df_res[use])
d <- (sqrt(0.5*df_res))*(log(sigma2_res) - ls_tot)
(H <- sum((d^2)[use]))

#the associated p-value is:
1-pchisq(H, sum(use)-1)
length(use)

#this will plot d, which is Gausian if there is level-one homoscedasticity
qqnorm(d[use])
qqline(d[use])

### ### ###

#Example 10.2
#the goal here is to inspect level-one residuals, 
#step one is to plot the unstandardised OLS residuals against level-one explanatory variables to check for non-linearity (there are none here)
#step two is to make a normal probability plot of the standardised OLS residuals to check the assumption of a normal distribution

### MAKE A NORMAL PROBABILITY PLOT OF THE STANDARDISED OLS RESIDUALS TO CHECK THE ASSUMPTIONS OF A NORMAL DISTRIBUTION ###

#first define a function that gives the studentized OLS residuals for this model in a given data frame:
res_st5 <- function(x){
  rstudent(lm(enjoyment ~ parkrun_community_factor,
              data=x))
}

#compute within-participant studentized OLS residuals
resi_st <- by(mydata, mydata$Athlete_ID, res_st5)
rs <- unlist(resi_st)

#some of the residuals are NA or exactly 0.
sum(is.na(rs))
sum(rs[!is.na(rs)]==0)

#this is because of the presence of small groups (Snijders & Bosker, 2012)

#make a QQ plot with use the <use> vector defined above (to only include the parkrunners that have enough runs)
qqnorm(rs[use], main = "")
qqline(rs[use])

###################################################################
###                                                             ###
### Residuals at level two                                      ###
###                                                             ###
###################################################################

#Example 10.3
#the goal here is to inspect level-two residuals
#step one is to plot the unstandardised level-two residuals against "relevant level-two variables" to check for non-linearity
#step one will be skipped, since there are no relevant level-two predictor variables
#step two is to make normal probability plots of standardised level-two residuals to check the assumption of a normal distribution, and
#step three is to plot the squared standardised level-two residuals as a function of level-two variables to check homoscedasticity
#step three will be skipped, since there are no relevant level-two predictor variables

#lme4 is used for these checks
detach("package:nlme")
library(lme4)

#this is the model for which the level-two residuals will be inspected (this should be the full model run above)

#get the random effects for this model
re.mod103 <- ranef(hypothesis_3.2, condVar=TRUE, standard=TRUE)

#this will get the posterior means (taking out the columns of random slopes and intercepts for each participant):
postmean  <- re.mod103$Athlete_ID[,1] #first column is the intercept
postmean  <- re.mod103$Athlete_ID$`(Intercept)` #first column is the intercept (this is the same as above)
postslope.pcc <- re.mod103$Athlete_ID$parkrun_community_factor

#this will get the posterior variances:
postmeanvar <-  attr(re.mod103$Athlete_ID,'postVar')[1,1,]
postslopevar.pcc <-  attr(re.mod103$Athlete_ID,'postVar')[2,2,] 

#these are the comparative variances, cf. Section 4.8. in Snijders & Bosker (2012; p. 65)

#posterior means are computed by taking information from the overall mean and the group mean, which is influenced by the size and variability of the observations within the group
#posterior means, in other words, are the empirical Bayes' estiamtes of the random slopes and intercepts (Snijders & Bosker, 2012; p. 64)

#the parameters of the random part
VarCorr(hypothesis_3.1)

#diagnostic variance is calculated using 4.18 from Snijders & Bosker (2012; p. 65):
diagmeanvar  <- VarCorr(hypothesis_2.1)$Athlete_ID[1,1] - postmeanvar
diagslopevar.pcc <- VarCorr(hypothesis_2.1)$Athlete_ID[2,2] - postslopevar.pcc

### NORMAL PROBABILITY PLOTS OF STANDARDISED LEVEL-TWO RESIDUALS TO CHECK FOR LEVEL-TWO RESIDUAL NORMALITY ###

#make a plot like figure 10.6 (Snijders & Bosker, 2012; p. 166) for the intercepts and both the 'partner_relation.lg' and 'previous_runs.lg' variables:

#for the intercepts
postmean.stand <- postmean/sqrt(diagmeanvar)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#for the 'came_with2' variable
postmean.stand <- postslope.came_with/sqrt(diagslopevar.came_with)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

###################################################################
###                                                             ###
### Influence of level-two units                                ###
###                                                             ###
###################################################################

#(from this package: https://cran.r-project.org/web/packages/influence.ME/influence.ME.pdf)

library(influence.ME)

#check the influence of data points included in the model
alt.est <- influence(hypothesis_3.2, group= "Athlete_ID")

#Cook's distances
cooks.distance.estex(alt.est)
range(cooks.distance.estex(alt.est))
mean(cooks.distance(alt.est))
sd(cooks.distance(alt.est))
hist(cooks.distance(alt.est), breaks = 100, main = "", xlab = "Cook's distance")

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 5.1) ###

### ASSUMPTION CHECKS (FROM SNIJDERS & BOSKER, 2012) - WHO PARTICIPANTS CAME OR MET UP WITH ON FELT ENERGY ###

med.fit <- lmer(how_energising ~ came_with2_int + slowed_down2_int + (1 + came_with2_int | Athlete_ID), data = mydata)

###################################################################
###                                                             ###
### Within - group OLS residuals                                ###
###                                                             ###
###################################################################

#normality of residuals
qqnorm(residuals(med.fit), main = "")
qqline(residuals(med.fit))

###################################################################
###                                                             ###
### Residuals at level two                                      ###
###                                                             ###
###################################################################

#Example 10.3
#the goal here is to inspect level-two residuals
#step one is to plot the unstandardised level-two residuals against "relevant level-two variables" to check for non-linearity
#step one will be skipped, since there are no relevant level-two predictor variables
#step two is to make normal probability plots of standardised level-two residuals to check the assumption of a normal distribution, and
#step three is to plot the squared standardised level-two residuals as a function of level-two variables to check homoscedasticity
#step three will be skipped, since there are no relevant level-two predictor variables

#lme4 is used for these checks
detach("package:nlme")
library(lme4)

#this is the model for which the level-two residuals will be inspected (this should be the full model run above)

#get the random effects for this model
re.mod103 <- ranef(med.fit, condVar=TRUE, standard=TRUE)

#this will get the posterior means (taking out the columns of random slopes and intercepts for each participant):
postmean  <- re.mod103$Athlete_ID[,1] #first column is the intercept
postmean  <- re.mod103$Athlete_ID$`(Intercept)` #first column is the intercept (this is the same as above)
postslope.came_with <- re.mod103$Athlete_ID$came_with2_int

#this will get the posterior variances:
postmeanvar <-  attr(re.mod103$Athlete_ID,'postVar')[1,1,]
postslopevar.came_with <-  attr(re.mod103$Athlete_ID,'postVar')[2,2,] 

#these are the comparative variances, cf. Section 4.8. in Snijders & Bosker (2012; p. 65)

#posterior means are computed by taking information from the overall mean and the group mean, which is influenced by the size and variability of the observations within the group
#posterior means, in other words, are the empirical Bayes' estiamtes of the random slopes and intercepts (Snijders & Bosker, 2012; p. 64)

#the parameters of the random part
VarCorr(med.fit)

#diagnostic variance is calculated using 4.18 from Snijders & Bosker (2012; p. 65):
diagmeanvar  <- VarCorr(med.fit)$Athlete_ID[1,1] - postmeanvar
diagslopevar.came_with <- VarCorr(med.fit)$Athlete_ID[2,2] - postslopevar.came_with

### NORMAL PROBABILITY PLOTS OF STANDARDISED LEVEL-TWO RESIDUALS TO CHECK FOR LEVEL-TWO RESIDUAL NORMALITY ###

#make a plot like figure 10.6 (Snijders & Bosker, 2012; p. 166) for the intercepts and both the 'partner_relation.lg' and 'previous_runs.lg' variables:

#for the intercepts
postmean.stand <- postmean/sqrt(diagmeanvar)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#for the 'came_with2' variable
postslope.stand <- postslope.came_with/sqrt(diagslopevar.came_with)
qqnorm(postslope.stand, main = "")
qqline(postslope.stand)

###################################################################
###                                                             ###
### Influence of level-two units                                ###
###                                                             ###
###################################################################

#(from this package: https://cran.r-project.org/web/packages/influence.ME/influence.ME.pdf)

library(influence.ME)

#check the influence of data points included in the model
alt.est <- influence(med.fit, group= "Athlete_ID")

#Cook's distances
cooks.distance.estex(alt.est)
range(cooks.distance.estex(alt.est))
mean(cooks.distance(alt.est))
sd(cooks.distance(alt.est))
hist(cooks.distance(alt.est), breaks = 100, main = "", xlab = "Cook's distance")

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 5.1) ###

### ASSUMPTION CHECKS (FROM SNIJDERS & BOSKER, 2012) - WHO PARTICIPANTS CAME OR MET UP WITH ON RUN TIMES, CONTROLLING FOR FELT ENERGY ###

out.fit <- lmer(time.lg ~ how_energising + came_with2_int + slowed_down2_int + (1 + came_with2_int| Athlete_ID), data = mydata) #more complex random effects structures don't converge

###################################################################
###                                                             ###
### Within - group OLS residuals                                ###
###                                                             ###
###################################################################


#(nlme cannot be loaded for this to run)
detach("package:lmerTest", unload=TRUE)
detach("package:lme4", unload=TRUE)
detach("package:nlme", unload=TRUE)
library(lme4)

#rename variables for graph
mydata$Came = mydata$came_with2_int
mydata$Energy = mydata$how_energising
mydata$Slowed = mydata$slowed_down2_int


#this will estimate seperate linear models for every level of the grouping variable 
olselist <- lmList(time.lg ~ 1 + Energy + Came + Slowed | Athlete_ID, data = mydata)

#this runs the main multilevel model
library(nlme)
summary(mod1 <- lme(time.lg ~ 1 + Energy + Came + Slowed, random = ~ Came | Athlete_ID, data = mydata, na.action = na.omit))

#The compareFits function compares the OLS estimates with the posterior means of the multilevel model. This is done only for the level-1 effects.
#The graph produced below will compare the variance of the OLS regression estimates from each participant to the posterior means calculated in the multilevel model.

comp.models <- compareFits(coef(olselist), coef(mod1))
plot(comp.models, ylab = "")

#the plots indicate that variabiliry for the OLS estimates is indeed larger, but less so for the came_with2 variable

#this will get the within-group residual d.f.
#first see how olselist is structured:
str(olselist, 1)

#it is a list of results for each of the participants
str(olselist[[1]], 1)

#the within-group residual d.f. can be obtained as follows (this is number of runs minus the number of parameters plus 1):

df_res <- sapply(olselist, function(x){x$df.residual})
table(df_res)

#to study the outliers for the effect of the came_with2 variable the plot of comp.models shows that the value 1 could be used as to seperate the outliers from the rest.
strong.came_with.effect <- which(abs(comp.models[,1,"came_with2_int"]) > .1) 

#check whether these observations come from relatively small groups - the average number of responses per participant is:
mean_responses = mean(table(mydata$Athlete_ID))

strong.came_with.table = df_res[strong.came_with.effect] #smaller than average, but larger groups are also included
mean(strong.came_with.table)

#this will give the percentage of participants who have a total number of responses greater than the average
counter = 0

for (i in strong.came_with.table) {
  
  if (mean_runs < i)
    
    counter = counter + 1
  
}

#percentage of these participants who have fewer than the average amount of survey responses
1 - (counter / length(strong.came_with.table))

#the relatively small size of these groups may well account for their deviating observed effects of the came_with2 variable; 
#these outliers are thus not very bothering (Snijders & Bosker, 2012)

### TEST FOR HOMOSCEDASTICITY (NULL IS HOMOSCEDASTICITY, AND IT HAS A CHI-SQUARE DISTRIBUTION UNDER THE NULL) ###

#Example 10.1
#make a selection of the participants with a d.f. that is relatively large (Snijders & Bosker, 2012), in this case, at least 9 (gives 20 participants)
use <- df_res >= 7

#the number of such participants is:
sum(use)

#now get the within-group residual standard deviations
summary(olselist[[1]])$sigma

#make a plot of residual variance against residual d.f.:
sigma2_res <- sapply(olselist, function(x){(summary(x)$sigma)^2})
plot(df_res, sigma2_res) 

#there are some outliers for the very low residual d.f.

#Formula (10.3); "Used to detect heteroscedasticity in the form of between-group differences in level-one residual variance" (Snijders & Bosker, 2012; p. 159).

#test heteroscedasticity
ls_tot <- sum((df_res*log(sigma2_res))[use])/sum(df_res[use])
d <- (sqrt(0.5*df_res))*(log(sigma2_res) - ls_tot)
(H <- sum((d^2)[use]))

#the associated p-value is:
1-pchisq(H, sum(use)-1)
length(use)

#this will plot d, which is Gausian if there is level-one homoscedasticity
qqnorm(d[use])
qqline(d[use])

### ### ###

#Example 10.2
#the goal here is to inspect level-one residuals, 
#step one is to plot the unstandardised OLS residuals against level-one explanatory variables to check for non-linearity (there are none here)
#step two is to make a normal probability plot of the standardised OLS residuals to check the assumption of a normal distribution

### MAKE A NORMAL PROBABILITY PLOT OF THE STANDARDISED OLS RESIDUALS TO CHECK THE ASSUMPTIONS OF A NORMAL DISTRIBUTION ###

#first define a function that gives the studentized OLS residuals for this model in a given data frame:
res_st5 <- function(x){
  rstudent(lm(time.lg ~ Energy + Came + Slowed,
              data=x))
}

#compute within-participant studentized OLS residuals
resi_st <- by(mydata, mydata$Athlete_ID, res_st5)
rs <- unlist(resi_st)

#some of the residuals are NA or exactly 0.
sum(is.na(rs))
sum(rs[!is.na(rs)]==0)

#this is because of the presence of small groups (Snijders & Bosker, 2012)

#make a QQ plot with use the <use> vector defined above (to only include the parkrunners that have enough runs)
qqnorm(rs[use], main = "")
qqline(rs[use])

###################################################################
###                                                             ###
### Residuals at level two                                      ###
###                                                             ###
###################################################################

#Example 10.3
#the goal here is to inspect level-two residuals
#step one is to plot the unstandardised level-two residuals against "relevant level-two variables" to check for non-linearity
#step one will be skipped, since there are no relevant level-two predictor variables
#step two is to make normal probability plots of standardised level-two residuals to check the assumption of a normal distribution, and
#step three is to plot the squared standardised level-two residuals as a function of level-two variables to check homoscedasticity
#step three will be skipped, since there are no relevant level-two predictor variables

#lme4 is used for these checks
detach("package:nlme")
library(lme4)

#this is the model for which the level-two residuals will be inspected (this should be the full model run above)

#get the random effects for this model
re.mod103 <- ranef(out.fit, condVar=TRUE, standard=TRUE)

#this will get the posterior means (taking out the columns of random slopes and intercepts for each participant):
postmean  <- re.mod103$Athlete_ID[,1] #first column is the intercept
postmean  <- re.mod103$Athlete_ID$`(Intercept)` #first column is the intercept (this is the same as above)
postslope.came_with <- re.mod103$Athlete_ID$came_with2_int

#this will get the posterior variances:
postmeanvar <-  attr(re.mod103$Athlete_ID,'postVar')[1,1,]
postslopevar.came_with <-  attr(re.mod103$Athlete_ID,'postVar')[2,2,] 

#these are the comparative variances, cf. Section 4.8. in Snijders & Bosker (2012; p. 65)

#posterior means are computed by taking information from the overall mean and the group mean, which is influenced by the size and variability of the observations within the group
#posterior means, in other words, are the empirical Bayes' estiamtes of the random slopes and intercepts (Snijders & Bosker, 2012; p. 64)

#the parameters of the random part
VarCorr(med.fit)

#diagnostic variance is calculated using 4.18 from Snijders & Bosker (2012; p. 65):
diagmeanvar  <- VarCorr(med.fit)$Athlete_ID[1,1] - postmeanvar
diagslopevar.came_with <- VarCorr(med.fit)$Athlete_ID[2,2] - postslopevar.came_with

### NORMAL PROBABILITY PLOTS OF STANDARDISED LEVEL-TWO RESIDUALS TO CHECK FOR LEVEL-TWO RESIDUAL NORMALITY ###

#make a plot like figure 10.6 (Snijders & Bosker, 2012; p. 166) for the intercepts and both the 'partner_relation.lg' and 'previous_runs.lg' variables:

#for the intercepts
postmean.stand <- postmean/sqrt(diagmeanvar)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#for the 'came_with2' variable
postslope.stand <- postslope.came_with/sqrt(abs(diagslopevar.came_with))
qqnorm(postslope.stand, main = "")
qqline(postslope.stand)

###################################################################
###                                                             ###
### Influence of level-two units                                ###
###                                                             ###
###################################################################

#(from this package: https://cran.r-project.org/web/packages/influence.ME/influence.ME.pdf)

library(influence.ME)

#check the influence of data points included in the model
alt.est <- influence(out.fit, group= "Athlete_ID")

#Cook's distances
cooks.distance.estex(alt.est)
range(cooks.distance.estex(alt.est))
mean(cooks.distance(alt.est))
sd(cooks.distance(alt.est))
hist(cooks.distance(alt.est), breaks = 100, main = "", xlab = "Cook's distance")

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 5.1) ###

### MEDIATION ASSUMPTIONS ###

library(mediation)
set.seed(2014)
detach("package:lmerTest", unload=TRUE)

med.fit <- lmer(how_energising ~ came_with2_int + slowed_down2_int + (1 + came_with2_int | Athlete_ID), data = mydata)
summary(med.fit)
out.fit <- lmer(time.lg ~ how_energising + came_with2_int + slowed_down2_int + (1 + came_with2_int| Athlete_ID), data = mydata) #more complex random effects structures don't converge
summary(out.fit)

#mediation analyses rely upon the sequential ignorability assumption (which cannot be tested with lmer models)
med.fit.lm <- lm(how_energising ~ came_with2_int + slowed_down2_int + Athlete_ID, data = mydata)
out.fit.lm <- lm(time.lg ~ came_with2_int + how_energising + slowed_down2_int + Athlete_ID, data = mydata)


med.out.lm <- mediate(med.fit.lm, out.fit.lm, treat = "came_with2_int", mediator = "how_energising", covariates = c("slowed_down2", "Athlete_ID"), sims = 1000, boot.ci.type = "bca")
summary(med.out.lm)

#sensitivity analysis for possible existence of unobserved pre-treatment covariates (Imai et al., 2010)
sens.out <- medsens(med.out.lm, rho.by = 0.1, effect.type = "indirect", sims = 100)
summary(sens.out)

#compare this plot to that in Imai et al., 2010 (p. 64)
plot(sens.out, sens.par = "rho", main = "how_energising", ylim = c(-.01, .01))
#the above (plot and summary) imply that the conclusion about the direction of the ACME under Assumption 1 would be maintained unless rho (p) is less than -0.1 (where the line intersects with 0)

#this is the alternative method for the sensitivity analysis
#sign.prod = "negative"; proposed confounder affects mediator and outcome in opposite directions
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive", main = "Positive confounder", ylim = c(-0.1, 0.3), xlim = c(0.0, 0.5))
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "negative", main = "Negative confounder", ylim = c(-0.1, 0.3), xlim = c(0.0, 0.5))

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 5.2) ###

med.fit <- lmer(how_energising ~ parkrun_community_factor + slowed_down2_int + (1 + parkrun_community_factor | Athlete_ID), data = mydata)

### ASSUMPTION CHECKS (FROM SNIJDERS & BOSKER, 2012) - PARKRUN COMMUNITY COMPONENT ON RUN TIMES, CONTROLLING FOR FELT ENERGY ###

###################################################################
###                                                             ###
### Within - group OLS residuals                                ###
###                                                             ###
###################################################################

#this will run a liner model at every level of the grouping factor that is present in the data set (so, a linear model will be run on every participants' data that is in the data set)

#(nlme cannot be loaded for this to run)
detach("package:lmerTest", unload=TRUE)
detach("package:lme4", unload=TRUE)
detach("package:nlme", unload=TRUE)
library(lme4)

#rename variables for graph
mydata$Energy = mydata$how_energising
mydata$Slowed = mydata$slowed_down2_int
mydata$Community = mydata$parkrun_community_factor

#these participants are extreme outliers; excluding them helps with plot interpretation
mydata.sub = subset(mydata, Athlete_ID != "A687538" & Athlete_ID != "A1463656")

#this will estimate seperate linear models for every level of the grouping variable 
olselist <- lmList(how_energising ~ 1 + Community + Slowed | Athlete_ID, data = mydata.sub)

#this runs the main multilevel model
library(nlme)
summary(mod1 <- lme(how_energising ~ 1 + Community + Slowed, random = ~ Community | Athlete_ID, data = mydata.sub, na.action = na.omit))

#The compareFits function compares the OLS estimates with the posterior means of the multilevel model. This is done only for the level-1 effects.
#The graph produced below will compare the variance of the OLS regression estimates from each parkrunner to the posterior means calculated in the multilevel model.

comp.models <- compareFits(coef(olselist), coef(mod1))
plot(comp.models, ylab = "")

#the plots indicate that variabiliry for the OLS estimates is indeed larger, but less so for the came_with2 variable

#the within-group residual d.f. can be obtained as follows (this is number of runs minus the number of parameters plus 1):

df_res <- sapply(olselist, function(x){x$df.residual})

#to study the outliers for the effect of the Community variable the plot of comp.models shows that the value 1 could be used as to seperate the outliers from the rest.
strong.community.effect <- which(abs(comp.models[,1,"Community"]) > 1) 

#check whether these observations come from relatively small groups - the average number of responses per participant is:
mean_responses = mean(table(mydata.sub$Athlete_ID))

strong.community.effect.table = df_res[strong.community.effect] #smaller than average, but larger groups are also included
mean(strong.community.effect)

#this will give the percentage of these participants who have a total number of survey responses larger than the average
counter = 0

for (i in strong.community.effect.table) {
  
  if (mean_responses < i)
    
    counter = counter + 1
  
}

#percentage of these participants who have fewer than the average amount of survey responses
1 - (counter / length(strong.community.effect.table))

#the relatively small size of these groups may well account for their deviating observed effects of the came_with2 variable; 
#these outliers are thus not very bothering (Snijders & Bosker, 2012)

### TEST FOR HOMOSCEDASTICITY (NULL IS HOMOSCEDASTICITY, AND IT HAS A CHI-SQUARE DISTRIBUTION UNDER THE NULL) ###

#Example 10.1
#make a selection of the participants with a d.f. that is relatively large (Snijders & Bosker, 2012), in this case, at least 101
use <- df_res >= 8

#the number of such participants is:
sum(use)

#now get the within-group residual standard deviations
summary(olselist[[1]])$sigma

#make a plot of residual variance against residual d.f.:
sigma2_res <- sapply(olselist, function(x){(summary(x)$sigma)^2})
plot(df_res, sigma2_res) 

#there are some outliers for the very low residual d.f.

#Formula (10.3); "Used to detect heteroscedasticity in the form of between-group differences in level-one residual variance" (Snijders & Bosker, 2012; p. 159).

#test heteroscedasticity
ls_tot <- sum((df_res*log(sigma2_res))[use])/sum(df_res[use])
d <- (sqrt(0.5*df_res))*(log(sigma2_res) - ls_tot)
(H <- sum((d^2)[use]))

#the associated p-value is:
1-pchisq(H, sum(use)-1)
length(use)

#this will plot d, which is Gausian if there is level-one homoscedasticity
qqnorm(d[use])
qqline(d[use])

### ### ###

#Example 10.2
#the goal here is to inspect level-one residuals, 
#step one is to plot the unstandardised OLS residuals against level-one explanatory variables to check for non-linearity (there are none here)
#step two is to make a normal probability plot of the standardised OLS residuals to check the assumption of a normal distribution

### MAKE A NORMAL PROBABILITY PLOT OF THE STANDARDISED OLS RESIDUALS TO CHECK THE ASSUMPTIONS OF A NORMAL DISTRIBUTION ###

#first define a function that gives the studentized OLS residuals for this model in a given data frame:
res_st5 <- function(x){
  rstudent(lm(how_energising ~ Community,
              data=x))
}

#compute within-participant studentized OLS residuals
resi_st <- by(mydata, mydata$Athlete_ID, res_st5)
rs <- unlist(resi_st)

#some of the residuals are NA or exactly 0.
sum(is.na(rs))
sum(rs[!is.na(rs)]==0)

#this is because of the presence of small groups, or participants with few survey responses (Snijders & Bosker, 2012)

#remove participants with residuals of 0
rs.no_zero = subset(rs, (rs[!is.na(rs)]==0))

#make a QQ plot of participants with few survey responses
qqnorm(rs.no_zero, main = "")
qqline(rs.no_zero)

###################################################################
###                                                             ###
### Residuals at level two                                      ###
###                                                             ###
###################################################################

#Example 10.3
#the goal here is to inspect level-two residuals
#step one is to plot the unstandardised level-two residuals against "relevant level-two variables" to check for non-linearity
#step one will be skipped, since there are no relevant level-two predictor variables
#step two is to make normal probability plots of standardised level-two residuals to check the assumption of a normal distribution, and
#step three is to plot the squared standardised level-two residuals as a function of level-two variables to check homoscedasticity
#step three will be skipped, since there are no relevant level-two predictor variables

#lme4 is used for these checks
detach("package:nlme")
library(lme4)

#this is the model for which the level-two residuals will be inspected (this should be the full model run above)

#get the random effects for this model
re.mod103 <- ranef(med.fit, condVar=TRUE, standard=TRUE)

#this will get the posterior means (taking out the columns of random slopes and intercepts for each participant):
postmean  <- re.mod103$Athlete_ID[,1] #first column is the intercept
postmean  <- re.mod103$Athlete_ID$`(Intercept)` #first column is the intercept (this is the same as above)
postslope.pcc <- re.mod103$Athlete_ID$parkrun_community_factor

#this will get the posterior variances:
postmeanvar <-  attr(re.mod103$Athlete_ID,'postVar')[1,1,]
postslopevar.pcc <-  attr(re.mod103$Athlete_ID,'postVar')[2,2,] 

#these are the comparative variances, cf. Section 4.8. in Snijders & Bosker (2012; p. 65)

#posterior means are computed by taking information from the overall mean and the group mean, which is influenced by the size and variability of the observations within the group
#posterior means, in other words, are the empirical Bayes' estiamtes of the random slopes and intercepts (Snijders & Bosker, 2012; p. 64)

#the parameters of the random part
VarCorr(med.fit)

#diagnostic variance is calculated using 4.18 from Snijders & Bosker (2012; p. 65):
diagmeanvar  <- VarCorr(med.fit)$Athlete_ID[1,1] - postmeanvar
diagslopevar.pcc <- VarCorr(med.fit)$Athlete_ID[2,2] - postslopevar.pcc

### NORMAL PROBABILITY PLOTS OF STANDARDISED LEVEL-TWO RESIDUALS TO CHECK FOR LEVEL-TWO RESIDUAL NORMALITY ###

#make a plot like figure 10.6 (Snijders & Bosker, 2012; p. 166) for the intercepts and both the 'partner_relation.lg' and 'previous_runs.lg' variables:

#for the intercepts
postmean.stand <- postmean/sqrt(diagmeanvar)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#for the 'came_with2' variable
postmean.stand <- postslope.pcc/sqrt(diagslopevar.pcc)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

###################################################################
###                                                             ###
### Influence of level-two units                                ###
###                                                             ###
###################################################################

#(from this package: https://cran.r-project.org/web/packages/influence.ME/influence.ME.pdf)

library(influence.ME)

#check the influence of data points included in the model
alt.est <- influence(med.fit, group= "Athlete_ID")

#Cook's distances
cooks.distance.estex(alt.est)
range(cooks.distance.estex(alt.est))
mean(cooks.distance(alt.est))
sd(cooks.distance(alt.est))
hist(cooks.distance(alt.est), breaks = 100, main = "", xlab = "Cook's distance")

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 5.2) ###

### ASSUMPTION CHECKS (FROM SNIJDERS & BOSKER, 2012) - PARKRUN COMMUNITY COMPONENT ON RUN TIMES, CONTROLLING FOR FELT ENERGY ###

out.fit <- lmer(time.lg ~ how_energising + parkrun_community_factor + slowed_down2_int + (1 + parkrun_community_factor | Athlete_ID), data = mydata)

###################################################################
###                                                             ###
### Within - group OLS residuals                                ###
###                                                             ###
###################################################################

#(nlme cannot be loaded for this to run)
detach("package:lmerTest", unload=TRUE)
detach("package:lme4", unload=TRUE)
detach("package:nlme", unload=TRUE)
library(lme4)

#rename variables for graph
mydata$Community = mydata$parkrun_community_factor
mydata$Energy = mydata$how_energising
mydata$Slowed = mydata$slowed_down2_int

#these participants are extreme outliers; excluding them helps with plot interpretation
mydata.sub = subset(mydata, Athlete_ID != "A1420478")

#this will estimate seperate linear models for every level of the grouping variable 
olselist <- lmList(time.lg ~ 1 + Energy + Community + Slowed | Athlete_ID, data = mydata.sub)

#this runs the main multilevel model
library(nlme)
summary(mod1 <- lme(time.lg ~ 1 + Energy + Community + Slowed, random = ~ Community | Athlete_ID, data = mydata.sub, na.action = na.omit))

#The compareFits function compares the OLS estimates with the posterior means of the multilevel model. This is done only for the level-1 effects.
#The graph produced below will compare the variance of the OLS regression estimates from each participant to the posterior means calculated in the multilevel model.

comp.models <- compareFits(coef(olselist), coef(mod1))
plot(comp.models, ylab = "")

#the plots indicate that variabiliry for the OLS estimates is indeed larger, but less so for the came_with2 variable

#this will get the within-group residual d.f.
#first see how olselist is structured:
str(olselist, 1)

#it is a list of results for each of the participants
str(olselist[[1]], 1)

#the within-group residual d.f. can be obtained as follows (this is number of runs minus the number of parameters plus 1):

df_res <- sapply(olselist, function(x){x$df.residual})
table(df_res)

#to study the outliers for the effect of the community component variable the plot of comp.models shows that the value .075 could be used as to seperate the outliers from the rest
strong.pcc.effect <- which(abs(comp.models[,1,"Community"]) > .075) 

#check whether these observations come from relatively small groups - the average number of responses per participant is:
mean_responses = mean(table(mydata$Athlete_ID))

strong.pcc.table = df_res[strong.pcc.effect] #all have one or two obvervations
mean(strong.pcc.table)

#this will give the percentage of participants who have a total number of responses greater than the average
counter = 0

for (i in strong.pcc.table) {
  
  if (mean_responses < i)
    
    counter = counter + 1
  
}

#percentage of these participants who have fewer than the average amount of survey responses
1 - (counter / length(strong.pcc.table))

#the relatively small size of these groups may well account for their deviating observed effects of the came_with2 variable; 
#these outliers are thus not very bothering (Snijders & Bosker, 2012)

### TEST FOR HOMOSCEDASTICITY (NULL IS HOMOSCEDASTICITY, AND IT HAS A CHI-SQUARE DISTRIBUTION UNDER THE NULL) ###

#Example 10.1
#make a selection of the participants with a d.f. that is relatively large (Snijders & Bosker, 2012), in this case, at least 9 (gives 20 participants)
use <- df_res >= 7

#the number of such participants is:
sum(use)

#now get the within-group residual standard deviations
summary(olselist[[1]])$sigma

#make a plot of residual variance against residual d.f.:
sigma2_res <- sapply(olselist, function(x){(summary(x)$sigma)^2})
plot(df_res, sigma2_res) 

#there are some outliers for the very low residual d.f.

#Formula (10.3); "Used to detect heteroscedasticity in the form of between-group differences in level-one residual variance" (Snijders & Bosker, 2012; p. 159).

#test heteroscedasticity
ls_tot <- sum((df_res*log(sigma2_res))[use])/sum(df_res[use])
d <- (sqrt(0.5*df_res))*(log(sigma2_res) - ls_tot)
(H <- sum((d^2)[use]))

#the associated p-value is:
1-pchisq(H, sum(use)-1)
length(use)

#this will plot d, which is Gausian if there is level-one homoscedasticity
qqnorm(d[use])
qqline(d[use])

### ### ###

#Example 10.2
#the goal here is to inspect level-one residuals, 
#step one is to plot the unstandardised OLS residuals against level-one explanatory variables to check for non-linearity (there are none here)
#step two is to make a normal probability plot of the standardised OLS residuals to check the assumption of a normal distribution

### MAKE A NORMAL PROBABILITY PLOT OF THE STANDARDISED OLS RESIDUALS TO CHECK THE ASSUMPTIONS OF A NORMAL DISTRIBUTION ###

#first define a function that gives the studentized OLS residuals for this model in a given data frame:
res_st5 <- function(x){
  rstudent(lm(time.lg ~ Energy + Community + Slowed,
              data=x))
}

#compute within-participant studentized OLS residuals
resi_st <- by(mydata, mydata$Athlete_ID, res_st5)
rs <- unlist(resi_st)

#some of the residuals are NA or exactly 0.
sum(is.na(rs))
sum(rs[!is.na(rs)]==0)

#this is because of the presence of small groups (Snijders & Bosker, 2012)

#make a QQ plot with use the <use> vector defined above (to only include the parkrunners that have enough runs)
qqnorm(rs[use], main = "")
qqline(rs[use])

###################################################################
###                                                             ###
### Residuals at level two                                      ###
###                                                             ###
###################################################################

#Example 10.3
#the goal here is to inspect level-two residuals
#step one is to plot the unstandardised level-two residuals against "relevant level-two variables" to check for non-linearity
#step one will be skipped, since there are no relevant level-two predictor variables
#step two is to make normal probability plots of standardised level-two residuals to check the assumption of a normal distribution, and
#step three is to plot the squared standardised level-two residuals as a function of level-two variables to check homoscedasticity
#step three will be skipped, since there are no relevant level-two predictor variables

#lme4 is used for these checks
detach("package:nlme")
library(lme4)

#this is the model for which the level-two residuals will be inspected (this should be the full model run above)

#get the random effects for this model
re.mod103 <- ranef(out.fit, condVar=TRUE, standard=TRUE)

#this will get the posterior means (taking out the columns of random slopes and intercepts for each participant):
postmean  <- re.mod103$Athlete_ID[,1] #first column is the intercept
postmean  <- re.mod103$Athlete_ID$`(Intercept)` #first column is the intercept (this is the same as above)
postslope.pcc <- re.mod103$Athlete_ID$parkrun_community_factor

#this will get the posterior variances:
postmeanvar <-  attr(re.mod103$Athlete_ID,'postVar')[1,1,]
postslopevar.came_with <-  attr(re.mod103$Athlete_ID,'postVar')[2,2,] 

#these are the comparative variances, cf. Section 4.8. in Snijders & Bosker (2012; p. 65)

#posterior means are computed by taking information from the overall mean and the group mean, which is influenced by the size and variability of the observations within the group
#posterior means, in other words, are the empirical Bayes' estiamtes of the random slopes and intercepts (Snijders & Bosker, 2012; p. 64)

#the parameters of the random part
VarCorr(med.fit)

#diagnostic variance is calculated using 4.18 from Snijders & Bosker (2012; p. 65):
diagmeanvar  <- VarCorr(med.fit)$Athlete_ID[1,1] - postmeanvar
diagslopevar.pcc <- VarCorr(med.fit)$Athlete_ID[2,2] - postslopevar.pcc

### NORMAL PROBABILITY PLOTS OF STANDARDISED LEVEL-TWO RESIDUALS TO CHECK FOR LEVEL-TWO RESIDUAL NORMALITY ###

#make a plot like figure 10.6 (Snijders & Bosker, 2012; p. 166) for the intercepts and both the 'partner_relation.lg' and 'previous_runs.lg' variables:

#for the intercepts
postmean.stand <- postmean/sqrt(diagmeanvar)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#for the 'parkrun community component' variable
postslope.stand <- postslope.came_with/sqrt(abs(diagslopevar.came_with))
qqnorm(postslope.stand, main = "")
qqline(postslope.stand)

###################################################################
###                                                             ###
### Influence of level-two units                                ###
###                                                             ###
###################################################################

#(from this package: https://cran.r-project.org/web/packages/influence.ME/influence.ME.pdf)

library(influence.ME)

#check the influence of data points included in the model
alt.est <- influence(out.fit, group= "Athlete_ID")

#Cook's distances
cooks.distance.estex(alt.est)
range(cooks.distance.estex(alt.est))
mean(cooks.distance(alt.est))
sd(cooks.distance(alt.est))
hist(cooks.distance(alt.est), breaks = 100, main = "", xlab = "Cook's distance")

################################################################################################################################################

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 5.2) ###

med.out <- mediate(med.fit, out.fit, treat = "parkrun_community_factor", mediator = "how_energising", covariates = "slowed_down2_int", sims = 1000, boot.ci.type = "bca")

### MEDIATION ASSUMPTIONS ###

library(mediation)
set.seed(2014)
detach("package:lmerTest", unload=TRUE)

med.fit <- lmer(how_energising ~ parkrun_community_factor + slowed_down2_int + (1 + parkrun_community_factor | Athlete_ID), data = mydata)
summary(med.fit)
out.fit <- lmer(time.lg ~ how_energising + parkrun_community_factor + slowed_down2_int + (1 + parkrun_community_factor| Athlete_ID), data = mydata) #more complex random effects structures don't converge
summary(out.fit)

#mediation analyses rely upon the sequential ignorability assumption (which cannot be tested with lmer models)
med.fit.lm <- lm(how_energising ~ parkrun_community_factor + slowed_down2_int + Athlete_ID, data = mydata)
out.fit.lm <- lm(time.lg ~ parkrun_community_factor + how_energising + slowed_down2_int + Athlete_ID, data = mydata)

med.out.lm <- mediate(med.fit.lm, out.fit.lm, treat = "parkrun_community_factor", mediator = "how_energising", covariates = c("slowed_down2", "Athlete_ID"), sims = 1000, boot.ci.type = "bca")
summary(med.out.lm)

#sensitivity analysis for possible existence of unobserved pre-treatment covariates (Imai et al., 2010)
sens.out <- medsens(med.out.lm, rho.by = 0.1, effect.type = "indirect", sims = 100)
summary(sens.out)

#compare this plot to that in Imai et al., 2010 (p. 64)
plot(sens.out, sens.par = "rho", main = "how_energising", ylim = c(-.01, .01))
#the above (plot and summary) imply that the conclusion about the direction of the ACME under Assumption 1 would be maintained unless rho (p) is less than -0.1 (where the line intersects with 0)

#this is the alternative method for the sensitivity analysis
#sign.prod = "negative"; proposed confounder affects mediator and outcome in opposite directions
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive", main = "Positive confounder", ylim = c(-0.1, 0.3), xlim = c(0.0, 0.4))
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "negative", main = "Negative confounder", ylim = c(-0.1, 0.3), xlim = c(0.0, 0.4))

################################################################################################################################################

### HYPOTHESIS 5.3 ###

med.out <- mediate(med.fit, out.fit, treat = "just_before2_int", mediator = "how_energising", covariates = "slowed_down2_int", sims = 1000, boot.ci.type = "bca")
summary(med.out)

### MULTILEVEL MEDIATION MODELS (HYPOTHESIS 5.2) ###

### MEDIATION ASSUMPTIONS ###

library(mediation)
set.seed(2014)
detach("package:lmerTest", unload=TRUE)

#mediation analyses rely upon the sequential ignorability assumption (which cannot be tested with lmer models)
med.fit.lm <- lm(how_energising ~ just_before2_int + slowed_down2_int + Athlete_ID, data = mydata)
out.fit.lm <- lm(time.lg ~ just_before2_int + how_energising + slowed_down2_int + Athlete_ID, data = mydata)

med.out.lm <- mediate(med.fit.lm, out.fit.lm, treat = "just_before2_int", mediator = "how_energising", covariates = c("slowed_down2", "Athlete_ID"), sims = 1000, boot.ci.type = "bca")
summary(med.out.lm)

#sensitivity analysis for possible existence of unobserved pre-treatment covariates (Imai et al., 2010)
sens.out <- medsens(med.out.lm, rho.by = 0.1, effect.type = "indirect", sims = 100)
summary(sens.out)

#compare this plot to that in Imai et al., 2010 (p. 64)
plot(sens.out, sens.par = "rho", main = "how_energising", ylim = c(-.01, .01))
#the above (plot and summary) imply that the conclusion about the direction of the ACME under Assumption 1 would be maintained unless rho (p) is less than -0.1 (where the line intersects with 0)

#this is the alternative method for the sensitivity analysis
#sign.prod = "negative"; proposed confounder affects mediator and outcome in opposite directions
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive", main = "Positive confounder", ylim = c(-0.1, 0.3), xlim = c(0.0, 0.5))
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "negative", main = "Negative confounder", ylim = c(-0.1, 0.3), xlim = c(0.0, 0.5))
