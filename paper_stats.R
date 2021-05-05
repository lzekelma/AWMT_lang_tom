
library(HistogramTools)

setwd("/Users/leozekelman/Dropbox (Partners HealthCare)/HCP_S1200")
total_data <- read.csv("total_data.csv")

genetic_tested <- total_data[total_data$HasGT,] #915 

mono <- genetic_tested[genetic_tested$ZygosityGT == "MZ",] # everything but monozygote
#dy <- genetic_tested[genetic_tested$ZygosityGT == "DZ",]
#not_twin <- genetic_tested[genetic_tested$ZygosityGT == " ",]
not_mono <- genetic_tested[genetic_tested$ZygosityGT != "MZ",]

#uniquemono <- distinct(mono$Family_ID)
mono_singles <- !duplicated(mono$Family_ID)
#dy_singles <- !duplicated(dy$Family_ID)
#not_twin_singles <- !duplicated(not_twin$Family_ID)

mono_unique_subset <- mono[mono_singles,]
#dy_unique_subset <- dy[dy_singles,]
#not_twin_unique_subset <- not_twin[not_twin_singles,]

data <- rbind(not_mono,mono_unique_subset) #811
# the HCP dataset contains monozygotic and dyzygotic twins, siblings and unrealted participants.
# we include non-twin siblings and dyzygotic twins. 
# we also include only one member of monozygotic twins pairs. 
# 428 of the 811 subjects in this study are unrelated.
# 429 are female, 382 are male
# the minimum age is 22, the maximum age is 36, mean age is 28.64.

data <- data[!is.na(data$ER40_CR),] # we exclude 2 subjects without PERT scores.

length(data$Subject)

males <- subset(data, Gender.x == "M")
females <- subset(data, Gender.x == "F")

length(males$Subject)
length(females$Subject)

summary(data$Age_in_Yrs)

summary(data$Handedness)

right_handers <- subset(data, Handedness >= 40)
length(right_handers$Subject)

hist(data$Handedness, breaks = 100, main = "Histogram of Participant Handednesss", xlab = "Handedness Score", ylab = "Number of Participants")


hist(data$ER40_CR, breaks = 40, main = "Histogram of PERT",
     xlab = "PERT Correct Response Score")

hist(data$PicVocab_Unadj, breaks = 40, main = "Histogram of TPVT",
     xlab = "Unadjusted TPVT Score")
summary(data$PicVocab_Unadj)

length(subset(data, ER40_CR == 40)$Subject)

hist(data$ILF_R_trace1_Mean, breaks = 50)





