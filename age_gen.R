# with only one monozygote data 
# age, gender model
# tracts split

library(lm.beta)

setwd("/Users/leozekelman/Dropbox (Partners HealthCare)/HCP_S1200")
total_data <- read.csv("total_data.csv")

genetic_tested <- total_data[total_data$HasGT,] #915. we include subjects with genetic testing only. 

mono <- genetic_tested[genetic_tested$ZygosityGT == "MZ",] # monozygotic twins
not_mono <- genetic_tested[genetic_tested$ZygosityGT != "MZ",] # everything but monozygote twins

mono_singles <- !duplicated(mono$Family_ID) #one twin sibling from each monozygotic twin pair
mono_unique_subset <- mono[mono_singles,] 

data <- rbind(not_mono,mono_unique_subset) #811
# the HCP dataset contains monozygotic and dyzygotic twins, siblings and unrealted participants.
# we include non-twin siblings and dyzygotic twins. 
# we also include only one member of monozygotic twins pairs. 
# 428 of the 811 subjects in this study are unrelated.
# 429 are female, 382 are male
# the minimum age is 22, the maximum age is 36, mean age is 28.64.

data <- data[!is.na(data$ER40_CR),] # we exclude 2 subjects without PERT scores.

# this function pulls out the p-values from the overall regression models
# i found it on stacks: https://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

###### tracts measures ###### 
tracts_measures_L <- list("ILF_L_FA1_Mean",
                          "ILF_L_FA2_Mean",
                          "ILF_L_Num_Fibers",
                          "ILF_L_trace1_Mean",
                          "ILF_L_trace2_Mean",
                          "IOFF_L_FA1_Mean",
                          "IOFF_L_FA2_Mean",
                          "IOFF_L_Num_Fibers",
                          "IOFF_L_trace1_Mean",
                          "IOFF_L_trace2_Mean",
                          "UF_L_FA1_Mean",
                          "UF_L_FA2_Mean",
                          "UF_L_Num_Fibers",
                          "UF_L_trace1_Mean",
                          "UF_L_trace2_Mean",
                          "CB_L_FA1_Mean",
                          "CB_L_FA2_Mean",
                          "CB_L_Num_Fibers",
                          "CB_L_trace1_Mean",
                          "CB_L_trace2_Mean",
                          "AF_L_FA1_Mean",
                          "AF_L_FA2_Mean",
                          "AF_L_Num_Fibers",
                          "AF_L_trace1_Mean",
                          "AF_L_trace2_Mean",
                          "MdLF_L_FA1_Mean",
                          "MdLF_L_FA2_Mean",
                          "MdLF_L_Num_Fibers",
                          "MdLF_L_trace1_Mean",
                          "MdLF_L_trace2_Mean",
                          "EmC_L_FA1_Mean",
                          "EmC_L_FA2_Mean",
                          "EmC_L_Num_Fibers",
                          "EmC_L_trace1_Mean",
                          "EmC_L_trace2_Mean",
                          "SLF1_L_FA1_Mean",
                          "SLF1_L_FA2_Mean",
                          "SLF1_L_Num_Fibers",
                          "SLF1_L_trace1_Mean",
                          "SLF1_L_trace2_Mean",
                          "SLF2_L_FA1_Mean",
                          "SLF2_L_FA2_Mean",
                          "SLF2_L_Num_Fibers",
                          "SLF2_L_trace1_Mean",
                          "SLF2_L_trace2_Mean",
                          "SLF3_L_FA1_Mean",
                          "SLF3_L_FA2_Mean",
                          "SLF3_L_Num_Fibers",
                          "SLF3_L_trace1_Mean",
                          "SLF3_L_trace2_Mean")

tracts_measures_R <- list("ILF_R_FA1_Mean",
                          "ILF_R_FA2_Mean",
                          "ILF_R_Num_Fibers",
                          "ILF_R_trace1_Mean",
                          "ILF_R_trace2_Mean",
                          "IOFF_R_FA1_Mean",
                          "IOFF_R_FA2_Mean",
                          "IOFF_R_Num_Fibers",
                          "IOFF_R_trace1_Mean",
                          "IOFF_R_trace2_Mean",
                          "UF_R_FA1_Mean",
                          "UF_R_FA2_Mean",
                          "UF_R_Num_Fibers",
                          "UF_R_trace1_Mean",
                          "UF_R_trace2_Mean",
                          "CB_R_FA1_Mean",
                          "CB_R_FA2_Mean",
                          "CB_R_Num_Fibers",
                          "CB_R_trace1_Mean",
                          "CB_R_trace2_Mean",
                          "AF_R_FA1_Mean",
                          "AF_R_FA2_Mean",
                          "AF_R_Num_Fibers",
                          "AF_R_trace1_Mean",
                          "AF_R_trace2_Mean",
                          "MdLF_R_FA1_Mean",
                          "MdLF_R_FA2_Mean",
                          "MdLF_R_Num_Fibers",
                          "MdLF_R_trace1_Mean",
                          "MdLF_R_trace2_Mean",
                          "EmC_R_FA1_Mean",
                          "EmC_R_FA2_Mean",
                          "EmC_R_Num_Fibers",
                          "EmC_R_trace1_Mean",
                          "EmC_R_trace2_Mean",
                          "SLF1_R_FA1_Mean",
                          "SLF1_R_FA2_Mean",
                          "SLF1_R_Num_Fibers",
                          "SLF1_R_trace1_Mean",
                          "SLF1_R_trace2_Mean",
                          "SLF2_R_FA1_Mean",
                          "SLF2_R_FA2_Mean",
                          "SLF2_R_Num_Fibers",
                          "SLF2_R_trace1_Mean",
                          "SLF2_R_trace2_Mean",
                          "SLF3_R_FA1_Mean",
                          "SLF3_R_FA2_Mean",
                          "SLF3_R_Num_Fibers",
                          "SLF3_R_trace1_Mean",
                          "SLF3_R_trace2_Mean")


### Picture Vocabulary Models
# create empty lists
formula <- list(); 
model <- list(); 
r_cor <- list();
r2_cor <- list(); 
p_vals <- list(); 
tract_L_pval <- list();
tract_R_pval <- list();
gender_pval <- list(); 
age_pvals <- list(); 
f_value <- list();
tract_L_tval <- list();
tract_R_tval <- list();
gender_tval<- list();
age_tval<- list();
tract_L_beta <- list();
tract_R_beta <- list();
gender_beta<- list();
age_beta<- list();

# loop that creates a model that predictts picture vocabulary scores using
# different tracts.  this loop also stores the various statistics calculated
# for each model and predictor variable.
for (i in 1:50) {
  formula[[i]] = paste0("PicVocab_Unadj",  " ~ ", 
                        tracts_measures_L[i], "+",
                        tracts_measures_R[i], "+",
                        "Gender.x", "+",
                        "Age_in_Yrs")
  model[[i]] = lm(formula[[i]], data = data) 
  r_cor[[i]] = sqrt(summary(model[[i]])$r.squared)
  r2_cor[[i]] <- summary(model[[i]])$r.squared
  p_vals[[i]] = lmp(model[[i]])
  tract_L_pval[[i]] <- summary(model[[i]])$coefficients[17]
  tract_R_pval[[i]] <- summary(model[[i]])$coefficients[18]
  gender_pval[[i]] <- summary(model[[i]])$coefficients[19]
  age_pvals[[i]] <- summary(model[[i]])$coefficients[20]
  f_value[[i]] <- summary(model[[i]])$fstatistic[[1]]
  tract_L_tval[[i]] <- summary(model[[i]])$coefficients[12]
  tract_R_tval[[i]] <- summary(model[[i]])$coefficients[13]
  gender_tval[[i]] <- summary(model[[i]])$coefficients[14]
  age_tval[[i]] <- summary(model[[i]])$coefficients[15]
  tract_L_beta[[i]] <- coef(lm.beta(model[[i]]))[[2]]
  tract_R_beta[[i]] <- coef(lm.beta(model[[i]]))[[3]]
  gender_beta[[i]] <- coef(lm.beta(model[[i]]))[[4]]
  age_beta[[i]] <- coef(lm.beta(model[[i]]))[[5]]

}

# we bring all these statistics together in one dataframe
pic_vocab_df <- NULL
pic_vocab_df <- as.data.frame(t(t(r_cor)))
colnames(pic_vocab_df) <- c("r_cor")
pic_vocab_df$assessment <- "TPVT"
pic_vocab_df$r2_cor <- as.data.frame(t(t(r2_cor)))
pic_vocab_df$model_pval <- as.data.frame(t(t(p_vals)))
a <- unlist(strsplit(as.character(tracts_measures_R), "_"))
tracts_list <- a[seq(1, length(a), 4)]
measure_list <- a[seq(3, length(a), 4)]
pic_vocab_df$tracts <- tracts_list
pic_vocab_df$measures <- measure_list
pic_vocab_df$tract_L_pval <- as.data.frame(t(t(tract_L_pval)))
pic_vocab_df$tract_R_pval <- as.data.frame(t(t(tract_R_pval)))
pic_vocab_df$gender_pval <- as.data.frame(t(t(gender_pval)))
pic_vocab_df$age_pval <- as.data.frame(t(t(age_pvals)))
pic_vocab_df$model <- "Age_Gender_L_R"
pic_vocab_df$f_value <- as.data.frame(t(t(f_value)))
pic_vocab_df$tract_L_tval <- as.data.frame(t(t(tract_L_tval)))
pic_vocab_df$tract_R_tval <- as.data.frame(t(t(tract_R_tval)))
pic_vocab_df$gender_tval <- as.data.frame(t(t(gender_tval)))
pic_vocab_df$age_tval <- as.data.frame(t(t(age_tval)))
pic_vocab_df$tract_L_beta <- as.data.frame(t(t(tract_L_beta)))
pic_vocab_df$tract_R_beta <- as.data.frame(t(t(tract_R_beta)))
pic_vocab_df$gender_beta <- as.data.frame(t(t(gender_beta)))
pic_vocab_df$age_beta <- as.data.frame(t(t(age_beta)))


### ER40_CR Model
# we repeate the above process but for Emotion Recognition
formula <- list(); 
model <- list(); 
r_cor <- list();
r2_cor <- list(); 
p_vals <- list(); 
tract_L_pval <- list();
tract_R_pval <- list();
gender_pval <- list(); 
age_pvals <- list(); 
f_value <- list();
tract_L_tval <- list();
tract_R_tval <- list();
gender_tval<- list();
age_tval<- list();
tract_L_beta <- list();
tract_R_beta <- list();
gender_beta<- list();
age_beta<- list();

for (i in 1:50) {
  formula[[i]] = paste0("ER40_CR",  " ~ ", 
                        tracts_measures_L[i], "+",
                        tracts_measures_R[i], "+",
                        "Gender.x", "+",
                        "Age_in_Yrs")
  model[[i]] = lm(formula[[i]], data = data) 
  r_cor[[i]] = sqrt(summary(model[[i]])$r.squared)
  r2_cor[[i]] <- summary(model[[i]])$r.squared
  p_vals[[i]] = lmp(model[[i]])
  tract_L_pval[[i]] <- summary(model[[i]])$coefficients[17]
  tract_R_pval[[i]] <- summary(model[[i]])$coefficients[18]
  gender_pval[[i]] <- summary(model[[i]])$coefficients[19]
  age_pvals[[i]] <- summary(model[[i]])$coefficients[20]
  f_value[[i]] <- summary(model[[i]])$fstatistic[[1]]
  tract_L_tval[[i]] <- summary(model[[i]])$coefficients[12]
  tract_R_tval[[i]] <- summary(model[[i]])$coefficients[13]
  gender_tval[[i]] <- summary(model[[i]])$coefficients[14]
  age_tval[[i]] <- summary(model[[i]])$coefficients[15]
  tract_L_beta[[i]] <- coef(lm.beta(model[[i]]))[[2]]
  tract_R_beta[[i]] <- coef(lm.beta(model[[i]]))[[3]]
  gender_beta[[i]] <- coef(lm.beta(model[[i]]))[[4]]
  age_beta[[i]] <- coef(lm.beta(model[[i]]))[[5]]
  
}

er_df <- NULL
er_df <- as.data.frame(t(t(r_cor)))
colnames(er_df) <- c("r_cor")
er_df$assessment <- "PERT"
er_df$r2_cor <- as.data.frame(t(t(r2_cor)))
er_df$model_pval <- as.data.frame(t(t(p_vals)))
a <- unlist(strsplit(as.character(tracts_measures_R), "_"))
tracts_list <- a[seq(1, length(a), 4)]
measure_list <- a[seq(3, length(a), 4)]
er_df$tracts <- tracts_list
er_df$measures <- measure_list
er_df$tract_L_pval <- as.data.frame(t(t(tract_L_pval)))
er_df$tract_R_pval <- as.data.frame(t(t(tract_R_pval)))
er_df$gender_pval <- as.data.frame(t(t(gender_pval)))
er_df$age_pval <- as.data.frame(t(t(age_pvals)))
er_df$model <- "Age_Gender_L_R"
er_df$f_value <- as.data.frame(t(t(f_value)))
er_df$tract_L_tval <- as.data.frame(t(t(tract_L_tval)))
er_df$tract_R_tval <- as.data.frame(t(t(tract_R_tval)))
er_df$gender_tval <- as.data.frame(t(t(gender_tval)))
er_df$age_tval <- as.data.frame(t(t(age_tval)))
er_df$tract_L_beta <- as.data.frame(t(t(tract_L_beta)))
er_df$tract_R_beta <- as.data.frame(t(t(tract_R_beta)))
er_df$gender_beta <- as.data.frame(t(t(gender_beta)))
er_df$age_beta <- as.data.frame(t(t(age_beta)))

# FDR corrections are performed in another script. 


library("effects")
# we investigte effect plots for two tracts... 

#### SLF-III #####
SLF3_NoF_T <- lm(PicVocab_Unadj ~ SLF3_L_Num_Fibers + SLF3_R_Num_Fibers + Age_in_Yrs + Gender.x, data = data)
SLF3_NoF_P <- lm(ER40_CR ~ SLF3_L_Num_Fibers + SLF3_R_Num_Fibers + Age_in_Yrs + Gender.x, data = data)

plot(predictorEffect("SLF3_L_Num_Fibers",SLF3_NoF_T,residuals=FALSE),
     partial.residuals=list(smooth=FALSE),
     main = "NoF of left SLF-III predictor effect plot",
     ylab = "TPVT",
     xlab = "NoF of left SLF-III",
     band.transparency = ".5",
     band.colors = "black",
     lines  = list(lty = 1, lwd = 2, col = "black"),
     axes = list( y = list(cex = 2, lim = c(110,124)),
                  x=list(cex = 2)))

plot(predictorEffect("SLF3_R_Num_Fibers",SLF3_NoF_T,residuals=FALSE),
     partial.residuals=list(smooth=FALSE),
     main = "NoF of right SLF-III predictor effect plot",
     ylab = "TPVT",
     xlab = "NoF of right SLF-III",
     band.transparency = ".5",
     band.colors = "black",
     lines  = list(lty = 1, lwd = 2, col = "black"),
     axes = list( y = list(cex = 2, lim = c(110,124)),
                  x=list(cex = 2)))


plot(predictorEffect("SLF3_L_Num_Fibers",SLF3_NoF_P,residuals=FALSE),
     partial.residuals=list(smooth=FALSE, col = "darkviolet"),
     main = "NoF of left SLF-III predictor effect plot",
     ylab = "PERT",
     xlab = "NoF of left SLF-III",
     band.transparency = ".3",
     band.colors = "lightgray",
     lines = list(lty = 1, lwd = 2, col = "black"),
     axes = list( x=list(cex = 2),
                  y=list(cex = 2, lim = c(33.5,37.5))))



plot(predictorEffect("SLF3_R_Num_Fibers",SLF3_NoF_P,residuals=FALSE),
     partial.residuals=list(smooth=FALSE, col = "darkviolet"),
     main = "NoF of right SLF-III predictor effect plot",
     ylab = "PERT",
     xlab = "NoF of right SLF-III",
     band.transparency = ".3",
     band.colors = "lightgray",
     lines = list(lty = 1, lwd = 2, col = "black"),
     axes = list( x=list(cex = 2),
                  y=list(cex = 2, lim = c(33.5,37.5))))



#### AF ####
AF_FA_T <- lm(PicVocab_Unadj ~ AF_L_FA1_Mean + AF_R_FA1_Mean + Age_in_Yrs + Gender.x, data = data)
AF_FA_P <- lm(ER40_CR ~ AF_L_FA1_Mean + AF_R_FA1_Mean + Age_in_Yrs + Gender.x, data = data)

plot(predictorEffect("AF_L_FA1_Mean",AF_FA_T,residuals=FALSE),
     partial.residuals=list(smooth=FALSE),
     main = "FA left AF predictor effect plot",
     ylab = "TPVT",
     xlab = "FA of left AF",
     band.transparency = ".5",
     band.colors = "black",
     lines  = list(lty = 1, lwd = 2, col = "black"),
     axes = list( y = list(cex = 2, lim = c(110,124)),
                  x=list(cex = 2)))

plot(predictorEffect("AF_R_FA1_Mean",AF_FA_T,residuals=FALSE),
     partial.residuals=list(smooth=FALSE),
     main = "FA right AF predictor effect plot",
     ylab = "TPVT",
     xlab = "FA of right AF",
     band.transparency = ".5",
     band.colors = "black",
     lines  = list(lty = 1, lwd = 2, col = "black"),
     axes = list( x=list(cex = 2),
                  y=list(cex = 2, lim = c(110,124))))

plot(predictorEffect("AF_L_FA1_Mean",AF_FA_P,residuals=FALSE),
     partial.residuals=list(smooth=FALSE, col = "darkviolet"),
     main = "FA left AF predictor effect plot",
     ylab = "PERT",
     xlab = "FA of left AF",
     band.transparency = ".3",
     band.colors = "lightgray",
     lines = list(lty = 1, lwd = 2, col = "black"),
     axes = list( x=list(cex = 2),
                  y=list(cex = 2, lim = c(33.5,37.5))))

plot(predictorEffect("AF_R_FA1_Mean",AF_FA_P,residuals=FALSE),
     partial.residuals=list(smooth=FALSE, col = "darkviolet"),
     main = "FA Right AF predictor effect plot",
     ylab = "PERT",
     xlab = "FA of Right AF",     band.transparency = ".3",
     band.colors = "lightgray",
     lines = list(lty = 1, lwd = 2, col = "black"),
     axes = list( x=list(cex = 2),
                  y=list(cex = 2, lim = c(33.5,37.5))))




