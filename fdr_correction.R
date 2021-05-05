
install.packages("xlsx")
library(xlsx)
setwd("/Users/leozekelman/Dropbox (Partners HealthCare)/Lang_ToM/age_gen/tracts_together/20_tracts")

FA1 <- read.xlsx("uncorrected.xlsx", sheetIndex=1, trim_ws = TRUE)
FA2 <- read.xlsx("uncorrected.xlsx", sheetIndex=2, trim_ws = TRUE)
Num_Fiber <- read.xlsx("uncorrected.xlsx", sheetIndex=3, trim_ws = TRUE)
trace1 <- read.xlsx("uncorrected.xlsx", sheetIndex=4, trim_ws = TRUE)
trace2 <- read.xlsx("uncorrected.xlsx", sheetIndex=5, trim_ws = TRUE)

FA1$model_pval_fdr <- p.adjust(FA1$model_pval, method = "fdr")
FA1$tract_L_pval_fdr <- p.adjust(FA1$tract_L_pval, method = "fdr")
FA1$tract_R_pval_fdr <- p.adjust(FA1$tract_R_pval, method = "fdr")
FA1$gender_pval_fdr <- p.adjust(FA1$gender_pval, method = "fdr")
FA1$age_pval_fdr <- p.adjust(FA1$age_pval, method = "fdr")

FA2$model_pval_fdr <- p.adjust(FA2$model_pval, method = "fdr")
FA2$tract_L_pval_fdr <- p.adjust(FA2$tract_L_pval, method = "fdr")
FA2$tract_R_pval_fdr <- p.adjust(FA2$tract_R_pval, method = "fdr")
FA2$gender_pval_fdr <- p.adjust(FA2$gender_pval, method = "fdr")
FA2$age_pval_fdr <- p.adjust(FA2$age_pval, method = "fdr")

Num_Fiber$model_pval_fdr <- p.adjust(Num_Fiber$model_pval, method = "fdr")
Num_Fiber$tract_L_pval_fdr <- p.adjust(Num_Fiber$tract_L_pval, method = "fdr")
Num_Fiber$tract_R_pval_fdr <- p.adjust(Num_Fiber$tract_R_pval, method = "fdr")
Num_Fiber$gender_pval_fdr <- p.adjust(Num_Fiber$gender_pval, method = "fdr")
Num_Fiber$age_pval_fdr <- p.adjust(Num_Fiber$age_pval, method = "fdr")

trace1$model_pval_fdr <- p.adjust(trace1$model_pval, method = "fdr")
trace1$tract_L_pval_fdr <- p.adjust(trace1$tract_L_pval, method = "fdr")
trace1$tract_R_pval_fdr <- p.adjust(trace1$tract_R_pval, method = "fdr")
trace1$gender_pval_fdr <- p.adjust(trace1$gender_pval, method = "fdr")
trace1$age_pval_fdr <- p.adjust(trace1$age_pval, method = "fdr")

trace2$model_pval_fdr <- p.adjust(trace2$model_pval, method = "fdr")
trace2$tract_L_pval_fdr <- p.adjust(trace2$tract_L_pval, method = "fdr")
trace2$tract_R_pval_fdr <- p.adjust(trace2$tract_R_pval, method = "fdr")
trace2$gender_pval_fdr <- p.adjust(trace2$gender_pval, method = "fdr")
trace2$age_pval_fdr <- p.adjust(trace2$age_pval, method = "fdr")

write.xlsx(FA1,"corrected.xlsx", sheetName="FA1_mean")
write.xlsx(FA2,"corrected.xlsx", sheetName="FA2_mean", append=TRUE)
write.xlsx(Num_Fiber,"corrected.xlsx", sheetName="Num_Fibers", append=TRUE)
write.xlsx(trace1,"corrected.xlsx", sheetName="trace1_mean", append=TRUE)
write.xlsx(trace2,"corrected.xlsx", sheetName="trace2_mean", append=TRUE)


