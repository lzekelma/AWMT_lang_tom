library("car")
library("gridExtra")
library("yarrr")

setwd("/Users/leozekelman/Dropbox (Partners HealthCare)/HCP_S1200")
total_data_1 <- read.csv("total_data.csv")

genetic_tested <- total_data_1[total_data_1$HasGT,] #915 

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

data <- data[!is.na(data$ER40_CR),] # we exclude 2 subjects without PERT scores.

total_data <- data


#LI = (L − R) / (L + R)

ILF_FA1_Mean_LI <- (total_data$ILF_L_FA1_Mean - total_data$ILF_R_FA1_Mean) / (total_data$ILF_L_FA1_Mean + total_data$ILF_R_FA1_Mean)
IOFF_FA1_Mean_LI  <- (total_data$IOFF_L_FA1_Mean - total_data$IOFF_R_FA1_Mean) / (total_data$IOFF_L_FA1_Mean + total_data$IOFF_R_FA1_Mean)
UF_FA1_Mean_LI  <- (total_data$UF_L_FA1_Mean - total_data$UF_R_FA1_Mean) / (total_data$UF_L_FA1_Mean + total_data$UF_R_FA1_Mean)
CB_FA1_Mean_LI <- (total_data$CB_L_FA1_Mean - total_data$CB_R_FA1_Mean) / (total_data$CB_L_FA1_Mean + total_data$CB_R_FA1_Mean)
AF_FA1_Mean_LI <- (total_data$AF_L_FA1_Mean - total_data$AF_R_FA1_Mean) / (total_data$AF_L_FA1_Mean + total_data$AF_R_FA1_Mean)
MdLF_FA1_Mean_LI <- (total_data$MdLF_L_FA1_Mean - total_data$MdLF_R_FA1_Mean) / (total_data$MdLF_L_FA1_Mean + total_data$MdLF_R_FA1_Mean)
EmC_FA1_Mean_LI <- (total_data$EmC_L_FA1_Mean - total_data$EmC_R_FA1_Mean) / (total_data$EmC_L_FA1_Mean + total_data$EmC_R_FA1_Mean)
SLF1_FA1_Mean_LI <- (total_data$SLF1_L_FA1_Mean - total_data$SLF1_R_FA1_Mean) / (total_data$SLF1_L_FA1_Mean + total_data$SLF1_R_FA1_Mean)
SLF2_FA1_Mean_LI <- (total_data$SLF2_L_FA1_Mean - total_data$SLF2_R_FA1_Mean) / (total_data$SLF2_L_FA1_Mean + total_data$SLF2_R_FA1_Mean)
SLF3_FA1_Mean_LI <- (total_data$SLF3_L_FA1_Mean - total_data$SLF3_R_FA1_Mean) / (total_data$SLF3_L_FA1_Mean + total_data$SLF3_R_FA1_Mean)
EC_FA1_Mean_LI <- (total_data$EC_L_FA1_Mean - total_data$EC_R_FA1_Mean) / (total_data$EC_L_FA1_Mean + total_data$EC_R_FA1_Mean)
PLIC_FA1_Mean_LI <- (total_data$PLIC_L_FA1_Mean - total_data$PLIC_R_FA1_Mean) / (total_data$PLIC_L_FA1_Mean + total_data$PLIC_R_FA1_Mean)

FA1_Mean_LI <- c(ILF_FA1_Mean_LI, IOFF_FA1_Mean_LI,UF_FA1_Mean_LI,CB_FA1_Mean_LI,AF_FA1_Mean_LI,MdLF_FA1_Mean_LI,EmC_FA1_Mean_LI,
  SLF1_FA1_Mean_LI,SLF2_FA1_Mean_LI,SLF3_FA1_Mean_LI)


ILF_FA2_Mean_LI <- (total_data$ILF_L_FA2_Mean - total_data$ILF_R_FA2_Mean) / (total_data$ILF_L_FA2_Mean + total_data$ILF_R_FA2_Mean)
IOFF_FA2_Mean_LI  <- (total_data$IOFF_L_FA2_Mean - total_data$IOFF_R_FA2_Mean) / (total_data$IOFF_L_FA2_Mean + total_data$IOFF_R_FA2_Mean)
UF_FA2_Mean_LI  <- (total_data$UF_L_FA2_Mean - total_data$UF_R_FA2_Mean) / (total_data$UF_L_FA2_Mean + total_data$UF_R_FA2_Mean)
CB_FA2_Mean_LI <- (total_data$CB_L_FA2_Mean - total_data$CB_R_FA2_Mean) / (total_data$CB_L_FA2_Mean + total_data$CB_R_FA2_Mean)
AF_FA2_Mean_LI <- (total_data$AF_L_FA2_Mean - total_data$AF_R_FA2_Mean) / (total_data$AF_L_FA2_Mean + total_data$AF_R_FA2_Mean)
MdLF_FA2_Mean_LI <- (total_data$MdLF_L_FA2_Mean - total_data$MdLF_R_FA2_Mean) / (total_data$MdLF_L_FA2_Mean + total_data$MdLF_R_FA2_Mean)
EmC_FA2_Mean_LI <- (total_data$EmC_L_FA2_Mean - total_data$EmC_R_FA2_Mean) / (total_data$EmC_L_FA2_Mean + total_data$EmC_R_FA2_Mean)
SLF1_FA2_Mean_LI <- (total_data$SLF1_L_FA2_Mean - total_data$SLF1_R_FA2_Mean) / (total_data$SLF1_L_FA2_Mean + total_data$SLF1_R_FA2_Mean)
SLF2_FA2_Mean_LI <- (total_data$SLF2_L_FA2_Mean - total_data$SLF2_R_FA2_Mean) / (total_data$SLF2_L_FA2_Mean + total_data$SLF2_R_FA2_Mean)
SLF3_FA2_Mean_LI <- (total_data$SLF3_L_FA2_Mean - total_data$SLF3_R_FA2_Mean) / (total_data$SLF3_L_FA2_Mean + total_data$SLF3_R_FA2_Mean)
EC_FA2_Mean_LI <- (total_data$EC_L_FA2_Mean - total_data$EC_R_FA2_Mean) / (total_data$EC_L_FA2_Mean + total_data$EC_R_FA2_Mean)
PLIC_FA2_Mean_LI <- (total_data$PLIC_L_FA2_Mean - total_data$PLIC_R_FA2_Mean) / (total_data$PLIC_L_FA2_Mean + total_data$PLIC_R_FA2_Mean)

FA2_Mean_LI <- c(ILF_FA2_Mean_LI, IOFF_FA2_Mean_LI,UF_FA2_Mean_LI,CB_FA2_Mean_LI,AF_FA2_Mean_LI,MdLF_FA2_Mean_LI,EmC_FA2_Mean_LI,
                 SLF1_FA2_Mean_LI,SLF2_FA2_Mean_LI,SLF3_FA2_Mean_LI)



ILF_Num_Fibers_LI <- (total_data$ILF_L_Num_Fibers - total_data$ILF_R_Num_Fibers) / (total_data$ILF_L_Num_Fibers + total_data$ILF_R_Num_Fibers)
IOFF_Num_Fibers_LI  <- (total_data$IOFF_L_Num_Fibers - total_data$IOFF_R_Num_Fibers) / (total_data$IOFF_L_Num_Fibers + total_data$IOFF_R_Num_Fibers)
UF_Num_Fibers_LI  <- (total_data$UF_L_Num_Fibers - total_data$UF_R_Num_Fibers) / (total_data$UF_L_Num_Fibers + total_data$UF_R_Num_Fibers)
CB_Num_Fibers_LI <- (total_data$CB_L_Num_Fibers - total_data$CB_R_Num_Fibers) / (total_data$CB_L_Num_Fibers + total_data$CB_R_Num_Fibers)
AF_Num_Fibers_LI <- (total_data$AF_L_Num_Fibers - total_data$AF_R_Num_Fibers) / (total_data$AF_L_Num_Fibers + total_data$AF_R_Num_Fibers)
MdLF_Num_Fibers_LI <- (total_data$MdLF_L_Num_Fibers - total_data$MdLF_R_Num_Fibers) / (total_data$MdLF_L_Num_Fibers + total_data$MdLF_R_Num_Fibers)
EmC_Num_Fibers_LI <- (total_data$EmC_L_Num_Fibers - total_data$EmC_R_Num_Fibers) / (total_data$EmC_L_Num_Fibers + total_data$EmC_R_Num_Fibers)
SLF1_Num_Fibers_LI <- (total_data$SLF1_L_Num_Fibers - total_data$SLF1_R_Num_Fibers) / (total_data$SLF1_L_Num_Fibers + total_data$SLF1_R_Num_Fibers)
SLF2_Num_Fibers_LI <- (total_data$SLF2_L_Num_Fibers - total_data$SLF2_R_Num_Fibers) / (total_data$SLF2_L_Num_Fibers + total_data$SLF2_R_Num_Fibers)
SLF3_Num_Fibers_LI <- (total_data$SLF3_L_Num_Fibers - total_data$SLF3_R_Num_Fibers) / (total_data$SLF3_L_Num_Fibers + total_data$SLF3_R_Num_Fibers)
EC_Num_Fibers_LI <- (total_data$EC_L_Num_Fibers - total_data$EC_R_Num_Fibers) / (total_data$EC_L_Num_Fibers + total_data$EC_R_Num_Fibers)
PLIC_Num_Fibers_LI <- (total_data$PLIC_L_Num_Fibers - total_data$PLIC_R_Num_Fibers) / (total_data$PLIC_L_Num_Fibers + total_data$PLIC_R_Num_Fibers)

Num_Fibers_LI <- c(ILF_Num_Fibers_LI, IOFF_Num_Fibers_LI,UF_Num_Fibers_LI,CB_Num_Fibers_LI,AF_Num_Fibers_LI,MdLF_Num_Fibers_LI,EmC_Num_Fibers_LI,
                 SLF1_Num_Fibers_LI,SLF2_Num_Fibers_LI,SLF3_Num_Fibers_LI)


ILF_trace1_Mean_LI <- (total_data$ILF_L_trace1_Mean - total_data$ILF_R_trace1_Mean) / (total_data$ILF_L_trace1_Mean + total_data$ILF_R_trace1_Mean)
IOFF_trace1_Mean_LI  <- (total_data$IOFF_L_trace1_Mean - total_data$IOFF_R_trace1_Mean) / (total_data$IOFF_L_trace1_Mean + total_data$IOFF_R_trace1_Mean)
UF_trace1_Mean_LI  <- (total_data$UF_L_trace1_Mean - total_data$UF_R_trace1_Mean) / (total_data$UF_L_trace1_Mean + total_data$UF_R_trace1_Mean)
CB_trace1_Mean_LI <- (total_data$CB_L_trace1_Mean - total_data$CB_R_trace1_Mean) / (total_data$CB_L_trace1_Mean + total_data$CB_R_trace1_Mean)
AF_trace1_Mean_LI <- (total_data$AF_L_trace1_Mean - total_data$AF_R_trace1_Mean) / (total_data$AF_L_trace1_Mean + total_data$AF_R_trace1_Mean)
MdLF_trace1_Mean_LI <- (total_data$MdLF_L_trace1_Mean - total_data$MdLF_R_trace1_Mean) / (total_data$MdLF_L_trace1_Mean + total_data$MdLF_R_trace1_Mean)
EmC_trace1_Mean_LI <- (total_data$EmC_L_trace1_Mean - total_data$EmC_R_trace1_Mean) / (total_data$EmC_L_trace1_Mean + total_data$EmC_R_trace1_Mean)
SLF1_trace1_Mean_LI <- (total_data$SLF1_L_trace1_Mean - total_data$SLF1_R_trace1_Mean) / (total_data$SLF1_L_trace1_Mean + total_data$SLF1_R_trace1_Mean)
SLF2_trace1_Mean_LI <- (total_data$SLF2_L_trace1_Mean - total_data$SLF2_R_trace1_Mean) / (total_data$SLF2_L_trace1_Mean + total_data$SLF2_R_trace1_Mean)
SLF3_trace1_Mean_LI <- (total_data$SLF3_L_trace1_Mean - total_data$SLF3_R_trace1_Mean) / (total_data$SLF3_L_trace1_Mean + total_data$SLF3_R_trace1_Mean)
EC_trace1_Mean_LI <- (total_data$EC_L_trace1_Mean - total_data$EC_R_trace1_Mean) / (total_data$EC_L_trace1_Mean + total_data$EC_R_trace1_Mean)
PLIC_trace1_Mean_LI <- (total_data$PLIC_L_trace1_Mean - total_data$PLIC_R_trace1_Mean) / (total_data$PLIC_L_trace1_Mean + total_data$PLIC_R_trace1_Mean)

trace1_Mean_LI <- c(ILF_trace1_Mean_LI, IOFF_trace1_Mean_LI,UF_trace1_Mean_LI,CB_trace1_Mean_LI,AF_trace1_Mean_LI,MdLF_trace1_Mean_LI,EmC_trace1_Mean_LI,
                   SLF1_trace1_Mean_LI,SLF2_trace1_Mean_LI,SLF3_trace1_Mean_LI)


ILF_trace2_Mean_LI <- (total_data$ILF_L_trace2_Mean - total_data$ILF_R_trace2_Mean) / (total_data$ILF_L_trace2_Mean + total_data$ILF_R_trace2_Mean)
IOFF_trace2_Mean_LI  <- (total_data$IOFF_L_trace2_Mean - total_data$IOFF_R_trace2_Mean) / (total_data$IOFF_L_trace2_Mean + total_data$IOFF_R_trace2_Mean)
UF_trace2_Mean_LI  <- (total_data$UF_L_trace2_Mean - total_data$UF_R_trace2_Mean) / (total_data$UF_L_trace2_Mean + total_data$UF_R_trace2_Mean)
CB_trace2_Mean_LI <- (total_data$CB_L_trace2_Mean - total_data$CB_R_trace2_Mean) / (total_data$CB_L_trace2_Mean + total_data$CB_R_trace2_Mean)
AF_trace2_Mean_LI <- (total_data$AF_L_trace2_Mean - total_data$AF_R_trace2_Mean) / (total_data$AF_L_trace2_Mean + total_data$AF_R_trace2_Mean)
MdLF_trace2_Mean_LI <- (total_data$MdLF_L_trace2_Mean - total_data$MdLF_R_trace2_Mean) / (total_data$MdLF_L_trace2_Mean + total_data$MdLF_R_trace2_Mean)
EmC_trace2_Mean_LI <- (total_data$EmC_L_trace2_Mean - total_data$EmC_R_trace2_Mean) / (total_data$EmC_L_trace2_Mean + total_data$EmC_R_trace2_Mean)
SLF1_trace2_Mean_LI <- (total_data$SLF1_L_trace2_Mean - total_data$SLF1_R_trace2_Mean) / (total_data$SLF1_L_trace2_Mean + total_data$SLF1_R_trace2_Mean)
SLF2_trace2_Mean_LI <- (total_data$SLF2_L_trace2_Mean - total_data$SLF2_R_trace2_Mean) / (total_data$SLF2_L_trace2_Mean + total_data$SLF2_R_trace2_Mean)
SLF3_trace2_Mean_LI <- (total_data$SLF3_L_trace2_Mean - total_data$SLF3_R_trace2_Mean) / (total_data$SLF3_L_trace2_Mean + total_data$SLF3_R_trace2_Mean)
EC_trace2_Mean_LI <- (total_data$EC_L_trace2_Mean - total_data$EC_R_trace2_Mean) / (total_data$EC_L_trace2_Mean + total_data$EC_R_trace2_Mean)
PLIC_trace2_Mean_LI <- (total_data$PLIC_L_trace2_Mean - total_data$PLIC_R_trace2_Mean) / (total_data$PLIC_L_trace2_Mean + total_data$PLIC_R_trace2_Mean)

trace2_Mean_LI <- c(ILF_trace2_Mean_LI, IOFF_trace2_Mean_LI,UF_trace2_Mean_LI,CB_trace2_Mean_LI,AF_trace2_Mean_LI,MdLF_trace2_Mean_LI,EmC_trace2_Mean_LI,
                    SLF1_trace2_Mean_LI,SLF2_trace2_Mean_LI,SLF3_trace2_Mean_LI)





# tracts <- c(rep("ILF", length(ILF_FA1_Mean_LI)), 
#             rep("IOFF", length(IOFF_FA1_Mean_LI)),
#             rep("UF", length(UF_FA1_Mean_LI)),
#             rep("CB", length(CB_FA1_Mean_LI)),
#             rep("AF", length(AF_FA1_Mean_LI)),
#             rep("MdLF", length(MdLF_FA1_Mean_LI)),
#             rep("EmC", length(EmC_FA1_Mean_LI)),
#             rep("SLF1", length(SLF1_FA1_Mean_LI)),
#             rep("SLF2", length(SLF2_FA1_Mean_LI)),
#             rep("SLF3", length(SLF3_FA1_Mean_LI)),
#             rep("EC", length(EC_FA1_Mean_LI)),
#             rep("PLIC", length(PLIC_FA1_Mean_LI)))


tracts <- c(rep("ILF", length(ILF_FA1_Mean_LI)), 
            rep("IOFF", length(IOFF_FA1_Mean_LI)),
            rep("UF", length(UF_FA1_Mean_LI)),
            rep("CB", length(CB_FA1_Mean_LI)),
            rep("AF", length(AF_FA1_Mean_LI)),
            rep("MdLF", length(MdLF_FA1_Mean_LI)),
            rep("EmC", length(EmC_FA1_Mean_LI)),
            rep("SLF1", length(SLF1_FA1_Mean_LI)),
            rep("SLF2", length(SLF2_FA1_Mean_LI)),
            rep("SLF3", length(SLF3_FA1_Mean_LI)))

me_data <- data.frame(tracts,FA1_Mean_LI, FA2_Mean_LI, Num_Fibers_LI, trace1_Mean_LI,trace2_Mean_LI)

# pirateplot(FA1_Mean_LI ~ tracts, data = me_data, sortx = "mean", main = "Mean FA1 Laterality of Associaiton Tracts")
# pirateplot(FA2_Mean_LI~tracts, data = me_data, sortx = "mean",main = "Mean FA2 Laterality of Associaiton Tracts")
# pirateplot(Num_Fibers_LI~tracts, data = me_data, sortx = "mean",main = "Num Fibers Laterality of Associaiton Tracts")
# 
# pirateplot(trace1_Mean_LI ~ tracts, data = me_data, sortx = "mean", main = "Mean Trace1 Laterality of Associaiton Tracts")
# pirateplot(trace2_Mean_LI ~ tracts, data = me_data, sortx = "mean", main = "Mean Trace2 Laterality of Associaiton Tracts")
# 
# pirateplot(FA1_Mean_LI ~ tracts, data = me_data, sortx = "mean", main = "Mean FA1 Laterality of Associaiton Tracts")
# pirateplot(Num_Fibers_LI~tracts, data = me_data, sortx = "mean",main = "Num Fibers Laterality of Associaiton Tracts")

pirateplot(FA1_Mean_LI ~ tracts, data = me_data, sortx = "mean", main = "FA Laterality Indices of Association Tracts", ylab = "FA Laterality Index")
legend(-1.9,0.19,"leftward", bg="transparent", bty = "n", xjust = 0, yjust = 1)
legend(-1.9,-0.1,"rightward", bg="transparent", bty = "n", xjust = 0, yjust = 1)

pirateplot(Num_Fibers_LI~tracts, data = me_data, sortx = "mean",main = "NoS Laterality Indices of Association Tracts", ylab = "NoF Laterality Index")
legend(-1.9,1.2,"leftward", bg="transparent", bty = "n", xjust = 0, yjust = 1)
legend(-1.9,-.6,"rightward", bg="transparent", bty = "n", xjust = 0, yjust = 1)

pirateplot(trace1_Mean_LI ~ tracts, data = me_data, sortx = "mean", main = "MD Laterality Indices of Association Tracts", ylab = "MD Laterality Index")
legend(-1.9,0.096,"leftward", bg="transparent", bty = "n", xjust = 0, yjust = 1)
legend(-1.9,-0.036,"rightward", bg="transparent", bty = "n", xjust = 0, yjust = 1)




