############################################################################
############################################################################

#From,

#      Gokul Kaisaravalli Bhojraj
#      Id: 80789
#      Business Intelligence

############################################################################
############################################################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


install.packages("agricolae")
library(agricolae)

# 1) ###########################################################

treatments <- c("A","B","C")
# Three treatment on three workers,
#  design.rcbd(trt=treatments,r=3,seed=2)

design.rcbd(trt=treatments,r=3)

# 2) ###########################################################

#The layout for an LSD to allocate 3 treatments 
#   on two blocking factors(worker and working slots of the day) 
#   each having 3 levels is given below.

design.lsd(treatments,serie=3,seed=3)

# 3) ##########################################################

library(tidyverse)
library(readxl)

# upload the file from excel to R and converting to data frame
data_df<- data.frame(read_excel("C:/Users/Gokul/Desktop/Lab excer 2/Lab2data.xlsx"))
data_df

# Removing row 1 as to make it more sensable
data_df<-data_df[-1,-1]
data_df

# Giving respective column names
colnames(data_df) <-c("A-Paint","B-Paint","C-Paint","D-Paint")
data_df

# coverting values in dataframe into list and then to vector 
val <- c(t(as.matrix(data_df)))
val

# treatment levels
f<-c("Paint-A","Paint-B","Paint-C","Paint-D")
# number of treatment levels
k<-4
# number of control blocks
n<-6
# vector for treatment factors (paint) that correspond to the data set
tm_vector<-gl(k,1,n*k,factor(f))

# blocks
Blocks<-gl(n,k,k*n)
# regression
reg<-lm(val~tm_vector+Blocks)
anova(reg)
summary(reg)


# It is a CRD experiment design because we need to find how much
#   the means of each treatment differs so we need to run anova 
#   (estimate in summary are the means)so the hypothsesis will be 
#   that all treatments are the same, alternate that they are not
#   "A paint" is the reference(the estimate part is the overall mean)
#   we can see from the rest of the treatments(by comparing to the overall mean) 
#   that "B paint" lasts 9 months less, "C paint" 11 months less, 
#   "D paint" lasts about a month less but we can see from the p-value
#   that it is not significant

# 4) ##########################################################

# Power analysis 
library(pwr)
pwr.anova.test(k=3, f=0.20, sig.level=0.05, power=0.9)

#Arguments
#k
#Number observations (per group)
#f
#Effect size of groups
#n
#Number of sig.level
#Significance level (Type I error probability)
#power
#Power of test (1 minus Type II error probability)

###########################################################################

#               @@@@@@@@@@@@@@@@@@@@@@@@@ 

###########################################################################