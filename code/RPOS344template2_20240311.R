#############################################
#
# Forecasting SC decisions
# Matthew Ingram
# RPOS 344
# UAlbany
# Last revised: 2024-03-11
#
#############################################

########################
# SET DIRECTORY
########################

# check curret working directory
getwd()

# set
path <- 'C:/Users/mi122167/Dropbox/SUNYAlbany/PredictingSCOTUS/Data/SCDB'
setwd(path)

# check again
getwd()

########################
# SET ENVIRONMENT
########################

#install.packages("pacman", repos="https://cloud.r-project.org")
library(pacman)
p_load(aod,
       ggplot2,
       caTools,
       rpart,
       rpart.plot,
       randomForest,
       party, # another tree/classification package
       tree,
       magrittr,
       visreg,
       rattle, # fancy rpart plots
       reprtree,
       remotes
)

#######################################
# note: if reprtree not installed

# followed answer here: https://stackoverflow.com/questions/51210386/devtools-install-github-failes-for-reprtree
# You can download the package as a .zip and unzip it. 
# Then remove everything after line 39 in ./man/text.tree.RD. Afterwards you can install the package like this:

#library(devtools)
#if(!('reprtree' %in% installed.packages())){
#  install_local('C:/Users/mi122167/Desktop/reprtree-master/reprtree-master/')
#}
#library(reprtree)
# WORKS!!!
########################################

sessionInfo()

################################################
# Load data
################################################

# load RData file
# this is 202 data file
load("./data/original/SCDB_2023_01_justiceCentered_Docket.Rdata/SCDB_2023_01_justiceCentered_Docket.RData")
# rename object to simpler name
data <- SCDB_2023_01_justiceCentered_Docket # need this originally with first loading of data
# remove original object that was loaded
rm(SCDB_2023_01_justiceCentered_Docket)

# load data from web; not working
#temp <- tempfile()
#download.file("http://scdb.wustl.edu/_brickFiles/2021_01/SCDB_2021_01_justiceCentered_Docket.Rdata.zip",temp)
#data <- load(unz(temp, "SCDB_2021_01_justiceCentered_Docket.RData"))
#unlink(temp)

# save partial file with only data since 2000
#temp <- subset(data, term>1999)
#write.csv(temp, file="./data/working/SCDB_2021_01_justiceCentered_Docket_2000s.csv")

# read csv file
#data <- read.csv(file="./data/working/SCDB_2021_01_justiceCentered_Docket_2000s.csv")

#################################################
# Inspect/explore data

names(data)
str(data)
head(data)
summary(data)

#################################################
# Define outcome variables
#################################################

# The variable "direction" captures the ideological direction of the 
# individual justice's vote in each case. 
# See online codebook for SCDB data with description of all 
# variables here: <http://http://scdb.wustl.edu//documentation.php?s=1>.
# The variable is coded 1=conservative and 2=liberal. 
# We want to recode to make simpler to interpret: 0=conservative and 1=liberal. 
# So, we generate a new variable 'directiondum' that captures this binary character.

data$directiondum <- as.factor(data$direction - 1)  # 1 = liberal; 0 = conservative
# check
data %>%
  subset(, select=c("direction", "directiondum")) %>%
  summary()

# confirm coding
table(data$direction, data$directiondum)


# later, after prediction, translate to affirm/reverse based on lower court direction
# see coding notes at WUSTL site: http://wusct.wustl.edu/media/trees.pdf

table(data$lcDispositionDirection)
# 1= conservation, 2=liberal, 3 = unspecifiable

data$lcdirectiondum <- 0
data$lcdirectiondum[data$lcDispositionDirection==2] <- 1
data$lcdirectiondum[data$lcDispositionDirection==3] <- NA
# 0= conservative, 1=liberal, rest NA

data %>%
  subset(, select=c("lcDispositionDirection", "lcdirectiondum")) %>%
  summary()

table(data$lcDispositionDirection, data$lcdirectiondum)

# create affirm/reverse outcome variable

data$reverse <- 0
data$reverse[data$lcdirectiondum==0 & data$directiondum==0] <- 0
data$reverse[data$lcdirectiondum==1 & data$directiondum==1] <- 0
data$reverse[data$lcdirectiondum==0 & data$directiondum==1] <- 1
data$reverse[data$lcdirectiondum==1 & data$directiondum==0] <- 1

summary(data$reverse)

table(data$reverse, data$lcdirectiondum)
table(data$reverse, data$directiondum)


###################################################
# can use partyWinning to capture overall reversal rates
# and graph reversal rates over time

#############################################
# *** NOTE: don't use partyWinning variable in justice-level analysis ***
# this is a case outcome, not a justice vote
#############################################

# overall reversal rate
table(data$partyWinning)[[2]]/sum(table(data$partyWinning))
# 0.6349175
# 63.49%   # this is overall, historical reversal rate from 1946-2021

# reversal rate from 1946-2022
table(data$partyWinning[data$term<2022])[[2]]/sum(table(data$partyWinning[data$term<2022]))
# 0.6329865
# 63.30%

temp <- data.frame()
term <- seq(1946, 2022, 1)
for (i in term){
  rate <- table(data$partyWinning[data$term==i])[[2]]/sum(table(data$partyWinning[data$term==i]))
  temp <- rbind(temp, c(i, rate))
}

temp
colnames(temp) <- c("term", "reversal_pct")
g <- ggplot(temp, aes(x=term, y=reversal_pct)) +
  geom_point() +
  geom_smooth(method="loess")
g

# rolling, 2-year avg reversal rate
temp$twoyearavg <- NA
for (i in 2:nrow(temp)){
  temp$twoyearavg[i] <- (temp$reversal_pct[i]+temp$reversal_pct[i-1])/2
}
temp

g <- ggplot(temp, aes(x=term, y=reversal_pct)) +
  geom_hline(
    yintercept=
      table(data$partyWinning)[[2]]/sum(table(data$partyWinning)),
    linetype=2) +
  geom_point() +
  geom_text(data=subset(temp, reversal_pct>.77),
            aes(x=term, y=reversal_pct, label=paste(
              round(reversal_pct,3), 
              " (", 
              term, 
              ")",
              sep="")),
            hjust=.8,vjust=-.4, size=2.5) +
  geom_line(aes(x=term, y=twoyearavg),
              col="black") +
  geom_smooth(method="loess", se=FALSE) +
  labs(title = "Supreme Court, reversal rates over time",
       y="reversal rate (%)",
       caption= "blue = lowess line; black= two-year average; dashed = historical average") +
  theme_minimal()
g

png(filename="./figures/reversalratesOT1946-OT2022.png", width=6, height=6, units="in", res=300)
g
dev.off()

# only reversals of lower liberal decisions (lcdirectiondum==1)
templib <- data.frame()
term <- seq(1946, 2022, 1)
for (i in term){
  rate <- table(data$partyWinning[data$term==i & data$lcdirectiondum==1])[[2]]/sum(table(data$partyWinning[data$term==i & data$lcdirectiondum==1]))
  templib <- rbind(templib, c(i, rate))
}

templib
colnames(templib) <- c("term", "lclib_revpct")
g <- ggplot(templib, aes(x=term, y=lclib_revpct)) +
  geom_point() +
  geom_smooth(method="loess")
g

# rolling, 2-year avg reversal rate
templib$twoyearavg <- NA
for (i in 2:nrow(templib)){
  templib$twoyearavg[i] <- (templib$lclib_revpct[i]+templib$lclib_revpct[i-1])/2
}
templib

g <- ggplot(templib, aes(x=term, y=lclib_revpct)) +
  geom_hline(
    yintercept=
      table(data$partyWinning)[[2]]/sum(table(data$partyWinning)),
    linetype=2) +
  geom_point() +
  geom_text(data=subset(templib, lclib_revpct>.8),
            aes(x=term, y=lclib_revpct, label=paste(
              round(lclib_revpct,3), 
              " (", 
              term, 
              ")",
              sep="")),
            hjust=.8,vjust=-.4, size=2.5) +
  geom_line(aes(x=term, y=twoyearavg),
            col="black") +
  geom_smooth(method="loess", se=FALSE) +
  labs(title = "Supreme Court, reversal rates when lcdirectiondum=1",
       y="reversal rate (%)",
       caption= "blue = lowess line; black= two-year average; dashed = historical average") +
  theme_minimal()
g

png(filename="./figures/lclib_reversalratesOT1946-OT2022.png", width=6, height=6, units="in", res=300)
g
dev.off()



# % close cases
# at least 4 minority votes (5-4 cases)
table(data$minVotes[data$term==2022])

temp2 <- data.frame()
term <- seq(1946, 2022, 1)
for (i in term){
  split <- table(data$minVotes[data$term==i])[[5]]/sum(table(data$minVotes[data$term==i]))
  split2 <- (table(data$minVotes[data$term==i])[[5]]+table(data$minVotes[data$term==i])[[4]])/sum(table(data$minVotes[data$term==i]))
  unan <- table(data$minVotes[data$term==i])[[1]]/sum(table(data$minVotes[data$term==i]))
  unan2 <- (table(data$minVotes[data$term==i])[[1]]+table(data$minVotes[data$term==i])[[2]])/sum(table(data$minVotes[data$term==i]))
  temp2 <- rbind(temp2, c(i, split, split2, unan, unan2))
}
colnames(temp2) <- c("term", "split", "split2", "unan", "unan2")

# unanimous cases (9-0)
g <- ggplot(temp2, aes(x=term, y=unan)) +
  geom_hline(
    yintercept=
      table(data$minVotes)[[1]]/sum(table(data$minVotes)),
    linetype=2) +
  geom_point() +
  geom_text(data=subset(temp2, unan>.5 | unan<.3),
            aes(x=term, y=unan, label=paste(
              round(unan,3), 
              " (", 
              term, 
              ")",
              sep="")),
            hjust=.8,vjust=-.4, size=2.5) +
  #geom_line(aes(x=term, y=twoyearavg),
  #          col="black") +
  geom_smooth(method="loess", se=FALSE) +
  labs(title = "Supreme Court, unanimous cases",
       y="unanimous (%)"#,
       #caption= "blue = lowess line; black= two-year average; dashed = historical average"
       ) +
  theme_minimal()
g

png(filename="./figures/unanimous_cases_OT1946-OT2022.png", width=6, height=6, units="in", res=300)
g
dev.off()


# unanimous2 (9-0 and 8-1)

g <- ggplot(temp2, aes(x=term, y=unan2)) +
  geom_hline(
    yintercept=
      (table(data$minVotes)[[1]]+table(data$minVotes)[[2]])/sum(table(data$minVotes)),
    linetype=2) +
  geom_point() +
  geom_text(data=subset(temp2, unan2>.65 | unan2<.39),
            aes(x=term, y=unan2, label=paste(
              round(unan2,3), 
              " (", 
              term, 
              ")",
              sep="")),
            hjust=.8,vjust=-.4, size=2.5) +
  #geom_line(aes(x=term, y=twoyearavg),
  #          col="black") +
  geom_smooth(method="loess", se=FALSE) +
  labs(title = "Supreme Court, 9-0 and 8-1 cases",
       y="9-0 and 8-1 cases (%)"#,
       #caption= "blue = lowess line; black= two-year average; dashed = historical average"
  ) +
  theme_minimal()
g

png(filename="./figures/unanimous9-0and8-1_cases_OT1946-OT2022.png", width=6, height=6, units="in", res=300)
g
dev.off()


# split cases (5-4)
g <- ggplot(temp2, aes(x=term, y=split)) +
  geom_hline(
    yintercept=
      table(data$minVotes)[[5]]/sum(table(data$minVotes)),
    linetype=2) +
  geom_point() +
  geom_text(data=subset(temp2, split>.31 | split<.045),
            aes(x=term, y=split, label=paste(
              round(split,3), 
              " (", 
              term, 
              ")",
              sep="")),
            hjust=.8,vjust=-.4, size=2.5) +
  #geom_line(aes(x=term, y=twoyearavg),
  #          col="black") +
  geom_smooth(method="loess", se=FALSE) +
  labs(title = "Supreme Court, 5-4 cases",
       y="5-4 cases (%)"#,
       #caption= "blue = lowess line; black= two-year average; dashed = historical average"
  ) +
  theme_minimal()
g

png(filename="./figures/split5-4_cases_OT1946-OT2021.png", width=6, height=6, units="in", res=300)
g
dev.off()


# 5-4 and 6-3 cases

g <- ggplot(temp2, aes(x=term, y=split2)) +
  geom_hline(
    yintercept=
      (table(data$minVotes)[[5]]+table(data$minVotes)[[4]])/sum(table(data$minVotes)),
    linetype=2) +
  geom_point() +
  geom_text(data=subset(temp2, split2>.48 | split2<.17),
            aes(x=term, y=split2, label=paste(
              round(split2,3), 
              " (", 
              term, 
              ")",
              sep="")),
            hjust=.8,vjust=-.4, size=2.5) +
  #geom_line(aes(x=term, y=twoyearavg),
  #          col="black") +
  geom_smooth(method="loess", se=FALSE) +
  labs(title = "Supreme Court, 5-4 and 6-3 cases",
       y="5-4 and 6-3 cases (%)"#,
       #caption= "blue = lowess line; black= two-year average; dashed = historical average"
  ) +
  theme_minimal()
g

png(filename="./figures/split5-4and6-3_cases_OT1946-OT2022.png", width=6, height=6, units="in", res=300)
g
dev.off()


#############################################################
# Define predictors
#############################################################

#We want to define explanatory variables (predictors) that we can use. 
#We should start with the variables used by Martin et al. (2002).
#Later, we can also consider expanding to the variables raised by critiques and commentaries on their work, 
#as well as other variables we have raised in class.

# Above, we already defined the variable that captures 
# the direction of the lower court decision.

table(data$lcdirectiondum)
table(data$lcdirectiondum, data$lcDispositionDirection)

# note that lcDispositionDirection has value of 3 (undefinable), and there were
# 1205 observations there out of around 100,000, or about 1%

# we also already translate to affirm/reverse above

#############################################
# Exclude all unanimous cases
data2 <- subset(data, data$minVotes>0)
#############################################


# Other variables we could include are identified below, but we could 
# consider recoding them here.

# Subset by variable value

# Shows most welfare-related cases were decided by 1990.

#data <- subset(data, issue==20180 | issue==20190)
#summary(data)
hist(subset(data2, (issue==20180 | issue==20190))$term)



##############################################################################
# e.g., subset OT2022 only
#data <- subset(data, term==2022)
##############################################################################


##############################################################################
##############################################################################

# Main Method: Classification Trees

##############################################################################
##############################################################################

# See Kastellec (2010) for nice, accessible introduction to classification trees.

# Install and open required library (only need to open if already have installed).

# install and open rpart
#install.packages("rpart", repos="https://cloud.r-project.org")
library(rpart)
#install.packages("rpart.plot", repos="https://cloud.r-project.org")
library(rpart.plot)

## Subset data according to each justice.

#Here, we break the data up according to each justice, 
# so that we end up with several mini-data sets. 
#Each of the mini-data sets contains the votes of only that justice.

# Question: what is the current "natural court"?

# For justices on court as of OT2022
barrett22 <- subset(data2, justiceName=="ACBarrett" & term==2022) 
# this is all barrett votes; OT2020 and OT2021

barrett20_22 <- subset(data2, justiceName=="ACBarrett" & (term==2020 | 
                                                            term==2021 |
                                                            term==2022)) 
# this is all barrett votes since she joined court in OT2020; OT2020-22
# or, simply
barrett <- subset(data2, justiceName=="ACBarrett") # all barrett


gorsuch <- subset(data2, justiceName=="NMGorsuch") # all gorsuch
# just gorsuch ot22
gorsuch22 <- subset(data2, justiceName=="NMGorsuch" & term==2022) #  gorsuch OT2020
# just gorsuch since 6-3 conservative majority
gorsuch20_22 <- subset(data2, justiceName=="NMGorsuch" & (term==2020 | 
                                                              term==2021 |
                                                              term==2022)) 

roberts <- subset(data, justiceName=="JGRoberts") # all roberts
roberts22 <- subset(data2, justiceName=="JGRoberts" & term==2022) #  roberts OT2020
# just roberts since 6-3 conservative majority
roberts20_22 <- subset(data2, justiceName=="JGRoberts" & (term==2020 | 
                                                            term==2021 |
                                                            term==2022)) 

thomas <- subset(data, justiceName=="CThomas") # all thomas
thomas22 <- subset(data2, justiceName=="CThomas" & term==2022) 

kavanaugh <- subset(data, justiceName=="BMKavanaugh") # all kavanaugh
kavanaugh22 <- subset(data2, justiceName=="BMKavanaugh" &  term==2022) 

alito <- subset(data, justiceName=="SAAlito") # all alito
alito22 <- subset(data2, justiceName=="SAAlito" & term==2022) 

sotomayor <- subset(data, justiceName=="SSotomayor") # all sotomayor
sotomayor22 <- subset(data2, justiceName=="SSotomayor" & term==2022) 

kagan <- subset(data, justiceName=="EKagan") # all kagan
kagan22 <- subset(data2, justiceName=="EKagan" & term==2022)

jackson <- subset(data, justiceName=="KBJackson") # all kagan
jackson22 <- subset(data2, justiceName=="KBJackson" & term==2022)


save.image("./data/working/working20240311.RData")

# Other justices
#ginsburg1 <- subset(data, justiceName=="RBGinsburg" & term<2002 & term>1995) # term< 2002 to see if can 
# match natural court and tree from Martin et al 2002
#ginsburg2 <- subset(data, justiceName=="RBGinsburg") # all ginsburg
#oconnor1 <- subset(data, justiceName=="SDOConnor" & term<2002 & term>1994)  # term<2002 to see if can
# match tree from Martin et al 2002
#oconnor2 <- subset(data, justiceName=="SDOConnor") # all oconnor
#scalia <- subset(data, justiceName=="AScalia") # all scalia
#kennedy <- subset(data, justiceName=="AMKennedy") # all kennedy

#breyer <- subset(data, justiceName=="SGBreyer") # all breyer
#breyer1819 <- subset(data, justiceName=="SGBreyer" & term>2017 & term<2020) #  breyer OT2018 and 2019
#breyer20 <- subset(data2, justiceName=="SGBreyer" & term==2020) #  breyer OT2020
#breyer21 <- subset(data2, justiceName=="SGBreyer" & term==2021) #  breyer OT2021
#breyer20_21 <- subset(data2, justiceName=="SGBreyer" & (term==2020 | term==2021)) #  breyer OT2020-2021


#
#And we could continue to include all other justices, if interested.


###########################################################################3
# Cautionary Notes

#If you are having a hard time following this thus far, it would be a good idea 
#to go back and review some of the basic documentation regarding the data at 
# the SCDB website.

#CHECK THE PDF AT WUSTL SITE REGARDING CODINGS.

#ALSO, NOTE USE OF VOTE OF OTHER JUSTICES IN RELATED DOCS (i.e., INTERDEPENDENCE).

#GENERATE FACTOR VARIRABLES WITH LOW, REASONABLE AND READABLE NUMBER OF CODINGS.

#ALSO, COULD RECODE OUTCOME AS AFFIRM OR REVERSE 
# (BASED on tabulations showing low frequency of contadictory combos)

#ALSO, TRY ADDING OTHER VARS USED BY KATZ, BOMMARITO, AND BLACKMAN, 
# including disagreement below.

##############################################################################
# Trees
##############################################################################

# load processed data
load("./data/working/working20240311.RData")

# Sotomayor Trees

### Subset training and testing data 

#OT2022

#using package caTools
df <- sotomayor22
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = .7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

SotomayorTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent + 
                        lcdirectiondum + lawType,# + term, 
                      #data = Train, 
                      data=df,
                      method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(SotomayorTree)
fancyRpartPlot(SotomayorTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020/sotomayor22p.png", width=6, height=6, units="in", res=300)
prp(SotomayorTree)
dev.off()

png(filename="./figures/OT2020/sotomayor22fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(SotomayorTree)
dev.off()

## Predictions

PredictCART = predict(SotomayorTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$directiondum, PredictCART)
preds

## Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 54.55% for 2022

# Questions:
# - is this good?
# - why or why not?


# what next? all? 2021?

#using package caTools
df <- sotomayor21
set.seed(3000)
spl = sample.split(df$reverse2, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

SotomayorTree = rpart(reverse2 ~ caseSource + issueArea + petitioner + respondent + 
                        lcdirectiondum + lawType,# + term, 
                      #data = Train, 
                      data=df,
                      method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(SotomayorTree)
fancyRpartPlot(SotomayorTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2021/sotomayor20p.png", width=6, height=6, units="in", res=300)
prp(SotomayorTree)
dev.off()

png(filename="./figures/OT2021/sotomayor20fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(SotomayorTree)
dev.off()

## Predictions

PredictCART = predict(SotomayorTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$directiondum, PredictCART)
preds

## Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 80%  (!!!)


# OT2020-OT2021

#using package caTools
df <- sotomayor20_21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

SotomayorTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent + 
                        lcdirectiondum + lawType,# + term, 
                      #data = Train, 
                      data=df,
                      method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(SotomayorTree)
par(oma=c(0,0,0,1))
fancyRpartPlot(SotomayorTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020_21/sotomayor20_21p.png", width=6, height=6, units="in", res=300)
prp(SotomayorTree)
dev.off()

png(filename="./figures/OT2020_21/sotomayor20_21fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(SotomayorTree)
dev.off()

## Predictions

PredictCART = predict(SotomayorTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$reverse, PredictCART)
preds

## Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 58.62%  (!!!)

########################################################################
# Sotomayor Tree, all with term, but just criminal procedure cases
########################################################################
### Subset training and testing data 

#using package caTools
set.seed(3000)
sotomayorsub <- subset(sotomayor, issueArea==1)
spl = sample.split(sotomayorsub$directiondum, SplitRatio = 0.7)
Train = subset(sotomayorsub, spl==TRUE)
Test = subset(sotomayorsub, spl==FALSE)

SotomayorTree = rpart(directiondum ~ 
                        caseSource  + issueArea + petitioner + respondent +
                        lcdirectiondum + lawType,# + term, 
                      data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
# took issueArea out since focused only on issueArea==1
prp(SotomayorTree)
fancyRpartPlot(SotomayorTree)
par(oma=c(0,0,0,1))


## Predictions

PredictCART = predict(SotomayorTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$reverse2, PredictCART)
preds

## Accuracy

#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 67.1%

###############################################
# Breyer Trees

# OT2020

### Subset training and testing data 

#using package caTools
df <- breyer20
set.seed(3000)
spl = sample.split(df$reverse2, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

BreyerTree = rpart(reverse2 ~ caseSource + issueArea + petitioner + respondent +
                     lcdirectiondum + lawType,# + term, 
                   data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(BreyerTree)
fancyRpartPlot(BreyerTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020/breyer20p.png", width=6, height=6, units="in", res=300)
prp(BreyerTree)
dev.off()

png(filename="./figures/OT2020/breyer20fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(BreyerTree)
dev.off()

## Predictions

PredictCART = predict(BreyerTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$directiondum, PredictCART)
preds

## Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 50%

# OT2021

df <- breyer21
set.seed(3000)
spl = sample.split(df$reverse2, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

BreyerTree = rpart(reverse2 ~ caseSource + issueArea + petitioner + respondent +
                     lcdirectiondum + lawType,# + term, 
                   data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(BreyerTree)
fancyRpartPlot(BreyerTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2021/breyer20p.png", width=6, height=6, units="in", res=300)
prp(BreyerTree)
dev.off()

png(filename="./figures/OT2021/breyer20fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(BreyerTree)
dev.off()

## Predictions

PredictCART = predict(BreyerTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$reverse2, PredictCART)
preds

## Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
#50%

# OT2020-2021

df <- breyer20_21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

BreyerTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                     lcdirectiondum + lawType,# + term, 
                   data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(BreyerTree)
fancyRpartPlot(BreyerTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020_21/breyer20_21p.png", width=6, height=6, units="in", res=300)
prp(BreyerTree)
dev.off()

par(oma=c(0,0,0,1))

png(filename="./figures/OT2020_21/breyer20_21fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(BreyerTree)
dev.off()

par(oma=c(0,0,0,1))

PredictCART = predict(BreyerTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$reverse, PredictCART)
preds

## Accuracy
#Accuracy = (topleft + bottomright)/total
(preds[1]+preds[4])/(sum(preds[1:4]))

# 51.72%

###############################################################
# Kagan Tree

# OT 2020

### Subset training and testing data 

#using package caTools
df <- kagan20
set.seed(3000)
spl = sample.split(df$reverse2, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

KaganTree = rpart(reverse2 ~ caseSource + issueArea + lcdirectiondum + lawType,# + term, 
                  data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(KaganTree)
fancyRpartPlot(KaganTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020/kagan20p.png", width=6, height=6, units="in", res=300)
prp(KaganTree)
dev.off()

png(filename="./figures/OT2020/kagan20fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(KaganTree)
dev.off()

# OT 2020-21

df <- kagan20_21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

KaganTree = rpart(reverse ~ caseSource + issueArea + 
                    lcdirectiondum + lawType,# + term, 
                  data = Train, method="class", 
                  control = rpart.control(minsplit = 5, minbucket= 2))
prp(KaganTree)
fancyRpartPlot(KaganTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020_21/kagan20_21p.png", width=6, height=6, units="in", res=300)
prp(KaganTree)
dev.off()

par(oma=c(0,0,0,1))

png(filename="./figures/OT2020_21/kagan20_21fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(KaganTree)
dev.off()

par(oma=c(0,0,0,1))

## Predictions

PredictCART = predict(KaganTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$reverse, PredictCART)
preds

## Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 65.52%%

##############################################################################
## Thomas tree 

#OT2020

#Note: generally hinged on Scalia vote while two justices were on court

### Subset training and testing data 

df <- thomas20
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

### Run tree

ThomasTree = rpart(reverse2 ~ 
                     caseSource + issueArea + petitioner + respondent + 
                     lcdirectiondum + lawType, 
                   data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))

# to complete, would need to recode caseSource and lawType to simplify values of each
prp(ThomasTree)

png(filename="./figures/OT2020/thomas20p.png", width=6, height=6, units="in", res=300)
prp(ThomasTree)
dev.off()

png(filename="./figures/OT2020/thomas20fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(ThomasTree)
dev.off()

### Predictions

PredictCART = predict(ThomasTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$directiondum, PredictCART)
preds

### Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 52.63%


# OT2021

df <- thomas21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

### Run tree

ThomasTree = rpart(reverse2 ~ 
                     caseSource + issueArea + petitioner + respondent + 
                     lcdirectiondum + lawType, 
                   data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))

# to complete, would need to recode caseSource and lawType to simplify values of each
prp(ThomasTree)

png(filename="./figures/OT2020/thomas20p.png", width=6, height=6, units="in", res=300)
prp(ThomasTree)
dev.off()

png(filename="./figures/OT2020/thomas20fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(ThomasTree)
dev.off()

### Predictions

PredictCART = predict(ThomasTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$reverse2, PredictCART)
preds

### Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 66.67%


# OT 2020-21

df <- thomas20_21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

### Run tree

ThomasTree = rpart(reverse ~ 
                     caseSource + issueArea + petitioner + respondent + 
                     lcdirectiondum + lawType, 
                   data = Train, method="class", 
                   control = rpart.control(minsplit = 5, minbucket= 2))

# to complete, would need to recode caseSource and lawType to simplify values of each
prp(ThomasTree)

png(filename="./figures/OT2020_21/thomas20_21p.png", width=6, height=6, units="in", res=300)
prp(ThomasTree)
dev.off()

par(oma=c(0,0,0,1))

png(filename="./figures/OT2020_21/thomas20_21fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(ThomasTree)
dev.off()

par(oma=c(0,0,0,1))

PredictCART = predict(ThomasTree, newdata = Test, type = "class")
preds <- table(Test$reverse, PredictCART)
preds

### Accuracy
(preds[1]+preds[4])/(sum(preds[1:4]))
# 58.62%

######################################################################
# Gorsuch Tree, OT2020-OT2021

### Subset training and testing data 

df <- gorsuch20
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

GorsuchTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                      lcdirectiondum + lawType,# + term, 
                    data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(GorsuchTree)
fancyRpartPlot(GorsuchTree)
par(oma=c(0,0,0,1))

#png(filename="./figures/OT2020/gorsuch20p.png", width=6, height=6, units="in", res=300)
#prp(GorsuchTree)
#dev.off()

#png(filename="./figures/OT2020/gorsuch20fp.png", width=6, height=6, units="in", res=300)
#fancyRpartPlot(GorsuchTree)
#dev.off()

## Predictions

PredictCART = predict(GorsuchTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$directiondum, PredictCART)
preds

## Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 57.14%

# OT 2021

df <- gorsuch21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

GorsuchTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                      lcdirectiondum + lawType,# + term, 
                    data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(GorsuchTree)
fancyRpartPlot(GorsuchTree)
par(oma=c(0,0,0,1))

## Predictions

PredictCART = predict(GorsuchTree, newdata = Test, type = "class")
preds <- table(Test$directiondum, PredictCART)
preds

## Accuracy
(preds[1]+preds[4])/(sum(preds[1:4]))
# 70%

# OT 2020-21

df <- gorsuch20_21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

GorsuchTree = rpart(reverse2 ~ caseSource + issueArea + petitioner + respondent +
                      lcdirectiondum + lawType,# + term, 
                    data = Train, method="class", 
                    control = rpart.control(minsplit = 5, minbucket= 2))
prp(GorsuchTree)
fancyRpartPlot(GorsuchTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020_21/gorsuch20_21p.png", width=6, height=6, units="in", res=300)
prp(GorsuchTree)
dev.off()
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020_21/gorsuch20_21fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(GorsuchTree)
dev.off()
par(oma=c(0,0,0,1))

## Predictions

PredictCART = predict(GorsuchTree, newdata = Test, type = "class")
preds <- table(Test$reverse2, PredictCART)
preds

## Accuracy
(preds[1]+preds[4])/(sum(preds[1:4]))
# 53.57%   

##################################################################
# Kavanaugh Tree
##################################################################

# OT2020

### Subset training and testing data 

df <- kavanaugh20
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

KavanaughTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                        lcdirectiondum + lawType,# + term, 
                      data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(KavanaughTree)
fancyRpartPlot(KavanaughTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020/kavanaugh20p.png", width=6, height=6, units="in", res=300)
prp(KavanaughTree)
dev.off()

png(filename="./figures/OT2020/kavanaugh20fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(KavanaughTree)
dev.off()

## Predictions

PredictCART = predict(KavanaughTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$directiondum, PredictCART)
preds

## Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 60%

# OT 2021

df <- kavanaugh21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

KavanaughTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                        lcDispositionDirection + lawType,# + term, 
                      data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(KavanaughTree)
fancyRpartPlot(KavanaughTree)
par(oma=c(0,0,0,1))

#png(filename="./figures/OT2020/kavanaugh20p.png", width=6, height=6, units="in", res=300)
#prp(KavanaughTree)
#dev.off()
#
#png(filename="./figures/OT2020/kavanaugh20fp.png", width=6, height=6, units="in", res=300)
#fancyRpartPlot(KavanaughTree)
#dev.off()

## Predictions

PredictCART = predict(KavanaughTree, newdata = Test, type = "class")
preds <- table(Test$directiondum, PredictCART)
preds

## Accuracy
(preds[1]+preds[4])/(sum(preds[1:4]))
# 61.90

# OT 2020-21

df <- kavanaugh20_21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.10)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

#Train = subset(df, term==2020)
#Test = subset(df, term==2021)


KavanaughTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                        lcdirectiondum + lawType,# + term, 
                      data = Train, method="class", 
                      control = rpart.control(minsplit = 5, minbucket= 2))
prp(KavanaughTree)

png(filename="./figures/OT2020_21/kavanaugh20_21p.png", width=6, height=6, units="in", res=300)
prp(KavanaughTree)
dev.off()
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020_21/kavanaugh20_21fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(KavanaughTree)
dev.off()
par(oma=c(0,0,0,1))

# Predictions
PredictCART = predict(KavanaughTree, newdata = Test, type = "class")
preds <- table(Test$reverse, PredictCART)
preds

## Accuracy
(preds[1]+preds[4])/(sum(preds[1:4]))
# 75.86%  

# accuracy graph
n <- seq(.05,.95, .01)
temp <- data.frame()
for (i in n){
  df <- kavanaugh20_21
  set.seed(3000)
  spl = sample.split(df$reverse, SplitRatio = i)
  Train = subset(df, spl==TRUE)
  Test = subset(df, spl==FALSE)
  
  KavanaughTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                          lcdirectiondum + lawType,# + term, 
                        data = Train, method="class", 
                        control = rpart.control(minsplit = 5, minbucket= 2))
  PredictCART = predict(KavanaughTree, newdata = Test, type = "class")
  preds <- table(Test$reverse, PredictCART)
  #preds
  
  ## Accuracy
  accuracy <- (preds[1]+preds[4])/(sum(preds[1:4]))
  temp <- rbind(temp, c(i, accuracy))
  
}

temp
colnames(temp) <- c("i", "accuracy")
ggplot(temp, aes(x=i, y=accuracy)) +
  geom_step()
       
##################################################################33
# Roberts Tree

#OT2020

### Subset training and testing data 

df <- roberts20
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

RobertsTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                        lcdirectiondum + lawType,# + term, 
                      data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(RobertsTree)
fancyRpartPlot(RobertsTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020/roberts20p.png", width=6, height=6, units="in", res=300)
prp(RobertsTree)
dev.off()

png(filename="./figures/OT2020/roberts20fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(RobertsTree)
dev.off()

## Predictions

PredictCART = predict(RobertsTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$directiondum, PredictCART)
preds

## Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 70%

# OT 2021

df <- roberts21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

RobertsTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                      lcdirectiondum + lawType,# + term, 
                    data = Train, method="class", 
                    control = rpart.control(minsplit = 5, minbucket= 2))
prp(RobertsTree)

## Predictions

PredictCART = predict(RobertsTree, newdata = Test, type = "class")
preds <- table(Test$directiondum, PredictCART)
preds

## Accuracy
(preds[1]+preds[4])/(sum(preds[1:4]))
# 61.90%

# OT 2020-2021

df <- roberts20_21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

RobertsTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                      lcdirectiondum + lawType,# + term, 
                    data = Train, method="class", 
                    control = rpart.control(minsplit = 5, minbucket= 2))

png(filename="./figures/OT2020_21/roberts20_21p.png", width=6, height=6, units="in", res=300)
prp(RobertsTree)
dev.off()

png(filename="./figures/OT2020_21/roberts20_21fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(RobertsTree)
dev.off()

# Predictions

PredictCART = predict(RobertsTree, newdata = Test, type = "class")
preds <- table(Test$reverse2, PredictCART)
preds

## Accuracy
(preds[1]+preds[4])/(sum(preds[1:4]))
# 62.07%

#########################################################################
# Alito Tree

# OT2020

### Subset training and testing data 

df <- alito20
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

AlitoTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                    lcdirectiondum + lawType,# + term, 
                      data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(AlitoTree)
fancyRpartPlot(AlitoTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020/alito20p.png", width=6, height=6, units="in", res=300)
prp(AlitoTree)
dev.off()

png(filename="./figures/OT2020/alito20fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(AlitoTree)
dev.off()

## Predictions

PredictCART = predict(AlitoTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$directiondum, PredictCART)
preds

## Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 52.38%

# OT 2021

df <- alito21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

AlitoTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                    lcdirectiondum + lawType,# + term, 
                  data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(AlitoTree)
fancyRpartPlot(AlitoTree)
par(oma=c(0,0,0,1))

#png(filename="./figures/OT2020/alito20p.png", width=6, height=6, units="in", res=300)
#prp(AlitoTree)
#dev.off()
#
#png(filename="./figures/OT2020/alito20fp.png", width=6, height=6, units="in", res=300)
#fancyRpartPlot(AlitoTree)
#dev.off()

## Predictions

PredictCART = predict(AlitoTree, newdata = Test, type = "class")
preds <- table(Test$directiondum, PredictCART)
preds

## Accuracy
(preds[1]+preds[4])/(sum(preds[1:4]))
# 61.90

# OT 2020-2021

df <- alito20_21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

AlitoTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                    lcdirectiondum + lawType,# + term, 
                  data = Train, method="class", 
                  control = rpart.control(minsplit = 5, minbucket= 2))
prp(AlitoTree)
fancyRpartPlot(AlitoTree, cex=0.35)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020_21/alito20_21p.png", width=6, height=6, units="in", res=300)
prp(AlitoTree)
dev.off()
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020_21/alito20_21fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(AlitoTree)
dev.off()
par(oma=c(0,0,0,1))

## Predictions

PredictCART = predict(AlitoTree, newdata = Test, type = "class")
preds <- table(Test$reverse2, PredictCART)
preds

## Accuracy
(preds[1]+preds[4])/(sum(preds[1:4]))
# 65.52

##################################################################33
# Barrett Tree, just OT2020

### Subset training and testing data 

df <- barrett20
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

BarrettTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                      lcdirectiondum + lawType,# + term, 
                      data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
prp(BarrettTree)
fancyRpartPlot(BarrettTree)
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020/Barrett20p.png", width=6, height=6, units="in", res=300)
prp(BarrettTree)
dev.off()

png(filename="./figures/OT2020/Barrett20fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(BarrettTree)
dev.off()

## Predictions

PredictCART = predict(BarrettTree, newdata = Test, type = "class")
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$directiondum, PredictCART)
preds

## Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 56.25% for OT2020


# OT 2021
df <- barrett21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

BarrettTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                      lcdirectiondum + lawType,# + term, 
                    data = Train, method="class", control = rpart.control(minsplit = 5, minbucket= 2))
# predictions
PredictCART = predict(BarrettTree, newdata = Test, type = "class")
preds <- table(Test$directiondum, PredictCART)
preds
## Accuracy
(preds[1]+preds[4])/(sum(preds[1:4]))
# 76.19%


# OT 2020-2021
df <- barrett20_21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)

BarrettTree = rpart(reverse ~ caseSource + issueArea + petitioner + respondent +
                      lcdirectiondum + lawType,# + term, 
                    data = Train, method="class", 
                    control = rpart.control(minsplit = 5, minbucket= 2))

png(filename="./figures/OT2020_21/Barrett20_21p.png", width=6, height=6, units="in", res=300)
prp(BarrettTree)
dev.off()
par(oma=c(0,0,0,1))

png(filename="./figures/OT2020_21/Barrett20_21fp.png", width=6, height=6, units="in", res=300)
fancyRpartPlot(BarrettTree)
dev.off()
par(oma=c(0,0,0,1))

# predictions
PredictCART = predict(BarrettTree, newdata = Test, type = "class")
preds <- table(Test$reverse2, PredictCART)
preds
## Accuracy
(preds[1]+preds[4])/(sum(preds[1:4]))
# 55.17% 


############################################################################

############################################################################

# Additional Material: Random Forests

# Additional, more advanced material follows below. 
# Please do not move on to that material unless you are confident 
# you understand above material. 
# We will return to material below as a class at a later point in semester.


# Random Forest (e.g. with Sotomayor, OT 2020)

#p_load(reprtree)
# may not load correctly
# if errors, then backup is to source file with code
source("reprtree2.R")

#Harder to interpret, but this is not a problem if goal is empirical prediction rather than causal explanation.

df <- sotomayor20_21
set.seed(3000)
spl = sample.split(df$reverse, SplitRatio = 0.7)
Train = subset(df, spl==TRUE)
Test = subset(df, spl==FALSE)


# using randomForest package

# Build random forest model
#GinsburgForest = randomForest(directiondum ~ caseSource + issueArea + petitioner + respondent + lcDispositionDirection + lawType, data = Train, ntree=200, nodesize=25 ) #gives error

# Convert outcome to factor // This should be done already above
#Train$directiondum = as.factor(Train$directiondum)
#Test$directiondum = as.factor(Test$directiondum)

# Try again
SotomayorForest = randomForest(reverse ~ 
                                 caseSource + issueArea + petitioner + respondent +
                                 lcdirectiondum + lawType, 
                               data = Train, ntree=200, nodesize=25, na.action=na.omit) 


#plot(SotomayorForest)
visreg(SotomayorForest)
reprtree:::plot.getTree(SotomayorForest) # gets a single tree
reprtree:::plot.reprtree(SotomayorForest, all=T, depth=0) # plots a representative tree
par(oma=c(0,0,0,1))

# using party package
SotomayorForest2 <- cforest(directiondum ~ 
                             caseSource + issueArea + petitioner + respondent +
                              lcdirectiondum + lawType, 
                           data = Train, 
                           controls=cforest_control(mtry=2, mincriterion=0, ntree=200)) 
tr <- party:::prettytree(SotomayorForest2@ensemble[[1]], names(SotomayorForest2@data@get("input")))
plot(new("BinaryTree", tree=tr, data=SotomayorForest2@data, responses=SotomayorForest2@responses))

### Predictions

# Make predictions
PredictForest = predict(SotomayorForest, newdata = Test)
# if use factored out predictors above, predict command may generate errors if have categories
# that were in training data but not in testing data
preds <- table(Test$directiondum, PredictForest)
preds


### Accuracy
#Accuracy = (topleft + bottomright)/total

(preds[1]+preds[4])/(sum(preds[1:4]))
# 100% (with both 2020 and 2021 data)


######################################################################

# save data
save.image("./data/working/working20230325.RData")

#end
