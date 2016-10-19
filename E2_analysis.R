## Bodo Winter
## Originally written: April 27, 2016
## Revised and adapted to E2: September 14, 2016
## Analysis of second ACE experiment

## TO DO:
# - am I sure that the direction is right?


##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Packages:

library(lme4)
library(dplyr)

## Set options:

options(stringsAsFactors = F)
options(dplyr.width = Inf)

## Load in data:

setwd('/Users/winterb/Research/marcus_ACE/analysis/')
second <- read.csv('E2.csv') %>% tbl_df()

## Get rid of practice trials and filler items:

second <- filter(second, Condition %in% c('FAST', 'SLOW'))

## Select columns needed:

second <- second %>% select(Subject, Age, Sex, Handedness,
	Group, Condition, Animacy, Landmark,
	Verb, ItemNo, CorrectResponse,
	MainDecision.RESP:MainDecision.RT,
	MainCrossair.RT, MainSentence.RT,
	MainDecision2.RESP:MainDecision2.RT,
	MainCrossair2.RT, MainSentence2.RT)

## Merge columns from the two blocks:

second[is.na(second$MainDecision.RESP), ]$MainDecision.RESP <- 
	second[is.na(second$MainDecision.RESP), ]$MainDecision2.RESP
second[is.na(second$MainCrossair.RT), ]$MainCrossair.RT <- 
	second[is.na(second$MainCrossair.RT), ]$MainCrossair2.RT
second[is.na(second$MainDecision.RT), ]$MainDecision.RT <- 
	second[is.na(second$MainDecision.RT), ]$MainDecision2.RT
second[is.na(second$MainSentence.RT), ]$MainSentence.RT <- 
	second[is.na(second$MainSentence.RT), ]$MainSentence2.RT

## Get rid of the "2" ones (not needed anymore since merged):

second <- second %>% select(-MainDecision2.RESP,
	-MainDecision2.RT, -MainCrossair2.RT, -MainSentence2.RT)

## The direction variable is miscoded, the CorrectResponse give indication:

second <- second %>% mutate(Direction = ifelse(CorrectResponse == 9, 'right', 'left'))

## Create an accuracy variable:

second$ACC <- 0
second[second$CorrectResponse == 9 & second$MainDecision.RESP == 9, ]$ACC <- 1
second[second$CorrectResponse == 1 & second$MainDecision.RESP == 1, ]$ACC <- 1

## Get rid of participants who are left-handed:

second <- filter(second, Handedness == 'right')

## Get rid of the not-any-more-relevant variables:

second <- second %>% select(-CorrectResponse,
	-MainDecision.RESP)

## Rename:

second <- second %>% rename(Sub = Subject,
	Hand = Handedness, Experiment = Group,
	Item = ItemNo, MoveRT = MainDecision.RT,
	CrossRT = MainCrossair.RT, SentRT = MainSentence.RT)

## Check subjects:

table(second$Sub)	# something is off with subject = 1
table(second[second$Sub == 1, ]$Age)	# it's two different people
second[second$Sub == 1, ]$Sub <- paste(second[second$Sub == 1, ]$Sub,
	second[second$Sub == 1, ]$Age, sep = ':')

## Create a combined RT and log transform:

second <- mutate(second,
	FullRT = MoveRT + SentRT,
	LogMoveRT = log10(MoveRT),
	LogSentRT = log10(SentRT),
	LogFullRT = log10(FullRT))

## Add trial variable:

second$Trial <- rep(1:18, times = length(unique(second$Sub)))

## Load in word frequencies of main verb:

SUBTL <- read.csv('verb_SUBTLEX_frequencies.csv')

## Create a chosen verb variable:

position <- ifelse(second$Condition == 'SLOW', 1, 2)
verblist <- strsplit(second$Verb, split = '/')
main_verb <- character(length(nrow(second)))
for (i in 1:length(verblist)) main_verb[i] <- verblist[[i]][position[i]]
second$ThisVerb <- main_verb

## Load in corresponding frequency data:

second$Freq <- SUBTL[match(second$ThisVerb, SUBTL$Verb), ]$Freq

## Log transform and center this, also center CrossAir RT (will be covariate):

second <- second %>% mutate(LogFreq = log10(Freq),
	LogFreq_c = LogFreq - mean(LogFreq),
	CrossRT_c = CrossRT - mean(CrossRT),
	Trial_c = Trial - mean(Trial))

## The dancing / shooting item has been miscoded with respect to the two conditions:

second[second$ThisVerb == 'shoot', ]$Condition <- 'FAST'
second[second$ThisVerb == 'dance', ]$Condition <- 'SLOW'


##------------------------------------------------------------------
## Mixed models:
##------------------------------------------------------------------

## For analysis of accuracy data, I'm going to look at Wald's Z
## so let's just sum code the predictors:

second <- second %>% mutate(Condition_c = as.factor(Condition),
	Direction_c = as.factor(Direction))
contrasts(second$Condition_c) <- contr.sum(2)
contrasts(second$Direction_c) <- contr.sum(2)

## Analyze accuracy data:

# xmdl <- glmer(ACC ~ Condition_c * Direction_c + 
	# Trial_c + 
	# (1 + Condition_c|Sub) + (1 + Direction_c|Item) + (1 + Direction_c|ThisVerb),
	# data = second, family = 'binomial',
	# control = glmerControl(optimizer = 'bobyqa'))
# summary(xmdl)	# significant effects

## Get rid of those subjects that had ACC < 80%:

ACCs <- aggregate(ACC ~ Sub, second, mean)
bad_subs <- ACCs$Sub[ACCs$ACC < 0.8]
length(bad_subs)	# 9
oldN <- nrow(second)
second <- filter(second, !(Sub %in% bad_subs))
1 - (nrow(second) / oldN)	# ~20% throw-out

## Re-do accuracy analysis within the set of people that have ACC > 80%:

# xmdl <- glmer(ACC ~ Condition_c * Direction_c + 
	# (1 + Condition_c|Sub) + (1 + Direction_c|Item) + (1 + Direction_c|ThisVerb),
	# data = second, family = 'binomial',
	# control = glmerControl(optimizer = 'bobyqa'))
# summary(xmdl)	# p = 0.048

## Explore what this means:

aggregate(ACC ~ Condition * Direction, second, mean)	# slow and to the right = inacc

## Get rid of inaccurates:

oldN <- nrow(second)
second <- filter(second, ACC == 1)
1 - (nrow(second) / oldN)	# 6.8% inaccurates deleted

## Write means and sd's to file:

xdescripts <- second %>% group_by(Condition) %>%
	summarise(meanSentRT = mean(SentRT),
		sdSentRT = sd(SentRT),
		meanLogSentRT = mean(LogSentRT),
		sdLogSentRT = sd(LogSentRT))
xdescripts$N <- length(unique(second$Sub))
write.table(xdescripts, file = 'second_descriptive.csv',
	sep = ',', row.names = F)

## Fit model, full RT:

second$RT <- second$LogFullRT	# change to SentenceRT or MoveRT for other effects
second$RT <- second$LogMoveRT
second$RT <- second$LogSentRT
summary(xmdl.cond <- lmer(RT ~ Condition * Direction +
	CrossRT_c + Trial_c + 
	(1 + Condition + Direction|Sub) + (1 + Direction|Item),
	data = second, REML = F))
summary(xmdl.cond.noint <- lmer(RT ~ Condition + Direction +
	CrossRT_c + Trial_c + 
	(1 + Condition + Direction|Sub) + (1 + Direction|Item),
	data = second, REML = F))
summary(xmdl.cond.nocond <- lmer(RT ~ Direction +
	CrossRT_c + Trial_c + 
	(1 + Condition + Direction|Sub) + (1 + Direction|Item),
	data = second, REML = F))
summary(xmdl.cond.nodir <- lmer(RT ~ Condition + 
	CrossRT_c + Trial_c + 
	(1 + Condition + Direction|Sub) + (1 + Direction|Item),
	data = second, REML = F))
anova(xmdl.cond.noint, xmdl.cond, test = 'Chisq')
anova(xmdl.cond.nocond, xmdl.cond.noint, test = 'Chisq')	# condition effect
anova(xmdl.cond.nodir, xmdl.cond.noint, test = 'Chisq')

## Do the same thing with frequency instead of condition...

# second$RT <- second$LogFullRT	# change to SentenceRT or MoveRT for other effects
# second$RT <- second$LogMoveRT
second$RT <- second$LogSentRT
summary(xmdl.freq <- lmer(RT ~ LogFreq_c * Direction +
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Direction|Sub) + (1 + Direction|Item),
	data = second, REML = F))
summary(xmdl.freq.noint <- lmer(RT ~ LogFreq_c + Direction +
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Direction|Sub) + (1 + Direction|Item),
	data = second, REML = F))
summary(xmdl.freq.nocond <- lmer(RT ~ Direction +
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Direction|Sub) + (1 + Direction|Item),
	data = second, REML = F))
summary(xmdl.freq.nodir <- lmer(RT ~ LogFreq_c + 
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Direction|Sub) + (1 + Direction|Item),
	data = second, REML = F))
anova(xmdl.freq.noint, xmdl.freq, test = 'Chisq')
anova(xmdl.freq.nocond, xmdl.freq.noint, test = 'Chisq')
anova(xmdl.freq.nodir, xmdl.freq.noint, test = 'Chisq')

## Create a model of Condition controlling for frequency:

summary(xmdl.both <- lmer(RT ~ LogFreq_c + Condition +
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Condition|Sub) + (1|Item),
	data = second, REML = F))
summary(xmdl.both.nocond <- lmer(RT ~ LogFreq_c + 
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Condition|Sub) + (1|Item),
	data = second, REML = F))
anova(xmdl.both.nocond, xmdl.both, test = 'Chisq')

## Get predictions for the condition effect:

source('predict.glmm.R')
preds <- data.frame(Condition = c('SLOW', 'FAST'),
	LogFreq_c = 0, CrossRT_c = 0, Trial_c = 0)
preds <- predict.glmm(xmdl.both, newdata = preds, type = 'gaussian')

## Write these results:

write.table(preds, 'E2_preds.csv', row.names = F, sep = ',')


