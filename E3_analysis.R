## Bodo Winter
## Originally written: April 27, 2016
## Revised: September 14, 2016
## Analysis of third ACE experiment

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
third <- read.csv('E3.csv') %>% tbl_df()

## Get rid of practice trials and filler items:

third <- filter(third, Condition %in% c('FAST', 'SLOW'))

## Select columns needed:

third <- third %>% select(Subject, Age, Sex, Handedness,
	Group, Direction, Condition, Direction, Animacy, Landmark,
	Verb, ItemNo, CorrectResponse,
	MainDecision.RESP:MainDecision.RT, MainCrossair.RT, 
	MainSentence.RT)

## The direction variable is miscoded, the CorrectResponse give indication:

third <- third %>%
	mutate(Direction = ifelse(CorrectResponse == 9, 'right', 'left'))

## Create an accuracy variable:

third$ACC <- 0
third[third$CorrectResponse == 9 & third$MainDecision.RESP == 9, ]$ACC <- 1
third[third$CorrectResponse == 1 & third$MainDecision.RESP == 1, ]$ACC <- 1

## Get rid of participants who are left-handed:

third <- filter(third, Handedness == 'right')

## Get rid of the not-any-more-relevant variables:

third <- third %>% select(-CorrectResponse,
	-MainDecision.RESP)

## Rename:

third <- third %>% rename(Sub = Subject,
	Hand = Handedness, Experiment = Group,
	Item = ItemNo, MoveRT = MainDecision.RT,
	CrossRT = MainCrossair.RT, SentRT = MainSentence.RT)

## Check subjects:

table(third$Sub)	# something is off with subject = 49
table(third[third$Sub == 49, ]$Age)	# it's two different people
third[third$Sub == 49, ]$Sub <- paste(third[third$Sub == 49, ]$Sub,
	third[third$Sub == 49, ]$Age, sep = ':')

## Create a combined RT and log transform:

third <- mutate(third,
	FullRT = MoveRT + SentRT,
	LogMoveRT = log10(MoveRT),
	LogSentRT = log10(SentRT),
	LogFullRT = log10(FullRT))

## Add trial variable:

third$Trial <- rep(1:18, times = length(unique(third$Sub)))

## Load in word frequencies of main verb:

SUBTL <- read.csv('verb_SUBTLEX_frequencies.csv')

## Create a chosen verb variable:

position <- ifelse(third$Condition == 'SLOW', 1, 2)
verblist <- strsplit(third$Verb, split = '/')
main_verb <- character(length(nrow(third)))
for (i in 1:length(verblist)) main_verb[i] <- verblist[[i]][position[i]]
third$ThisVerb <- main_verb

## Load in corresponding frequency data:

third$Freq <- SUBTL[match(third$ThisVerb, SUBTL$Verb), ]$Freq

## Log transform and center this, also center CrossAir RT (will be covariate):

third <- third %>% mutate(LogFreq = log10(Freq),
	LogFreq_c = LogFreq - mean(LogFreq),
	CrossRT_c = CrossRT - mean(CrossRT),
	Trial_c = Trial - mean(Trial))

## The dancing / shooting item has been miscoded with respect to the two conditions:

third[third$ThisVerb == 'shoot', ]$Condition <- 'FAST'
third[third$ThisVerb == 'dance', ]$Condition <- 'SLOW'



##------------------------------------------------------------------
## Mixed models:
##------------------------------------------------------------------

## For analysis of accuracy data, I'm going to look at Wald's Z
## so let's just sum code the predictors:

third <- third %>% mutate(Condition_c = as.factor(Condition),
	Direction_c = as.factor(Direction))
contrasts(third$Condition_c) <- contr.sum(2)
contrasts(third$Direction_c) <- contr.sum(2)

## Analyze accuracy data:

# xmdl <- glmer(ACC ~ Condition_c * Direction_c + 
	# Trial_c + 
	# (1 + Condition_c|Sub) + (1 + Direction_c|Item) + (1 + Direction_c|ThisVerb),
	# data = third, family = 'binomial',
	# control = glmerControl(optimizer = 'bobyqa'))
# summary(xmdl)	# nearly significant effects

## Get rid of those subjects that had ACC < 80%:

ACCs <- aggregate(ACC ~ Sub, third, mean)
bad_subs <- ACCs$Sub[ACCs$ACC < 0.8]
length(bad_subs)	# 14
oldN <- nrow(third)
third <- filter(third, !(Sub %in% bad_subs))
1 - (nrow(third) / oldN)	# ~18% throw-out

## Re-do accuracy analysis within the set of people that have ACC > 80%:

# xmdl <- glmer(ACC ~ Condition_c * Direction_c + 
	# (1 + Condition_c|Sub) + (1 + Direction_c|Item) + (1 + Direction_c|ThisVerb),
	# data = third, family = 'binomial',
	# control = glmerControl(optimizer = 'bobyqa'))
# summary(xmdl)	# p = 0.048

## Explore what this means:

aggregate(ACC ~ Condition * Direction, third, mean)	# slow and to the right = inacc

## Get rid of inaccurates:

oldN <- nrow(third)
third <- filter(third, ACC == 1)
1 - (nrow(third) / oldN)	# 7.2% inaccurates deleted

## Write means and sd's to file:

xdescripts <- third %>% group_by(Condition) %>%
	summarise(meanSentRT = mean(SentRT),
		sdSentRT = sd(SentRT),
		meanLogSentRT = mean(LogSentRT),
		sdLogSentRT = sd(LogSentRT))
xdescripts$N <- length(unique(third$Sub))
write.table(xdescripts, file = 'third_descriptive.csv',
	sep = ',', row.names = F)

## Fit model, full RT:

# third$RT <- third$LogFullRT	# change to SentenceRT or MoveRT for other effects
# third$RT <- third$LogMoveRT
third$RT <- third$LogSentRT
summary(xmdl.cond <- lmer(RT ~ Condition * Direction +
	CrossRT_c + Trial_c + 
	(1 + Condition|Sub) + (1 + Direction|Item),
	data = third, REML = F))
summary(xmdl.cond.noint <- lmer(RT ~ Condition + Direction +
	CrossRT_c + Trial_c + 
	(1 + Condition|Sub) + (1 + Direction|Item),
	data = third, REML = F))
summary(xmdl.cond.nocond <- lmer(RT ~ Direction +
	CrossRT_c + Trial_c + 
	(1 + Condition|Sub) + (1 + Direction|Item),
	data = third, REML = F))
summary(xmdl.cond.nodir <- lmer(RT ~ Condition + 
	CrossRT_c + Trial_c + 
	(1 + Condition|Sub) + (1 + Direction|Item),
	data = third, REML = F))
anova(xmdl.cond.noint, xmdl.cond, test = 'Chisq')
anova(xmdl.cond.nocond, xmdl.cond.noint, test = 'Chisq')	# condition effect
anova(xmdl.cond.nodir, xmdl.cond.noint, test = 'Chisq')

## Do the same thing with frequency instead of condition...

# third$RT <- third$LogFullRT	# change to SentenceRT or MoveRT for other effects
# third$RT <- third$LogMoveRT
third$RT <- third$LogSentRT
summary(xmdl.freq <- lmer(RT ~ LogFreq_c * Direction +
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c|Sub) + (1 + Direction|Item),
	data = third, REML = F))
summary(xmdl.freq.noint <- lmer(RT ~ LogFreq_c + Direction +
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c|Sub) + (1 + Direction|Item),
	data = third, REML = F))
summary(xmdl.freq.nocond <- lmer(RT ~ Direction +
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c|Sub) + (1 + Direction|Item),
	data = third, REML = F))
summary(xmdl.freq.nodir <- lmer(RT ~ LogFreq_c + 
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c|Sub) + (1 + Direction|Item),
	data = third, REML = F))
anova(xmdl.freq.noint, xmdl.freq, test = 'Chisq')
anova(xmdl.freq.nocond, xmdl.freq.noint, test = 'Chisq')
anova(xmdl.freq.nodir, xmdl.freq.noint, test = 'Chisq')

## Create a model of Condition controlling for frequency:

summary(xmdl.both <- lmer(RT ~ LogFreq_c + Condition +
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Condition|Sub) + (1|Item),
	data = third, REML = F))
summary(xmdl.both.nocond <- lmer(RT ~ LogFreq_c + 
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Condition|Sub) + (1|Item),
	data = third, REML = F))
anova(xmdl.both.nocond, xmdl.both, test = 'Chisq')

## Get predictions for the condition effect:

source('predict.glmm.R')
preds <- data.frame(Condition = c('SLOW', 'FAST'),
	LogFreq_c = 0, CrossRT_c = 0, Trial_c = 0)
preds <- predict.glmm(xmdl.both, newdata = preds, type = 'gaussian')

## Write these results:

write.table(preds, 'E3_preds.csv', row.names = F, sep = ',')



