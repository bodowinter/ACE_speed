## Bodo Winter
## Originally written: April 27, 2016
## Revised and adapted to E1: September 15, 2016
## Analysis of first ACE experiment

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
first <- read.csv('E1.csv') %>% tbl_df()

## Get rid of practice trials and filler items:

first <- filter(first, Condition %in% c('FAST', 'SLOW'))

## Select columns needed:

first <- first %>% select(Subject, Age, Sex, Handedness,
	Group, Condition, Animacy, Landmark,
	Verb, ItemNo, CorrectResponse,
	MainDecision.RT, MainCrossair.RT, MainSentence.RT,
	MainDecision.ACC)

## Use CorrectResponse to reconstruct distance ('1' key was origin):

first <- first %>% mutate(Distance = ifelse(CorrectResponse == 9, 'far', 'near'))

## Get rid of participants who are left-handed:

first <- filter(first, Handedness == 'right')

## Get rid of the not-any-more-relevant variables:

first <- first %>% select(-CorrectResponse)

## Rename:

first <- first %>% rename(Sub = Subject,
	Hand = Handedness, Experiment = Group,
	Item = ItemNo, MoveRT = MainDecision.RT,
	CrossRT = MainCrossair.RT, SentRT = MainSentence.RT,
	ACC = MainDecision.ACC)

## Check subjects:

all(table(first$Sub) == 18)		# all good

## Create a combined RT and log transform:

first <- mutate(first,
	FullRT = MoveRT + SentRT,
	LogMoveRT = log10(MoveRT),
	LogSentRT = log10(SentRT),
	LogFullRT = log10(FullRT))

## Add trial variable:

first$Trial <- rep(1:18, times = length(unique(first$Sub)))

## Load in word frequencies of main verb:

SUBTL <- read.csv('verb_SUBTLEX_frequencies.csv')

## Create a chosen verb variable:

position <- ifelse(first$Condition == 'SLOW', 1, 2)
verblist <- strsplit(first$Verb, split = '/')
main_verb <- character(length(nrow(first)))
for (i in 1:length(verblist)) main_verb[i] <- verblist[[i]][position[i]]
first$ThisVerb <- main_verb

## Load in corresponding frequency data:

first$Freq <- SUBTL[match(first$ThisVerb, SUBTL$Verb), ]$Freq

## Log transform and center this, also center CrossAir RT (will be covariate):

first <- first %>% mutate(LogFreq = log10(Freq),
	LogFreq_c = LogFreq - mean(LogFreq),
	CrossRT_c = CrossRT - mean(CrossRT),
	Trial_c = Trial - mean(Trial))

## The dancing / shooting item has been miscoded with respect to the two conditions:

first[first$ThisVerb == 'shoot', ]$Condition <- 'FAST'
first[first$ThisVerb == 'dance', ]$Condition <- 'SLOW'



##------------------------------------------------------------------
## Mixed models:
##------------------------------------------------------------------

## For analysis of accuracy data, I'm going to look at Wald's Z
## so let's just sum code the predictors:

first <- first %>% mutate(Condition_c = as.factor(Condition),
	Distance_c = as.factor(Distance))
contrasts(first$Condition_c) <- contr.sum(2)
contrasts(first$Distance_c) <- contr.sum(2)

## Analyze accuracy data:

# xmdl <- glmer(ACC ~ Condition_c * Distance_c + 
	# Trial_c + 
	# (1 + Distance_c + Condition_c|Sub) +
		# (1 + Distance_c|Item) + (1 + Distance_c|ThisVerb),
	# data = first, family = 'binomial',
	# control = glmerControl(optimizer = 'bobyqa'))
# summary(xmdl)	# significant effects

## Get rid of those subjects that had ACC < 80%:

ACCs <- aggregate(ACC ~ Sub, first, mean)
bad_subs <- ACCs$Sub[ACCs$ACC < 0.8]
length(bad_subs)	# 8
oldN <- nrow(first)
first <- filter(first, !(Sub %in% bad_subs))
1 - (nrow(first) / oldN)	# ~18.6% throw-out

## Re-do accuracy analysis within the set of people that have ACC > 80%:

# xmdl <- glmer(ACC ~ Condition_c * Distance_c + 
	# Trial_c + 
	# (1 + Distance_c + Condition_c|Sub) + (1 + Distance_c|Item) +
		# (1 + Distance_c|ThisVerb),
	# data = first, family = 'binomial',
	# control = glmerControl(optimizer = 'bobyqa'))
# summary(xmdl)	# n.s.

## Explore what this means:

aggregate(ACC ~ Condition * Distance, first, mean)	# slow and to the right = inacc

## Get rid of inaccurates:

oldN <- nrow(first)
first <- filter(first, ACC == 1)
1 - (nrow(first) / oldN)	# 5.9% inaccurates deleted

## Write means and sd's to file:

xdescripts <- first %>% group_by(Condition) %>%
	summarise(meanSentRT = mean(SentRT),
		sdSentRT = sd(SentRT),
		meanLogSentRT = mean(LogSentRT),
		sdLogSentRT = sd(LogSentRT))
xdescripts$N <- length(unique(first$Sub))
write.table(xdescripts, file = 'first_descriptive.csv',
	sep = ',', row.names = F)

## Fit model, full RT:

# first$RT <- first$LogFullRT	# change to SentenceRT or MoveRT for other effects
# first$RT <- first$LogMoveRT
first$RT <- first$LogSentRT
summary(xmdl.cond <- lmer(RT ~ Condition * Distance +
	CrossRT_c + Trial_c + 
	(1 + Condition + Distance|Sub) + (1 + Distance|Item),
	data = first, REML = F))
summary(xmdl.cond.noint <- lmer(RT ~ Condition + Distance +
	CrossRT_c + Trial_c + 
	(1 + Condition + Distance|Sub) + (1 + Distance|Item),
	data = first, REML = F))
summary(xmdl.cond.nocond <- lmer(RT ~ Distance +
	CrossRT_c + Trial_c + 
	(1 + Condition + Distance|Sub) + (1 + Distance|Item),
	data = first, REML = F))
summary(xmdl.cond.nodir <- lmer(RT ~ Condition + 
	CrossRT_c + Trial_c + 
	(1 + Condition + Distance|Sub) + (1 + Distance|Item),
	data = first, REML = F))
anova(xmdl.cond.noint, xmdl.cond, test = 'Chisq')
anova(xmdl.cond.nocond, xmdl.cond.noint, test = 'Chisq')
anova(xmdl.cond.nodir, xmdl.cond.noint, test = 'Chisq')

## Do the same thing with frequency instead of condition...

# first$RT <- first$LogFullRT	# change to SentenceRT or MoveRT for other effects
# first$RT <- first$LogMoveRT
first$RT <- first$LogSentRT
summary(xmdl.freq <- lmer(RT ~ LogFreq_c * Distance +
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Distance|Sub) + (1 + Distance|Item),
	data = first, REML = F))
summary(xmdl.freq.noint <- lmer(RT ~ LogFreq_c + Distance +
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Distance|Sub) + (1 + Distance|Item),
	data = first, REML = F))
summary(xmdl.freq.nocond <- lmer(RT ~ Distance +
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Distance|Sub) + (1 + Distance|Item),
	data = first, REML = F))
summary(xmdl.freq.nodir <- lmer(RT ~ LogFreq_c + 
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Distance|Sub) + (1 + Distance|Item),
	data = first, REML = F))
anova(xmdl.freq.noint, xmdl.freq, test = 'Chisq')
anova(xmdl.freq.nocond, xmdl.freq.noint, test = 'Chisq')
anova(xmdl.freq.nodir, xmdl.freq.noint, test = 'Chisq')

## Create a model of Condition controlling for frequency:

summary(xmdl.both <- lmer(SentRT ~ LogFreq_c + Condition +
	CrossRT_c + Trial_c + 
	(1 + LogFreq_c + Distance|Sub) + (1 + Distance|Item),
	data = first, REML = F))

## Get predictions for the condition effect:

source('predict.glmm.R')
preds <- data.frame(Condition = c('SLOW', 'FAST'),
	LogFreq_c = 0, CrossRT_c = 0, Trial_c = 0)
preds <- predict.glmm(xmdl.both, newdata = preds, type = 'gaussian')

## Write these results:

write.table(preds, 'E1_preds.csv', row.names = F, sep = ',')





