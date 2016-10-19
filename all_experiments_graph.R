## Bodo Winter
## Created September 15, 2016
## Analysis of SPEED effect across all experiments

## The individual scripts (E1_analysis.R, E2_analysis.R, E3_analysis.R)
## need to be executed first

##------------------------------------------------------------------
## Pre-processing:
##------------------------------------------------------------------

## Load in packages:

library(dplyr)

## Load in data:

setwd('/Users/winterb/Research/marcus_ACE/analysis/')
E1 <- read.csv('E1_preds.csv')
E2 <- read.csv('E2_preds.csv')
E3 <- read.csv('E3_preds.csv')

## Append:

preds <- rbind(E1, E2, E3)
preds <- cbind(data.frame(Experiment = rep(c('E1', 'E2', 'E3'),
	each = 2)), preds)

## Back-transform out of log space:

preds <- preds %>% mutate(RT = 10 ^ RT,
	UB = 10 ^ UB,
	LB = 10 ^ LB)

## Make a plot:

xfac <- 0.27
xfac2 <- 0.5
quartz('', 11, 6)
par(mai = c(1.35, 1.75, 0.5, 0.5))
plot(1, 1, type = 'n',
	xlim = c(1.25, 5.75), ylim = c(1600, 2600),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
axis(side = 1,
	at = 1:6 + c(xfac, -xfac) + c(xfac2, xfac2, 0, 0, -xfac2, -xfac2),
	labels = rep(c('fast', 'slow'), 3),
	lwd = 2, lwd.ticks = 2,
	font = 2, cex.axis = 1.4)
axis(side = 1,
	at = c(1.5 + xfac2, 3.5, 5.5 - xfac2),
	tick = F, line = 2.65, font = 2, cex.axis = 2,
	labels = c('E1', 'E2', 'E3'))
axis(side = 2,
	at = seq(1600, 2600, 200),
	font = 2, las = 2, cex.axis = 1.25)
mtext('Reaction time (ms)', side = 2,
	line = 4.65, cex = 2, font = 2)
arrows(x0 = c(1, 3, 5) + xfac + c(xfac2, 0, -xfac2),
	y0 = preds[preds$Condition == 'FAST', ]$LB,
	y1 = preds[preds$Condition == 'FAST', ]$UB,
	angle = 90, code = 3, length = 0.15, lwd = 2)
points(x = c(1, 3, 5) + xfac + c(xfac2, 0, -xfac2),
	y = preds[preds$Condition == 'FAST', ]$RT, pch = 16, cex = 2)
arrows(x0 = c(2, 4, 6) - xfac + c(xfac2, 0, -xfac2),
	y0 = preds[preds$Condition == 'SLOW', ]$LB,
	y1 = preds[preds$Condition == 'SLOW', ]$UB,
	angle = 90, code = 3, length = 0.15, lwd = 2)
points(x = c(2, 4, 6) - xfac + c(xfac2, 0, -xfac2),
	y = preds[preds$Condition == 'SLOW', ]$RT, pch = 22, cex = 2,
	bg = 'white')
UB_mean <- mean(preds[preds$Experiment == 'E3', ]$UB)
text(x = 5,
	y = UB_mean + 0.08 * UB_mean,
	labels = '*', font = 2, cex = 3)
box(lwd = 2)

