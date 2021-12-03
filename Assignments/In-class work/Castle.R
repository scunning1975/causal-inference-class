
devtools::install_github("synth-inference/synthdid")
library(synthdid)
library(ggplot2)
library(readstata13) 
library(dplyr)
library(readstata13)

### Load the castle doctrine from github and name CD
CD <- data.frame(read.dta13('https://github.com/scunning1975/mixtape/raw/master/castle.dta'))

### Filling NA value in effective year by zero
table(CD$effyear, useNA = "ifany")
CD$effyear[is.na(CD$effyear)] = 0

### Check that it made the change to effective year (i.e., NA to 0)
sum(is.na(CD$effyear))

## Dropping All the treatment group except the States that adopted the CDL in 2006.
Group_2006<- CD %>%
  filter(effyear!=2005 , effyear!=2007, effyear!=2008, effyear!=2009)
table(Group_2006$effyear)


Group_2006_1<- Group_2006 %>%
  select(state ,year,l_homicide, cdl)

table(Group_2006_1$cdl)

Group_2006_1$cdl<- ifelse(Group_2006_1$cdl>0, 1,0)

table(Group_2006_1$cdl)


## Sdid
setup = panel.matrices(Group_2006_1)

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
Sdid<- print(summary(tau.hat))


### plot Sdid
plot_sdid<-plot(tau.hat, se.method='bootstrap')

## The control unit contribution plot.
plot_sdid_weight<-synthdid_units_plot(tau.hat, se.method='bootstrap')



## DID 
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
DID<-summary(tau.did)


### SC
tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
SC<-summary(tau.sc)



## estimates SDID, DID, SC
estimates = list(tau.did, tau.sc, tau.hat)
print(unlist(estimates))



### plot  SDID, DID, SC 
Plot<-synthdid_plot(estimates, facet.vertical=FALSE, 
              control.name='control', treated.name='Treatment', 
              lambda.comparable=TRUE, se.method = 'none', 
              trajectory.linetype = 1, line.width=.75, effect.curvature=-.4,
              trajectory.alpha=.7, effect.alpha=.7, 
              diagram.alpha=1, onset.alpha=.7) + expand_limits(y=seq(0,3,1))+ scale_y_continuous(breaks=seq(0,3,1)) +  
  theme(legend.position=c(.26,.07), legend.direction='horizontal', 
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank())


Weights<-synthdid_units_plot(estimates, se.method='bootstrap') + 
  theme(legend.background=element_blank(), legend.title = element_blank(), 
        legend.direction='horizontal', legend.position=c(.17,.07), 
        strip.background=element_blank(), strip.text.x = element_blank())


