


devtools::install_github("synth-inference/synthdid")
install.packages("ggplot2")

library(synthdid)
library(ggplot2)




data('california_prop99')
setup = panel.matrices(california_prop99)
View(setup$Y)

## Sdid

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
print(summary(tau.hat))

set.seed(12345)
se = sqrt(vcov(tau.hat, method='placebo'))

sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)

### plot Sdid

plot(tau.hat, se.method='placebo')

## The control unit contribution plot.

synthdid_units_plot(tau.hat, se.method='placebo')



## DID 
tau.did  = did_estimate(setup$Y, setup$N0, setup$T0)
summary(tau.did)

se = sqrt(vcov(tau.did, method='placebo'))




### SC
tau.sc   = sc_estimate(setup$Y, setup$N0, setup$T0)
summary(tau.sc)

se = sqrt(vcov(tau.sc, method='placebo'))



## estimates SDID, DID, SC 
estimates = list(tau.did, tau.sc, tau.hat)
print(unlist(estimates))


###  ### plot  SDID, DID, SC 


synthdid_plot(estimates, facet.vertical=FALSE, 
              control.name='control', treated.name='california', 
              lambda.comparable=TRUE, se.method = 'none', 
              trajectory.linetype = 1, line.width=.75, effect.curvature=-.4,
              trajectory.alpha=.7, effect.alpha=.7, 
              diagram.alpha=1, onset.alpha=.7) + 
  theme(legend.position=c(.26,.07), legend.direction='horizontal', 
        legend.key=element_blank(), legend.background=element_blank(),
        strip.background=element_blank(), strip.text.x = element_blank())


#### 

synthdid_units_plot(estimates, se.method='placebo') + 
  theme(legend.background=element_blank(), legend.title = element_blank(), 
        legend.direction='horizontal', legend.position=c(.17,.07), 
        strip.background=element_blank(), strip.text.x = element_blank())






