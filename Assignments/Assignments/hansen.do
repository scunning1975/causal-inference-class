****************************************************
* title: hansen.do
* name: scott cunningham (baylor)
* description: replication of the Hansen DWI paper
* last updated: august 12, 2021
****************************************************

capture log close
clear
use https://github.com/scunning1975/causal-inference-class/raw/master/hansen_dwi, clear

* Create the dummy equalling if bac1>=0.08
gen 	dui = 0
replace dui = 1 if bac1>=0.08 & bac1~=.

* Create the polynomials
gen bac_squared = bac1^2

* Start with a real quick density test from rddensity
rddensity bac1, c(0.08) plot

* 
twoway (histogram bac1 if bac1 >= 0.03 & bac1 < 0.08, freq color(blue)) (histogram bac1 if bac1 >= 0.08 & bac1 <= 0.13, freq color(red)), xlabel(0.03(0.01)0.13) graphregion(color(white)) xtitle(bac1) ytitle(Number of Observations) legend(off)



* I need to go back and look more closely at this heaping, bc visually I can see
* regularly spaced spikes in the density at certain intervals, but I'm not yet
* sure where those are. I'm going to maybe want to think about a donut hole
* regression specification as a robustness.

* Regression model: yi = Xi′γ + α1DUIi + α2BACi + α3BACi × DUIi + u
* First use covariates as the outcome

reg white dui##c.(bac1 bac_squared) if bac1>=0.03 & bac1<=0.13, robust
reg acc dui##c.(bac1 bac_squared)  if bac1>=0.03 & bac1<=0.13, robust
reg male dui##c.(bac1 bac_squared) if bac1>=0.03 & bac1<=0.13, robust
reg aged dui##c.(bac1 bac_squared) if bac1>=0.03 & bac1<=0.13, robust

* Look at some nonparametrics

* Linear fits with confidence intervals
cmogram white bac1, cut(0.08) scatter line(0.08) lfitci
cmogram acc bac1, cut(0.08) scatter line(0.08) lfitci
cmogram male bac1, cut(0.08) scatter line(0.08) lfitci
cmogram aged bac1, cut(0.08) scatter line(0.08) lfitci


* Quadratic fits with confidence intervals
cmogram white bac1, cut(0.08) scatter line(0.08) qfitci
cmogram acc bac1, cut(0.08) scatter line(0.08) qfitci
cmogram male bac1, cut(0.08) scatter line(0.08) qfitci
cmogram aged bac1, cut(0.08) scatter line(0.08) qfitci

* Outcome regressions using local polynomial regressions
* Nonparametric
cmogram recidivism bac1, cut(0.08) scatter line(0.08) qfitci
cmogram recidivism bac1 if bac1>=0.03 & bac1<=0.13, cut(0.08) scatter line(0.08) lfitci
cmogram recidivism bac1 if bac1>=0.03 & bac1<=0.13, cut(0.08) scatter line(0.08)


* Linear
reg recid dui##c.(bac1) white male acc aged if bac1>=0.03 & bac1<=0.13, robust


* Quadratic
reg recid dui##c.(bac1 bac_squared) white male acc aged if bac1>=0.03 & bac1<=0.13, robust

reg recid dui##c.(bac1 bac_squared) white male acc aged, robust

* Nonparametric method (Cattaneo, et al)
rdrobust recidivism bac1, c(0.08)

* Summarizing: no effect in the quadratic, but in the nonparemetric visualization
* you can see the drop.  It could be the quadratic is overfitting maybe. When
* we use the linear fit, we can find strong effects, and when we use the robust
* local polynomial regression, we also find strong effects of around -.018. I think
* that's a 1.8% reduction in the probability of repeat offending if given a 
* 0.08 blood alcohol content score in their first encounter with the police.
