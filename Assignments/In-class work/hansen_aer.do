************************************************************
* name: hansen_aer.do
* author: scott cunningham at baylor
* description: completing the rdd assignment on Hansen's DWI AER
* last updated: october 16, 2021
************************************************************

capture log close
use https://github.com/scunning1975/causal-inference-class/raw/master/hansen_dwi, clear

* Question 1: create the cutoff variable
gen cutoff=0 if bac1~=.
replace cutoff=1 if bac1>=0.08 & bac1~=.

gen bac1_sq = bac1^2

* Recreate Figure 1 showing the density across the running variable (bac1)
histogram bac1, discrete ytitle(Density) xtitle(Blood alcohol content) xline(0.08, lwidth(medium) lpattern(solid) lcolor(red)) title(BAC Histogram) note(Hansen AER)

* McCrary density test will do a more careful close evaluation for whether there is
* manipulation on the running variable
rddensity bac1, c(0.08) plot

* There is evidence for heaping in that density test, but the density test isn't robust
* to heaping because it's not focusing on heaps, it's focusing on discontinuities
* Barreca, et al. said that if there is non-random heaping at uniform intervals on the
* running variable, you may want to consider a "donut hole" approach. That'll be to
* drop units in the vicinity of the cutoff.  So we will use both all the data in our estimation
* and we will use bac1 outside of the interval 0.078 and 0.082 (the donut hole bandwidth)

* Question 2: covariate balance on white, male, age and accident use Hansen's regression model.

* Econometric model (equation 1): yi = Xi′γ + α1DUIi + α2BACi + α3BACi × DUIi + ui
* The syntax "cutoff##c.bac1" is Stata syntax to crete *interactions* between a dummy (cutoff)
* and a continuous variable (bac1, the running variable)
regress white cutoff##c.bac1 if bac1>=0.03 & bac1<=0.13, robust
regress male cutoff##c.bac1 if bac1>=0.03 & bac1<=0.13, robust
regress age cutoff##c.(bac1 bac1_sq) if bac1>=0.03 & bac1<=0.13, robust
regress acc cutoff##c.(bac1 bac1_sq) if bac1>=0.03 & bac1<=0.13, robust

* There appears to be imbalance at the cutoff on two of our covariates, so I'm curious
* and I want to investigate it a little closer using nonparametric (visual) expected means

* Recreate Figure 2 panels a-d for covariates (which is the nonparametric equivalent to our
* global regressions above)
* Let's use cmogram and rdplot with automatic uniform bins (cmogram) and data-driven optimal bins
* (rdplot) with linear and quadratic functional forms.
cmogram aged bac1, cut(0.08) scatter line(0.08) lfitci
cmogram aged bac1, cut(0.08) scatter line(0.08) qfitci
rdplot aged bac1, p(2) masspoints(off) c(0.08) graph_options(title(RD Plot for Age of the Driver and blood alcohol content))

cmogram white bac1, cut(0.08) scatter line(0.08) lfitci
cmogram white bac1, cut(0.08) scatter line(0.08) qfitci
rdplot white bac1, p(2) masspoints(off) c(0.08) graph_options(title(RD Plot for White of the Driver and blood alcohol content))

cmogram male bac1, cut(0.08) scatter line(0.08) lfitci
cmogram male bac1, cut(0.08) scatter line(0.08) qfitci
rdplot male bac1, p(2) masspoints(off) c(0.08) graph_options(title(RD Plot for Male of the Driver and blood alcohol content))

cmogram acc bac1, cut(0.08) scatter line(0.08) lfitci
cmogram acc bac1, cut(0.08) scatter line(0.08) qfitci
rdplot acc bac1, p(1) masspoints(off) c(0.08) graph_options(title(RD Plot for Accident of the Driver and blood alcohol content))

* Reproduce Table 3 with differing bandwidths (0.05 from the cutoff; 0.025 from the cutoff)
* Local linear regressions on the exogenously chosen bandwidths.
* Econometric model (equation 1): yi = Xi′γ + α1DUIi + α2BACi + α3BACi × DUIi + ui

* Covariates, 0.05 from the cutoff bandwidth (panel A)
regress recidivism cutoff bac1 i.year male white aged if bac1>=0.03 & bac1<=0.13, robust
regress recidivism cutoff bac1 bac1_sq i.year male white aged if bac1>=0.03 & bac1<=0.13, robust

* Covariates, 0.05 from the cutoff bandwidth (panel A)
regress recidivism cutoff##c.bac1 i.year male white aged if bac1>=0.03 & bac1<=0.13, robust
regress recidivism cutoff##c.(bac1 bac1_sq) i.year male white aged if bac1>=0.03 & bac1<=0.13, robust

* Covariates. 0.025 from the cutoff (Panel B)
regress recidivism cutoff##c.bac1 i.year male white aged if bac1>=0.055 & bac1<=0.105, robust
regress recidivism cutoff##c.(bac1 bac1_sq) i.year male white aged if bac1>=0.055 & bac1<=0.105, robust

* rdrobust which is local polynomial regressions with optimally chosen data-driven bandwidths
rdrobust recidivism bac1, kernel(triangular) masspoints(off) p(2) c(0.08)
rdrobust recidivism bac1, kernel(uniform) masspoints(off) p(2) c(0.08)
rdrobust recidivism bac1, kernel(epanechnikov) masspoints(off) p(2) c(0.08)

* Create Figure 3: nonparametric estimates of recidivism at the DWI cutoff of 0.08 using
* cmogram (automatically evenly spaced bins of unknown math) and rdplot (optimal bandwidths)

cmogram recidivism bac1, cut(0.08) scatter line(0.08) lfitci
cmogram recidivism bac1 if bac1<0.4, cut(0.08) scatter line(0.08) lfitci
cmogram recidivism bac1, cut(0.08) scatter line(0.08) qfitci
cmogram recidivism bac1 if bac1<0.4, cut(0.08) scatter line(0.08) qfitci
rdplot recidivism bac1 if bac1<0.35, p(1) masspoints(off) c(0.08) graph_options(title(RD Plot for Recividism of the Driver and blood alcohol content))
rdplot recidivism bac1 if bac1<0.35, p(2) masspoints(off) c(0.08) graph_options(title(RD Plot for Recividism of the Driver and blood alcohol content))
rdplot recidivism bac1 if bac1<0.35, p(3) masspoints(off) c(0.08) graph_options(title(RD Plot for Recividism of the Driver and blood alcohol content))
rdplot recidivism bac1 if bac1<0.35, p(4) masspoints(off) c(0.08) graph_options(title(RD Plot for Recividism of the Driver and blood alcohol content))

* Donut regression: drop units at the cutoff. 
drop if bac1>=0.078 & bac1<=0.082 // drops 3,765 observations which is 1.7% of the sample
* local OLS
regress recidivism cutoff##c.bac1 i.year male white aged if bac1>=0.03 & bac1<=0.13, robust
regress recidivism cutoff##c.(bac1 bac1_sq) i.year male white aged if bac1>=0.03 & bac1<=0.13, robust

* local polynomial optimal
rdrobust recidivism bac1, kernel(triangular) masspoints(off) p(2) c(0.08)
rdrobust recidivism bac1, kernel(uniform) masspoints(off) p(2) c(0.08)
rdrobust recidivism bac1, kernel(epanechnikov) masspoints(off) p(2) c(0.08)

* Wow!  Effects are stable, but the histogram showed evidence for non-random heaping
* and when we drop at the vicinity of the cutoff, the results vanished. But let's 
* try a different donut hole.
drop if bac1>=0.079 & bac1<=0.081 // drops 1,182 observations which is 0.55% of the sample
* local polynomial optimal
rdrobust recidivism bac1, kernel(triangular) masspoints(off) p(2) c(0.08)
rdrobust recidivism bac1, kernel(uniform) masspoints(off) p(2) c(0.08)
rdrobust recidivism bac1, kernel(epanechnikov) masspoints(off) p(2) c(0.08)

cmogram recidivism bac1 if bac1<0.4, cut(0.08) scatter line(0.08) qfitci
rdplot recidivism bac1 if bac1<0.35, p(1) masspoints(off) c(0.08) graph_options(title(RD Plot for Recividism of the Driver and blood alcohol content))
rdplot recidivism bac1 if bac1<0.35, p(2) masspoints(off) c(0.08) graph_options(title(RD Plot for Recividism of the Driver and blood alcohol content))
