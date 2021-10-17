********************************************************************************
* name: castle_cs.do
* author: scott cunningham at baylor university
* description: callaway and sant'anna with event_plot of castle doctrine reform
* last updated: october 16, 2021
********************************************************************************

clear 
capture log close

use https://github.com/scunning1975/mixtape/raw/master/castle.dta, replace

/*
3.	You can find the data in programs castle_1.do, castle_1.R and castle_1.py. How many states are treated and when?  How many states are "never treated"?
*/

gen n=1
replace effyear=0 if effyear==.
bysort effyear: egen count=sum(n)
replace count=count/11
ta effyear count

* 29 never treated states. The rest are treated. 

/*
4.	Recreate figure 9.12 from the Mixtape comparing Florida to all "never treated" states. What appears to happen in Florida with respect to homicide rates relative to the rest of the US?
*/

preserve
gen 	usa = 1 if effyear==0
replace usa = 0 if effyear==2005
drop if usa==.

collapse (sum) homicide_c population, by(year usa)
gen rate = homicide_c/population * 100000

tsset usa year

gen log_rate = ln(rate)
twoway (tsline log_rate if usa==0) (tsline log_rate if usa==1), tline(2005) title(Log homicides) subtitle(Florida vs USA) legend(order(1 "Florida" 2 "USA (excluding Florida)"))
restore

/*
6.	Estimate the effect of castle doctrine reform using CS.
a.	Specify the model using OR, IPW and DR and compare your results
b.	Specify the model using the never-treated states as controls and then separately the not-yet-treated.  Compare the ATT under both.  Show both the overall-ATT for both as well as the dynamic event study model.  Discuss who is the controls under not-yet-treated.
c.	Using the never-treated, what is the group ATT by treatment date?

*/

csdid l_homicide, ivar(sid) time(year) gvar(effyear)  method(dripw) 
// Estimation with cldid of Callaway and Sant'Anna (2020)
estat event, estore(cs) // this produces and stores the estimates at the same time
event_plot cs, default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-14(1)5) ///
	title("Callaway and Sant'Anna (2020)")) stub_lag(T+#) stub_lead(T-#) together
