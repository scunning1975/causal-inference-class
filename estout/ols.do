clear
capture log close

* This do file will estimate a simple regression and produce a simple table with covariates

use https://github.com/scunning1975/mixtape/raw/master/card.dta, clear


				cap n tempvar tempsample
				cap n local specname=`specname'+1

				* Column 1: No covariates
				cap n reg lwage educ, robust
				cap n estimates store educ1
				cap n estadd ysumm


				* Column 2: Covariates
				cap n reg lwage educ exper black south married smsa, robust
				cap n estimates store educ2
				cap n estadd ysumm


#delimit ;
	cap n estout * using ./education.tex,
		style(tex) label notype
		cells((b(star fmt(%9.3f))) (se(fmt(%9.3f)par))) 		
		stats(N ymean,
			labels("N" "Mean of dependent variable")
			fmt(%9.0fc %9.2fc 2))
			drop(_cons) 
			replace noabbrev starlevels(* 0.10 ** 0.05 *** 0.01) 
			title(OLS estimates of effect of college on log wages)   
			collabels(none) eqlabels(none) mlabels(none) mgroups(none) substitute(_ \_)
			prehead("\begin{table}[htbp]\centering" "\small" "\caption{@title}" "\begin{center}" "\begin{threeparttable}" "\begin{tabular}{l*{@E}{c}}"
	"\toprule"
	"\multicolumn{1}{l}{\textbf{Dependent variable: log(wages)}}&"
	"\multicolumn{1}{c}{\textbf{(1)}}&"
	"\multicolumn{1}{c}{\textbf{(2)}}\\")
		posthead("\midrule")
		prefoot("\midrule")  
		postfoot("\bottomrule" "\end{tabular}" "\begin{tablenotes}" "\tiny" "\item Data is from NLS.  Heteroskedastic standard errors shown in parenthesis.  * p$<$0.10, ** p$<$0.05, *** p$<$0.01" "\end{tablenotes}" "\end{threeparttable}" "\end{center}" "\end{table}");
#delimit cr
	cap n estimates clear