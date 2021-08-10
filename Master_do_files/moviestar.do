clear all 
set seed 3444 

* 2500 independent draws from standard normal distribution 
set obs 250000 
generate beauty=rnormal() 
generate talent=rnormal() 

* Creating the collider variable (star) 
gen score=(beauty+talent) 
egen c85=pctile(score), p(85)   
gen star=(score>=c85) 
label variable star "Movie star" 

* No collider
gen score2 = rnormal()
egen c852=pctile(score2), p(85)   
gen star2=(score2>=c852) 
label variable star2 "Movie star" 


* Conditioning on the top 15\% 
twoway (scatter beauty talent, mcolor(black) msize(small) msymbol(smx)), ytitle(Beauty) xtitle(Talent) subtitle(Aspiring actors and actresses) by(star, total)

* Conditioning on the top 15% of the star2 variable
twoway (scatter beauty talent, mcolor(black) msize(small) msymbol(smx)), ytitle(Beauty) xtitle(Talent) subtitle(Aspiring actors and actresses) by(star2, total)
