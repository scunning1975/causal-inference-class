********************************************************************************
* name: pod.do
* author: scott cunningham (baylor) 
* description: independence example
* last updated: August 2, 2021
********************************************************************************

clear
capture log close
set seed 20200403

* This is an example of independence because notice that the treatment
* variable, d, isn't dependent on y0 or y1 or any combination of them.
set obs 20000
gen person = _n
gen y0 = runiform(0,100)
gen y1 = 100 + runiform(0,100)

* d has some distribution that is completely unrelated to each person's
* potential outcome
gen treatment = runiform(0,1)
gen d=0
replace d=1 if treatment>=0.5

* (Y0,Y1) _||_ D is nothing more than lines 14-23. 

bysort d: sum y0
bysort d: sum y1
