* collect sex, age, and education from V[H]LSS data (1993, 1998, 2002, 2004, 2006, 2008)

merge 1:1 tinh huyen xa diaban hoso matv using muc2a1 //there is no m123a.dta in 2012 VHLSS
drop _merge

destring ed_yrs, replace


merge 1:1 tinh huyen xa diaban hoso matv using muc2a1 //there is no muc123a.dta in 2010 VHLSS
drop _merge

destring ed_yrs, replace ///because value of ed_yrs is string
