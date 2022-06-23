// TWFE - Solutions - on Simulated data
// Set Wd
cd "/Users/hhs/Dropbox/VIVE20220621"
// Load data
u "analysisdata.dta",clear
// ATT
sum true 
// TWFE  
reghdfe y D, absorb(G t)
// Show scatter
preserve
collapse (mean) y ,by(t G)
	tw (scatter y t if G==1) ///
	   (scatter y t if G==2) ///
	   (scatter y t if G==3) 
restore

// Goodman-Bacon decomposition
*ssc install bacondecomp
xtset id t
bacondecomp y D, ddetail
mat list e(dd)
mat list e(wt)

// de Chaisemartin and D'Haultfoeuille weights
*ssc install twowayfeweights
twowayfeweights y G t D  , type(feTR) path("weights.dta") 
sum(true)
u weights.dta,clear


// de Chaisemartin and D'Haultfoeuille estimator (non dynamic)
u "analysisdata.dta",clear
*ssc install did_multiplegt
did_multiplegt y G t D,breps(50)



//  Callaway and Sant'Anna (2020)
*ssc install csdid
*ssc install event_plot*
u "analysisdata.dta",clear
gen newG=0 if G==1
replace newG=5 if G==2
replace newG=30 if G==3

csdid y, ivar(id) time(t) gvar(newG) method(reg)
csdid y, ivar(id) time(t) gvar(newG) agg(event)  method(reg)
estat event, estore(cs)


event_plot cs, default_look stub_lag(Tp#) stub_lead(Tm#) 
	
// Borusyak et al. (2021)
*ssc install did_imputation
u "analysisdata.dta",clear
gen _t=t if D==1
bys G: egen startdate=min(_t)
drop _t
did_imputation y id t startdate, allhorizons pretrend(10) fe(G)

event_plot, default_look
	
ereturn list
// Aggregate
did_imputation y id t startdate, fe(G)
