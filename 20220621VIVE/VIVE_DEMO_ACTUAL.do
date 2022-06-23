// TWFE - Solutions - on actual data
// Set Wd
cd "/Users/hhs/Dropbox/VIVE20220621"
// Load data
u "nepaldata.dta",clear

// TWFE  
reghdfe o_neo t_chx_start_treated, absorb(c_cmc c_sdist )

// GB decomp
gen id=_n
xtset id c_cmc 
bacondecomp o_neo t_chx_start_treated, ddetail

// CD weights
twowayfeweights o_neo c_sdist c_cmc t_chx_start_treated  , type(feTR) path("weights.dta") 


// CD estiamator (static)
did_multiplegt o_neo c_sdist c_cmc t_chx_start_treated,breps(50) 

// CS
u "nepaldata.dta",clear
gen id=_n
gen _t=c_cmcq if t_chx_start_treated==1
bys c_sdist: egen Ei=min(_t)
gen gvar = cond(Ei==., 0, Ei)
csdid o_neo, ivar(id) time(c_cmcq) gvar(gvar)  method(reg) notyet


// Borusyak
u "nepaldata.dta",clear
gen id=_n
gen _t=c_cmcq if t_chx_start_treated==1
bys c_sdist: egen Ei=min(_t)
did_imputation o_neo id c_cmcq Ei,  horizons(0/12) pretrend(12)   fe(c_sdist ) 

event_plot, default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") /// 
xlabel(-12(2)12))
	
did_imputation o_neo id c_cmcq Ei,    fe(c_sdist ) cluster(c_sdist)
