// load data with Sønderborg and Tønder
u analysisdata.dta,clear

// basic chart (note that I used jitter in R to spread obs more out)
tw (scatter y Date if G==1) ///
   (scatter y Date if G==2) ///
    , legend(order(1 "Tønder" 2 "Sønderborg"))
// basic reg
gen after=t>5
reg y after treated afterXtreated

// twfe 
reghdfe y D,absorb(m t)

// load updated data
u analysisdata_update.dta,clear


// basic chart (note that I used jitter in R to spread obs more out)
tw (scatter y Date if G==1) ///
   (scatter y Date if G==2) ///
   (scatter y Date if G==3) ///
    , legend(order(1 "Tønder" 2 "Sønderborg" 3 "Aabenraa"))
	
// twfe 
reghdfe y D,absorb(m t)	


