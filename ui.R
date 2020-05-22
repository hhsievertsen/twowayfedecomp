# Define UI for bacondecomp
mycss <- "
.irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
  background: black;
  border-color: black;
},
body{font-size:11px;background-color:#FFFFFF
}
#RegSum1{font-size:11px}
#RegSum2{font-size:9px}
label{font-weight:normal}

"
ui <- fluidPage(
    tags$head(
        tags$style(mycss)
    ),
    # Sidebar for settings
    column(3,
           # Settings
           strong("Settings"),br(),
           # Inputs
           numericInput(inputId= "seed",label="Set seed", 1909, min = 0, max = NA),
           strong("Treatment effects"),
           sliderInput(inputId = "group2treatmenteffect",label = "Group 2 (baseline):",min = 0,max = 3,value = 1.4,step=0.05, ticks = FALSE),
           sliderInput(inputId = "group3treatmenteffect",label = "Group 3 (baseline):",min = 0,max = 3,value = 1.6,step=0.05, ticks = FALSE),br(),
           strong("Time-varying treatment effects"),
           sliderInput(inputId = "group2timeeffect",label = "Group 2 growth rate:",min = -0.03,max = 0.03,value = 0.02,step=0.005, ticks = FALSE),
           sliderInput(inputId = "group3timeeffect",label = "Group 3 growth rate:",min = -0.03,max = 0.03,value = 0,step=0.005, ticks = FALSE),br(),
           strong("Treatment timing"),
           sliderInput(inputId = "group2treatment",label = "Group 2:",min = 2,max = 49,value = 20, ticks = FALSE),
           sliderInput(inputId = "group3treatment",label = "Group 3:",min = 2,max = 49,value = 40, ticks = FALSE),br(),
           strong("Group size"),
           sliderInput(inputId = "group2size",label = "Group 2:",min = 2,max = 50,value = 25, ticks = FALSE),
           sliderInput(inputId = "group3size",label = "Group 3:",min = 2,max = 50,value = 25, ticks = FALSE),
           
    ),
    # Main panel with results
    column(6,
           h3("Illustration of two-way fixed effects estimator decomposition"),br(),
           "The simulation is based on Goodman-Bacon (2019): 'DD  with Variation in Treatment Timing' and adopts a staggered adoption design'.", br(),
          "For the general case see de Chaisemartin & D'Haultfoeuille (2019): 'Two-way fixed effects estimators with heterogeneous treatment effects'",br(),br(),
           "by Hans H. Sievertsen", tags$a(href="https://github.com/hhsievertsen/", "(github)"), tags$a(href="mailto:h.h.sievertsen@bristol.ac.uk", "h.h.sievertsen@bristol.ac.uk"),br(),br(),br(),
           # chart
           plotOutput(outputId = "distPlot"),
           tableOutput(outputId = "RegSum1"),br(),
           "Notes: The DGP  for the overall ATT (first row) refers to the population weighted ATT across the two groups. The estimate for the overall ATT (first row) is the 2-way fixed effects estimate.",
    ),
    # Side bar with info
    fluidRow(column(3,
                    # Explanation
                  strong("! Change settings in the panel on the left !"),br(), 
                  br(),
                    strong("Setup"),br(),
                    "- 3 groups. Group 1 has a fixed size of 30.",br(),
                    "- Group 1 is never treated.",br(),
                    "- Groups 2 & 3 get treated at some point (see left panel).",br(),
                    "- Treatment effects can vary across groups and over time (see left panel).",br(),
                    "- The two-way fixed effects DD  is estimated with felm() from the lfe package, by estimating equation (2) from Goodman-Bacon (2019):",br(),
                    br("Sources"),
                    "* ",  tags$a(href="https://cdn.vanderbilt.edu/vu-my/wp-content/uploads/sites/2318/2019/07/29170757/ddtiming_7_29_2019.pdf", "Goodman-Bacon (2019) Working Paper"),br(),
                  "* ",  tags$a(href="https://sites.google.com/site/clementdechaisemartin/two_way_FE.pdf?attredirects=0&d=1", "de Chaisemartin & D'Haultfoeuille (2019): 'Two-way fixed effects estimators with heterogeneous treatment effects'"),
                    br(),
                    "* ",tags$a(href="https://cran.r-project.org/web/packages/bacondecomp/readme/README.html", "bacondecomp for R"),
               
                   br(),br(),
                   strong("Updates:"),br(),
                   "- May 12, 2020: first version by Hans H. Sievertsen",br(),
                   "- May 13, 2020: incorporated changes by Matthieu Stigler (https://matthieustigler.github.io/)",br(),
                   "- May 14, 2020: corrected my mistake in calculating ATT and updated table.",br(),
                   "- May 17, 2020: added event study chart and DGP values to table.",br(),
                    "- May 22, 2020: minor adjustments.",br(),br(),
                   "Corrections and suggestions are very welcome (by e-mail:", tags$a(href="mailto:h.h.sievertsen@bristol.ac.uk", "h.h.sievertsen@bristol.ac.uk"), "or on github:", 
                  tags$a(href="https://github.com/hhsievertsen/twowayfedecomp","github.com/hhsievertsen/twowayfedecomp"),br(),br(),br()
                  ,
                  strong("Regression output:"),
                  verbatimTextOutput(outputId = "RegSum2"),
    )
   ))