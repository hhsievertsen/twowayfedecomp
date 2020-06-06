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
    withMathJax(),
    tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")),
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
           sliderInput(inputId = "group2size",label = "Group 2:",min = 2,max = 30,value = 15, ticks = FALSE),
           sliderInput(inputId = "group3size",label = "Group 3:",min = 2,max = 30,value = 15, ticks = FALSE),
           downloadButton('download',"Download the data"),
           
    ),
    # Main panel with results
    column(6,
           h3("Illustration of the two-way fixed effects estimator decomposition"),br(),
          
          
           plotOutput(outputId = "distPlot"),
          textOutput("cdout"),br(),
          strong("A. DGP, population shares and the two FE estimate:"),
           tableOutput(outputId = "RegSum1"),br(),
          column(6, strong("B. Goodman-Bacon Decomposition:"),
          tableOutput(outputId = "bacon"),br()),
          column(1," "),
          column(6,strong("C. Chaisemartin & D'Haultfoeuille decomposition:"),
          tableOutput(outputId = "cd"))
    ),
    # Side bar with info
    fluidRow(column(3,
 
                    strong("Description"),br(),br(),
                  "- Let g denote the group, t the period, d be a binary treatment indicator, y an outcome.",br(),
                  "- We estimate $ y_{i,g,t}=\\alpha+\\gamma' G_{g}+\\psi' T_{t}+ \\beta_{fe} D_{g,t}+e_{i,g,t} $",br(), "using OLS.",br(),br(),
                  "- ", tags$a(href="https://sites.google.com/site/clementdechaisemartin/two_way_FE.pdf?attredirects=0&d=1", 
                               "Chaisemartin & D'Haultfoeuille [forthcoming, AER]"), "show that we can decompose $\\beta_{fe}$ into a weighted average of the treated $(g,t)$ cells ATTs.",br(),
                  "-  ", tags$a(href="https://cdn.vanderbilt.edu/vu-my/wp-content/uploads/sites/2318/2019/07/29170757/ddtiming_7_29_2019.pdf",
                                "Goodman-Bacon [2019, WP]"), "shows that we can decompose $\\beta_{fe}$ into a weighted average of the separate difference-in-differences [DD].",br(),br(),
                  tags$b("This tool"), "allows you to",br(),
                  "1. simulate a simple scenario with G=3, T=50, two treated groups and treatment effects that vary over time and across groups",br(),
                  "2. compare $\\hat{\\beta}_{fe}$  to the population weighted true treatment effects.",br(),
                  "3. decompose $\\hat{\\beta}_{fe}$  in four DDs and obtain their estimates & weights.",br(),
                  "4. calculate each treated $(g,t)$ cells weight.",br(),
                  "5. download the simulated data.",br(),br(),
                  "by Hans H. Sievertsen [", tags$a(href="https://github.com/hhsievertsen/twowayfedecomp", "source code,"),tags$a(href="https://github.com/hhsievertsen/twowayfedecomp/raw/master/slides/Notes_on_DiD_weights.pdf", "slides"),"]",tags$a(href="mailto:h.h.sievertsen@bristol.ac.uk", "h.h.sievertsen@bristol.ac.uk"),br(),
                  " - Feedback and suggestions are very welcome."    ,br(),              
                  
                  
                  br(),
                  strong("Setup"),br(),
                    "- 3 groups. Group 1 has a fixed size of 30.",br(),
                    "- Group 1 is never treated.",br(),
                    "- Groups 2 & 3 get treated at some point [see left panel].",br(),
                    "- Treatment effects can vary across groups and over time [see left panel].",br(),
                    "- The two-way fixed effects DD  is estimated with felm from the lfe package."
        
    )
   ))
