library(shinyBS)
library(shinyjs)
library(shinyalert)

ui <- fluidPage(title = "Breeding Scheme Designer",
                
                # theme = "bootstrap.css", # CSS theme file could be added here
                withMathJax(), # display mathematical equations using LaTeX format
                useShinyjs(), # hide() or toggle() using shinyjs package
                useShinyalert(), # create pop-up alert messages 
                
                tags$img(src = "eib_logo.svg", height = "50", align = "right"),
                titlePanel("Breeding Scheme Designer", windowTitle="Breeding Scheme Designer"),  
                #titlePanel( div(column(width = 6, h2("Breeding Scheme Designer")), 
                #                column(width = 6, tags$img(src = "eib_logo.svg", height = "50", align = "right"))),
                #            windowTitle="Breeding Scheme Designer"),
                
                sidebarLayout(
                  
                  sidebarPanel(
                    
                    width = 4,
                    
                    tags$h3("Create a new scenario"),
                    
                    # Currently scenario ID are generated automatically. Alternatively,
                    # user-defined scenario ID could be typed using the following:
                    # textInput("divID", "Enter a unique ID for your scenario:", ""),
                    # helpText("Leave the text input blank for automatically unique IDs."),
                    
                    tags$h4("Variance"),
                    
                    # Make divs appear in one line
                    bootstrapPage(
                      # Set Genetic Variance 
                      div(style="display:inline-block",numericInput("varG", "Genetic:",
                                                                    min = 0, max = 100, value = 1, step = 0.1, width = '80px')),
                      # Set GxL(Y) Variance 
                      div(style="display:inline-block",numericInput("varGxL", "GxL(Y):",
                                                                    min = 0, max = 100, value = 1, step = 0.1, width = '80px')),
                      # Set GxY Variance 
                      div(style="display:inline-block",numericInput("varGxY", "GxY:",
                                                                    min = 0, max = 100, value = 1, step = 0.1, width = '80px'))
                    ),
                    
                    # Add tooltip with instructions/info
                    bsTooltip("varG", "Genetic variance. The variance between entries in the first stage of yield trials.",
                              "right", "hover", NULL),
                    # Add tooltip with instructions/info
                    bsTooltip("varGxL", "Genotype-by-location nested in year interaction variance. This value is equivalent to the sum of genotype-by-location interaction variance and genotype-by-location-by-year interaction varaince.",
                              "right", "hover", NULL),
                    # Add tooltip with instructions/info
                    bsTooltip("varGxY", "Genotype-by-year interaction variance.",
                              "right", "hover", NULL),
                    
                    numericInput("negen", "Multiplication Time (Years)",
                                min = 0, max = 100, value = 4, step = 1, width = '80px'),
                    # sliderInput("negen", "Crossing/Selfing Years",
                    #             min = 0, max = 10, value = 4, width = '240px'), 
                    # Add tooltip with instructions/info
                    bsTooltip("negen", "GNumber of early generation years. This phase of the breeding program is modeled without selection.",
                              "right", "hover", NULL),
                  
                    
                    tags$h4("Yield Trials"),
                    div( # CUSTOMISE div CSS style for DT
                      DT::DTOutput("stages_table"),
                      style = "font-size: 85%; width: 100%"
                    ),
                    # Add tooltip with instructions/info
                    # bsTooltip("stages_table", "Number of entries. Must be smaller than or equal to the number of entries in the previous stage.
                    #                           Number of years. Increasing this value will increase heritability by decreasing variation due to GxY, GxL(Y) and plot error.
                    #                           Number of locations. Increasing this value will increase heritability by decreasing variation due to GxL(Y) and plot error.
                    #                           Number of replications. Increasing this value will increase heritability by decreasing variation due to plot error.",
                    #                           "right", "hover", NULL),
                    actionButton("add_btn", "Add"), # Add stage
                    actionButton("delete_btn", "Delete"), # Delete last stage
                    
                    numericInput("varieties", "Selected Parents",
                                 min = 1, max = 10, value = 1, step = 1, width = '80px'), 
                    # Add tooltip with instructions/info
                    bsTooltip("varieties", "The final number of selected entries. Must be smaller than or equal to the number of entries in the last stage.",
                              "right", "hover", NULL),
                    
                    tags$h4("Ranges:"),
                    
                    # Input: Specification of range within an interval ----
                    sliderInput("entries_range", "First Stage Entries:",
                                min = 100, max = 5000,
                                value = c(100,1000)),
                    # Input: Specification of range within an interval ----
                    sliderInput("years_range", "First Stage Years:",
                                min = 1, max = 10,
                                value = c(1,5)),
                    # Input: Specification of range within an interval ----
                    sliderInput("locs_range", "First Stage Locs:",
                                min = 1, max = 10,
                                value = c(1,5)),
                    # Input: Specification of range within an interval ----
                    sliderInput("reps_range", "First Stage Reps:",
                                min = 1, max = 30,
                                value = c(1,10)),
                    
                    sliderInput("grain", "Samples in Range", min = 2, max = 5, value = 2, step = 1, width = '30%'),
                    
                    tags$h4("Costs:"),
                    # Make cost input divs appear in one line
                    bootstrapPage(
                      # Set Plot Cost 
                      div(style="display:inline-block",numericInput("costPerPlot", "Plot Cost($):",
                                                                    min = 0, max = 1000, value = 10, step = 1, width = '80px')),
                      # Set Location Cost 
                      div(style="display:inline-block",numericInput("costPerLoc", "Loc Cost($):",
                                                                    min = 0, max = 1000000, value = 1000, step = 1, width = '80px')),
                      
                      # Set Fixed Cost 
                      div(style="display:inline-block",numericInput("costFixed", "Fixed Cost($):",
                                                                    min = 0, max = 1000000, value = 1000, step = 1, width = '80px'))
                    ),
                    # tooltip with help
                    bsTooltip("costPerPlot", "The cost of a single plot in the program.",
                              "right", "hover", NULL),
                    # tooltip with help
                    bsTooltip("costPerLoc", "The cost of a single location in the program.",
                              "right", "hover", NULL),
                    # tooltip with help
                    bsTooltip("costFixed", "The fixed cost for running the program.",
                              "right", "hover", NULL),
                    
                    # Economic cost summary output for scenario
                    tags$h4("Summary Cost"),
                    div( # CUSTOMISE div style for DT
                      DT::DTOutput("cost_table"),
                      style = "font-size: 85%; width: 100%"
                    ),
                    # div( # CUSTOMISE div style for DT
                    #   DT::DTOutput("total_cost_table"),
                    #   style = "font-size: 85%; width: 100%"
                    # ),
                    
                    tags$br(),
                    
                    actionButton("run_btn", "Run")
                    
                    
                    
                  ), # endof SidebarPanel
                  # Show plots and charts 
                  mainPanel(
                    # uiOutput('mytabs') # old
                    # #splitLayout(,)
                    tabsetPanel(id = "my_tabs",
                                tabPanel("ALL", tags$div(id = "placeholder")),
                                tabPanel("Scenarios", uiOutput('mytabs')),
                                tabPanel("Overview", plotOutput('overviewTab'), plotOutput('overviewTabxTime'), plotOutput('overviewTabxCost')),
                                tabPanel(title = "About",
                                         tags$h2("About"),
                                         tags$br(),
                                         tags$p("The Breeding Scheme Designer is a GUI tool (based on R-shiny) that will allow breeders and donors 
                                                to compare different breeding strategies and calculate expected genetic gain given a set of parameters."),
                                         tags$p("Users can:"),
                                         tags$ol(
                                           tags$li("Define the input parameters of a breeding cycle scenario with multiple stages."),
                                           tags$li("Calculate parameters such as expected genetic gain, heritability, gain per year, gain per cost etc."),
                                           tags$li("Compare genetic gain results between different scenarios."),
                                           tags$li("Estimate the cost of scenarios and compare gain per cost."),
                                           tags$li("Explore expected genetic gain for ranges of input parameters and their combination.")
                                         ),
                                         tags$br(),
                                         tags$p("To create a new breeding scenario, fill in the input fields on the left sidebar and press the Run button at the bottom."),
                                         tags$br(),
                                         tags$p("Results for each scenario are displayed in different tabs at the right panel.")
                                ), # endof About
                                tabPanel(title = "Glossary",
                                         # Glossary TODO
                                         tags$h2("Glossary of Terms"),
                                         tags$br(),
                                         tags$ul(
                                           tags$li(tags$b("Genetic Variance:"), "The variance between entries in the first stage of yield trials."),
                                           tags$li(tags$b("GxL(Y):"), "Genotype-by-location nested in year interaction variance. This value is equivalent to the sum of genotype-by-location interaction variance and genotype-by-location-by-year interaction varaince."),
                                           tags$li(tags$b("GxY:"), "Genotype-by-year interaction variance."),
                                           tags$li(tags$b("Multiplication Time (Years):"), "GNumber of early generation years. This phase of the breeding program is modeled without selection."),
                                           tags$li(tags$b("Stage:"), "The allocation of resources during a breeding program before selection takes place."),
                                           tags$li(tags$b("Entries:"), "Number of entries. Must be smaller than or equal to the number of entries in the previous stage."),
                                           tags$li(tags$b("Years:"), "Number of years. Increasing this value will increase heritability by decreasing variation due to GxY, GxL(Y) and plot error."),
                                           tags$li(tags$b("Locs:"), "Number of locations. Increasing this value will increase heritability by decreasing variation due to GxL(Y) and plot error."),
                                           tags$li(tags$b("Reps:"), "Number of replications. Increasing this value will increase heritability by decreasing variation due to plot error."),
                                           tags$li(tags$b("Plot Error:"), "Plot error affects the heritability and it can be used to adjust its calculated value when heritability of a trait is known."),
                                           tags$li(tags$b("Heritability:"), "Calculated using a function that takes into account the variances, years, locs, reps and plot error of yield trials at each stage."),
                                           tags$li(tags$b("Selected Parents:"), "The final number of selected entries. Must be smaller than or equal to the number of entries in the last stage."),
                                           tags$li(tags$b("Samples in Range:"), "The number of samples or intervals to be considered for each set of ranges."),
                                           tags$li(tags$b("Genetic Gain:"), "the change in the breeding/genetic value which is occuring for a particular trait of interest in a period of time.")
                                         )
                                )
                              # tabPanel("Range", plotOutput('rangePlotEntriesReps'))
                    ) # endo of tabsetPanel
                  ), # endof mainPanel
                  fluid = T # layout is not fixed, default is T
                ) # endof sidebarLayout
                
)