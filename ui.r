library(shinyBS)
library(shinyjs)

ui <- fluidPage(title = "Breeding Scheme Designer",
                
                # theme = "bootstrap.css", # CSS theme file could be added here
                withMathJax(), # display mathematical equations using LaTeX format
                useShinyjs(), # hide() or toggle() using shinyjs package
                
                titlePanel("Breeding Scheme Designer"),            
                
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
                    
                    numericInput("negen", "Crossing/Selfing Years",
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
                    
                    numericInput("varieties", "Varieties",
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
                    
                    
                    numericInput("grain", "Samples in Range", min = 2, max = 1000, value = 5, step = 1, width = '20%'),
                    

                    tags$h4("Costs:"),
                    # Make cost input divs appear in one line
                    bootstrapPage(
                      # Set Plot Cost 
                      div(style="display:inline-block",numericInput("costPerPlot", "Plot Cost($):",
                                                                    min = 0, max = 1000, value = 10, step = 1, width = '80px')),
                      # Set Location Cost 
                      div(style="display:inline-block",numericInput("costPerLoc", "Loc Cost($):",
                                                                    min = 0, max = 1000000, value = 1000, step = 1, width = '80px'))
                    ),
                    # tooltip with help
                    bsTooltip("costPerPlot", "The cost of a single plot in the program.",
                              "right", "hover", NULL),
                    # tooltip with help
                    bsTooltip("costPerLoc", "The cost of a single location in the program.",
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
                                tabPanel("Overview", plotOutput('overviewTab'), plotOutput('overviewTabxTime'), plotOutput('overviewTabxCost'))
                              # tabPanel("Range", plotOutput('rangePlotEntriesReps'))
                    ) # endo of tabsetPanel
                  ), # endof mainPanel
                  fluid = T # layout is not fixed, default is T
                ) # endof sidebarLayout
                
)