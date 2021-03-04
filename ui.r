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
                
                tags$style(type="text/css", "body {padding-bottom: 50px;}"), # padding added to avoid navbar overlay the body

                navbarPage(id="navbar", title="", windowTitle="Breeding Scheme Designer", # navlistPanel( # tabsetPanel(
                  position = "fixed-bottom",
                  collapsible = TRUE,

                  tabPanel("App", fluid = TRUE,
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
                                   min = 1, max = 100, value = 1, step = 1, width = '80px'), 
                      # Add tooltip with instructions/info
                      bsTooltip("varieties", "The final number of selected entries. Must be smaller than or equal to the number of entries in the last stage.",
                                "right", "hover", NULL),
                      
                      
                      # tags$h4("Costs:"),
                      # Make cost input divs appear in one line
                      # bootstrapPage(
                      #   # Set Plot Cost
                      #   div(style="display:inline-block",numericInput("costPerPlot", "Plot Cost($):",
                      #                                                 min = 0, max = 1000, value = 10, step = 1, width = '80px')),
                      #   # Set Location Cost
                      #   div(style="display:inline-block",numericInput("costPerLoc", "Loc Cost($):",
                      #                                                 min = 0, max = 1000000, value = 1000, step = 1, width = '80px')),
                      # 
                      #   # Set Fixed Cost
                      #   div(style="display:inline-block",numericInput("costFixed", "Fixed Cost($):",
                      #                                                 min = 0, max = 1000000, value = 1000, step = 1, width = '80px'))
                      # ),
                      # # tooltip with help
                      # bsTooltip("costPerPlot", "The cost of a single plot in the program.",
                      #           "right", "hover", NULL),
                      # # tooltip with help
                      # bsTooltip("costPerLoc", "The cost of a single location in the program.",
                      #           "right", "hover", NULL),
                      # # tooltip with help
                      # bsTooltip("costFixed", "The fixed cost for running the program.",
                      #           "right", "hover", NULL),
                      
                      # Economic cost summary output for scenario
                      tags$h4("Cost Summary"),
                      div( # CUSTOMISE div style for DT
                        DT::DTOutput("cost_table"),
                        style = "font-size: 85%; width: 100%"
                      ),
                      # div( # CUSTOMISE div style for DT
                      #   DT::DTOutput("total_cost_table"),
                      #   style = "font-size: 85%; width: 100%"
                      # ),
  
                      
                      checkboxInput("chk_ranges", "Set Ranges", FALSE),
                      
                      # Only show this panel if the chk_ranges is checked
                      conditionalPanel(
                        condition = "input.chk_ranges",
                        tags$div(class = "div_ranges", checked = NA, 
                                 
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
                                 
                                 sliderInput("grain", "Samples in Range", min = 2, max = 5, value = 2, step = 1, width = '30%')
                        
                        ) # endof div ranges
                        
                      ), # endof conditionalPanel
                    
                      #tags$br(),
                      
                      actionButton("run_btn", "Run"),
                      
                    ), # endof SidebarPanel
                    # Show plots and charts 
                    mainPanel(
                      # uiOutput('mytabs') # old
                      # #splitLayout(,)
                      tabsetPanel(id = "my_tabs",
                                  tabPanel("ALL", tags$div(id = "placeholder")),
                                  tabPanel("Scenarios", uiOutput('mytabs')),
                                  tabPanel("Overview", tags$div(id = "dbtn_placeholder"), plotOutput('overviewTab'), plotOutput('overviewTabxTime'), plotOutput('overviewTabxCost')),
                                  # tabPanel("Overview", plotOutput('overviewTab'), plotOutput('overviewTabxTime'), plotOutput('overviewTabxCost')),
                                  tabPanel(title = "About",
                                           tags$h2("About"),
                                           tags$br(),
                                           tags$p("The Breeding Scheme Designer is a GUI tool (based on R-shiny) that will allow breeders and donors 
                                                  to compare different breeding strategies and calculate expected genetic gain given a set of parameters.
                                                  The tool can simulate different scenarios for a ", tags$b("single cycle"), "of a breeding program."),
                                           tags$br(),
                                           tags$p("Users can:"),
                                           tags$ol(
                                             tags$li("Define the input parameters of breeding scenarios with multiple stages."),
                                             tags$li("Calculate parameters such as expected genetic gain, heritability, gain per year, gain per cost etc."),
                                             tags$li("Compare results between different scenarios (stage-by-stage)."),
                                             tags$li("Estimate the cost of scenarios and compare gain per cost."),
                                             tags$li("Explore expected genetic gain for ranges of input parameters and their pairwise combinations.")
                                           ),
                                           tags$br(),
                                           tags$p("Basic Instructions:"),
                                           tags$ul(
                                             tags$li("To create a new breeding scenario, fill in the input fields on the left sidebar and press the Run button at the bottom."),
                                             tags$li("Repeat the same process to create more breeding scenarios."),
                                             tags$li("Results for each scenario will be displayed in different \"Scenario\" tabs at the area on the right."),
                                             tags$li("To compare between different scenarios, click on the \"Overview\" tab.")
                                           )
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
                                             tags$li(tags$b("Stage:"), "The allocation of resources during a breeding program period, before a selection step takes place."),
                                             tags$li(tags$b("Entries:"), "Number of entries. Must be smaller than or equal to the number of entries in the previous stage."),
                                             tags$li(tags$b("Years:"), "Number of years. Increasing this value will increase heritability by decreasing variation due to GxY, GxL(Y) and plot error."),
                                             tags$li(tags$b("Locs:"), "Number of locations. Increasing this value will increase heritability by decreasing variation due to GxL(Y) and plot error."),
                                             tags$li(tags$b("Reps:"), "Number of replications. Increasing this value will increase heritability by decreasing variation due to plot error."),
                                             tags$li(tags$b("Plot Error Variance:"), "Plot error variance affects the heritability and it can be used to adjust its calculated value when heritability of a trait is known."),
                                             tags$li(tags$b("Heritability:"), " Proportion of phenotypic variance attributable to additive genetic effects. Calculated using a function that takes into account the variances, years, locs, reps and plot error variance of yield trials at each stage."),
                                             tags$li(tags$b("Selected Parents:"), "The final number of selected entries. Must be smaller than or equal to the number of entries in the last stage."),
                                             tags$li(tags$b("Samples in Range:"), "The number of samples or intervals to be considered for each set of ranges."),
                                             tags$li(tags$b("Genetic Gain:"), "the change in the breeding/genetic value which is occuring for a particular trait of interest in a period of time.")
                                           ),
                                           tags$br(),
                                           tags$p("Find more details about the definitions of those terms and how they can be calculated in the ", 
                                                  tags$a(href="https://excellenceinbreeding.org/toolbox/tools/eib-breeding-scheme-optimization-manuals", "EiB breeding scheme optimization manuals.")),
                                           
                                           
                                  )
                                # tabPanel("Range", plotOutput('rangePlotEntriesReps'))
                      ) # endo of tabsetPanel
                    )#, # endof mainPanel
                    #fluid = T # layout is not fixed, default is T
                  ) # endof sidebarLayout
                ), # endof tabPanel
                tabPanel("Ranges", fluid = TRUE,
                         sidebarLayout(
                           sidebarPanel(
                             width = 4,
                             tags$h3("Set ranges"),
                      
                             tags$h4("Variances"),
                             
                             # Make divs appear in one line
                             bootstrapPage(
                               # Set Genetic Variance 
                               div(style="display:inline-block",numericInput("varG_r", "Genetic:",
                                                                             min = 0, max = 100, value = 1, step = 0.1, width = '80px')),
                               # Set GxL(Y) Variance 
                               div(style="display:inline-block",numericInput("varGxL_r", "GxL(Y):",
                                                                             min = 0, max = 100, value = 1, step = 0.1, width = '80px')),
                               # Set GxY Variance 
                               div(style="display:inline-block",numericInput("varGxY_r", "GxY:",
                                                                             min = 0, max = 100, value = 1, step = 0.1, width = '80px')),
                               # Set Plot Error Variance 
                               div(style="display:inline-block",numericInput("varErr_r", "Plot Error:",
                                                                             min = 0, max = 100, value = 1, step = 0.1, width = '80px'))
                             ),
                             
                             # Add tooltip with instructions/info
                             bsTooltip("varG_r", "Genetic variance. The variance between entries in the first stage of yield trials.",
                                       "right", "hover", NULL),
                             # Add tooltip with instructions/info
                             bsTooltip("varGxL_r", "Genotype-by-location nested in year interaction variance. This value is equivalent to the sum of genotype-by-location interaction variance and genotype-by-location-by-year interaction varaince.",
                                       "right", "hover", NULL),
                             # Add tooltip with instructions/info
                             bsTooltip("varGxY_r", "Genotype-by-year interaction variance.",
                                       "right", "hover", NULL),
                             # Add tooltip with instructions/info
                             bsTooltip("varErr_r", "Plot error variance.",
                                       "right", "hover", NULL),
                             
                             tags$h4("Ranges"),
                             
                             # Input: Specification of range within an interval ----
                             sliderInput("entries_range_r", "Entries:",
                                         min = 100, max = 5000,
                                         value = c(100,1000)),
                             # Input: Specification of range within an interval ----
                             sliderInput("years_range_r", "Years:",
                                         min = 1, max = 10,
                                         value = c(1,5)),
                             # Input: Specification of range within an interval ----
                             sliderInput("locs_range_r", "Locs:",
                                         min = 1, max = 10,
                                         value = c(1,5)),
                             # Input: Specification of range within an interval ----
                             sliderInput("reps_range_r", "Reps:",
                                         min = 1, max = 30,
                                         value = c(1,10)),
                             # Input: Specification of error within an interval ----
                             # sliderInput("error_range_r", "Plot Error Variance:",
                             #             min = 1, max = 30,
                             #             value = c(1,10)),
                             
                             sliderInput("grain_r", "Samples in Range", min = 2, max = 5, value = 2, step = 1, width = '30%'),
                             
                             numericInput("varieties_r", "Selected Parents",
                                          min = 1, max = 100, value = 1, step = 1, width = '80px'), 
                             # Add tooltip with instructions/info
                             bsTooltip("varieties_r", "The final number of selected entries. Must be smaller than or equal to the minimum number of entries.",
                                       "right", "hover", NULL),
                             
                             actionButton("run_btn_r", "Run")
                           ),
                           mainPanel(

                             tags$div(class = "div_plot_ranges_r", checked = NA, 
                                      tags$h3("Set axes to display plot"),
                                      # Drop down list to select input for X axis
                                      selectInput(inputId = 'xAxis', label =  "x axis:",
                                                  choices =  c("entries", "years", "locs", "reps"),
                                                  selected = "entries",
                                                  multiple = FALSE,
                                                  selectize = TRUE
                                      ),
                                      # Drop down list to select input for Y axis
                                      selectInput(inputId = 'yAxis', label =  "y axis:",
                                                  choices =  c("entries", "years", "locs", "reps"),
                                                  selected = "years",
                                                  multiple = FALSE,
                                                  selectize = TRUE
                                      ),
                                      
                                      # actionButton("plot_btn", "Plot"),
                                      
                                      plotlyOutput('plotXY'), # will be generated based on X Y input above

                                      
                             ) # endof div plot ranges
                             
                           )
                         )
                ) # endof tabPanel ranges
               
              ) # endof navbarPage # tabsetPanel
)