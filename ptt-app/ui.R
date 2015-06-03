library(shiny)
library(markdown)

shinyUI(
    navbarPage("Perfect t-test",

    # first tab that opens is About tab
    tabPanel("Instructions & Data", 
        fluidPage(

            # row with explanations of the figure
            fluidRow(
                column(12, includeMarkdown("instructions_initial.md") )
            ),  # END of row

            br(),
            
            #h1("Your data"),
            
            fluidRow(
                column(6, 
                    wellPanel(
                        fileInput('data_file', 
                                  h4('Step 1: Upload the file'),
                                  accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                        #tags$hr(),
                        radioButtons('sep', 'Separator',
                            c(Comma=',', Semicolon=';', Tab='\t'), '\t'),
                        radioButtons('dec', 'Decimal point',
                            c(Comma=',', Point='.'), '.'),
                        radioButtons('quote', 'Quote',
                            c(None='', 'Double Quote'='"', 'Single Quote'="'"),
                            '"')
                    ),
                    wellPanel(
                        h4("Step2: What type of test do you wish to do?"),
                        radioButtons('test_type', "",
                                     c(Independent = 'independent', 
                                       Dependent = 'dependent'),
                                     'independent')
                    ),
                    wellPanel(
                        h4("Step 3: Set the variables"),
                        # textInput("factorlabel", "What variable determines the groups?", "Group variable"),
                        # textInput("measurelabel", "What variable is the dependent measure?", "Dependent variable"),
                        # textInput("xlabel", "What will be your group 1?", value = "Group 1"),
                        # textInput("ylabel", "What will be your group 2?", value = "Group 2")
                        uiOutput("variables")
                        #uiOutput("groups")
                    ),
                    wellPanel(
                        h4("Step 4: Set the labels for figures"),
                        textInput("xlabelstring", "Define the name of the variable displayed on x axis", value = "x axis label"),
                        textInput("ylabelstring", "Define the name of the variable displayed on y axis", value = "y axis label")
                        #uiOutput("labels")
                    ),
                    # submit button
                    submitButton("Data is ready!"),
                    br(),
                    br()
                ),
                column(2),
                # Show the output
                column(4,
                    h4("First 30 rows of your data"),
                    tableOutput(outputId = "data_table")
                ) 
            )  # END of row
        )  # end of page
    ),

    # second tab are diagnostic graphs about outliers, normality assumptions...
    tabPanel("Diagnostics",
        fluidPage(

            # row with explanations of the figure
            fluidRow(
                column(10, includeMarkdown("instructions_diagnostics.md") ),
                column(2)
            ),  # END of row

            br(),

            # row with buttons and the figure
            fluidRow(
                column(12,
                    # outlier plot
                    h2("Outliers"),
                    htmlOutput(outputId = "outlier_text"),
                    plotOutput(outputId = "outlier_plot"),

                    # Normality
                    h2("Normality assumption"),
                    htmlOutput(outputId = "normality_text"),
                    #tableOutput(outputId = "normality_test_results"),
                    plotOutput(outputId = "hist_plot"),
                    htmlOutput(outputId = "qqplot_text"),
                    plotOutput(outputId = "qq_plot"),
                    htmlOutput(outputId = "eqvar_text"),
                    br(),
                    br()
                )
            )  # END of row
        )
    ),  # END of tab

    # 3rd tab is utility and cost interactive figure
    tabPanel("Frequentist tests",

        fluidPage(
            # rendering equations anywhere in static text
            withMathJax(),

            fluidRow(
                column(10, includeMarkdown("instructions_freq.md") ),
                column(2)
            ),

            br(),

            # Sidebar with a slider input for the number of bins
            fluidRow(
                column(4,
                    wellPanel(

                        br(),
                        numericInput("alpha", 
                            label = h5("Significance level, alpha:"), 
                            value = 0.05),
                        br(),  
                        sliderInput("conf_int", label = h5("Select confidence interval level:"), min = 0, max = 1, value = 0.95, step = 0.01),
                        br(), 
                        radioButtons('alt_hyp', label = h5("Select alternative hypothesis:"),
                            c("Two sided"="two.sided", "Less"="less", "Greater"="greater"),
                            "two.sided"),                         
                                                
                        # submit button
                        submitButton("Update report!")
                    )
                ),

                # Show a report
                column(8,
                    htmlOutput(outputId = "ttestOut")
                )
            )  # END of row
        )
    ),  # END of tab

    
    # 4th tab with robust stats
    tabPanel("Robust tests",
        fluidPage(
            withMathJax(),
            # row with explanations of the figure
            fluidRow(
                column(10, includeMarkdown("instructions_robust.md") ),
                column(2)
            ),  # END of row

            br(),

            # user input
            fluidRow(
                column(4, 
                    wellPanel(
                        
                        br(),
                        numericInput("alpha", 
                            label = h5("Significance level, alpha:"), 
                            value = 0.05),
                        br(),  
                        sliderInput("conf_int", label = h5("Select confidence interval level:"), min = 0, max = 1, value = 0.95, step = 0.01),
                        br(), 
                        radioButtons('alt_hyp', label = h5("Select alternative hypothesis:"),
                            c("Two sided"="two.sided", "Less"="less", "Greater"="greater"),
                            "two.sided"), 
                        br(),  
                        checkboxInput("InAHurry", label = h5("Are you in a hurry?"), TRUE),
                        br(),  
                        sliderInput("bootstraps", label = h5("Number of bootstrap replications:"), min = 0, max = 100000, value = 2000, step = 1),
                                                
                        # submit button
                        submitButton("Update report!")
                    )
                ),
                
                # Show a report
                column(8,
                    htmlOutput(outputId = "robustOut")
                )
            )  # END of row
        )
    ),  # END of tab


    # 5th tab with bayes tests
    tabPanel("Bayesian tests",
        fluidPage(
            withMathJax(),
            helpText('An irrational number \\(\\sqrt{2}\\)
           and a fraction $$1-\\frac{1}{2}$$'),
            # TO DO 
            # 1) we'll need some waiting indicator, or computation in progress
            # 2) in a hurry TRUE by default
            
            # row with explanations of the figure
            fluidRow(
                column(10, includeMarkdown("instructions_bayes.md") ),
                column(2)
            ),  # END of row

            br(),

            # user input
            fluidRow(
                column(4, 
                    wellPanel(
                        
                        br(), 
                        radioButtons('alt_hyp', label = h5("Select alternative hypothesis:"),
                            c("Two sided"="two.sided", "Less"="less", "Greater"="greater"),
                            "two.sided"), 
                        br(),  
                        sliderInput("conf_int", label = h5("Select confidence interval level:"), min = 0, max = 1, value = 0.95, step = 0.01),
                        br(),
                        numericInput("BFrscale", 
                            label = h5("Specify expected effect:"), 
                            value = 0.5),
                        br(),  
                        checkboxInput("InAHurry", label = h5("Are you in a hurry?"), TRUE),
                                                
                        # submit button
                        submitButton("Update report!")
                    )
                ),
                
                # Show a report
                column(8,
                    htmlOutput(outputId = "bayesOut")
                )
            )  # END of row
        )
    ),  # END of tab


    # 6th tab, references used in the text
    tabPanel("References", includeMarkdown("references.md") 
            
    )  # END of tab

    
))  # END of user interface function
