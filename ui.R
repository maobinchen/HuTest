#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

##this app is used for statistical analysis for group comparison

library(shiny)
library(shinythemes)


shinyUI(fluidPage(
    theme=  shinytheme('spacelab'),
    useShinyjs(),
    #shinythemes::themeSelector(),
    navbarPage("Statistical Data Analysis",id='main',
               tabPanel("Start",
                        div(id='input',
                            column(4,
                                   fileInput(inputId='input_file',
                                              label='Select input file for statistical analysis',
                                              accept = c(".csv",".xls",".xlsx")
                                   ),
                                   actionButton("submit",'Start Analysis'),
                                   actionButton("reset", "Reset")
                            )
                        )
               ),
               
               # Show a plot of the generated distribution
               
               tabPanel(title = "Data", value='Data',
                        br(),
                        DT::dataTableOutput(outputId = "data_table")
               ),
               
               tabPanel(title = "Summary", 
                      br(),
                      navlistPanel(id='summary',
                          #converting wide to long format for future data analysis
                          tabPanel("Converting data",
                                   checkboxInput(inputId='convert_data',
                                                 label="Converting data from wide format to long format?",
                                                 value=FALSE),
                                   conditionalPanel('input.convert_data',
                                       wellPanel(uiOutput('id_vars'),uiOutput('measurement_vars'),
                                                 textInput('v_name','variable name','Day'),
                                                 textInput('m_name','measurement name','TV'))),
                                                 actionButton("submit_cd",'Next')),
                           tabPanel("Variable definition",
                               wellPanel(uiOutput('sample_var'),uiOutput('group_var'),
                                         uiOutput('analysis_var'),
                                         checkboxInput(inputId='split_data',
                                                       label="Split dataset?",
                                                       value=FALSE),
                                         conditionalPanel('input.split_data',
                                         uiOutput('split_var')),
                                         actionButton("submit_vd",'Next'))),
                           tabPanel("Summary statistics",DT::dataTableOutput(outputId = "data_summary_table")),
                           tabPanel("Overall group comparison",plotOutput(outputId = "all_grp_cmp")),
                           widths=c(2,10)
                      )
                      #plotOutput(outputId='gates',height="1000px")
               ),
               
               
               tabPanel(title="Analysis",value='Analysis',
                        br(),
                        tabsetPanel(
                            tabPanel("Normality test",
                                     column(3,textInput('xx','xx')),
                                     column(9,textInput('xx','xx'))),
                            tabPanel("Homogeneity of variance test",
                                      column(3,textInput('xx','xx')),
                                      column(9,textInput('xx','xx'))),
                            tabPanel("Overall equality test among groups",
                                      column(3,textInput('xx','xx')),
                                      column(9,textInput('xx','xx'))),     
                            tabPanel("pairwise comparison between groups",
                                      column(3,textInput('xx','xx')),
                                      column(9,textInput('xx','xx')))          
                        )
               )
    )
))
