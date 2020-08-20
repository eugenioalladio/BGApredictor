library(shiny)
library(plotly)
library(DT)
library(ggplot2)
library(tidyverse)
library(purrr)
library(plyr);
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(wesanderson)
library(RColorBrewer)
library(shinyLP)
library(shinyjs)
library(e1071) 
library(shinyBS)
library(ggfortify)
library(grid)
library(gridExtra)
library(gtable)
library(isotone)
library(ggthemes)
library(scatterplot3d)
library(KernSmooth)
library(gridExtra)
library(grid)
library(flexdashboard)
library(tinytex)
library(logisticPCA)
library(kernlab)
library(pryr)
library(shinycssloaders)

options(repos = BiocManager::repositories())
getOption("repos")



# Define UI for application that draws a histogram
shinyUI(fluidPage(#shinythemes::themeSelector(),   #shows all themes
    theme = shinytheme("flatly"), # change theme changing "cerulean"
    #theme = shinytheme("cerulean"),
    #NavBar and layout
    navbarPage(
        "BGApredictor", 
        #NavBar  Home
        #### UI HOME ####
        tabPanel("Home", icon = icon("home"),
                 sidebarLayout( #sidebarLayout "Home"
                     sidebarPanel(width=3, #sidebarPanel "Home"
                                  # LOGO University Glasgow, Torino, CAD
                                  #img(src = "carabinieri-logo.png", height = "100%", width = "100%", align="center"),
                                  # Horizontal line 
                                  #tags$hr(),
                                  img(src = "logo_1.png", height = "100%", width = "92%", align="center"),
                                  # Horizontal line 
                                  tags$hr(),
                                  img(src = "logo_2.png", height = "100%", width = "96%", align="center"),
                                  tags$hr(),
                                  img(src = "sapienza.png", height = "100%", width = "95%", align="center"),
                                  tags$hr()#,
                                  # img(src = "RS.png", height = "100%", width = "95%", align="center"),
                                  # tags$hr(),
                                  #img(src = "lib.png", height = "100%", width = "90%", align="center")
                     ),# end sidebarPanel "Home"
                     # mainPanel for displaying outputs in "Home"       
                     mainPanel(
                         tabsetPanel(id='dataset00',
                                     tabPanel(("About the Project"), 
                                              tags$hr(),
                                              strong(div("BGApredictor: A MULTIVARIATE STATISTICAL TOOL FOR THE EVALUATION OF",style = "color: FireBrick", align="center"),div("THE BIOGEOGRAPHICAL ANCESTRY INFORMATION", style = "color: FireBrick", align="center")
                                                     ,div("FROM TRADITIONAL STRs DATA", style = "color: FireBrick", align="center")),
                                              br(),
                                              p("The capability to distinguish ethnic groups and biogeographic ancestry (BGA) affiliations have been largely explored in forensic genetics because of its potential usefulness in providing with investigative clues. For law enforcement and security purposes, when genetic data have been obtained from unknown evidence, but no reference samples are available, and no hits come out from DNA databases it would be extremely useful at least to infer the ethno-geographic origin of the stain donor by just examining traditional STRs DNA profiles.
                                         Current protocols for ethnic origin estimation using STRs profiles are usually based on Principal Component Analysis approaches and Bayesian methods. The present study provides an alternative and dynamic tool that involves the use of several multivariate data analysis strategies for estimation of the BGA information from unknown biological traces. Powerful multivariate techniques such as Principal Component Analysis (PCA), Partial Least Squares-Discriminant Analysis (PLS-DA) and Support Vector Machines (SVM) are employed and their discriminating power has been compared. These techniques have been applied on specific population datasets containing the allele frequencies of several African populations and tribes, Caucasian individuals and Asian subjects. Both PLS-DA and SVM techniques provided robust classifications, yielding high sensitivity and specificity models capable of discriminating the populations on ethnic basis. Lastly, examples of real cases have been examined, and the developed models have been easily extended to smaller and more specific populations. 
                                         The developed interpretation approach is aimed to be converted into an open-source user-friendly R Shiny application, in order to represent a helpful and powerful instrument for law enforcement agencies and analysts.
                                         ", align = "justify"),
                                              tags$hr()),
                                     tabPanel(("Related Papers"),
                                              tags$hr(),
                                              fluidPage(
                                                  fluidRow(
                                                      column(10,
                                                             em(p("A multivariate statistical approach for the estimation of the ethnic origin of 
                                                      unknown genetic profiles in forensic genetics", align = "justify"))),
                                                      column(2,
                                                             actionButton("generate2", "Read PDF", icon("book-reader"),#"file-pdf" 
                                                                          style="color: white; background-color: FireBrick; border-color: White")))),
                                              tags$hr(),
                                              uiOutput("pdfview")),
                                     tabPanel(("About the App"),
                                              tags$hr(),
                                              p("Shiny is an open source R package that provides an elegant and powerful web framework for building web applications using R."),
                                              tags$hr(),
                                              img(src = "Supplementary_material.png", height = "100%", width = "100%", align="center")
                                     )
                         ) # end tabsetPanel id='dataset00'
                     )# end mainPanel "Home"
                 )# end sidebarLayout "Home"
        ),# end tabPanel "Home"
        
        #### UI DATABASES ####
        
        tabPanel("Database", icon = icon("users"),
                 sidebarLayout( #sidebarLayout "Databases"
                     sidebarPanel(width=3, #sidebarPanel "Databases"
                                  conditionalPanel('input.dataset01 === "Import Allele Freqs."',
                                                   fileInput("file0",
                                                             "Import .csv/.txt file(s) from directory",
                                                             multiple = TRUE,
                                                             accept=c('text/csv', 
                                                                      'text/comma-separated-values,text/plain', 
                                                                      '.csv')),
                                                   shinyjs::useShinyjs(),
                                                   actionButton("set0", label = HTML('<b>File Settings</b>'),
                                                                icon=icon("sliders-h", lib = "font-awesome"),#9*9*9*9*
                                                                style="width: 100%;white-space:normal; color: white; 
                                                   background-color: #337ab7; border-color: steelblue"), #9*9*9*9*9*
                                                   hidden(
                                                       checkboxInput("header0", "Header", TRUE), # Input: Checkbox if file has header
                                                       radioButtons("sep0", HTML('<h5><b>Separator</b></h5>'),  # Input: Select separator 
                                                                    choices = c(Semicolon = ";",
                                                                                Comma = ",",
                                                                                Tab = "\t"),
                                                                    selected = "\t"),
                                                       radioButtons("disp0", HTML('<h5><b>Display</b></h5>'), # Input: Select number of rows to display 
                                                                    choices = c(All = "all",
                                                                                Head = "head"), selected = "all")
                                                       
                                                   ), # end hidden
                                                   br(),br(),br(),
                                                   pickerInput(
                                                       inputId = "str_kit0",
                                                       label = "Select STRs DNA Amplification kit:", 
                                                       choices = list("Global" = "global",
                                                                      "NGM SElect" = "ngmsel",
                                                                      "Fusion 6C" = "fusion6c",
                                                                      "ESX 17" = "esx17",
                                                                      "24plex" = "qs24plex",
                                                                      "ESSplex Plus" = "essseplus",
                                                                      "All the data" = "all_data"),
                                                       choicesOpt = list(
                                                           subtext = paste("Kit", 
                                                                           c("GlobalFiler PCR Amplification Kit", "AmpFlSTR NGM SElect",
                                                                             "PowerPlex Fusion 6C System", "PowerPlex ESX 17", "Investigator 24plex QS Kit",
                                                                             "Investigator ESSplex Plus",
                                                                             "All the data"),
                                                                           sep = ": "))
                                                   ),
                                                   br(),
                                                   numericInput("num", label = HTML('<b>Nr. of simulated genotypes</b>'), value = NULL),
                                                   br(),
                                                   textInput("class", label = HTML('<b>Name of Population</b>'), value = "Enter text..."),
                                                   #downloadButton('downloadData', 'Download'),
                                                   br(),br(),
                                                   actionButton("go_button0",  HTML('<b>  Import/Update dataset</b>'),
                                                                icon("file-import"), 
                                                                style="width: 100%;white-space:normal; color: white; 
                                                   background-color: #ff5c5c; border-color: white"),
                                                   br(),br(),
                                                   uiOutput("checkbox0") # Select variables to display in Population Dataset
                                  ), # end of conditionalPanel "Import Allele Freqs"
                                  conditionalPanel('input.dataset01 === "Import Database"',
                                                   fileInput("file1",
                                                             "Import csv/txt file(s) from directory",
                                                             multiple = TRUE,
                                                             accept=c('text/csv', 
                                                                      'text/comma-separated-values,text/plain', 
                                                                      '.csv')),
                                                   shinyjs::useShinyjs(),
                                                   actionButton("set", label = HTML('<b>File Settings</b>'),
                                                                icon=icon("sliders-h", lib = "font-awesome"),#9*9*9*9*
                                                                style="width: 100%;white-space:normal; color: white; 
                                                   background-color: #337ab7; border-color: steelblue"), #9*9*9*9*9*
                                                   hidden(
                                                       checkboxInput("header", "Header", TRUE), # Input: Checkbox if file has header
                                                       radioButtons("sep", HTML('<h5><b>Separator</b></h5>'),  # Input: Select separator 
                                                                    choices = c(Semicolon = ";",
                                                                                Comma = ",",
                                                                                Tab = "\t"),
                                                                    selected = "\t"),
                                                       radioButtons("disp", HTML('<h5><b>Display</b></h5>'), # Input: Select number of rows to display 
                                                                    choices = c(All = "all",
                                                                                Head = "head"), selected = "all")
                                                       
                                                   ),
                                                   
                                                   br(),br(),br(),
                                                   pickerInput("funct", "Select type of data:",
                                                                c("Autosomal STRs (a-STRs)" = "strs")),#,
                                                   br(),br(),
                                                   pickerInput(
                                                       inputId = "str_kit",
                                                       label = "Select STRs DNA Amplification kit:", 
                                                       choices = list("Global" = "global",
                                                                      "NGM SElect" = "ngmsel",
                                                                      "Fusion 6C" = "fusion6c",
                                                                      "ESX 17" = "esx17",
                                                                      "24plex" = "qs24plex",
                                                                      "ESSplex Plus" = "essseplus",
                                                                      "All the data" = "all_data"),
                                                       choicesOpt = list(
                                                           subtext = paste("Kit", 
                                                                           c("GlobalFiler PCR Amplification Kit", "AmpFlSTR NGM SElect",
                                                                             "PowerPlex Fusion 6C System", "PowerPlex ESX 17", "Investigator 24plex QS Kit",
                                                                             "Investigator ESSplex Plus",
                                                                             "All the data"),
                                                                           sep = ": "))
                                                   ),
                                                    # end hidden
                                                   br(),br(),
                                                   uiOutput("FirstCheckPopulations"),#,
                                                   
                                                   br(),br(),
                                                   #downloadButton('downloadData', 'Download')
                                                   actionButton("go_button",  HTML('<b>  Import/Update dataset</b>'),
                                                                icon("file-import"), 
                                                                style="width: 100%;white-space:normal; color: white; 
                                                   background-color: #ff5c5c; border-color: white"),
                                                   br(),br(),
                                                   uiOutput("checkbox1") # Select variables to display in Population Dataset
                                  ), # end of conditionalPanel "Import Database
                                  conditionalPanel(
                                      'input.dataset01 === "Import New Subjects"',
                                      fileInput("file2", HTML('<h5><b>Import New Subjects:</b></h5>'), 
                                                multiple = TRUE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")),
                                      shinyjs::useShinyjs(),
                                      actionButton("set2", label = HTML('<b>File Settings</b>'),
                                                   icon=icon("sliders-h", lib = "font-awesome"),#9*9*9*9*
                                                   style="width: 100%;white-space:normal; color: white; 
                                                   background-color: #337ab7; border-color: steelblue"), #9*9*9*9*9*
                                      hidden(
                                          checkboxInput("header2", "Header", TRUE), # Input: Checkbox if file has header
                                          radioButtons("sep2", HTML('<h5><b>Separator</b></h5>'),  # Input: Select separator 
                                                       choices = c(Semicolon = ";",
                                                                   Comma = ",",
                                                                   Tab = "\t"),
                                                       selected = "\t"),
                                          radioButtons("disp2", HTML('<h5><b>Display</b></h5>'), # Input: Select number of rows to display 
                                                       choices = c(All = "all",
                                                                   Head = "head"), selected = "all")
                                      ),# end hidden
                                      br(),br(),br(),
                                      pickerInput("funct3", "Select type of data:",
                                                   c("Autosomal STRs (a-STRs)" = "strs")),
                                      
                                      br(),
                                      uiOutput("FirstCheckSubjects"),
                                      br(),br(),
                                      actionButton("subjects_button", HTML('<b>Import new</b>'),
                                                   icon("file-import", lib = "font-awesome"), #9*9*9*9*9*
                                                   style="white-space:normal;width:100%;
                                                   color: white; 
                                                   background-color: #ff5c5c; 
                                                   border-color: white") #  Main button PCA
                                  )# end of conditionalPanel "Import New Subjects"
                     ), # end sidebarPanel "Databases"
                     mainPanel(
                         tabsetPanel(id='dataset01',
                                     tabPanel(("Import Allele Freqs."),
                                               dataTableOutput("rendered_file0")
                                     ), #end tabPanel "Import Allele Freqs."
                                     tabPanel(("Import Database"),
                                              dataTableOutput("rendered_file1")
                                     ), #end tabPanel "Import Database"
                                     tabPanel(("Import New Subjects"),
                                              dataTableOutput("rendered_file1_subj")
                                     ) #end tabPanel "New Subjects"
                         ) # end of tabsetPanel "dataset01"
                     ) # end of mainPanel "Database"
                 )# end of sidebarLayout "Databases"
        ), # end of tabPanel "Databases"
        
        #### UI MODELS ####
        tabPanel("Models", icon = icon("cogs"),
                 sidebarLayout( #sidebarLayout "Models"
                     sidebarPanel(width=3, #sidebarPanel "Models"
                                  conditionalPanel('input.dataset02 === "PCA"',
                                                   uiOutput("SecondSelectPCA"),
                                                   br(),
                                                   numericInput("PC_number", "Nr. of PCs:", value= "10"),
                                                   br(),
                                                   pickerInput("funct_logitpcaok", "Select algorithm of PCA:",
                                                                c("Classic" = "pca",
                                                                  "Logistic SVD" = "svd",
                                                                  "Logistic NIPALS" = "nipals"
                                                                )),
                                                   br(),
                                                   actionButton("filePCA",  HTML("<b>Show dataset for PCA</b>"),
                                                                icon("eye", lib = "font-awesome"), #8*8*8*8*
                                                                style=" width:100%; white-space:normal;color: white;
                                                   background-color: #45c50e; border-color: white"),  #8*8*8*9*9*9*
                                                   bsModal("filePCAID", title = "Dataset for PCA model",
                                                           trigger = "filePCA", size = "large", #bsModal e' triggerato dal bottone, basta inserire l'input id
                                                           dataTableOutput("filexpca")), #8
                                                   br(),br(),
                                                   actionButton("pca_button", HTML('<b>Run PCA</b>'),
                                                                icon("calculator", lib = "font-awesome"), #9*9*9*9*9*
                                                                style="white-space:normal;width:100%;height=80px;
                                                   color: white;
                                                   background-color: #ff5c5c; border-color: white"),#9*9*9*9*  Main button PCA
                                                   br(),br(),br(),
                                                   uiOutput("PCA_axis_1"),
                                                   uiOutput("PCA_axis_2"),
                                                   uiOutput("PCA_axis_3")
                                  ), # end of conditionalPanel "PCA"
                                  conditionalPanel('input.dataset02 === "PLS-DA"',
                                                   uiOutput("SecondSelectPLSDA"),
                                                   br(),
                                                   numericInput("LV_number", "Nr. of LVs:", value= "10"),
                                                   br(),
                                                   pickerInput("funct_sparseplsda", "Select algorithm of PLS-DA:",
                                                                c("Classic PLS-DA" = "plsda",
                                                                  "Sparse PLS-DA" = "splsda"
                                                                )),
                                                   br(),
                                                   actionButton("filePLSDA",  HTML("<b>Show dataset for PLS-DA</b>"),
                                                                icon("eye", lib = "font-awesome"), #8*8*8*8*
                                                                style=" width:100%; white-space:normal;color: white;
                                                   background-color: #45c50e; border-color: white"),  #8*8*8*9*9*9*
                                                   bsModal("filePLSDAID", title = "Dataset for PLS-DA model",
                                                           trigger = "filePLSDA", size = "large", #bsModal e' triggerato dal bottone, basta inserire l'input id
                                                           dataTableOutput("filexplsda")), #8
                                                   br(),br(),
                                                   actionButton("plsda_button", HTML('<b>Run PLSDA</b>'),
                                                                icon("calculator", lib = "font-awesome"), #9*9*9*9*9*
                                                                style="white-space:normal;width:100%;height=80px;
                                                   color: white;
                                                   background-color: #ff5c5c; border-color: white"),#9*9*9*9*  Main button PCA
                                                   br(),br(),br(),
                                                   uiOutput("PLSDA_axis_1"),
                                                   uiOutput("PLSDA_axis_2"),
                                                   uiOutput("PLSDA_axis_3")
                                  ), # end of conditionalPanel "PLS-DA"
                                  conditionalPanel('input.dataset02 === "SVM"',
                                                   uiOutput("SecondSelectSVM"),
                                                   br(),
                                                   pickerInput("funct_SVM", "Select algorithm of SVM:",
                                                                c("SVM on input data" = "svm",
                                                                  "SVM on PCA" = "svmpca"
                                                                  )),
                                                   br(),
                                                   actionButton("fileSVM",  HTML("<b>Show dataset for SVM</b>"),
                                                                icon("eye", lib = "font-awesome"), #8*8*8*8*
                                                                style=" width:100%; white-space:normal;color: white;
                                                   background-color: #45c50e; border-color: white"),  #8*8*8*9*9*9*
                                                   bsModal("fileSVMID", title = "Dataset for SVM model",
                                                           trigger = "fileSVM", size = "large", #bsModal e' triggerato dal bottone, basta inserire l'input id
                                                           dataTableOutput("filexsvm")), #8
                                                   br(),br(),
                                                   actionButton("svm_button", HTML('<b>Run SVM</b>'),
                                                                icon("calculator", lib = "font-awesome"), #9*9*9*9*9*
                                                                style="white-space:normal;width:100%;height=80px;
                                                   color: white;
                                                   background-color: #ff5c5c; border-color: white"),#9*9*9*9*  Main button PCA
                                                   br()
                                  ) # end of conditionalPanel "SVM"
                     ),
                     mainPanel(
                         tabsetPanel(id='dataset02',
                                     tabPanel(("PCA"),
                                              fluidRow(
                                                  column(12,
                                                         h3(textOutput("captionSCREE"), align="center"),#9*9*9*9*9*
                                                         # dataTableOutput("dfxpca"), #8*8*8*togli
                                                         fluidRow(
                                                             column(6, br(), plotlyOutput("HistPCA")%>% withSpinner(6, color="#f90000")), # factoextra library generated plot (looks better than R basic one)
                                                             column(6, br(), plotlyOutput("screeplot_PCA")%>% withSpinner(6, color="#f90000"))#9*9*9*9*
                                                             #dataTableOutput("summary_PCA")     
                                                         ))),
                                              br(),
                                              br(),
                                              fluidRow(
                                                  column(12,
                                                         h3(textOutput("captionB"), align="center"),#9*9*9*9*
                                                         fluidRow(
                                                             column(6, br(), plotlyOutput("scores_2D")%>% withSpinner(6, color="#f90000")), #Biplot 9*9*9*9*9*
                                                             column(6, br(), plotlyOutput("loadings_2D")%>% withSpinner(6, color="#f90000"))
                                                             #htmlOutput("TextAboveLoadings"),
                                                             #tableOutput("LoadingsPCA"))
                                                         ))),
                                              fluidRow(
                                                  column(12,
                                                         br(), plotlyOutput("scores_3D")%>% withSpinner(6, color="#f90000")
                                                  ))
                                     ), #end tabPanel "PCA"
                                     tabPanel(("PLS-DA"),
                                              fluidRow(
                                                  column(12,
                                                         h3(textOutput("captionSCREE_PLSDA"), align="center"),#9*9*9*9*9*
                                                         # dataTableOutput("dfxpca"), #8*8*8*togli
                                                         fluidRow(
                                                             column(6, br(), plotlyOutput("HistPLSDA")%>% withSpinner(6, color="#f90000")), # factoextra library generated plot (looks better than R basic one)
                                                             column(6, br(), plotlyOutput("screeplot_PLSDA")%>% withSpinner(6, color="#f90000"))#9*9*9*9*
                                                             #dataTableOutput("summary_PCA")     
                                                         ))),
                                              br(),
                                              br(),
                                              fluidRow(
                                                  column(12,
                                                         h3(textOutput("captionB_PLSDA"), align="center"),#9*9*9*9*
                                                         fluidRow(
                                                             column(6, br(), plotlyOutput("scores_2D_PLSDA")%>% withSpinner(6, color="#f90000")), #Biplot 9*9*9*9*9*
                                                             column(6, br(), plotlyOutput("loadings_2D_PLSDA")%>% withSpinner(6, color="#f90000"))
                                                             #htmlOutput("TextAboveLoadings"),
                                                             #tableOutput("LoadingsPCA"))
                                                         ))),
                                              fluidRow(
                                                  column(12,
                                                         br(), plotlyOutput("scores_3D_PLSDA")%>% withSpinner(6, color="#f90000")
                                                  ))
                                     ), #end tabPanel "PLS-DA",
                                     tabPanel(("SVM"),
                                              h3(textOutput("captionB_SVM"), align="center"),#9*9*9*9*
                                              fluidRow(
                                                  column(12,
                                                         br(), plotOutput("results_SVM")%>% withSpinner(6, color="#f90000")
                                                  ))
                                     ) #end tabPanel "SVM"
                         ) # end tabsetPanel dataset02
                     ) #end mainPanel "Models"
                 ) # end sidebarLayout "Models"
        ), # end tabPanel "Models"
        tabPanel("Prediction", icon = icon("magic"),
                 sidebarLayout( #sidebarLayout "Prediction"
                     sidebarPanel(width=3, #sidebarPanel "Prediction"
                                  conditionalPanel('input.dataset03 === "Prediction on PCA"',
                                                   br(),
                                                   actionButton("prediction_pca_button", 
                                                                HTML('<b>Predict new subject</b>'),
                                                                icon = icon("magic"), #9*9*9*9*9*
                                                                style="white-space:normal;width:100%;height=80px;
                                                   color: white;
                                                   background-color: #ff5c5c; border-color: white"),#9*9*9*9*  Main button PCA
                                                   
                                                   br(),br(),
                                                   uiOutput("PCA_axis_1_test"),
                                                   uiOutput("PCA_axis_2_test"),
                                                   uiOutput("PCA_axis_3_test"),
                                                   br(), br(),br(),
                                                   downloadButton("downloader", HTML('<b>Generate BGA Report</b>'),
                                                                  icon=icon("downloader", lib = "font-awesome"),#9*9*9*9*
                                                                  style="width: 100%;white-space:normal; color: white; 
                                                   background-color: #337ab7; border-color: steelblue")
                                  ),# end of conditionalPanel "Prediction on PCA"
                                  conditionalPanel('input.dataset03 === "Prediction on PLS-DA"',
                                                   br(),
                                                   actionButton("prediction_plsda_button",
                                                                HTML('<b>Predict new subject</b>'),
                                                                icon = icon("magic"), #9*9*9*9*9*
                                                                style="white-space:normal;width:100%;height=80px;
                                                   color: white;
                                                   background-color: #ff5c5c; border-color: white"),#9*9*9*9*  Main button PCA
                                                   br(),br(),
                                                   uiOutput("PLSDA_axis_1_test"),
                                                   uiOutput("PLSDA_axis_2_test"),
                                                   uiOutput("PLSDA_axis_3_test"),
                                                   br(), br(),br(),
                                                   downloadButton("downloader_PLSDA", HTML('<b>Generate BGA Report</b>'),
                                                                  icon=icon("downloader", lib = "font-awesome"),#9*9*9*9*
                                                                  style="width: 100%;white-space:normal; color: white; 
                                                   background-color: #337ab7; border-color: steelblue")
                                  ),# end of conditionalPanel "Prediction on PLS-DA"
                                  conditionalPanel('input.dataset03 === "Prediction on SVM"',
                                                   br(),
                                                   actionButton("prediction_svm_button",
                                                                HTML('<b>Predict new subject</b>'),
                                                                icon = icon("magic"), #9*9*9*9*9*
                                                                style="white-space:normal;width:100%;height=80px;
                                                   color: white;
                                                   background-color: #ff5c5c; border-color: white"),#9*9*9*9*  Main button PCA
                                                   br(), br(),br(),
                                                   downloadButton("downloader_SVM", HTML('<b>Generate BGA Report</b>'),
                                                                  icon=icon("downloader", lib = "font-awesome"),#9*9*9*9*
                                                                  style="width: 100%;white-space:normal; color: white; 
                                                   background-color: #337ab7; border-color: steelblue"),
                                                   br()
                                  )# end of conditionalPanel "Prediction on SVM"
                     ),
                     mainPanel(
                         tabsetPanel(id='dataset03',
                                     tabPanel(("Prediction on PCA"),
                                              fluidRow(
                                                  column(12,
                                                         br(), plotlyOutput("scores_2D_PCA_prediction")%>% withSpinner(6, color="#f90000")
                                                  )),
                                              fluidRow(
                                                  column(12,
                                                         br(), plotlyOutput("scores_3D_PCA_prediction")%>% withSpinner(6, color="#f90000")
                                                  )),
                                              fluidRow(
                                                  column(12,
                                                         br(), plotOutput("plot_pca_pred")%>% withSpinner(6, color="#f90000")
                                                  )),
                                              fluidRow(
                                                  column(12,
                                                         # verbatimTextOutput("plotta") 
                                                         h3(textOutput("caption_LR_PCA"), align="center"),
                                                  )),
                                              fluidRow(
                                                  column(12,
                                                         br(),pre(id = "console")
                                                  ))
                                     ), #end tabPanel "Prediction on PCA"
                                     tabPanel(("Prediction on PLS-DA"),
                                              fluidRow(
                                                  column(12,
                                                         br(), plotlyOutput("scores_2D_PLSDA_prediction")%>% withSpinner(6, color="#f90000")
                                                  )),
                                              fluidRow(
                                                  column(12,
                                                         br(), plotlyOutput("scores_3D_PLSDA_prediction")%>% withSpinner(6, color="#f90000")
                                                  )),
                                              fluidRow(
                                                  column(12,
                                                         br(), plotOutput("plot_plsda_pred")%>% withSpinner(6, color="#f90000")
                                                  )),
                                              fluidRow(
                                                  column(12,
                                                         h3(textOutput("caption_LR_PLSDA"), align="center"),
                                                  )),
                                              fluidRow(
                                                  column(12,
                                                         br(),pre(id = "console_PLSDA")
                                                  ))
                                     ), #end tabPanel "Prediction on PLS-DA"
                                     tabPanel(("Prediction on SVM"),
                                              fluidRow(
                                                  column(12,
                                                         br(), plotlyOutput("SVM_prediction")%>% withSpinner(6, color="#f90000")
                                                         #plotOutput("SVM_prediction")
                                                  )),
                                              fluidRow(
                                                  column(12,
                                                         verbatimTextOutput("SVM_prediction_tab")
                                                  )),
                                              fluidRow(
                                                  column(12,
                                                         h3(textOutput("caption_SVM_prediction"), align="center"),
                                                         dataTableOutput("SVM_results_prediction")
                                                  ))
                                     ) #end tabPanel "Prediction on SVM"
                         ) #end tabsetPanel dataset03
                     ) #end mainPanel "Prediction" 
                 ) # end sidebarLayout "Prediction"
        )# end tabPanel "Prediction"
    ) ### end navbarPage 
)) # end ui