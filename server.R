library(shiny)
library(plotly)
library(DT)
library(ggplot2)
library(tidyverse)
library(purrr)
library(plyr)
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

data_handling_STR_training <- function(df){
    Populations<-as.factor(df[,2])
    df<-df[,-2]
    names = c(colnames(df))
    evenvals <- seq(2, ncol(df), by=2) 
    names_ok<-colnames(df[,evenvals])
    data_unl<-as.numeric(unlist(df[,-c(1)]))
    data_names<-as.data.frame(df[,1])
    
    data_unl=dplyr::recode(data_unl,'1'=1,	'1.1'=2,	'1.2'=3,	'1.3'=4,	'2'=5,	'2.1'=6,	'2.2'=7,	'2.3'=8,	'3'=9,	'3.1'=10,	'3.2'=11,	'3.3'=12,	'4'=13,	'4.1'=14,	'4.2'=15,	'4.3'=16,	'5'=17,	'5.1'=18,	'5.2'=19,	'5.3'=20,	'6'=21,	'6.1'=22,	'6.2'=23,	'6.3'=24,	'7'=25,	'7.1'=26,	'7.2'=27,	'7.3'=28,	'8'=29,	'8.1'=30,	'8.2'=31,	'8.3'=32,	'9'=33,	'9.1'=34,	'9.2'=35,	'9.3'=36,	'10'=37,	'10.1'=38,	'10.2'=39,	'10.3'=40,	'11'=41,	'11.1'=42,	'11.2'=43,	'11.3'=44,	'12'=45,	'12.1'=46,	'12.2'=47,	'12.3'=48,	'13'=49,	'13.1'=50,	'13.2'=51,	'13.3'=52,	'14'=53,	'14.1'=54,	'14.2'=55,	'14.3'=56,	'15'=57,	'15.1'=58,	'15.2'=59,	'15.3'=60,	'16'=61,	'16.1'=62,	'16.2'=63,	'16.3'=64,	'17'=65,	'17.1'=66,	'17.2'=67,	'17.3'=68,	'18'=69,	'18.1'=70,	'18.2'=71,	'18.3'=72,	'19'=73,	'19.1'=74,	'19.2'=75,	'19.3'=76,	'20'=77,	'20.1'=78,	'20.2'=79,	'20.3'=80,	'21'=81,	'21.1'=82,	'21.2'=83,	'21.3'=84,	'22'=85,	'22.1'=86,	'22.2'=87,	'22.3'=88,	'23'=89,	'23.1'=90,	'23.2'=91,	'23.3'=92,	'24'=93,	'24.1'=94,	'24.2'=95,	'24.3'=96,	'25'=97,	'25.1'=98,	'25.2'=99,	'25.3'=100,	'26'=101,	'26.1'=102,	'26.2'=103,	'26.3'=104,	'27'=105,	'27.1'=106,	'27.2'=107,	'27.3'=108,	'28'=109,	'28.1'=110,	'28.2'=111,	'28.3'=112,	'29'=113,	'29.1'=114,	'29.2'=115,	'29.3'=116,	'30'=117,	'30.1'=118,	'30.2'=119,	'30.3'=120,	'31'=121,	'31.1'=122,	'31.2'=123,	'31.3'=124,	'32'=125,	'32.1'=126,	'32.2'=127,	'32.3'=128,	'33'=129,	'33.1'=130,	'33.2'=131,	'33.3'=132,	'34'=133,	'34.1'=134,	'34.2'=135,	'34.3'=136,	'35'=137,	'35.1'=138,	'35.2'=139,	'35.3'=140,	'36'=141,	'36.1'=142,	'36.2'=143,	'36.3'=144,	'37'=145,	'37.1'=146,	'37.2'=147,	'37.3'=148,	'38'=149,	'38.1'=150,	'38.2'=151,	'38.3'=152,	'39'=153,	'39.1'=154,	'39.2'=155,	'39.3'=156,	'40'=157,	'40.1'=158,	'40.2'=159,	'40.3'=160,	'41'=161,	'41.1'=162,	'41.2'=163,	'41.3'=164,	'42'=165,	'42.1'=166,	'42.2'=167,	'42.3'=168,	'43'=169,	'43.1'=170,	'43.2'=171,	'43.3'=172,	'44'=173,	'44.1'=174,	'44.2'=175,	'44.3'=176,	'45'=177,	'45.1'=178,	'45.2'=179,	'45.3'=180,	'46'=181,	'46.1'=182,	'46.2'=183,	'46.3'=184,	'47'=185,	'47.1'=186,	'47.2'=187,	'47.3'=188,	'48'=189,	'48.1'=190,	'48.2'=191,	'48.3'=192,	'49'=193,	'49.1'=194,	'49.2'=195,	'49.3'=196,	'50'=197,
    )
    nX<-ncol(df[,-c(1)])
    data_unl<-as.data.frame(matrix(data_unl,ncol = nX))
    data_def<-cbind(data_names,data_unl)
    colnames(data_def)<- names
    df<-data_def
    names = c(colnames(df))
    nX <- ncol(df)-1
    listofdf <- lapply(1:nX, function(x) NULL)
    for (i in 1:nX) {
        listofdf[[i]] <- data.frame(df[,1], df[i],df[i+1])
    }
    listofdf <- listofdf [-c(seq(1, 10000, by=2))]
    listofdf_subs <- lapply(1:length(listofdf), function(x) NULL)
    for (i in 1:length(listofdf)) {
        listofdf_subs[[i]] <- cbind(rep(x = 1:nrow(listofdf[[1]]),
                                        times = 2),c(listofdf[[i]][,2],listofdf[[i]][,3]))
    }
    required_vals <- rep(x = 1,
                         times = nrow(x = listofdf_subs[[1]]))
    required_sz <- c(nrow(x = df), 197)
    listofdf_accum<-lapply(1:length(listofdf), function(x) NULL)
    
    for (i in 1:length(listofdf)){ 
        listofdf_accum[[i]]<-pracma::accumarray(subs = listofdf_subs[[i]],
                                                val = required_vals,
                                                sz = required_sz)}
    data_def <- list()
    for(i in 1:length(listofdf)){
        data_def[[i]] <- listofdf_accum[[i]]
    }
    mydf <- data.frame(data_def)
    values <- rep_len(x = ((0:3) / 10),
                      length.out = 197) + rep(x = 1:50,
                                              each = 4,
                                              length.out = 197)
    variables_ID <- paste(rep(x = names_ok,
                              each = 197),
                          values,
                          sep = "_")
    colnames(mydf)<-variables_ID
    mydf<-mydf[,-(which(colSums(mydf) == 0))] 
    mydf<-cbind(data_names,mydf)
    mydf[mydf=="2"]<-1
    row.names(mydf) <- mydf[,1]
    mydf<-mydf[,-1]
    df1_sel2 <- cbind(Populations,mydf)
    df1_sel2[, colSums(df1_sel2 != 0) > 0]
    df1_sel2<-df1_sel2[sapply(df1_sel2, function(x) length(unique(na.omit(x)))) > 1]
    #df1_sel2 <<- df1_sel2
}

withConsoleRedirect <- function(containerId, expr) {
    # Change type="output" to type="message" to catch stderr
    # (messages, warnings, and errors) instead of stdout.
    txt <- capture.output(results <- expr, type = "output")
    if (length(txt) > 0) {
        insertUI(paste0("#", containerId), where = "beforeEnd",
                 ui = paste0(txt, "\n", collapse = "")
        )
    }
    results
}

cbp1 <- c("#E69F00", "#56B4E9", "#009E73","#D55E00", "#CC79A7",
          "#999999", "#F0E442", "#0072B2") #palette di colori

server <-  function(input, output, session) {
    observeEvent(input$generate1, {
        output$pdfview <- renderUI({
            tags$iframe(style="600px; width:100%", src="draft_bga.pdf", scrolling="yes")
        })
    })
    
    observeEvent(input$generate2, {
        output$pdfview <- renderUI({
            tags$iframe(style="600px; width:100%", src="draft_bga.pdf", scrolling="yes")
        })
    })
    
    #### Import Allele frequencies ####
    
    getData0 <- reactive({
        req(input$file0)
        inFile <- input$file0
        if (is.null(inFile)){
            return(NULL)
        }
        else {
            # browser()
            numfiles = nrow(inFile) 
            mydata = list()
            for (i in 1:numfiles)
            {
                imported_data = read.table(input$file0[[i, 'datapath']], 
                                           header = input$header0,
                                           sep=input$sep0)
                imported_data[is.na(imported_data)] = 0
                mydata[[i]] = imported_data
            }
            # browser()
            do.call(rbind, mydata)
            #mydata <<- mydata
        }
    })
    
    #hidden settings file1 population dataset
    observeEvent(input$set0, {
        toggle("header0")
        toggle("sep0")
        toggle("disp0")
    })
    # output checkbox0
    output$checkbox0 <- renderUI({
        req(input$file0)
        data <- as.data.frame(getData0())
        first <- colnames(data[,c(1)])
        str_kit0 <- switch(input$str_kit0,
                           all_data = c(colnames(as.data.frame(getData0()))),
                           global = c(first,"D13S317", "D7S820","D5S818","CSF1PO","D1S1656","D12S391", 
                                      "D2S441","D10S1248","D18S51","FGA","D21S11","D8S1179","vWA",
                                      "D16S539","TH01","D3S1358","D2S1338","D19S433","TPOX",
                                      "D22S1045","SE33"),
                           esx17 = c(first,"D3S1358","D8S1179","D18S51","D21S11",
                                     "FGA","TH01","vWA","D2S441","D10S1248","D22S1045","D1S1656",
                                     "D12S391","D2S1338","D16S539","D19S433","SE33"),
                           fusion6c = c(first, "CSF1PO","FGA","TH01","vWA","D1S1656","D2S1338","D2S441",
                                        "D3S1358","D5S818","D7S820","D8S1179","D10S1248","D12S391",
                                        "D13S317","D16S539","D18S51","D19S433","D21S11","Penta_D",
                                        "Penta_E","D22S1045","TPOX","SE33"),
                           ngmsel = c(first,"D1S1656","D12S391","D2S441","D10S1248","D18S51","FGA","D21S11",
                                      "D8S1179","vWA","D16S539","TH01","D3S1358","D2S1338","D19S433",
                                      "D22S1045","SE33"),
                           qs24plex = c(first, "TH01","D3S1358","vWA","D21S11","TPOX","D1S1656","D12S391",
                                        "SE33","D10S1248","D22S1045","D19S433","D8S1179","D2S1338","D2S441",
                                        "D18S51","FGA","D16S539","CSF1PO","D13S317","D5S818","D7S820"),
                           essseplus = c(first, "D1S1656","D2S441","D2S1338","D3S1358","D8S1179","D10S1248","D12S391",
                                         "D16S539","D18S51","D19S433","D21S11","D22S1045","FGA","TH01","vWA","SE33"),
                           c(colnames(as.data.frame(getData0()))))
        #getData()[str_kit]
        checkboxGroupInput(inputId = "select_var0", 
                           label = HTML('<h5><b>Select Variables</b></h5>'), 
                           choices = str_kit0,selected = str_kit0)
    })
    
    # Select columns to print 
    df1_sel0 <- reactive({
        #req(input$file1)
        data0 <- as.data.frame(getData0())
        data0 %>% dplyr::select(input$select_var0)
    })
    
    # Press button to see Imported table
    goButton0 <- eventReactive(input$go_button0,{
        freq <- as.data.frame(df1_sel0())
        library(forensim)
        freq<-tabfreq(freq)
        dat_geno <- forensim::simugeno(freq,n =input$num)
        dat_geno<-as.data.frame(dat_geno@tab.geno)
        dat_geno<-dat_geno[ , order(names(dat_geno))]
        dat_geno <<- dat_geno
        names_loci <- c(colnames(dat_geno))
        names_loci <- c(names_loci,names_loci)
        names_loci <- sort(names_loci)
        col_split <- colnames(dat_geno)
        
        # Very useful function from https://gist.github.com/mrdwab/11380733
        cSplit <- function(indt, splitCols, sep = ",", direction = "wide",  
                           makeEqual = NULL, fixed = TRUE, drop = TRUE, 
                           stripWhite = FALSE) {
            message("`cSplit` is now part of the 'splitstackshape' package (V1.4.0)")
            ## requires data.table >= 1.8.11
            require(data.table)
            if (!is.data.table(indt)) setDT(indt)
            if (is.numeric(splitCols)) splitCols <- names(indt)[splitCols]
            if (any(!vapply(indt[, splitCols, with = FALSE],
                            is.character, logical(1L)))) {
                indt[, eval(splitCols) := lapply(.SD, as.character),
                     .SDcols = splitCols]
            }
            
            if (length(sep) == 1) 
                sep <- rep(sep, length(splitCols))
            if (length(sep) != length(splitCols)) {
                stop("Verify you have entered the correct number of sep")
            }
            
            if (isTRUE(stripWhite)) {
                indt[, eval(splitCols) := mapply(function(x, y) 
                    gsub(sprintf("\\s+%s\\s+|\\s+%s|%s\\s+", 
                                 x, x, x), x, y), 
                    sep, indt[, splitCols, with = FALSE], 
                    SIMPLIFY = FALSE)]
            }  
            
            X <- lapply(seq_along(splitCols), function(x) {
                strsplit(indt[[splitCols[x]]], split = sep[x], fixed = fixed)
            })
            
            if (direction == "long") {
                if (is.null(makeEqual)) {
                    IV <- function(x,y) if (identical(x,y)) TRUE else FALSE
                    makeEqual <- ifelse(Reduce(IV, rapply(X, length, how = "list")),
                                        FALSE, TRUE)
                }
            } else if (direction == "wide") {
                if (!is.null(makeEqual)) {
                    if (!isTRUE(makeEqual)) {
                        message("makeEqual specified as FALSE but set to TRUE")
                        makeEqual <- TRUE
                    }
                    makeEqual <- TRUE
                } else {
                    makeEqual <- TRUE
                }
            }
            if (isTRUE(makeEqual)) {
                SetUp <- lapply(seq_along(X), function(y) {
                    A <- vapply(X[[y]], length, 1L)
                    list(Mat = cbind(rep(seq_along(A), A), sequence(A)),
                         Val = unlist(X[[y]]))
                })    
                Ncol <- max(unlist(lapply(SetUp, function(y) y[["Mat"]][, 2]), 
                                   use.names = FALSE))
                X <- lapply(seq_along(SetUp), function(y) {
                    M <- matrix(NA_character_, nrow = nrow(indt), ncol = Ncol)
                    M[SetUp[[y]][["Mat"]]] <- SetUp[[y]][["Val"]]
                    M
                })
                if (direction == "wide") {
                    X <- lapply(seq_along(X), function(x) {
                        colnames(X[[x]]) <- paste(splitCols[x], 
                                                  sequence(ncol(X[[x]])), 
                                                  sep = "_")
                        X[[x]]
                    })
                    if (isTRUE(drop)) {
                        cbind(indt, do.call(cbind, X))[, eval(splitCols) := NULL][]
                    } else {
                        cbind(indt, do.call(cbind, X))
                    }
                } else {
                    indt <- indt[rep(sequence(nrow(indt)), each = Ncol)]
                    X <- lapply(X, function(y) as.vector(t(y)))
                    indt[, eval(splitCols) := lapply(X, unlist, use.names = FALSE)][]
                }  
            } else {
                Rep <- vapply(X[[1]], length, integer(1L))
                indt <- indt[rep(sequence(nrow(indt)), Rep)]
                indt[, eval(splitCols) := lapply(X, unlist, use.names = FALSE)][]
            }
        }
        
        ok_dat_geno<-cSplit(as.data.table(dat_geno), col_split, "/")
        zz_label <- paste0(c(rep("zz", ncol(ok_dat_geno))),1:ncol(ok_dat_geno))
        colnames(ok_dat_geno) <- zz_label
        
        ok_dat_geno$Sample <- paste0(c(rep("ind", input$num)),"_",input$class,"_",1:input$num)
        ok_dat_geno$Pop <- paste0(c(rep(input$class,input$num)))
        ok_dat_geno = ok_dat_geno %>%
            dplyr::select(Sample, Pop, everything())
        col_ok <- c("Sample","Pop",names_loci)
        colnames(ok_dat_geno)<-col_ok
        ok_dat_geno <<- ok_dat_geno
        write.table(ok_dat_geno, file=paste(input$class, "_simulated_genotypes.txt", sep=""),row.names=FALSE,sep="\t", quote = FALSE)
        ok_dat_geno
    })
    
    # # Print Imported table 
    # output$rendered_file0 <- DT::renderDataTable({
    #     req(input$go_button0)
    #     goButton0()
    #     #getData()
    # })
    
    observeEvent(input$go_button0, {
        confirmSweetAlert(
            session = session,
            inputId = "data_simulation",
            type = "warning",
            title = "A simulated dataset will be created and automatically downloaded in your working folder",
            btn_labels = c("No", "Yes"),
            btn_colors = c("#FE642E", "#04B404")
        )
    })
    
    observeEvent(input$data_simulation, {
        if (isTRUE(input$data_simulation)) {
            output$rendered_file0 <- DT::renderDataTable({
                #req(input$go_button0)
                goButton0()
            })
        } else {
            return(NULL)
        }
    }, ignoreNULL = TRUE)
    
    
    
    #### Import database ####
    
    getData <- reactive({
        req(input$file1)
        
        inFile <- input$file1
        if (is.null(inFile)){
            return(NULL)
        }
        else {
            # browser()
            library(tidyverse)
            library(tidyselect)
            numfiles = nrow(inFile) 
            mydata = list()
            
            for (i in 1:numfiles)
            {
                imported_data = read.table(input$file1[[i, 'datapath']], 
                                           header = input$header,
                                           sep=input$sep)
                mydata[[i]] = imported_data
                ### test new ###
                
            }
            # browser()
            variables <- map(mydata,names)
            InAll <- variables %>% reduce(intersect)
            my_selected_data <- map(mydata,~select(.,all_of(InAll)))
            final <- my_selected_data %>% reduce(union_all)
            final_with_fact <- mutate_if(final,
                                         is.character,as_factor)
            final_with_fact
            #do.call(rbind, mydata)
            
        }
    })
    
    #hidden settings file1 population dataset
    observeEvent(input$set, {
        toggle("header")
        toggle("sep")
        toggle("disp")
    })
    
    
    output$checkbox1 <- renderUI({
        req(input$file1)
        first <- colnames(getData()[,c(1,2)])
        
        str_kit <- switch(input$str_kit,
                          global = c(first,"D13S317", "D13S317.1","D7S820","D7S820.1", "D5S818","D5S818.1",
                                     "CSF1PO","CSF1PO.1", "D1S1656", "D1S1656.1","D12S391","D12S391.1", 
                                     "D2S441", "D2S441.1", "D10S1248", "D10S1248.1", "D18S51", "D18S51.1",
                                     "FGA", "FGA.1", "D21S11", "D21S11.1", "D8S1179", "D8S1179.1", "vWA", "vWA.1",
                                     "D16S539", "D16S539.1", "TH01", "TH01.1", "D3S1358", "D3S1358.1",
                                     "D2S1338", "D2S1338.1", "D19S433", "D19S433.1", "TPOX", "TPOX.1",
                                     "D22S1045", "D22S1045.1", "SE33", "SE33.1"),
                          esx17 = c(first,"D3S1358", "D3S1358.1", "D8S1179", "D8S1179.1", "D18S51", "D18S51.1", "D21S11", "D21S11.1",
                                    "FGA", "FGA.1", "TH01", "TH01.1", "vWA", "vWA.1", "D2S441", "D2S441.1", "D10S1248", "D10S1248.1",
                                    "D22S1045", "D22S1045.1", "D1S1656", "D1S1656.1", "D12S391", "D12S391.1", "D2S1338", "D2S1338.1",
                                    "D16S539", "D16S539.1", "D19S433",  "D19S433.1", "SE33", "SE33.1"),
                          fusion6c = c(first, "CSF1PO", "CSF1PO.1", "FGA", "FGA.1", "TH01", "TH01.1", "vWA",  "vWA.1",
                                       "D1S1656", "D1S1656.1","D2S1338", "D2S1338.1", "D2S441", "D2S441.1",
                                       "D3S1358", "D3S1358.1", "D5S818", "D5S818.1", "D7S820", "D7S820.1",
                                       "D8S1179", "D8S1179.1", "D10S1248", "D10S1248.1", "D12S391",  "D12S391.1",
                                       "D13S317", "D13S317.1", "D16S539", "D16S539.1", "D18S51",  "D18S51.1",
                                       "D19S433", "D19S433.1", "D21S11", "D21S11.1", "Penta_D", "Penta_D.1",
                                       "Penta_E", "Penta_E.1", "D22S1045", "D22S1045.1", "TPOX", "TPOX.1", "SE33", "SE33.1"),
                          ngmsel = c(first,"D1S1656", "D1S1656.1","D12S391","D12S391.1","D2S441", "D2S441.1",
                                     "D10S1248", "D10S1248.1", "D18S51", "D18S51.1", "FGA", "FGA.1", "D21S11", "D21S11.1", 
                                     "D8S1179", "D8S1179.1", "vWA", "vWA.1", "D16S539", "D16S539.1", "TH01", "TH01.1", 
                                     "D3S1358", "D3S1358.1", "D2S1338", "D2S1338.1", "D19S433", "D19S433.1",
                                     "D22S1045", "D22S1045.1", "SE33", "SE33.1"),
                          qs24plex = c(first, "TH01", "TH01.1",	"D3S1358", "D3S1358.1",	"vWA", "vWA.1",	"D21S11",	"D21S11.1",	 	 
                                       "TPOX", "TPOX.1",	"D1S1656",	"D1S1656.1",	"D12S391",	"D12S391.1",	"SE33",	"SE33.1",	 	 
                                       "D10S1248",	"D10S1248.1",	"D22S1045",	"D22S1045.1",	"D19S433",	"D19S433.1",	"D8S1179",	"D8S1179.1",
                                       "D2S1338", "D2S1338.1", "D2S441", "D2S441.1", "D18S51", "D18S51.1",	"FGA",	"FGA.1",	 	 	 	 
                                       "D16S539",	"D16S539.1",	"CSF1PO",	"CSF1PO.1",	"D13S317",	"D13S317.1",	"D5S818",	"D5S818.1",	"D7S820",	"D7S820.1"),
                          essseplus = c(first, "D1S1656", "D1S1656.1", "D2S441", "D2S441.1", "D2S1338",  "D2S1338.1",
                                        "D3S1358", "D3S1358.1", "D8S1179",  "D8S1179.1", "D10S1248",  "D10S1248.1", "D12S391",  "D12S391.1", 
                                        "D16S539", "D16S539.1", "D18S51",  "D18S51.1", "D19S433", "D19S433.1", "D21S11",  "D21S11.1",
                                        "D22S1045", "D22S1045.1", "FGA",  "FGA.1", "TH01", "TH01.1", "vWA", "vWA.1", "SE33", "SE33.1"),
                          all_data = c(colnames(getData())),
                          c(colnames(getData())))
        #getData()[str_kit]
        checkboxGroupInput(inputId = "select_var1", 
                           label = HTML('<h5><b>Select Variables</b></h5>'), 
                           choices = str_kit,selected = str_kit)
    })
    
    output$FirstCheckPopulations <- renderUI({
        req(input$file1)
        choice <-  unique(getData()[,2])
        checkboxGroupInput("checkpopulations", label = HTML('<h5><b>Select Populations</b></h5>'), 
                           choices = choice, selected = choice)
    })
    
    
    
    # Select columns to print 
    df1_sel2 <- reactive({
        #req(input$file1)
        library(plyr); library(dplyr)
        data_int <- getData() %>% dplyr::select(input$select_var1)
        #df <- df1() %>% filter(df1()[,2] %in% input$checkpopulations)
        df <- data_int %>% filter(data_int[,2] %in% input$checkpopulations)
        funct <- switch(input$funct,
                        strs = data_handling_STR_training,
                        data_handling_STR_training)
        funct(df)
        
    })
    
    df_col <- reactive ({
        library(plyr); library(dplyr)
        df_col <- getData() %>% dplyr::select(input$select_var1)
        #df <- df1() %>% filter(df1()[,2] %in% input$checkpopulations)
        #df_col %>% filter(data_int[,2] %in% input$checkpopulations)
        
    })
    
    
    # Press button to see Imported table
    goButton <- eventReactive(input$go_button,{
        df1_sel2()
    })
    
    # Print Imported table 
    output$rendered_file1 <- DT::renderDataTable({
        req(input$checkpopulations)
        req(input$go_button)
        goButton()
        #getData()
    })
    
    #### Import New Subject ####
    
    # Read file 2 in Dataset 
    df2 <- reactive({ 
        req(input$file2)
        data<- read.table(input$file2$datapath,
                          header = input$header2,
                          sep = input$sep2)
    })
    
    #hidden settings file2 population dataset
    observeEvent(input$set2, {
        toggle("header2")
        toggle("sep2")
        toggle("disp2")
    })
    
    # Select populations to be used and converted
    output$FirstCheckSubjects <- renderUI({
        req(input$file2)
        choice <-  unique(df2()[,2])
        checkboxGroupInput("checksubjects", label = HTML('<h5><b>Select Individuals</b></h5>'), 
                           choices = choice, selected = choice)
    })
    
    # Select columns to print for new subjects
    df1_subj <- reactive({
        library(plyr); library(dplyr)
        data <- df2() %>% filter(df2()[,2] %in% input$checksubjects)
        #df1_subj <- df2() #%>% dplyr::select(input$select_var1)
        #data <- data[,-]
        
        data_handling_STR_test <- function(data){
        
        cols.num <- c("Allele.1","Allele.2")
        data<-data[data$Marker!='AMEL' & data$Marker!='Yindel' & data$Marker!='DYS391', ]
        data[cols.num] <- sapply(data[cols.num],as.character)
        data[cols.num] <- sapply(data[cols.num],as.numeric)
        data$Allele.2 <- ifelse(is.na(data$Allele.2), data$Allele.1, data$Allele.2)
        library(tidyr)
        data <- pivot_longer(data, Allele.1:Allele.2)
        data <- subset(data, select=-c(name))
        data<- as.data.frame(data)
        
        detach("package:plyr", unload=TRUE)
        library(tidyverse)
        data2 <- group_by(data,
                          #Sample.File,
                          Sample.Name,
                          Marker) %>%
            mutate(rn=row_number() -1 ,
                   rn2 = ifelse(rn == 0,"",paste0(".",as.character(rn))),
                   truemarker=paste0(Marker,rn2)) %>% 
            ungroup %>% 
            select(-Marker,-rn,-rn2)
        
        data2 <- data2 %>% pivot_wider(names_from = truemarker,
                                       values_from = value)
        first <- colnames(getData()[1:2])
        colnames(data2)[1:2] <- first
        data2<- as.data.frame(data2)
        str_kit <- switch(input$str_kit,
                          global = c(first,"D13S317", "D13S317.1","D7S820","D7S820.1", "D5S818","D5S818.1",
                                     "CSF1PO","CSF1PO.1", "D1S1656", "D1S1656.1","D12S391","D12S391.1", 
                                     "D2S441", "D2S441.1", "D10S1248", "D10S1248.1", "D18S51", "D18S51.1",
                                     "FGA", "FGA.1", "D21S11", "D21S11.1", "D8S1179", "D8S1179.1", "vWA", "vWA.1",
                                     "D16S539", "D16S539.1", "TH01", "TH01.1", "D3S1358", "D3S1358.1",
                                     "D2S1338", "D2S1338.1", "D19S433", "D19S433.1", "TPOX", "TPOX.1",
                                     "D22S1045", "D22S1045.1", "SE33", "SE33.1"),
                          esx17 = c(first,"D3S1358", "D3S1358.1", "D8S1179", "D8S1179.1", "D18S51", "D18S51.1", "D21S11", "D21S11.1",
                                    "FGA", "FGA.1", "TH01", "TH01.1", "vWA", "vWA.1", "D2S441", "D2S441.1", "D10S1248", "D10S1248.1",
                                    "D22S1045", "D22S1045.1", "D1S1656", "D1S1656.1", "D12S391", "D12S391.1", "D2S1338", "D2S1338.1",
                                    "D16S539", "D16S539.1", "D19S433",  "D19S433.1", "SE33", "SE33.1"),
                          fusion6c = c(first, "CSF1PO", "CSF1PO.1", "FGA", "FGA.1", "TH01", "TH01.1", "vWA",  "vWA.1",
                                       "D1S1656", "D1S1656.1","D2S1338", "D2S1338.1", "D2S441", "D2S441.1",
                                       "D3S1358", "D3S1358.1", "D5S818", "D5S818.1", "D7S820", "D7S820.1",
                                       "D8S1179", "D8S1179.1", "D10S1248", "D10S1248.1", "D12S391",  "D12S391.1",
                                       "D13S317", "D13S317.1", "D16S539", "D16S539.1", "D18S51",  "D18S51.1",
                                       "D19S433", "D19S433.1", "D21S11", "D21S11.1", "Penta_D", "Penta_D.1",
                                       "Penta_E", "Penta_E.1", "D22S1045", "D22S1045.1", "TPOX", "TPOX.1", "SE33", "SE33.1"),
                          ngmsel = c(first,"D1S1656", "D1S1656.1","D12S391","D12S391.1","D2S441", "D2S441.1",
                                     "D10S1248", "D10S1248.1", "D18S51", "D18S51.1", "FGA", "FGA.1", "D21S11", "D21S11.1", 
                                     "D8S1179", "D8S1179.1", "vWA", "vWA.1", "D16S539", "D16S539.1", "TH01", "TH01.1", 
                                     "D3S1358", "D3S1358.1", "D2S1338", "D2S1338.1", "D19S433", "D19S433.1",
                                     "D22S1045", "D22S1045.1", "SE33", "SE33.1"),
                          qs24plex = c(first, "TH01", "TH01.1",	"D3S1358", "D3S1358.1",	"vWA", "vWA.1",	"D21S11",	"D21S11.1",	 	 
                                       "TPOX", "TPOX.1",	"D1S1656",	"D1S1656.1",	"D12S391",	"D12S391.1",	"SE33",	"SE33.1",	 	 
                                       "D10S1248",	"D10S1248.1",	"D22S1045",	"D22S1045.1",	"D19S433",	"D19S433.1",	"D8S1179",	"D8S1179.1",
                                       "D2S1338", "D2S1338.1", "D2S441", "D2S441.1", "D18S51", "D18S51.1",	"FGA",	"FGA.1",	 	 	 	 
                                       "D16S539",	"D16S539.1",	"CSF1PO",	"CSF1PO.1",	"D13S317",	"D13S317.1",	"D5S818",	"D5S818.1",	"D7S820",	"D7S820.1"),
                          essseplus = c(first, "D1S1656", "D1S1656.1", "D2S441", "D2S441.1", "D2S1338",  "D2S1338.1",
                                        "D3S1358", "D3S1358.1", "D8S1179",  "D8S1179.1", "D10S1248",  "D10S1248.1", "D12S391",  "D12S391.1", 
                                        "D16S539", "D16S539.1", "D18S51",  "D18S51.1", "D19S433", "D19S433.1", "D21S11",  "D21S11.1",
                                        "D22S1045", "D22S1045.1", "FGA",  "FGA.1", "TH01", "TH01.1", "vWA", "vWA.1", "SE33", "SE33.1"),
                          all_data = c(colnames(getData())),
                          c(colnames(getData())))
        data2[str_kit]
        
        
        }
        #library(plyr);
        #library(dplyr)
        
        funct3 <- switch(input$funct3,
                         strs = data_handling_STR_test,
                         data_handling_STR_test)
        funct3(data)
    })
    
    df1_sel2_subj <- reactive ({
        input_subj <- df1_subj()
        data_handling_STR_test2 <- function(input_subj) {
            Populations<-as.factor(input_subj[,2])
            input_subj<-input_subj[,-c(2)]
            names = c(colnames(input_subj))
            evenvals <- seq(2, ncol(input_subj), by=2) 
            names_ok<-colnames(input_subj[,evenvals])
            data_unl<-as.numeric(unlist(input_subj[,-c(1)]))
            data_names<-as.data.frame(input_subj[,1])
            #data_unl=recode(data_unl,'1'=1,	'1.1'=2,	'1.2'=3,	'1.3'=4,	'2'=5,	'2.1'=6,	'2.2'=7,	'2.3'=8,	'3'=9,	'3.1'=10,	'3.2'=11,	'3.3'=12,	'4'=13,	'4.1'=14,	'4.2'=15,	'4.3'=16,	'5'=17,	'5.1'=18,	'5.2'=19,	'5.3'=20,	'6'=21,	'6.1'=22,	'6.2'=23,	'6.3'=24,	'7'=25,	'7.1'=26,	'7.2'=27,	'7.3'=28,	'8'=29,	'8.1'=30,	'8.2'=31,	'8.3'=32,	'9'=33,	'9.1'=34,	'9.2'=35,	'9.3'=36,	'10'=37,	'10.1'=38,	'10.2'=39,	'10.3'=40,	'11'=41,	'11.1'=42,	'11.2'=43,	'11.3'=44,	'12'=45,	'12.1'=46,	'12.2'=47,	'12.3'=48,	'13'=49,	'13.1'=50,	'13.2'=51,	'13.3'=52,	'14'=53,	'14.1'=54,	'14.2'=55,	'14.3'=56,	'15'=57,	'15.1'=58,	'15.2'=59,	'15.3'=60,	'16'=61,	'16.1'=62,	'16.2'=63,	'16.3'=64,	'17'=65,	'17.1'=66,	'17.2'=67,	'17.3'=68,	'18'=69,	'18.1'=70,	'18.2'=71,	'18.3'=72,	'19'=73,	'19.1'=74,	'19.2'=75,	'19.3'=76,	'20'=77,	'20.1'=78,	'20.2'=79,	'20.3'=80,	'21'=81,	'21.1'=82,	'21.2'=83,	'21.3'=84,	'22'=85,	'22.1'=86,	'22.2'=87,	'22.3'=88,	'23'=89,	'23.1'=90,	'23.2'=91,	'23.3'=92,	'24'=93,	'24.1'=94,	'24.2'=95,	'24.3'=96,	'25'=97,	'25.1'=98,	'25.2'=99,	'25.3'=100,	'26'=101,	'26.1'=102,	'26.2'=103,	'26.3'=104,	'27'=105,	'27.1'=106,	'27.2'=107,	'27.3'=108,	'28'=109,	'28.1'=110,	'28.2'=111,	'28.3'=112,	'29'=113,	'29.1'=114,	'29.2'=115,	'29.3'=116,	'30'=117,	'30.1'=118,	'30.2'=119,	'30.3'=120,	'31'=121,	'31.1'=122,	'31.2'=123,	'31.3'=124,	'32'=125,	'32.1'=126,	'32.2'=127,	'32.3'=128,	'33'=129,	'33.1'=130,	'33.2'=131,	'33.3'=132,	'34'=133,	'34.1'=134,	'34.2'=135,	'34.3'=136,	'35'=137,	'35.1'=138,	'35.2'=139,	'35.3'=140,	'36'=141,	'36.1'=142,	'36.2'=143,	'36.3'=144,	'37'=145,	'37.1'=146,	'37.2'=147,	'37.3'=148,	'38'=149,	'38.1'=150,	'38.2'=151,	'38.3'=152,	'39'=153,	'39.1'=154,	'39.2'=155,	'39.3'=156,	'40'=157,	'40.1'=158,	'40.2'=159,	'40.3'=160,	'41'=161,	'41.1'=162,	'41.2'=163,	'41.3'=164,	'42'=165,	'42.1'=166,	'42.2'=167,	'42.3'=168,	'43'=169,	'43.1'=170,	'43.2'=171,	'43.3'=172,	'44'=173,	'44.1'=174,	'44.2'=175,	'44.3'=176,	'45'=177,	'45.1'=178,	'45.2'=179,	'45.3'=180,	'46'=181,	'46.1'=182,	'46.2'=183,	'46.3'=184,	'47'=185,	'47.1'=186,	'47.2'=187,	'47.3'=188,	'48'=189,	'48.1'=190,	'48.2'=191,	'48.3'=192,	'49'=193,	'49.1'=194,	'49.2'=195,	'49.3'=196,	'50'=197,
            #)
            data_unl=dplyr::recode(data_unl,'1'=1,	'1.1'=2,	'1.2'=3,	'1.3'=4,	'2'=5,	'2.1'=6,	'2.2'=7,	'2.3'=8,	'3'=9,	'3.1'=10,	'3.2'=11,	'3.3'=12,	'4'=13,	'4.1'=14,	'4.2'=15,	'4.3'=16,	'5'=17,	'5.1'=18,	'5.2'=19,	'5.3'=20,	'6'=21,	'6.1'=22,	'6.2'=23,	'6.3'=24,	'7'=25,	'7.1'=26,	'7.2'=27,	'7.3'=28,	'8'=29,	'8.1'=30,	'8.2'=31,	'8.3'=32,	'9'=33,	'9.1'=34,	'9.2'=35,	'9.3'=36,	'10'=37,	'10.1'=38,	'10.2'=39,	'10.3'=40,	'11'=41,	'11.1'=42,	'11.2'=43,	'11.3'=44,	'12'=45,	'12.1'=46,	'12.2'=47,	'12.3'=48,	'13'=49,	'13.1'=50,	'13.2'=51,	'13.3'=52,	'14'=53,	'14.1'=54,	'14.2'=55,	'14.3'=56,	'15'=57,	'15.1'=58,	'15.2'=59,	'15.3'=60,	'16'=61,	'16.1'=62,	'16.2'=63,	'16.3'=64,	'17'=65,	'17.1'=66,	'17.2'=67,	'17.3'=68,	'18'=69,	'18.1'=70,	'18.2'=71,	'18.3'=72,	'19'=73,	'19.1'=74,	'19.2'=75,	'19.3'=76,	'20'=77,	'20.1'=78,	'20.2'=79,	'20.3'=80,	'21'=81,	'21.1'=82,	'21.2'=83,	'21.3'=84,	'22'=85,	'22.1'=86,	'22.2'=87,	'22.3'=88,	'23'=89,	'23.1'=90,	'23.2'=91,	'23.3'=92,	'24'=93,	'24.1'=94,	'24.2'=95,	'24.3'=96,	'25'=97,	'25.1'=98,	'25.2'=99,	'25.3'=100,	'26'=101,	'26.1'=102,	'26.2'=103,	'26.3'=104,	'27'=105,	'27.1'=106,	'27.2'=107,	'27.3'=108,	'28'=109,	'28.1'=110,	'28.2'=111,	'28.3'=112,	'29'=113,	'29.1'=114,	'29.2'=115,	'29.3'=116,	'30'=117,	'30.1'=118,	'30.2'=119,	'30.3'=120,	'31'=121,	'31.1'=122,	'31.2'=123,	'31.3'=124,	'32'=125,	'32.1'=126,	'32.2'=127,	'32.3'=128,	'33'=129,	'33.1'=130,	'33.2'=131,	'33.3'=132,	'34'=133,	'34.1'=134,	'34.2'=135,	'34.3'=136,	'35'=137,	'35.1'=138,	'35.2'=139,	'35.3'=140,	'36'=141,	'36.1'=142,	'36.2'=143,	'36.3'=144,	'37'=145,	'37.1'=146,	'37.2'=147,	'37.3'=148,	'38'=149,	'38.1'=150,	'38.2'=151,	'38.3'=152,	'39'=153,	'39.1'=154,	'39.2'=155,	'39.3'=156,	'40'=157,	'40.1'=158,	'40.2'=159,	'40.3'=160,	'41'=161,	'41.1'=162,	'41.2'=163,	'41.3'=164,	'42'=165,	'42.1'=166,	'42.2'=167,	'42.3'=168,	'43'=169,	'43.1'=170,	'43.2'=171,	'43.3'=172,	'44'=173,	'44.1'=174,	'44.2'=175,	'44.3'=176,	'45'=177,	'45.1'=178,	'45.2'=179,	'45.3'=180,	'46'=181,	'46.1'=182,	'46.2'=183,	'46.3'=184,	'47'=185,	'47.1'=186,	'47.2'=187,	'47.3'=188,	'48'=189,	'48.1'=190,	'48.2'=191,	'48.3'=192,	'49'=193,	'49.1'=194,	'49.2'=195,	'49.3'=196,	'50'=197,
            )
            #data_unl=recode(data_unl,"'1'=1;	'1.1'=2;	'1.2'=3;	'1.3'=4;	'2'=5;	'2.1'=6;	'2.2'=7;	'2.3'=8;	'3'=9;	'3.1'=10;	'3.2'=11;	'3.3'=12;	'4'=13;	'4.1'=14;	'4.2'=15;	'4.3'=16;	'5'=17;	'5.1'=18;	'5.2'=19;	'5.3'=20;	'6'=21;	'6.1'=22;	'6.2'=23;	'6.3'=24;	'7'=25;	'7.1'=26;	'7.2'=27;	'7.3'=28;	'8'=29;	'8.1'=30;	'8.2'=31;	'8.3'=32;	'9'=33;	'9.1'=34;	'9.2'=35;	'9.3'=36;	'10'=37;	'10.1'=38;	'10.2'=39;	'10.3'=40;	'11'=41;	'11.1'=42;	'11.2'=43;	'11.3'=44;	'12'=45;	'12.1'=46;	'12.2'=47;	'12.3'=48;	'13'=49;	'13.1'=50;	'13.2'=51;	'13.3'=52;	'14'=53;	'14.1'=54;	'14.2'=55;	'14.3'=56;	'15'=57;	'15.1'=58;	'15.2'=59;	'15.3'=60;	'16'=61;	'16.1'=62;	'16.2'=63;	'16.3'=64;	'17'=65;	'17.1'=66;	'17.2'=67;	'17.3'=68;	'18'=69;	'18.1'=70;	'18.2'=71;	'18.3'=72;	'19'=73;	'19.1'=74;	'19.2'=75;	'19.3'=76;	'20'=77;	'20.1'=78;	'20.2'=79;	'20.3'=80;	'21'=81;	'21.1'=82;	'21.2'=83;	'21.3'=84;	'22'=85;	'22.1'=86;	'22.2'=87;	'22.3'=88;	'23'=89;	'23.1'=90;	'23.2'=91;	'23.3'=92;	'24'=93;	'24.1'=94;	'24.2'=95;	'24.3'=96;	'25'=97;	'25.1'=98;	'25.2'=99;	'25.3'=100;	'26'=101;	'26.1'=102;	'26.2'=103;	'26.3'=104;	'27'=105;	'27.1'=106;	'27.2'=107;	'27.3'=108;	'28'=109;	'28.1'=110;	'28.2'=111;	'28.3'=112;	'29'=113;	'29.1'=114;	'29.2'=115;	'29.3'=116;	'30'=117;	'30.1'=118;	'30.2'=119;	'30.3'=120;	'31'=121;	'31.1'=122;	'31.2'=123;	'31.3'=124;	'32'=125;	'32.1'=126;	'32.2'=127;	'32.3'=128;	'33'=129;	'33.1'=130;	'33.2'=131;	'33.3'=132;	'34'=133;	'34.1'=134;	'34.2'=135;	'34.3'=136;	'35'=137;	'35.1'=138;	'35.2'=139;	'35.3'=140;	'36'=141;	'36.1'=142;	'36.2'=143;	'36.3'=144;	'37'=145;	'37.1'=146;	'37.2'=147;	'37.3'=148;	'38'=149;	'38.1'=150;	'38.2'=151;	'38.3'=152;	'39'=153;	'39.1'=154;	'39.2'=155;	'39.3'=156;	'40'=157;	'40.1'=158;	'40.2'=159;	'40.3'=160;	'41'=161;	'41.1'=162;	'41.2'=163;	'41.3'=164;	'42'=165;	'42.1'=166;	'42.2'=167;	'42.3'=168;	'43'=169;	'43.1'=170;	'43.2'=171;	'43.3'=172;	'44'=173;	'44.1'=174;	'44.2'=175;	'44.3'=176;	'45'=177;	'45.1'=178;	'45.2'=179;	'45.3'=180;	'46'=181;	'46.1'=182;	'46.2'=183;	'46.3'=184;	'47'=185;	'47.1'=186;	'47.2'=187;	'47.3'=188;	'48'=189;	'48.1'=190;	'48.2'=191;	'48.3'=192;	'49'=193;	'49.1'=194;	'49.2'=195;	'49.3'=196;	'50'=197;
            #")
            nX<-ncol(input_subj[,-c(1)])
            data_unl<-as.data.frame(matrix(data_unl,ncol = nX))
            data_def<-cbind(data_names,data_unl)
            colnames(data_def)<- names
            input_subj<-data_def
            names = c(colnames(input_subj))
            nX <- ncol(input_subj)-1
            listofdf <- lapply(1:nX, function(x) NULL)
            for (i in 1:nX) {
                listofdf[[i]] <- data.frame(input_subj[,1], input_subj[i],input_subj[i+1])
            }
            listofdf <- listofdf [-c(seq(1, 10000, by=2))]
            listofdf_subs <- lapply(1:length(listofdf), function(x) NULL)
            for (i in 1:length(listofdf)) {
                listofdf_subs[[i]] <- cbind(rep(x = 1:nrow(listofdf[[1]]),
                                                times = 2),c(listofdf[[i]][,2],listofdf[[i]][,3]))
            }
            required_vals <- rep(x = 1,
                                 times = nrow(x = listofdf_subs[[1]]))
            required_sz <- c(nrow(x = input_subj), 197)
            listofdf_accum<-lapply(1:length(listofdf), function(x) NULL)
            for (i in 1:length(listofdf)){ 
                listofdf_accum[[i]]<-pracma::accumarray(subs = listofdf_subs[[i]],
                                                        val = required_vals,
                                                        sz = required_sz)}
            data_def <- list()
            for(i in 1:length(listofdf)){
                data_def[[i]] <- listofdf_accum[[i]]
            }
            mydf_subj <- data.frame(data_def)
            values <- rep_len(x = ((0:3) / 10),
                              length.out = 197) + rep(x = 1:50,
                                                      each = 4,
                                                      length.out = 197)
            variables_ID <- paste(rep(x = names_ok,
                                      each = 197),
                                  values,
                                  sep = "_")
            colnames(mydf_subj)<-variables_ID
            mydf_subj<-mydf_subj[,-(which(colSums(mydf_subj) == 0))] 
            mydf_subj<-cbind(data_names,mydf_subj)
            #names(mydf)[names(mydf)=="input_subj$subject"] <- "Sample.Name"
            mydf_subj[mydf_subj=="2"]<-1
            row.names(mydf_subj) <- mydf_subj[,1]
            mydf_subj<-mydf_subj[,-1]
            df1_sel2_subj <- cbind(Populations,mydf_subj)
            #head(df1_sel2_subj)
            # summary(Populations)
            df1_sel2_subj[, colSums(df1_sel2_subj != 0) > 0]
            # df1_sel2_subj <<- df1_sel2_subj
            # rm(list = ls()[!ls() %in% c("Populations","df1_sel2_subj","df1_sel2")])
        }
        
        
        funct4 <- switch(input$funct3,
                         strs = data_handling_STR_test2,
                         data_handling_STR_test)
        funct4(input_subj)
        
        #data_handling_STR_test(input_subj)
        #df1_sel2_subj <- df1_subj()
    })
    
    
    # Press button to see Subjects table
    goButton2 <- eventReactive(input$subjects_button,{
        df1_sel2_subj()
    })
    
    # Print Imported table 
    output$rendered_file1_subj <- DT::renderDataTable({
        req(input$subjects_button)
        goButton2()
        #df2()
    })
    
    
    #### PCA ####
    
    # Progress bar for PCA calculation
    observeEvent(input$pca_button, {
        for (i in 1:100) {
            updateProgressBar(
                session = session,
                id = "progress_pca",
                value = i, total = 100,
                title = paste("Process", trunc(i/10))
            )
            Sys.sleep(0.05)
        }
    })
    
    # Uploaded dataset reactive subsetting through CheckboxInputGroup (id "checkpca")
    dfpca <- reactive({
        dfpca <- goButton()
    })
    
    # Selection of the factor for the colors of the plots
    output$SecondSelectPCA <- renderUI({
        req(input$file1)
        pickerInput("Factor3D", label = HTML('<h5><b>Select Factor</b></h5>'), 
                    choices = names(goButton()), selected = names(goButton()[,1]))
    })
    
    # show model PCA
    output$filexpca <- renderDataTable({ 
        data.frame(dfpca())
    })
    
    #Title
    output$captionSCREE<-renderText({
        req(input$pca_button)
        "Scree Plots"
    })
    
    # Title above Scores Plot
    output$captionB<-renderText({
        req(input$pca_button)
        "Scores & Loadings Plot"
    })
    
    # PCA function
    PCAButton <- eventReactive(input$pca_button,{
        withProgress(message = "PCA modelling", value = 0, {
            incProgress(0.2, detail = "Calculating the PCA model") #
                 
            req(input$PC_number)
            Populations<-as.factor(dfpca()[,1])
            
            inFile <- input$file2
            if (is.null(inFile)){
                dfpca <- dfpca()[,-1]
                funct7 <- switch(input$funct_logitpcaok,
                                 pca = prcomp,
                                 svd = logisticSVD,
                                 nipals = logisticPCA,
                                 logisticSVD)
                
                if(input$funct_logitpcaok == "pca"){
                    pca.model <- prcomp(dfpca, scale=TRUE, center=TRUE, rank.=input$PC_number)
                    
                } else if (input$funct_logitpcaok == "svd") {
                    library(logisticPCA)
                    pca.model = logisticSVD(dfpca, k = input$PC_number)
                } else {
                    library(logisticPCA)
                    pca.model = logisticPCA(dfpca, k = input$PC_number, m = 5)
                }
                
                #pca.model <- prcomp(dfpca, scale=TRUE, center=TRUE, rank.=input$PC_number)
            }
            else {
                df1_sel2 <- df1_sel2()
                df1_sel2_subj <- df1_sel2_subj()
                test <- c(levels(df1_sel2_subj[,1]))
                test <<- test
                train <- c(levels(df1_sel2[,1]))
                Populations <- df1_sel2[,1]
                dat<-plyr::rbind.fill(df1_sel2_subj,df1_sel2)
                dat[is.na(dat)] <- 0
                Populations<-dat[,1]
                dat<-dat[,-1]
                dat<-dat[,order(colnames(dat))]
                dat<-cbind(Populations,dat)
                dat <- dat[-1,]
                #dat <<- dat
                dfpca <- dat[,-1]
                
                if(input$funct_logitpcaok == "pca"){
                    pca.model <- prcomp(dfpca, scale=TRUE, center=TRUE, rank.=input$PC_number)
                    
                } else if (input$funct_logitpcaok == "svd") {
                    library(logisticPCA)
                    pca.model = logisticSVD(dfpca, k = input$PC_number)
                } else {
                    library(logisticPCA)
                    pca.model = logisticPCA(dfpca, k = input$PC_number, m = 5)
                }
                
                #pca.model <- prcomp(dfpca, scale=TRUE, center=TRUE, rank.=input$PC_number)
                #pca.model <<- pca.model
                Populations <- dat$Populations
            }
            
            Sys.sleep(1) #
            
            Sys.sleep(2) #
            incProgress(0.5, detail = "Preparing plots and tables") #
            
            if(input$funct_logitpcaok == "pca"){
                
            x.var <- pca.model$sdev^2
            x.pvar <- x.var/sum(x.var)*100
            x.cumsum <- cumsum(x.pvar)
            
            funct2 <- switch(input$funct,
                             strs = ncol,
                             ncol)
            
            x <- c(1:funct2(df1_sel2()[,-1]))
            data.var = data.frame(x,x.pvar,x.cumsum)
            data.var = data.var[1:ncol(pca.model$rotation),]
            pc.sel <- plot_ly(data.var, x = ~x) %>%
                layout(title = "Variance Plot",
                       xaxis = list(title = "PC number"),
                       yaxis = list(title = "Explained Variance (%)"))%>%
                add_trace(y = ~x.pvar, name = 'Explained Variance (%)',
                          type = "scatter", mode = 'lines+markers') %>%
                add_trace(y = ~x.cumsum, name = 'Cumulative Explained Variance(%)',
                          type = "scatter", mode = 'lines+markers')
            
            x <- c(1:input$PC_number)
            y <- c(data.var$x.pvar)
            text <- c(colnames(pca.model$rotation))
            data <- data.frame(x, y, text)
            scree_plot <- plot_ly(data, x = ~x, y = ~y, type = 'bar', text = text,
                                  marker = list(color = 'rgb(158,202,225)',
                                                line = list(color = 'rgb(8,48,107)',
                                                            width = 1.5))) %>%
                layout(title = "Scree Plot",
                       xaxis = list(title = "PC number"),
                       yaxis = list(title = "Explained Variance"))
            
            } else if (input$funct_logitpcaok == "svd") {
                x = seq(from = 0, to = pca.model$iters, by = 1)
                y = pca.model$loss_trace
                dat <- as.data.frame(cbind(x,y))
                pc.sel <- plot_ly(data = dat, x = ~x) %>%
                    layout(title = "Deviance as a function of iterations - Logistic SVD",
                           xaxis = list(title = "Iterations"),
                           yaxis = list(title = "Negative LogLikelihood"))%>%
                    add_trace(y = ~y, name = 'Deviance',
                              type = "scatter", mode = 'lines+markers')
                scree_plot <- pc.sel
            } else {
                x = seq(from = 0, to = pca.model$iters, by = 1)
                y = pca.model$loss_trace
                dat <- as.data.frame(cbind(x,y))
                pc.sel <- plot_ly(data = dat, x = ~x) %>%
                    layout(title = "Deviance as a function of iterations - Logistic NIPALS",
                           xaxis = list(title = "Iterations"),
                           yaxis = list(title = "Negative LogLikelihood"))%>%
                    add_trace(y = ~y, name = 'Deviance',
                              type = "scatter", mode = 'lines+markers')
                scree_plot <- pc.sel
            }
            
            if(input$funct_logitpcaok == "pca"){
            table_loadings.pca <- pca.model$rotation
            scores <- pca.model$x
            scores <- round(scores, digits = 6)
            table_scores.pca <- scores
            
            plotta <- as.data.frame(scores)
            vec_PC <- paste0(c(rep("PC", input$PC_number)),1:input$PC_number)
            colnames(plotta)=c(vec_PC)
            
            loadings <- pca.model$rotation
            loadings <- as.data.frame(loadings)
            colnames(loadings)=c(vec_PC)
            
            } else if (input$funct_logitpcaok == "svd") {
            scores<-pca.model$A
            plotta <- as.data.frame(scores)
            scores <- round(scores, digits = 6)
            table_scores.logsvd <- scores
            plotta <- as.data.frame(scores)
            vec_PC <- paste0(c(rep("PC", input$PC_number)),1:input$PC_number)
            colnames(plotta)=c(vec_PC)
                    
            loadings <- pca.model$B
            loadings <- as.data.frame(loadings)
            colnames(loadings)=c(vec_PC)
            
            } else {
            scores<-pca.model$PCs
            plotta <- as.data.frame(scores)
            scores <- round(scores, digits = 6)
            table_scores.logsvd <- scores
            plotta <- as.data.frame(scores)
            vec_PC <- paste0(c(rep("PC", input$PC_number)),1:input$PC_number)
            colnames(plotta)=c(vec_PC)
                    
            loadings <- pca.model$U
            loadings <- as.data.frame(loadings)
            colnames(loadings)=c(vec_PC)
            }
            
            incProgress(0.3, detail = "Done")
            
            list(pca.model = pca.model, pc.sel = pc.sel, scree_plot = scree_plot,
                 plotta = plotta, Populations = Populations, vec_PC = vec_PC,
                 loadings = loadings, dist = dist)
        })
    })
    
    # Calculation of variance of PCA    
    output$HistPCA <- renderPlotly({
        PCAButton()$pc.sel
    })
    
    # Calculation of histograms of PCA    
    output$screeplot_PCA <- renderPlotly({
        PCAButton()$scree_plot
    })
    
    # Selection of PCA_axis_1
    output$PCA_axis_1 <- renderUI({
        req(input$pca_button)
        choice = c("Samples",PCAButton()$vec_PC)
        pickerInput("PCA_axis_1", label = HTML('<h5><b>Select X axis</b></h5>'), 
                    choices = choice, selected = choice[2],options = list(`style` = "btn-info"))
    })
    
    # Selection of PCA_axis_2
    output$PCA_axis_2 <- renderUI({
        req(input$pca_button)
        choice = c("Samples",PCAButton()$vec_PC)
        pickerInput("PCA_axis_2", label = HTML('<h5><b>Select Y axis</b></h5>'), 
                    choices = choice, selected = choice[3],options = list(`style` = "btn-info"))
    })
    
    # Selection of PCA_axis_3
    output$PCA_axis_3 <- renderUI({
        req(input$pca_button)
        choice = c("Samples",PCAButton()$vec_PC)
        pickerInput("PCA_axis_3", label = HTML('<h5><b>Select Z axis</b></h5>'), 
                    choices = choice, selected = choice[4],options = list(`style` = "btn-info"))
    })
    
    # Preparation of PCA scores plot
    output$scores_2D <- renderPlotly({
        plotta <- PCAButton()$plotta
        plotta$Samples <- seq_len(nrow(plotta))
        Populations <- PCAButton()$Populations
        
        ax <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PCA_axis_1, zerolinewidth = 2
        )
        
        ay <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PCA_axis_2, zerolinewidth = 2
        )
        
        plot_ly(plotta, x = plotta[,input$PCA_axis_1], y = plotta[,input$PCA_axis_2],
                              type = "scatter", mode = 'markers',
                              color = Populations, colors = cbp1,
                              symbol = Populations,
                              opacity = 0.7,
                              symbols = c('circle','square','diamond','x','triangle-up',
                                          'triangle-down','triangle-left','triangle-right',
                                          'pentagon'),
                              marker = list(size = 10),
                              text = ~paste("Subject: ", rownames(plotta))) %>%
            layout(title = "Scores Plot",xaxis = ax, yaxis = ay)
        
    })
    
    # Preparation of PCA loadings plot
    output$loadings_2D <- renderPlotly({
        loadings <- PCAButton()$loadings
        loadings$Samples <- seq_len(nrow(loadings))
        dist <- PCAButton()$dist
        
        ax <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PCA_axis_1, zerolinewidth = 2
        )
        
        ay <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PCA_axis_2, zerolinewidth = 2
        )
        
        plot_ly(loadings, x = loadings[,input$PCA_axis_1], y = loadings[,input$PCA_axis_2],
                #color = dist, size = dist, colors = "Reds",
                color = "red", size = 3,
                type = "scatter",
                mode = "markers",
                text = ~paste("Variable: ", rownames(loadings))) %>%
            layout(title = "Loadings Plot",
                   xaxis = ax, yaxis = ay)
    })
    
    # Preparation of PCA scores plot 3D scatterplot
    output$scores_3D <- renderPlotly({
        plotta <- PCAButton()$plotta
        plotta$Samples <- seq_len(nrow(plotta))
        Populations <- PCAButton()$Populations
        
        ax <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks", 
            title = input$PCA_axis_1, zerolinewidth = 2
        )
        
        ay <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PCA_axis_2, zerolinewidth = 2
        )
        
        az <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PCA_axis_3, zerolinewidth = 2
        )
        
        plot_ly(plotta, x = plotta[,input$PCA_axis_1], y = plotta[,input$PCA_axis_2], 
                z = plotta[,input$PCA_axis_3],
                type = 'scatter3d',
                mode = 'markers',
                color = ~Populations,
                colors = cbp1,
                opacity = 0.3,
                symbol = ~Populations,
                symbols = c('circle','square','diamond','x','triangle-up',
                            'triangle-down','triangle-left','triangle-right',
                            'pentagon'),
                text = ~paste('Subject: ', rownames(plotta)))%>%
            layout(scene = list(xaxis=ax,yaxis=ay,zaxis=az))
        
    })
    
    #### PLS-DA ####
    
    # Progress bar for PLSDA calculation
    observeEvent(input$plsda_button, {
        for (i in 1:100) {
            updateProgressBar(
                session = session,
                id = "progress_plsda",
                value = i, total = 100,
                title = paste("Process", trunc(i/10))
            )
            Sys.sleep(0.05)
        }
    })
    
    # Uploaded dataset reactive subsetting through CheckboxInputGroup (id "checkpca")
    dfplsda <- reactive({
        dfplsda <- goButton()
    })
    
    # Selection of the factor for the colors of the plots
    output$SecondSelectPLSDA <- renderUI({
        req(input$file1)
        pickerInput("Factor3D", label = HTML('<h5><b>Select Factor</b></h5>'), 
                    choices = names(goButton()), selected = names(goButton()[,1]))
    })
    
    # show model PLSDA
    output$filexplsda <- renderDataTable({ 
        data.frame(dfplsda())
    })
    
    #Title
    output$captionSCREE_PLSDA<-renderText({
        req(input$plsda_button)
        "Scree Plots"
    })
    
    # Title above Scores Plot
    output$captionB_PLSDA<-renderText({
        req(input$plsda_button)
        "Scores & Loadings Plot"
    })
    
    # PLSDA function
    PLSDAButton <- eventReactive(input$plsda_button,{
        withProgress(message = "PLSDA modelling", value = 0, {
            incProgress(0.2, detail = "Calculating the PLSDA model") #
            #     
            req(input$LV_number)
            Populations<-as.factor(dfplsda()[,1])
            #dfplsda <- dfplsda()[,-1]
            
            inFile <- input$file2
            if (is.null(inFile)){
                Populations<-as.factor(dfplsda()[,1])
                dfplsda <- dfplsda()[,-1]
                X <- dfplsda
                Y <- as.factor(dfplsda()[,1])
                
                funct6 <- switch(input$funct_sparseplsda,
                                 plsda = mixOmics::plsda,
                                 splsda = mixOmics::splsda,
                                 mixOmics::plsda)
                
                if(input$funct_sparseplsda == "plsda"){
                    plsda.model = mixOmics::plsda(X, Y, ncomp = input$LV_number)
                } else {
                    list.keepX <- c(seq(10, 50, 10))
                    tune.splsda <- mixOmics::tune.splsda(X, Y, ncomp = input$LV_number, 
                                               validation = 'Mfold', folds = 5, 
                                               progressBar = TRUE, dist = 'max.dist',
                                               test.keepX = list.keepX, nrepeat = 10) 
                    choice.keepX <- tune.splsda$choice.keepX[1:input$LV_number]
                    plsda.model <- mixOmics::splsda(X, Y, ncomp = input$LV_number, keepX = choice.keepX)
                }
                
            }
            else {
                df1_sel2 <- df1_sel2()
                df1_sel2_subj <- df1_sel2_subj()
                Populations <- df1_sel2[,1]
                dat<-plyr::rbind.fill(df1_sel2_subj,df1_sel2)
                Populations<-dat[,1]
                dat<-dat[,-1]
                dat<-dat[,order(colnames(dat))]
                dat<-cbind(Populations,dat)
                df1_sel2_subj<-dat[seq_len(dim(df1_sel2_subj)[1]),]
                df1_sel2_subj[is.na(df1_sel2_subj)] <- 0
                df1_sel2<-dat[-seq_len(dim(df1_sel2_subj)[1]),]
                df1_sel2[is.na(df1_sel2)] <- 1
                Populations<-as.factor(df1_sel2[,1])
                dfplsda <- df1_sel2[,-1]
                X <- dfplsda
                Y <- as.factor(df1_sel2[,1])
                library(mixOmics)
                
                funct6 <- switch(input$funct_sparseplsda,
                                 plsda = mixOmics::plsda,
                                 splsda = mixOmics::splsda,
                                 mixOmics::plsda)
                
                if(input$funct_sparseplsda == "plsda"){
                    plsda.model = mixOmics::plsda(X, Y, ncomp = input$LV_number)
                } else {
                    list.keepX <- c(seq(10, 50, 10))
                    tune.splsda <- tune.splsda(X, Y, ncomp = input$LV_number, 
                                               validation = 'Mfold', folds = 5, 
                                               progressBar = TRUE, dist = 'max.dist',
                                               test.keepX = list.keepX, nrepeat = 10) 
                    choice.keepX <- tune.splsda$choice.keepX[1:input$LV_number]
                    plsda.model <- splsda(X, Y, ncomp = input$LV_number, keepX = choice.keepX)
                }
                
            }
            
            
            Sys.sleep(1) #
            
            scores<-plsda.model$variates$X
            plotta <- as.data.frame(scores)
            prefix = "LV"
            n = input$LV_number
            suffix = seq(1:n)
            mynames = paste(prefix,suffix,sep="")
            colnames(plotta)=c(mynames)
            library(ggplot2)
            library(plotly)
            cbp1 <- c("#E69F00", "#56B4E9", "#009E73","#D55E00", "#CC79A7",
                      "#999999", "#F0E442", "#0072B2") #palette di colori
            
            x.var <- plsda.model$explained_variance$X
            x.pvar <- x.var/sum(x.var)*100
            x.cumsum <- cumsum(x.pvar)
            
            funct2 <- switch(input$funct,
                             strs = ncol,
                             ncol)
            
            x <- c(1:ncol(plsda.model$loadings$X))
            data.var = data.frame(x,x.pvar,x.cumsum)
            data.var = data.var[1:ncol(as.data.frame(plsda.model$loadings$X)),]
            lv.sel_PLSDA <- plot_ly(data.var, x = ~x) %>%
                layout(title = "Variance Plot",
                       xaxis = list(title = "LV number"),
                       yaxis = list(title = "Explained Variance (%)"))%>%
                add_trace(y = ~x.pvar, name = 'Explained Variance (%)',
                          type = "scatter", mode = 'lines+markers') %>%
                add_trace(y = ~x.cumsum, name = 'Cumulative Explained Variance(%)',
                          type = "scatter", mode = 'lines+markers')

            x <- c(1:input$LV_number)
            y <- c(data.var$x.pvar)
            text <- c(colnames(plsda.model$loadings$X))
            data <- data.frame(x, y, text)
            
            scree_plot_PLSDA <- plot_ly(data, x = ~x, y = ~y, type = 'bar', text = text,
                                        marker = list(color = 'rgb(158,202,225)',
                                                      line = list(color = 'rgb(8,48,107)',
                                                                  width = 1.5))) %>%
                layout(title = "Scree Plot",
                       xaxis = list(title = "LV number"),
                       yaxis = list(title = "Explained Variance"))
            
            vec_LV <- paste0(c(rep("LV", input$LV_number)),1:input$LV_number)
            ax <- list(
                zeroline = TRUE,showline = TRUE,
                mirror = "ticks",zerolinewidth = 2
            )
            
            loadings <- plsda.model$loadings$X
            loadings <- as.data.frame(loadings)
            colnames(loadings)=vec_LV
            eucl <- loadings[,1:2]
            eucl_0 <- eucl*0
            euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
            dist <- NULL
            for(i in 1:nrow(eucl)) dist[i] <- euc.dist(eucl[i,],eucl_0[i,])
            
            Sys.sleep(2) #
            incProgress(0.5, detail = "Preparing plots and tables") #
            
            
            incProgress(0.3, detail = "Done")
            
            list(lv.sel_PLSDA = lv.sel_PLSDA, loadings = loadings,
                 scree_plot_PLSDA = scree_plot_PLSDA, plsda.model = plsda.model, plotta = plotta, X = X,
                 Populations = Populations, vec_LV = vec_LV)
        })
    })
    
    # Calculation of variance of PLS-DA
    output$HistPLSDA <- renderPlotly({
        PLSDAButton()$lv.sel_PLSDA
    })
    # 
    # Calculation of histograms of PLS-DA
    output$screeplot_PLSDA <- renderPlotly({
        PLSDAButton()$scree_plot_PLSDA
    })
    
    # Selection of PLSDA_axis_1
    output$PLSDA_axis_1 <- renderUI({
        req(input$plsda_button)
        choice = c("Samples",PLSDAButton()$vec_LV)
        pickerInput("PLSDA_axis_1", label = HTML('<h5><b>Select X axis</b></h5>'), 
                    choices = choice, selected = choice[2],options = list(`style` = "btn-info"))
    })
    
    # Selection of PLSDA_axis_2
    output$PLSDA_axis_2 <- renderUI({
        req(input$plsda_button)
        choice = c("Samples",PLSDAButton()$vec_LV)
        pickerInput("PLSDA_axis_2", label = HTML('<h5><b>Select Y axis</b></h5>'), 
                    choices = choice, selected = choice[3],options = list(`style` = "btn-info"))
    })
    
    # Selection of PLSDA_axis_3
    output$PLSDA_axis_3 <- renderUI({
        req(input$plsda_button)
        choice = c("Samples",PLSDAButton()$vec_LV)
        pickerInput("PLSDA_axis_3", label = HTML('<h5><b>Select Z axis</b></h5>'), 
                    choices = choice, selected = choice[4],options = list(`style` = "btn-info"))
    })
    
    
    # Preparation of PLS-DA scores plot
    output$scores_2D_PLSDA <- renderPlotly({
        plotta <- PLSDAButton()$plotta
        plotta$Samples <- seq_len(nrow(plotta))
        Populations <- PLSDAButton()$Populations
        
        ax <- list(
            zeroline = TRUE, showline = TRUE, title = input$PLSDA_axis_1,
            mirror = "ticks", zerolinewidth = 2
        )
        
        ay <- list(
            zeroline = TRUE, showline = TRUE, title = input$PLSDA_axis_2,
            mirror = "ticks", zerolinewidth = 2
        )

        plsda_mod_train <- plot_ly(plotta, x = plotta[,input$PLSDA_axis_1], y = plotta[,input$PLSDA_axis_2],
                                   type = "scatter",
                                   mode = 'markers',
                                   color = Populations,
                                   colors = cbp1,
                                   opacity = 0.7,
                                   symbol = Populations,
                                   symbols = c('circle','square','diamond','x','triangle-up',
                                               'triangle-down','triangle-left','triangle-right',
                                               'pentagon'),
                                   marker = list(size = 10),
                                   text = ~paste("Subject: ", rownames(plotta))) %>%
                                   layout(title = "Scores Plot", xaxis = ax, yaxis = ay)
    })
    
    # Preparation of PCA loadings plot
    output$loadings_2D_PLSDA <- renderPlotly({
        loadings <- PLSDAButton()$loadings
        loadings$Samples <- seq_len(nrow(loadings))
        
        ax <- list(
            zeroline = TRUE, showline = TRUE, title = input$PLSDA_axis_1,
            mirror = "ticks", zerolinewidth = 2
        )
        
        ay <- list(
            zeroline = TRUE, showline = TRUE, title = input$PLSDA_axis_2,
            mirror = "ticks", zerolinewidth = 2
        )
        
        plot_ly(loadings, x = loadings[,input$PLSDA_axis_1], y = loadings[,input$PLSDA_axis_2],
                color = "red",
                type = "scatter",
                mode = "markers",
                size = 3,
                text = ~paste("Variable: ", rownames(loadings))) %>%
            layout(title = "Loadings Plot", xaxis = ax, yaxis = ay)
    })
    # 
    # Preparation of PCA scores plot 3D scatterplot
    output$scores_3D_PLSDA <- renderPlotly({
        plotta <- PLSDAButton()$plotta
        plotta$Samples <- seq_len(nrow(plotta))
        Populations <- PLSDAButton()$Populations

        ax <- list(
            zeroline = TRUE, showline = TRUE, title = input$PLSDA_axis_1,
            mirror = "ticks", zerolinewidth = 2
        )

        ay <- list(
            zeroline = TRUE, showline = TRUE, title = input$PLSDA_axis_2,
            mirror = "ticks", zerolinewidth = 2
        )

        az <- list(
            zeroline = TRUE, showline = TRUE, title = input$PLSDA_axis_3,
            mirror = "ticks", zerolinewidth = 2
        )

        plsda_mod_train <- plot_ly(plotta, x = plotta[,input$PLSDA_axis_1], y = plotta[,input$PLSDA_axis_2],
                                   z = plotta[,input$PLSDA_axis_3],
                                   type = "scatter3d",
                                   mode = 'markers',
                                   color = Populations,
                                   colors = cbp1,
                                   opacity = 0.3,
                                   symbol = Populations,
                                   symbols = c('circle','square','diamond','x','triangle-up',
                                               'triangle-down','triangle-left','triangle-right',
                                               'pentagon'),
                                   marker = list(size = 10),
                                   text = ~paste("Subject: ", rownames(plotta))) %>%
                                   layout(scene=list(xaxis = ax, yaxis = ay, zaxis = az))
    })
    
    #### SVM ####
    
    # Progress bar for SVM calculation
    observeEvent(input$svm_button, {
        for (i in 1:100) {
            updateProgressBar(
                session = session,
                id = "progress_svm",
                value = i, total = 100,
                title = paste("Process", trunc(i/10))
            )
            Sys.sleep(0.05)
        }
    })
    
    # Uploaded dataset reactive subsetting through CheckboxInputGroup (id "checkpca")
    dfsvm <- reactive({
        dfsvm <- goButton()
    })
    
    # Selection of the factor for the colors of the plots
    output$SecondSelectSVM <- renderUI({
        req(input$file1)
        pickerInput("Factor3D", label = HTML('<h5><b>Select Factor</b></h5>'), 
                    choices = names(goButton()), selected = names(goButton()[,1]))
    })
    
    # show model PLSDA
    output$filexplsvm <- renderDataTable({ 
        data.frame(dfsvm())
    })
    
    #Title
    output$captionSCREE_SVM<-renderText({
        req(input$svm_button)
        "Scree Plots"
    })
    
    # Title above Scores Plot
    output$captionB_SVM<-renderText({
        req(input$svm_button)
        "Support Vector Machines (SVM) results"
    })
    
    # SVM function
    SVMButton <- eventReactive(input$svm_button,{
        withProgress(message = "SVM modelling", value = 0, {
            incProgress(0.2, detail = "Calculating the SVM model") #
            #     
            library(pryr)
            library(tibble)
            library(tidyr)
            library(tidyverse)
            library(gridExtra)
            library(grid)
            
            
            Sys.sleep(1) #
            
            inFile <- input$file2
            if (is.null(inFile)){
                
                funct8 <- switch(input$funct_SVM,
                                 svm = svm,
                                 svmpca = ksvm,
                                 svm)
                
                Populations<-as.factor(dfsvm()[,1])
                y <- dfsvm()[,1]
                x <- dfsvm()[,-1]
                data<-cbind(y,x)
                data_svm = data
                
                if(input$funct_SVM == "svm"){
                    svm_lin1=svm(y~.,data=data_svm, gamma = 0.001,
                                 cost = 10)
                    res=round(prop.table(table(data$y,svm_lin1$fitted),
                                         margin=2),3)*100
                    res = unclass(res)
                    perc=mean(svm_lin1$fitted == data$y)*100
                    res <- unclass(res)
                    a %<a-% {
                        res %>%
                            as.data.frame() %>%
                            rownames_to_column() %>% 
                            gather(column, value, -rowname) %>% 
                            arrange(column) %>% 
                            ggplot(aes(x = "", y = value, fill = rowname)) +
                            geom_col() +
                            geom_text(aes(label = paste0(round(value,2), "%")), 
                                      position = position_stack(vjust = 0.5)) +
                            labs(x = NULL, y = NULL, fill = "Populations", 
                                 title = "Pie charts for SVM calculations") +
                            coord_polar("y", start = 0) + 
                            theme_classic() +
                            theme(axis.line = element_blank(),
                                  axis.text = element_blank(),
                                  axis.ticks = element_blank(),
                                  plot.title = element_text(hjust = 0.5, color = "black"))+
                            facet_wrap(~column)
                    }
                    
                } else {
                    #req(input$pca_button)
                    library(kernlab)
                    library(RColorBrewer)
                    library(e1071)
                    Populations<-dfsvm()[,1]
                    y<-as.data.frame(dfsvm()[,1])
                    colnames(y)<-"y"
                    x<-PCAButton()$plotta
                    plotta<-x
                    dat<-cbind(y,x)
                    
                    svm_lin1=svm(y~.,data=dat, gamma = 0.001,cost = 10)
                    res=round(prop.table(table(dat$y,svm_lin1$fitted),
                                         margin=2),3)*100
                    res = unclass(res)
                    perc=mean(svm_lin1$fitted == dat$y)*100
                    
                    
                    library(pryr)
                    library(tibble)
                    library(tidyr)
                    library(tidyverse)
                    library(gridExtra)
                    library(grid)
                    a %<a-% {
                        res %>%
                            as.data.frame() %>%
                            rownames_to_column() %>% 
                            gather(column, value, -rowname) %>% 
                            arrange(column) %>% 
                            ggplot(aes(x = "", y = value, fill = rowname)) +
                            geom_col() +
                            geom_text(aes(label = paste0(round(value,2), "%")), 
                                      position = position_stack(vjust = 0.5)) +
                            labs(x = NULL, y = NULL, fill = "Populations", 
                                 title = "Pie charts for SVM on PCA calculations") +
                            coord_polar("y", start = 0) + 
                            theme_classic() +
                            theme(axis.line = element_blank(),
                                  axis.text = element_blank(),
                                  axis.ticks = element_blank(),
                                  plot.title = element_text(hjust = 0.5, color = "black"))+
                            facet_wrap(~column)
                    }
                    
                }
                
            } else {
                
                
                funct8 <- switch(input$funct_SVM,
                                 svm = svm,
                                 svmpca = ksvm,
                                 svm)
                
                if(input$funct_SVM == "svm"){
                    df1_sel2 <- df1_sel2()
                    df1_sel2_subj <- df1_sel2_subj()
                    Populations <- df1_sel2[,1]
                    dat<-plyr::rbind.fill(df1_sel2_subj,df1_sel2)
                    Populations<-dat[,1]
                    dat<-dat[,-1]
                    dat<-dat[,order(colnames(dat))]
                    dat<-cbind(Populations,dat)
                    df1_sel2_subj<-dat[seq_len(dim(df1_sel2_subj)[1]),]
                    df1_sel2_subj[is.na(df1_sel2_subj)] <- 0
                    df1_sel2<-dat[-seq_len(dim(df1_sel2_subj)[1]),]
                    df1_sel2[is.na(df1_sel2)] <- 1
                    
                    
                    Populations<-as.factor(df1_sel2[,1])
                    y <- as.factor(df1_sel2[,1])
                    x <- df1_sel2[,-1]
                    data<-cbind(y,x)
                    data_svm = data
                    svm_lin1=svm(y~.,data=data_svm, gamma = 0.001,
                                 cost = 10)
                    res=round(prop.table(table(data$y,svm_lin1$fitted),
                                         margin=2),3)*100
                    res = unclass(res)
                    perc=mean(svm_lin1$fitted == data$y)*100
                    res <- unclass(res)
                    a %<a-% {
                        res %>%
                            as.data.frame() %>%
                            rownames_to_column() %>% 
                            gather(column, value, -rowname) %>% 
                            arrange(column) %>% 
                            ggplot(aes(x = "", y = value, fill = rowname)) +
                            geom_col() +
                            geom_text(aes(label = paste0(round(value,2), "%")), 
                                      position = position_stack(vjust = 0.5)) +
                            labs(x = NULL, y = NULL, fill = "Populations", 
                                 title = "Pie charts for SVM calculations") +
                            coord_polar("y", start = 0) + 
                            theme_classic() +
                            theme(axis.line = element_blank(),
                                  axis.text = element_blank(),
                                  axis.ticks = element_blank(),
                                  plot.title = element_text(hjust = 0.5, color = "black"))+
                            facet_wrap(~column)
                    }
                    
                } else {
                    library(kernlab)
                    library(RColorBrewer)
                    library(e1071)
                    Populations<-dfsvm()[,1]
                    y<-as.data.frame(dfsvm()[,1])
                    colnames(y)<-"y"
                    x<-PCAButton()$plotta
                    plotta<-x
                    dat<-cbind(y,x)
                    
                    svm_lin1=svm(y~.,data=dat, gamma = 0.001,
                                 cost = 10)
                    res=round(prop.table(table(dat$y,svm_lin1$fitted),
                                         margin=2),3)*100
                    res = unclass(res)
                    perc=mean(svm_lin1$fitted == dat$y)*100
                    
                    library(pryr)
                    library(tibble)
                    library(tidyr)
                    library(tidyverse)
                    library(gridExtra)
                    library(grid)
                    a %<a-% {
                        res %>%
                            as.data.frame() %>%
                            rownames_to_column() %>% 
                            gather(column, value, -rowname) %>% 
                            arrange(column) %>% 
                            ggplot(aes(x = "", y = value, fill = rowname)) +
                            geom_col() +
                            geom_text(aes(label = paste0(round(value,2), "%")), 
                                      position = position_stack(vjust = 0.5)) +
                            labs(x = NULL, y = NULL, fill = "Populations", 
                                 title = "Pie charts for SVM on PCA calculations") +
                            coord_polar("y", start = 0) + 
                            theme_classic() +
                            theme(axis.line = element_blank(),
                                  axis.text = element_blank(),
                                  axis.ticks = element_blank(),
                                  plot.title = element_text(hjust = 0.5, color = "black"))+
                            facet_wrap(~column)
                    }
                    
                }
                
                
            }
            
            Sys.sleep(2) #
            incProgress(0.5, detail = "Preparing plots and tables") #
            
            
            incProgress(0.3, detail = "Done")
            
            list(a = a, svm_lin1 = svm_lin1)
        })
    })
    
    # Preparation of SVM plot of the results
    output$results_SVM <- renderPlot({
              SVMButton()$a
    })
    
    
    #### Prediction on PCA ####
    # Predict PCA function
    observeEvent(input$prediction_pca_button, {
        for (i in 1:100) {
            updateProgressBar(
                session = session,
                id = "progress_pca",
                value = i, total = 100,
                title = paste("Process", trunc(i/10))
            )
            Sys.sleep(0.05)
        }
    })
    
    Predict_PCAButton <- eventReactive(input$prediction_pca_button,{
        withProgress(message = "LR on PCA calculation", value = 0, {
            incProgress(0.2, detail = "Preparing the data for LR calculation") #
            
            req(input$PC_number)
            
            pca.model <- PCAButton()$pca.model
            
            df1_sel2 <- df1_sel2()
            df1_sel2_subj <- df1_sel2_subj()
            Populations <- df1_sel2[,1]
            dat<-plyr::rbind.fill(df1_sel2_subj,df1_sel2)
            Populations<-dat[,1]
            dat<-dat[,-1]
            dat<-dat[,order(colnames(dat))]
            dat<-cbind(Populations,dat)
            df1_sel2_subj<-dat[seq_len(dim(df1_sel2_subj)[1]),]
            df1_sel2_subj[is.na(df1_sel2_subj)] <- 0
            df1_sel2<-dat[-seq_len(dim(df1_sel2_subj)[1]),]
            df1_sel2[is.na(df1_sel2)] <- 1
            
            
            Sys.sleep(1) #
            pred <- as.data.frame(predict(pca.model, newdata=df1_sel2_subj[,-1]))
            
            if(input$funct_logitpcaok == "pca"){
                scores <- pca.model$x
                plotta <- as.data.frame(scores)
                scores <- round(scores, digits = 6)
                scores <<- scores
                plotta <- as.data.frame(scores)
                vec_PC <- paste0(c(rep("PC", input$PC_number)),1:input$PC_number)
                colnames(plotta)=c(vec_PC)
                
            } else if (input$funct_logitpcaok == "svd"){
                scores <- pca.model$A
                plotta <- as.data.frame(scores)
                scores <- round(scores, digits = 6)
                plotta <- as.data.frame(scores)
                vec_PC <- paste0(c(rep("PC", input$PC_number)),1:input$PC_number)
                colnames(plotta)=c(vec_PC)
            } else {
                scores <- pca.model$PCs
                plotta <- as.data.frame(scores)
                scores <- round(scores, digits = 6)
                plotta <- as.data.frame(scores)
                vec_PC <- paste0(c(rep("PC", input$PC_number)),1:input$PC_number)
                colnames(plotta)=c(vec_PC)
            }
            
            
            
            Sys.sleep(2) #
            incProgress(0.5, detail = "Preparing plots and tables") #
            
            colnames(pred) <- c(vec_PC)
            Populations <- as.character(dat[-seq_len(dim(df1_sel2_subj)[1]),1])
            
            dat<-cbind(plotta,Populations)
            Set<- rep("Model_data",nrow(dat))
            dat<- as.data.frame(cbind(dat,Set))
            dat_pca <- dat
            
            df<-pred
            populations<-as.character(df1_sel2_subj[,1])
            df$Populations<-populations
            Set <- rep("New_Samples",nrow(df))
            df <- as.data.frame(cbind(df,Set))
            dat_pca_pred<-df
            dat_test <- dat_pca_pred
            
            prova <- cbind(as.numeric(dat_pca$PC1),as.numeric(dat_pca$PC2))
            colnames(prova) = c("PC1","PC2")
            prova <- as.data.frame(prova)
            prova2 <- cbind(as.numeric(dat_test$PC1),as.numeric(dat_test$PC2))
            colnames(prova2) = c("PC1","PC2")
            rownames(prova2) = df1_sel2_subj[,1]
            library(ggplot2)
            plot_pca_pred <- ggplot(plotta,aes(x=plotta[,1],y=plotta[,2])) +
                geom_point(aes(color = factor(dat_pca$Populations)),
                           size=2.5) +
                labs(title="Logistic PCA model on unknowns", x="PC1", y = "PC2",
                     color = "Tested Populations")+
                geom_point(data=prova2, aes(x=prova2[,1],y=prova2[,2]),
                           colour="orange",size=8,shape=17) +
                geom_hline(aes(yintercept=0), linetype="dashed") +
                geom_vline(aes(xintercept=0), linetype="dashed") +
                theme_classic()+
                theme(axis.title=element_text(size=12,face="bold"),
                      plot.title = element_text(color = "red", size = 14,
                                                face = "bold",hjust = 0.5),
                      legend.title = element_text(colour="black", size=14, face="bold"),
                      legend.text = element_text(colour="black", size=10, face="bold"),
                      legend.background = element_rect(size=0.5, linetype="solid",colour ="black"))
            
            
            dat_pca$Populations = as.character(dat_pca$Populations)
            dat_pca<-subset(dat_pca, select=-c(Set))
            dat_pca_pred<-subset(dat_pca_pred, select=-c(Set))
            pops=c(levels(as.factor(dat_pca$Populations)))
            dat_test = dat_pca_pred
            
            allDf<-list()
            for (i in 1:nlevels(as.factor(dat_pca$Populations))) {
                allDf[[i]]<-assign(paste0("dat_pca","_",pops[i]), dat_pca[dat_pca$Populations==pops[i],])
            }
            options(stringsAsFactors = F)
            
            nDf = length(allDf)
            allComparisons = lapply(1:nDf, function(x){
                #Merge all df, but change the order of the first one in the list
                mergeDf = do.call(rbind, allDf[c(x, c(1:nDf)[-x])])
                #Set the new factor names, only keeing the names for the first one
                mergeDf$Populations = as.factor(c(allDf[[x]]$Populations, rep("Other_population_datasets", nrow(dat_pca)-nrow(allDf[[x]]))))
                mergeDf$Factor<-mergeDf$Populations
                mergeDf
            })
            allComparisons = lapply(allComparisons, function(x) {
                x["Populations"] <- NULL;
                x["Item"] <- seq(1,nrow(dat_pca),1);
                x["Piece"] <- rep(1,nrow(dat_pca));
                x
            })
            
            library(plyr); library(dplyr)
            
            allComparisons = lapply(allComparisons, function(x) {
                x %>%
                    dplyr::select(Factor, Item, Piece, everything())
            })
            
            dat_pca_pred$Name <- dat_pca_pred$Populations
            dat_pca_pred$Populations <- NULL
            dat_pca_pred$Item <- seq(1,nrow(dat_pca_pred),1)
            dat_pca_pred$Piece <- rep(1,nrow(dat_pca_pred))
            dat_pca_pred = dat_pca_pred %>%
                dplyr::select(Name, Item, Piece, everything())
            
            library(KernSmooth)
            
            vec_PC <- paste0(c(rep("PC", input$PC_number)),1:input$PC_number)
            cols.num <- c(vec_PC)
            data = dat_pca_pred
            Name = as.factor(data$Name)
            data = as.data.frame(subset(data, select=-c(Name)))
            data = as.data.frame(cbind(Name,data))
            
            population = allComparisons
            vec_PC <- paste0(c(rep("PC", input$PC_number)),1:input$PC_number)
            cols.num <- c(vec_PC)
            a = nlevels(as.factor(dat_pca$Populations))
            for (i in 1:a){
                population[[i]][cols.num] <- sapply(population[[i]][cols.num],as.numeric)
            }
            
            allComparisons = population
            dat_pca_pred = data
            
            incProgress(0.3, detail = "Done")
            
            list(pred = pred, plot_pca_pred = plot_pca_pred, df1_sel2_subj = df1_sel2_subj,
                 allComparisons = allComparisons, vec_PC = vec_PC,
                 dat_pca_pred = dat_pca_pred, dat_pca = dat_pca, dat_test = dat_test
            )
        })
    })
    
    # Selection of PCA_axis_1_test
    output$PCA_axis_1_test <- renderUI({
        req(input$prediction_pca_button)
        choice = c("Samples",Predict_PCAButton()$vec_PC)
        pickerInput("PCA_axis_1_test", label = HTML('<h5><b>Select X axis</b></h5>'), 
                    choices = choice, selected = choice[2],options = list(`style` = "btn-info"))
    })
    
    # Selection of PCA_axis_2_test
    output$PCA_axis_2_test <- renderUI({
        req(input$prediction_pca_button)
        choice = c("Samples",Predict_PCAButton()$vec_PC)
        pickerInput("PCA_axis_2_test", label = HTML('<h5><b>Select Y axis</b></h5>'), 
                    choices = choice, selected = choice[3],options = list(`style` = "btn-info"))
    })
    
    # Selection of PCA_axis_2_test
    output$PCA_axis_3_test <- renderUI({
        req(input$prediction_pca_button)
        choice = c("Samples",Predict_PCAButton()$vec_PC)
        pickerInput("PCA_axis_3_test", label = HTML('<h5><b>Select Z axis</b></h5>'), 
                    choices = choice, selected = choice[4],options = list(`style` = "btn-info"))
    })
    
    # Plot of PCA scores plot 2D on unknowns
    output$scores_2D_PCA_prediction <- renderPlotly({
        dat_pca <- Predict_PCAButton()$dat_pca
        dat_pca$Samples <- seq_len(nrow(dat_pca))
        dat_test <- Predict_PCAButton()$dat_test
        dat_test$Samples <- seq_len(nrow(dat_test))
        
        ax <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PCA_axis_1_test, zerolinewidth = 2
        )
        
        ay <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PCA_axis_2_test, zerolinewidth = 2
        )
        
        title_font <- list(
            family = "Arial", size = 18, color = 'black')
        
        plot_ly(dat_pca, x = dat_pca[,input$PCA_axis_1_test], y = dat_pca[,input$PCA_axis_2_test],
                               type = 'scatter',
                               mode = 'markers',
                               color = as.factor(dat_pca$Populations),
                               colors = cbp1,
                               opacity = 0.7,
                               symbol = as.factor(dat_pca$Populations),
                               symbols = c('circle','square','diamond','x','triangle-up',
                                           'triangle-down','triangle-left','triangle-right',
                                           'pentagon'),
                               text = ~paste('Subject: ', rownames(dat_pca))) %>%
            layout(title = "PCA Scores Plot on New Individuals",
                   titlefont = title_font)%>%
            add_trace(x = dat_test[,input$PCA_axis_1_test], y = dat_test[,input$PCA_axis_2_test],
                      type = 'scatter',
                      mode = 'markers',
                      color = rownames(dat_test$Populations),
                      colors = cbp1,
                      opacity = 1,
                      marker = list(size = 15),
                      symbol = as.factor(dat_test$Populations),
                      symbols = c('circle','square','diamond','x','triangle-up',
                                  'triangle-down','triangle-left','triangle-right',
                                  'pentagon'),
                      text = ~paste('Subject: ', dat_test$Populations))%>%
            layout(title = "PCA Scores Plot on New Individuals",xaxis = ax, yaxis = ay)
    })
    # 
    # Plot of PCA scores plot 3D on unknowns
    output$scores_3D_PCA_prediction <- renderPlotly({
        dat_pca <- Predict_PCAButton()$dat_pca
        dat_pca$Samples <- seq_len(nrow(dat_pca))
        dat_test <- Predict_PCAButton()$dat_test
        dat_test$Samples <- seq_len(nrow(dat_test))
        
        ax <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PCA_axis_1_test, zerolinewidth = 2
        )
        
        ay <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PCA_axis_2_test, zerolinewidth = 2
        )
        
        az <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PCA_axis_3_test, zerolinewidth = 2
        )
        
        title_font <- list(
            family = "Arial", size = 18, color = 'red')
        
        plot_ly(dat_pca, x = dat_pca[,input$PCA_axis_1_test], y = dat_pca[,input$PCA_axis_2_test],
                z = dat_pca[,input$PCA_axis_3_test],
                type = 'scatter3d',
                mode = 'markers',
                color = as.factor(dat_pca$Populations),
                colors = cbp1,
                opacity = 0.3,
                symbol = as.factor(dat_pca$Populations),
                symbols = c('circle','square','diamond','x','triangle-up',
                            'triangle-down','triangle-left','triangle-right',
                            'pentagon'),
                text = ~paste('Subject: ', rownames(dat_pca))) %>%
            layout(title = "3D PCA Scores Plot on New Individuals",
                   titlefont = title_font)%>%
            add_trace(x = dat_test[,input$PCA_axis_1_test], y = dat_test[,input$PCA_axis_2_test],
                      z = dat_test[,input$PCA_axis_3_test],
                      type = 'scatter3d',
                      mode = 'markers',
                      color = rownames(dat_test$Populations),
                      colors = cbp1,
                      opacity = 1,
                      marker = list(size = 10),
                      symbol = as.factor(dat_test$Populations),
                      symbols = c('circle','square','diamond','x','triangle-up',
                                  'triangle-down','triangle-left','triangle-right',
                                  'pentagon'),
                      text = ~paste('Subject: ', dat_test$Populations))%>%
            layout(scene = list(xaxis=ax,yaxis=ay,zaxis=az))
            
    })
    
    # ggplot of PCA scores plot 2D on unknowns
    output$plot_pca_pred <- renderPlot({
        Predict_PCAButton()$plot_pca_pred
    })
    
    # 
    # Title above LR values from PCA
    output$caption_LR_PCA<-renderText({
        req(input$pca_button)
        "LR values - prediction from PCA"
    })
    
    
    # Write LR results from PCA
    observe({
        withConsoleRedirect("console", {
            dat_pca_pred = Predict_PCAButton()$dat_pca_pred
            allComparisons = Predict_PCAButton()$allComparisons
            LR_from_PCA = function(allComparisons){
                data = dat_pca_pred
                population = allComparisons
                
                category.1.name = unique(population$Factor)[1]
                category.2.name = unique(population$Factor)[2]
                categories = paste(unique(population$Factor), collapse = "_")
                
                ##In the 'variables' it is necessary to enter the index of the column in the univariate problem (or columns in the multivariate problem) for the relevant variable(s)
                variables = seq(from=4,to=ncol(population),by=1) ##ENTER THE NUMBER OF THE COLUMN(S) THE RELEVANT VARIABLE(S) IS (ARE) LOCATED IN, E.G. 4 FOR UNIVARIATE OR 4,5 FOR MULTIVARIATE MODELS
                variables.names = colnames(population[variables])
                variable.name = paste(variables.names, collapse = "_")
                m.all.analysed = length(unique(data$Item)) ##'m.all.analysed' corresponds to the number of all objects subjected to calculations
                p = length(variables) ##number of variables considered; gives the idea of the problem dimensionality
                n = length(unique(population$Piece)) ##number of measurements per object
                
                ##'population.1' and 'population.2' refer to the populations of objects belonging to category 1 and 2 respectively
                population.1 = population[which(population$Factor == category.1.name),]
                population.2 = population[which(population$Factor == category.2.name),]
                
                UC.1 = function(population.1, variables, p)
                {
                    items.1 = unique(population.1$Item)
                    m.1 = length(items.1) ##'m.1' corresponds to the number of objects creating population.1
                    
                    ##Defining 'S.star' matrix initially filled with 0 at the beginning of the loops
                    S.star.1 = matrix(0, nrow = p, ncol = p)
                    variable.names = colnames(population.1[,variables])
                    rownames(S.star.1) = variable.names
                    colnames(S.star.1) = variable.names
                    
                    ##Dealing with multivariate data (p>1)
                    if (p>1)
                    {
                        ##'mean.all.1' corresponds to the vector of means of all variables calculated using all measurements for all objects belonging to category 1
                        mean.all.1 = matrix(apply(population.1[,variables],2,mean), nrow = 1)
                        colnames(mean.all.1) = variable.names
                        
                        ##'i' runs through all objects from the population.1
                        for (i in 1:m.1)
                        {
                            ##creating a matrix of measurements for the ith object
                            ith.object.1 = as.matrix(population.1[which(population.1$Item == items.1[i]),variables])
                            
                            ##calculating the mean for each object from category 1
                            object.mean.1 = matrix(apply(ith.object.1,2,mean), nrow = 1)
                            
                            S.star.1 = S.star.1 + t(object.mean.1-mean.all.1) %*% (object.mean.1-mean.all.1)
                        }
                        
                        ##creating 'C' matrix
                        C.1 = S.star.1/(m.1-1)
                    }
                    
                    ##Dealing with univariate data (p=1)
                    if (p==1)
                    {
                        mean.all.1 = matrix(mean(population.1[,variables], nrow = 1))
                        colnames(mean.all.1) = variable.names
                        
                        for (i in 1:m.1)
                        {
                            ith.object.1 = as.matrix(population.1[which(population.1$Item == items.1[i]),variables])
                            object.mean.1 = matrix(mean(ith.object.1), nrow = 1)
                            
                            S.star.1 = S.star.1 + t(object.mean.1-mean.all.1) %*% (object.mean.1-mean.all.1)
                        }
                        
                        C.1 = S.star.1/(m.1-1)
                    }
                    
                    result = list(C.1 = C.1, mean.all.1 = mean.all.1, object.mean.1 = object.mean.1, m.1 = m.1)
                    return (result)
                }
                
                UC.2 = function(population.2,variables, p)
                {
                    items.2 = unique(population.2$Item)
                    m.2 = length(items.2) ##'m.2' corresponds to the number of objects creating population.2
                    
                    ##Defining 'S.star' matrix initially filled with 0 at the beginning of the loops
                    S.star.2 = matrix(0, nrow = p, ncol = p)
                    variable.names = colnames(population.2[,variables])
                    rownames(S.star.2) = variable.names
                    colnames(S.star.2) = variable.names
                    
                    ##Dealing with multivariate data (p>1)
                    if (p>1)
                    {
                        ##'mean.all.2' corresponds to the vector of means of all variables calculated using all measurements for all objects belonging to category 2
                        mean.all.2 = matrix(apply(population.2[,variables],2,mean), nrow = 1)
                        colnames(mean.all.2) = variable.names
                        
                        ##'i' runs through all objects from the population.2
                        for (i in 1:m.2)
                        {
                            ##creating a matrix of measurements for the ith object
                            ith.object.2 = as.matrix(population.2[which(population.2$Item == items.2[i]),variables])
                            
                            ##calculating the mean for each object from category 2
                            object.mean.2 = matrix(apply(ith.object.2,2,mean),nrow = 1)
                            
                            S.star.2 = S.star.2 + t(object.mean.2-mean.all.2) %*% (object.mean.2-mean.all.2)
                        }
                        
                        ##creating 'C' matrix
                        C.2 = S.star.2/(m.2-1)
                    }
                    
                    ##Dealing with univariate data (p=1)
                    if (p==1)
                    {
                        mean.all.2 = matrix(mean(population.2[,variables], nrow = 1))
                        colnames(mean.all.2) = variable.names
                        
                        for (i in 1:m.2)
                        {
                            ith.object.2 = as.matrix(population.2[which(population.2$Item == items.2[i]),variables])
                            object.mean.2 = matrix(mean(ith.object.2), nrow = 1)
                            
                            S.star.2 = S.star.2 + t(object.mean.2-mean.all.2) %*% (object.mean.2-mean.all.2)
                        }
                        
                        C.2 = S.star.2/(m.2-1)
                    }
                    
                    result = list(C.2 = C.2, mean.all.2 = mean.all.2, object.mean.2 = object.mean.2, m.2 = m.2)
                    return (result)
                }
                
                LR.KDE.function = function(population.1, population.2, variables, y.mean, U.1, U.2, C.1, C.2, h.1, h.2, p, m.1, m.2)
                {
                    ##Numerator calculation
                    nom1 = 0
                    
                    ##'i' runs through all objects from the population.1
                    for(i in 1:m.1)
                    {
                        items.1 = unique(population.1$Item)
                        
                        ##creating a matrix of measurements for the ith object
                        ith.object.1 = as.matrix(population.1[which(population.1$Item == items.1[i]),variables])
                        
                        ##calculating the 'object.mean.1'
                        object.mean.1 = matrix(apply(ith.object.1,2,mean), nrow = 1)
                        
                        exp.1.1 = exp(-(y.mean-object.mean.1) %*% solve(h.1^2*C.1) %*% t(y.mean-object.mean.1)/2)
                        
                        nom1 = nom1 + exp.1.1
                    }
                    
                    nom2 = nom1/m.1
                    
                    exp.1.2 = (2*pi)^(-p/2) * (det(C.1*h.1^2))^(-1/2)
                    
                    nom = nom2*exp.1.2
                    
                    ##Denominator calculation
                    denom1 = 0
                    
                    for(i in 1:m.2)
                    {
                        items.2 = unique(population.2$Item)
                        ith.object.2 = as.matrix(population.2[which(population.2$Item == items.2[i]),variables])
                        object.mean.2 = matrix(apply(ith.object.2,2,mean), nrow = 1)
                        
                        exp.2.1 = exp(-(y.mean-object.mean.2) %*% solve(h.2^2*C.2) %*% t(y.mean-object.mean.2)/2)
                        
                        denom1 = denom1 + exp.2.1
                    }
                    
                    denom2 = denom1/m.2
                    
                    exp.2.2 = (2*pi)^(-p/2) * (det(C.2*h.2^2))^(-1/2)
                    denom = denom2*exp.2.2
                    
                    LR.KDE = nom/denom
                    
                    result = list(LR.KDE = LR.KDE)
                    return (result)
                }
                
                
                ##'UC' function provides information on within- (U) and between- (C) object variability matrices
                results.UC.1 = UC.1(population.1, variables, p)
                U.1 = results.UC.1$U.1
                C.1 = results.UC.1$C.1
                mean.all.1 = results.UC.1$mean.all.1 ##mean of all measurements performed on all objects from population.1
                m.1 = results.UC.1$m.1 ##number of objects in population.1
                
                results.UC.2 = UC.2(population.2, variables, p)
                U.2 = results.UC.2$U.2
                C.2 = results.UC.2$C.2
                mean.all.2 = results.UC.2$mean.all.2
                m.2 = results.UC.2$m.2
                
                ##Calculating smoothing parameters ('h.1' and 'h.2') as bandwidths for KDE procedure
                h.1 = (4/(m.1*(2*p+1)))^(1/(p+4))
                h.2 = (4/(m.2*(2*p+1)))^(1/(p+4))
                
                ##Defining matrices of LR results which are to be saved in a .txt file
                output.matrix.KDE = matrix(0, ncol = 2, nrow = m.all.analysed)
                rownames(output.matrix.KDE) = unique(data$Name)
                colnames(output.matrix.KDE) = c(paste("LR_PCA","_value", sep=""), paste("Population_",category.1.name,sep=""))
                
                ##'i' index runs through all the objects denoted by 'analysed.object'
                for (i in 1:m.all.analysed)
                {
                    analysed.object = data[which(data$Item == i),]
                    
                    ##Calculating parameters for the 'analysed.object', which is to be classified
                    y = data.frame(analysed.object[,variables])
                    y.mean = matrix(apply(y,2,mean), nrow = 1) ##mean for the analysed object
                    
                    ##Calculating the LR when between-object distribution is estimated by KDE
                    results.LR.KDE = LR.KDE.function(population.1, population.2, variables, y.mean, U.1, U.2, C.1, C.2, h.1, h.2, p, m.1, m.2)
                    LR.KDE = results.LR.KDE$LR.KDE
                    
                    ##Filling 'output.matrix.KDE' with LR results
                    output.matrix.KDE[i,1] = signif(LR.KDE, digits = 4)
                    if (LR.KDE > 1) {output.matrix.KDE[i,2] = as.character(category.1.name)}
                    else {output.matrix.KDE[i,2] = as.character(category.2.name)}
                    
                }
                output.matrix.KDE <<- output.matrix.KDE
                print(output.matrix.KDE)
            }
            lapply(allComparisons,FUN = LR_from_PCA)
        })
    })
    
    plot <- eventReactive(input$prediction_pca_button,{
        req(input$PC_number)
        Predict_PCAButton()$plot_pca_pred
    })
    
    prova1 <- eventReactive(input$prediction_pca_button,{
        dat_pca_pred = Predict_PCAButton()$dat_pca_pred
    })
    
    prova2 <- eventReactive(input$prediction_pca_button,{
        allComparisons = Predict_PCAButton()$allComparisons
    })
    
    n <- eventReactive(input$prediction_pca_button,{
        levels(df1_sel2_subj()$Populations)
    })
    
    output$plot <- renderPlot({
        Predict_PCAButton()$plot_pca_pred
    })
    
    # Write report for PCA
    output$downloader <- 
        downloadHandler(
            "results_from_shiny.pdf",
            content = 
                function(file)
                {
                    rmarkdown::render(
                        input = "report_file.Rmd",
                        output_file = "built_report.pdf",
                        params = list(plot = plot(),
                                      prova1 = prova1(),
                                      prova2 = prova2())
                    ) 
                    readBin(con = "built_report.pdf", 
                            what = "raw",
                            n = file.info("built_report.pdf")[, "size"]) %>%
                        writeBin(con = file)
                }
        )
    
    
    #### Prediction on PLS-DA ####
    # Predict PLS-DA function
    observeEvent(input$prediction_plsda_button, {
        for (i in 1:100) {
            updateProgressBar(
                session = session,
                id = "progress_plsda",
                value = i, total = 100,
                title = paste("Process", trunc(i/10))
            )
            Sys.sleep(0.05)
        }
    })
    
    Predict_PLSDAButton <- eventReactive(input$prediction_plsda_button,{
        withProgress(message = "LR on PLS-DA calculation", value = 0, {
             incProgress(0.2, detail = "Preparing the data for LR calculation") #
            
        req(input$LV_number)
        df1_sel2 <- df1_sel2()
        df1_sel2_subj <- df1_sel2_subj()
        Populations <- df1_sel2[,1]
        plsda.model <- PLSDAButton()$plsda.model
        dat<-plyr::rbind.fill(df1_sel2_subj,df1_sel2)
        Populations<-dat[,1]
        dat<-dat[,-1]
        dat<-dat[,order(colnames(dat))]
        dat<-cbind(Populations,dat)
        df1_sel2_subj<-dat[seq_len(dim(df1_sel2_subj)[1]),]
        df1_sel2_subj[is.na(df1_sel2_subj)] <- 0
        df1_sel2<-dat[-seq_len(dim(df1_sel2_subj)[1]),]
        df1_sel2[is.na(df1_sel2)] <- 1
            # 
        Sys.sleep(1) #
            # 
        req(input$LV_number)
        Populations<-as.factor(df1_sel2[,1])
        dfplsda <- df1_sel2[,-1]
        
        Sys.sleep(1) #
        
        X <- dfplsda
        Y <- as.factor(df1_sel2[,1])
        library(mixOmics)
        scores2<-plsda.model$variates$X
        scores2 <- as.data.frame(scores2)
        prefix = "LV"
        n = input$LV_number
        suffix = seq(1:n)
        mynames = paste(prefix,suffix,sep="")
        colnames(scores2)=c(mynames)
        scores2$Populations <- as.factor(Populations)
        library(plotly)
        
        pred <- predict(plsda.model, as.matrix(df1_sel2_subj[,-1]), dist = "max.dist")
        pred <- as.data.frame(pred$variates)
        prefix = "LV"
        n = input$LV_number
        suffix = seq(1:n)
        mynames = paste(prefix,suffix,sep="")
        colnames(pred)=c(mynames)
        
        dat<-scores2
        #dat['Set']='Model_data'
        dat_plsda <- dat
        df<-pred
        populations<-(df1_sel2_subj[,1])
        df$Populations<-populations
        #df['Set']='New_Samples'
        df <- as.data.frame(df)
        dat_plsda_pred<-as.data.frame(df) 
        
        title_font <- list(
            family = "Arial",
            size = 18,
            color = 'red')
        
        cbp1 <- c("#E69F00", "#56B4E9", "#009E73","#D55E00", "#CC79A7",
                  "#999999", "#F0E442", "#0072B2") #palette di colori
        
        dat_plsda_train <- dat_plsda
        rownames(dat_plsda_train) <- rownames(df1_sel2())
        dat_plsda_test <- dat_plsda_pred
        rownames(dat_plsda_test) <- df1_sel2_subj$Populations
        dat_plsda_train <<- dat_plsda_train
        dat_plsda_test <<- dat_plsda_test
        
        library(ggplot2)
        prova <- cbind(as.numeric(dat_plsda$LV1),as.numeric(dat_plsda$LV2))
        colnames(prova) = c("LV1","LV2")
        prova <- as.data.frame(prova)
        prova2 <- cbind(as.numeric(dat_plsda_pred$LV1),as.numeric(dat_plsda_pred$LV2))
        colnames(prova2) = c("LV1","LV2")
        rownames(prova2) = df1_sel2_subj[,1]
        library(ggplot2)
        plsda_mod_test2 <- ggplot(scores2,aes(x=scores2[,1],y=scores2[,2])) +
            geom_point(aes(color = factor(dat_plsda$Populations)),
                       size=2.5) +
            labs(title="PLS-DA model on unknowns", x="LV1", y = "LV2",
                 color = "Tested Populations")+
            geom_point(data=prova2, aes(x=prova2[,1],y=prova2[,2]),
                       colour="orange",size=8,shape=17) +
            geom_hline(aes(yintercept=0), linetype="dashed") +
            geom_vline(aes(xintercept=0), linetype="dashed") +
            theme_classic()+
            theme(axis.title=element_text(size=12,face="bold"),
                  plot.title = element_text(color = "red", size = 14,
                                            face = "bold",hjust = 0.5),
                  legend.title = element_text(colour="black", size=14, face="bold"),
                  legend.text = element_text(colour="black", size=10, face="bold"),
                  legend.background = element_rect(size=0.5, linetype="solid",colour ="black"))
        
            Sys.sleep(2) #
            incProgress(0.5, detail = "Preparing plots and tables") #
            
            
            dat_plsda$Populations = as.character(dat_plsda$Populations)
            pops=c(levels(as.factor(dat_plsda$Populations)))
            allDf<-list()
            for (i in 1:nlevels(as.factor(dat_plsda$Populations))) {
                allDf[[i]]<-assign(paste0("dat_plsda","_",pops[i]), dat_plsda[dat_plsda$Populations==pops[i],])
            }
            options(stringsAsFactors = F)

            nDf = length(allDf)
            allComparisons = lapply(1:nDf, function(x){
                #Merge all df, but change the order of the first one in the list
                mergeDf = do.call(rbind, allDf[c(x, c(1:nDf)[-x])])
                #Set the new factor names, only keeing the names for the first one
                mergeDf$Populations = as.factor(c(allDf[[x]]$Populations, rep("Other_population_datasets", nrow(dat_plsda)-nrow(allDf[[x]]))))
                mergeDf$Factor<-mergeDf$Populations
                mergeDf
            })
            allComparisons = lapply(allComparisons, function(x) {
                x["Populations"] <- NULL;
                x["Item"] <- seq(1,nrow(dat_plsda),1);
                x["Piece"] <- rep(1,nrow(dat_plsda));
                x
            })

            library(plyr); library(dplyr)

            allComparisons = lapply(allComparisons, function(x) {
                x %>%
                    dplyr::select(Factor, Item, Piece, everything())
            })

            dat_plsda_pred$Name <- dat_plsda_pred$Populations
            dat_plsda_pred$Populations <- NULL
            dat_plsda_pred$Item <- seq(1,nrow(dat_plsda_pred),1)
            dat_plsda_pred$Piece <- rep(1,nrow(dat_plsda_pred))
            dat_plsda_pred = dat_plsda_pred %>%
                dplyr::select(Name, Item, Piece, everything())

            library(KernSmooth)

            vec_LV <- paste0(c(rep("LV", input$LV_number)),1:input$LV_number)
            cols.num <- c(vec_LV)
            data = dat_plsda_pred
            Name = as.factor(data$Name)
            data = as.data.frame(subset(data, select=-c(Name)))
            data = as.data.frame(cbind(Name,data))

            population = allComparisons
            vec_LV <- paste0(c(rep("LV", input$LV_number)),1:input$LV_number)
            cols.num <- c(vec_LV)
            a = nlevels(as.factor(dat_plsda$Populations))
            for (i in 1:a){
                population[[i]][cols.num] <- sapply(population[[i]][cols.num],as.numeric)
            }

            allComparisons = population
            dat_plsda_pred = data
            
            # 
            incProgress(0.3, detail = "Done")
            list(pred=pred, plsda_mod_test2 = plsda_mod_test2, dat_plsda_train = dat_plsda_train,
                 dat_plsda_test = dat_plsda_test,
                 dat_plsda = dat_plsda, dat_plsda_pred = dat_plsda_pred, 
                 allComparisons = allComparisons, vec_LV = vec_LV) 
            })
    })
    
    output$plotta <- renderPrint({
        Predict_PLSDAButton()$allComparisons[1]
    })
    
    # Selection of PLSDA_axis_1_test
    output$PLSDA_axis_1_test <- renderUI({
        req(input$prediction_plsda_button)
        choice = c("Samples",Predict_PLSDAButton()$vec_LV)
        pickerInput("PLSDA_axis_1_test", label = HTML('<h5><b>Select X axis</b></h5>'), 
                    choices = choice, selected = choice[2],options = list(`style` = "btn-info"))
    })
    
    # Selection of PLSDA_axis_2_test
    output$PLSDA_axis_2_test <- renderUI({
        req(input$prediction_plsda_button)
        choice = c("Samples",Predict_PLSDAButton()$vec_LV)
        pickerInput("PLSDA_axis_2_test", label = HTML('<h5><b>Select Y axis</b></h5>'), 
                    choices = choice, selected = choice[3],options = list(`style` = "btn-info"))
    })
    
    # Selection of PLSDA_axis_3_test
    output$PLSDA_axis_3_test <- renderUI({
        req(input$prediction_plsda_button)
        choice = c("Samples",Predict_PLSDAButton()$vec_LV)
        pickerInput("PLSDA_axis_3_test", label = HTML('<h5><b>Select Z axis</b></h5>'), 
                    choices = choice, selected = choice[4],options = list(`style` = "btn-info"))
    })
    
    # Plot of PCA scores plot 2D on unknowns
    output$scores_2D_PLSDA_prediction <- renderPlotly({
        #Predict_PLSDAButton()$plsda_mod_test
        
        dat_plsda_train <- Predict_PLSDAButton()$dat_plsda_train
        dat_plsda_train$Samples <- seq_len(nrow(dat_plsda_train))
        dat_plsda_test <- Predict_PLSDAButton()$dat_plsda_test
        dat_plsda_test$Samples <- seq_len(nrow(dat_plsda_test))
        
        ax <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PLSDA_axis_1_test, zerolinewidth = 2
        )
        
        ay <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PLSDA_axis_2_test, zerolinewidth = 2
        )
        
        title_font <- list(
            family = "Arial", size = 18, color = 'black')
        
        plot_ly(dat_plsda_train, x = dat_plsda_train[,input$PLSDA_axis_1_test], 
                y = dat_plsda_train[,input$PLSDA_axis_2_test],
                type = 'scatter',
                mode = 'markers',
                color = as.factor(dat_plsda_train$Populations),
                colors = cbp1,
                opacity = 0.7,
                symbol = as.factor(dat_plsda_train$Populations),
                symbols = c('circle','square','diamond','x','triangle-up',
                            'triangle-down','triangle-left','triangle-right',
                            'pentagon'),
                text = ~paste('Subject: ', rownames(dat_plsda_train))) %>%
            layout(title = "PLS-DA Scores Plot on New Individuals",
                   titlefont = title_font)%>%
            add_trace(x = dat_plsda_test[,input$PLSDA_axis_1_test], 
                      y = dat_plsda_test[,input$PLSDA_axis_2_test],
                      type = 'scatter',
                      mode = 'markers',
                      color = rownames(dat_plsda_test$Populations),
                      colors = cbp1,
                      opacity = 1,
                      marker = list(size = 15),
                      symbol = as.factor(dat_plsda_test$Populations),
                      symbols = c('circle','square','diamond','x','triangle-up',
                                  'triangle-down','triangle-left','triangle-right',
                                  'pentagon'),
                      text = ~paste('Subject: ', dat_plsda_test$Populations))%>%
           layout(xaxis = ax, yaxis = ay)
      
    })
    
    # Plot of PLSDA scores plot 3D on unknowns
    output$scores_3D_PLSDA_prediction <- renderPlotly({
        dat_plsda_train <- Predict_PLSDAButton()$dat_plsda_train
        dat_plsda_train$Samples <- seq_len(nrow(dat_plsda_train))
        dat_plsda_test <- Predict_PLSDAButton()$dat_plsda_test
        dat_plsda_test$Samples <- seq_len(nrow(dat_plsda_test))
        
        ax <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PLSDA_axis_1_test, zerolinewidth = 2
        )
        
        ay <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PLSDA_axis_2_test, zerolinewidth = 2
        )
        
        az <- list(
            zeroline = TRUE, showline = TRUE, mirror = "ticks",
            title = input$PLSDA_axis_3_test, zerolinewidth = 2
        )
        
        title_font <- list(
            family = "Arial", size = 18, color = 'red')
        
        plot_ly(dat_plsda_train, x = dat_plsda_train[,input$PLSDA_axis_1_test], 
                y = dat_plsda_train[,input$PLSDA_axis_2_test],
                z = dat_plsda_train[,input$PLSDA_axis_3_test],
                               type = 'scatter3d',
                               mode = 'markers',
                               color = as.factor(dat_plsda_train$Populations),
                               colors = cbp1,
                               opacity = 0.1,
                               symbol = as.factor(dat_plsda_train$Populations),
                               symbols = c('circle','square','diamond','x','triangle-up',
                                           'triangle-down','triangle-left','triangle-right',
                                           'pentagon'),
                               text = ~paste('Subject: ', rownames(dat_plsda_train))) %>%
            layout(title = "PCA Scores Plot on New Individuals",
                   titlefont = title_font)%>%
            add_trace(x = dat_plsda_test[,input$PLSDA_axis_1_test], 
                      y = dat_plsda_test[,input$PLSDA_axis_2_test],
                      z = dat_plsda_test[,input$PLSDA_axis_3_test],
                      type = 'scatter3d',
                      mode = 'markers',
                      color = rownames(dat_plsda_test$Populations),
                      colors = cbp1,
                      opacity = 1,
                      symbol = as.factor(dat_plsda_test$Populations),
                      symbols = c('circle','square','diamond','x','triangle-up',
                                  'triangle-down','triangle-left','triangle-right',
                                  'pentagon'),
                      text = ~paste('Subject: ', as.character(dat_plsda_test$Populations)))%>%
                      layout(scene=list(xaxis = ax, yaxis = ay, zaxis =az))

    })
    
    # ggplot2 Scores Plot 2D
    output$plot_plsda_pred <- renderPlot({
        Predict_PLSDAButton()$plsda_mod_test2
    })
     
    # Title above LR values from PLS-DA
    output$caption_LR_PLSDA<-renderText({
        req(input$pca_button)
        "LR values - prediction from PLS-DA"
    })
     
    # Write LR results from PLS-DA
    observe({
        withConsoleRedirect("console_PLSDA", {
            dat_plsda_pred = Predict_PLSDAButton()$dat_plsda_pred
            allComparisons = Predict_PLSDAButton()$allComparisons
            LR_from_PLSDA = function(allComparisons){
                data = dat_plsda_pred
                population = allComparisons

                category.1.name = unique(population$Factor)[1]
                category.2.name = unique(population$Factor)[2]
                categories = paste(unique(population$Factor), collapse = "_")

                ##In the 'variables' it is necessary to enter the index of the column in the univariate problem (or columns in the multivariate problem) for the relevant variable(s)
                variables = seq(from=4,to=ncol(population),by=1) ##ENTER THE NUMBER OF THE COLUMN(S) THE RELEVANT VARIABLE(S) IS (ARE) LOCATED IN, E.G. 4 FOR UNIVARIATE OR 4,5 FOR MULTIVARIATE MODELS
                variables.names = colnames(population[variables])
                variable.name = paste(variables.names, collapse = "_")
                m.all.analysed = length(unique(data$Item)) ##'m.all.analysed' corresponds to the number of all objects subjected to calculations
                p = length(variables) ##number of variables considered; gives the idea of the problem dimensionality
                n = length(unique(population$Piece)) ##number of measurements per object

                ##'population.1' and 'population.2' refer to the populations of objects belonging to category 1 and 2 respectively
                population.1 = population[which(population$Factor == category.1.name),]
                population.2 = population[which(population$Factor == category.2.name),]

                UC.1 = function(population.1, variables, p)
                {
                    items.1 = unique(population.1$Item)
                    m.1 = length(items.1) ##'m.1' corresponds to the number of objects creating population.1

                    ##Defining 'S.star' matrix initially filled with 0 at the beginning of the loops
                    S.star.1 = matrix(0, nrow = p, ncol = p)
                    variable.names = colnames(population.1[,variables])
                    rownames(S.star.1) = variable.names
                    colnames(S.star.1) = variable.names

                    ##Dealing with multivariate data (p>1)
                    if (p>1)
                    {
                        ##'mean.all.1' corresponds to the vector of means of all variables calculated using all measurements for all objects belonging to category 1
                        mean.all.1 = matrix(apply(population.1[,variables],2,mean), nrow = 1)
                        colnames(mean.all.1) = variable.names

                        ##'i' runs through all objects from the population.1
                        for (i in 1:m.1)
                        {
                            ##creating a matrix of measurements for the ith object
                            ith.object.1 = as.matrix(population.1[which(population.1$Item == items.1[i]),variables])

                            ##calculating the mean for each object from category 1
                            object.mean.1 = matrix(apply(ith.object.1,2,mean), nrow = 1)

                            S.star.1 = S.star.1 + t(object.mean.1-mean.all.1) %*% (object.mean.1-mean.all.1)
                        }

                        ##creating 'C' matrix
                        C.1 = S.star.1/(m.1-1)
                    }

                    ##Dealing with univariate data (p=1)
                    if (p==1)
                    {
                        mean.all.1 = matrix(mean(population.1[,variables], nrow = 1))
                        colnames(mean.all.1) = variable.names

                        for (i in 1:m.1)
                        {
                            ith.object.1 = as.matrix(population.1[which(population.1$Item == items.1[i]),variables])
                            object.mean.1 = matrix(mean(ith.object.1), nrow = 1)

                            S.star.1 = S.star.1 + t(object.mean.1-mean.all.1) %*% (object.mean.1-mean.all.1)
                        }

                        C.1 = S.star.1/(m.1-1)
                    }

                    result = list(C.1 = C.1, mean.all.1 = mean.all.1, object.mean.1 = object.mean.1, m.1 = m.1)
                    return (result)
                }

                UC.2 = function(population.2,variables, p)
                {
                    items.2 = unique(population.2$Item)
                    m.2 = length(items.2) ##'m.2' corresponds to the number of objects creating population.2

                    ##Defining 'S.star' matrix initially filled with 0 at the beginning of the loops
                    S.star.2 = matrix(0, nrow = p, ncol = p)
                    variable.names = colnames(population.2[,variables])
                    rownames(S.star.2) = variable.names
                    colnames(S.star.2) = variable.names

                    ##Dealing with multivariate data (p>1)
                    if (p>1)
                    {
                        ##'mean.all.2' corresponds to the vector of means of all variables calculated using all measurements for all objects belonging to category 2
                        mean.all.2 = matrix(apply(population.2[,variables],2,mean), nrow = 1)
                        colnames(mean.all.2) = variable.names

                        ##'i' runs through all objects from the population.2
                        for (i in 1:m.2)
                        {
                            ##creating a matrix of measurements for the ith object
                            ith.object.2 = as.matrix(population.2[which(population.2$Item == items.2[i]),variables])

                            ##calculating the mean for each object from category 2
                            object.mean.2 = matrix(apply(ith.object.2,2,mean),nrow = 1)

                            S.star.2 = S.star.2 + t(object.mean.2-mean.all.2) %*% (object.mean.2-mean.all.2)
                        }

                        ##creating 'C' matrix
                        C.2 = S.star.2/(m.2-1)
                    }

                    ##Dealing with univariate data (p=1)
                    if (p==1)
                    {
                        mean.all.2 = matrix(mean(population.2[,variables], nrow = 1))
                        colnames(mean.all.2) = variable.names

                        for (i in 1:m.2)
                        {
                            ith.object.2 = as.matrix(population.2[which(population.2$Item == items.2[i]),variables])
                            object.mean.2 = matrix(mean(ith.object.2), nrow = 1)

                            S.star.2 = S.star.2 + t(object.mean.2-mean.all.2) %*% (object.mean.2-mean.all.2)
                        }

                        C.2 = S.star.2/(m.2-1)
                    }

                    result = list(C.2 = C.2, mean.all.2 = mean.all.2, object.mean.2 = object.mean.2, m.2 = m.2)
                    return (result)
                }

                LR.KDE.function = function(population.1, population.2, variables, y.mean, U.1, U.2, C.1, C.2, h.1, h.2, p, m.1, m.2)
                {
                    ##Numerator calculation
                    nom1 = 0

                    ##'i' runs through all objects from the population.1
                    for(i in 1:m.1)
                    {
                        items.1 = unique(population.1$Item)

                        ##creating a matrix of measurements for the ith object
                        ith.object.1 = as.matrix(population.1[which(population.1$Item == items.1[i]),variables])

                        ##calculating the 'object.mean.1'
                        object.mean.1 = matrix(apply(ith.object.1,2,mean), nrow = 1)

                        exp.1.1 = exp(-(y.mean-object.mean.1) %*% solve(h.1^2*C.1) %*% t(y.mean-object.mean.1)/2)

                        nom1 = nom1 + exp.1.1
                    }

                    nom2 = nom1/m.1

                    exp.1.2 = (2*pi)^(-p/2) * (det(C.1*h.1^2))^(-1/2)

                    nom = nom2*exp.1.2

                    ##Denominator calculation
                    denom1 = 0

                    for(i in 1:m.2)
                    {
                        items.2 = unique(population.2$Item)
                        ith.object.2 = as.matrix(population.2[which(population.2$Item == items.2[i]),variables])
                        object.mean.2 = matrix(apply(ith.object.2,2,mean), nrow = 1)

                        exp.2.1 = exp(-(y.mean-object.mean.2) %*% solve(h.2^2*C.2) %*% t(y.mean-object.mean.2)/2)

                        denom1 = denom1 + exp.2.1
                    }

                    denom2 = denom1/m.2

                    exp.2.2 = (2*pi)^(-p/2) * (det(C.2*h.2^2))^(-1/2)
                    denom = denom2*exp.2.2

                    LR.KDE = nom/denom

                    result = list(LR.KDE = LR.KDE)
                    return (result)
                }


                ##'UC' function provides information on within- (U) and between- (C) object variability matrices
                results.UC.1 = UC.1(population.1, variables, p)
                U.1 = results.UC.1$U.1
                C.1 = results.UC.1$C.1
                mean.all.1 = results.UC.1$mean.all.1 ##mean of all measurements performed on all objects from population.1
                m.1 = results.UC.1$m.1 ##number of objects in population.1

                results.UC.2 = UC.2(population.2, variables, p)
                U.2 = results.UC.2$U.2
                C.2 = results.UC.2$C.2
                mean.all.2 = results.UC.2$mean.all.2
                m.2 = results.UC.2$m.2

                ##Calculating smoothing parameters ('h.1' and 'h.2') as bandwidths for KDE procedure
                h.1 = (4/(m.1*(2*p+1)))^(1/(p+4))
                h.2 = (4/(m.2*(2*p+1)))^(1/(p+4))

                ##Defining matrices of LR results which are to be saved in a .txt file
                output.matrix.KDE = matrix(0, ncol = 2, nrow = m.all.analysed)
                rownames(output.matrix.KDE) = unique(data$Name)
                colnames(output.matrix.KDE) = c(paste("LR_PLSDA","_value", sep=""), paste("Population_",category.1.name,sep=""))

                ##'i' index runs through all the objects denoted by 'analysed.object'
                for (i in 1:m.all.analysed)
                {
                    analysed.object = data[which(data$Item == i),]

                    ##Calculating parameters for the 'analysed.object', which is to be classified
                    y = data.frame(analysed.object[,variables])
                    y.mean = matrix(apply(y,2,mean), nrow = 1) ##mean for the analysed object

                    ##Calculating the LR when between-object distribution is estimated by KDE
                    results.LR.KDE = LR.KDE.function(population.1, population.2, variables, y.mean, U.1, U.2, C.1, C.2, h.1, h.2, p, m.1, m.2)
                    LR.KDE = results.LR.KDE$LR.KDE

                    ##Filling 'output.matrix.KDE' with LR results
                    output.matrix.KDE[i,1] = signif(LR.KDE, digits = 4)
                    if (LR.KDE > 1) {output.matrix.KDE[i,2] = as.character(category.1.name)}
                    else {output.matrix.KDE[i,2] = as.character(category.2.name)}

                }
                output.matrix.KDE <<- output.matrix.KDE
                print(output.matrix.KDE)
            }
            lapply(allComparisons,FUN = LR_from_PLSDA)
        })
    })

    plotPLSDA <- eventReactive(input$prediction_plsda_button,{
        req(input$LV_number)
        Predict_PLSDAButton()$plsda_mod_test2
    })
    # 
    prova1PLSDA <- eventReactive(input$prediction_plsda_button,{
        dat_plsda_pred = Predict_PLSDAButton()$dat_plsda_pred
    })

    prova2PLSDA <- eventReactive(input$prediction_plsda_button,{
        allComparisons = Predict_PLSDAButton()$allComparisons
    })
    
    # Write report for PLS-DA
    output$downloader_PLSDA <-
        downloadHandler(
            "results_from_shiny.pdf",
            content =
                function(file)
                {
                    rmarkdown::render(
                        input = "report_file2.Rmd",
                        output_file = "built_report_PLSDA.pdf",
                        params = list(plotPLSDA = plotPLSDA(),
                                      prova1PLSDA = prova1PLSDA(),
                                      prova2PLSDA = prova2PLSDA())
                    )
                    readBin(con = "built_report_PLSDA.pdf",
                            what = "raw",
                            n = file.info("built_report_PLSDA.pdf")[, "size"]) %>%
                        writeBin(con = file)
                }
        )

    #### Prediction on SVM ####
    
    Predict_SVMButton <- eventReactive(input$prediction_svm_button,{
       
        df1_sel2 <- df1_sel2()
        df1_sel2_subj <- df1_sel2_subj()
        Populations <- df1_sel2[,1]
        dat<-plyr::rbind.fill(df1_sel2_subj,df1_sel2)
        Populations<-dat[,1]
        dat<-dat[,-1]
        dat<-dat[,order(colnames(dat))]
        dat<-cbind(Populations,dat)
        df1_sel2_subj<-dat[seq_len(dim(df1_sel2_subj)[1]),]
        df1_sel2_subj[is.na(df1_sel2_subj)] <- 0
        df1_sel2<-dat[-seq_len(dim(df1_sel2_subj)[1]),]
        df1_sel2[is.na(df1_sel2)] <- 1
        
        if(input$funct_SVM == "svm"){
            library(e1071)
            library(pryr)
            library(tibble)
            library(tidyr)
            library(tidyverse)
            library(gridExtra)
            library(grid)
            
            
            svm_lin1 <- SVMButton()$svm_lin1
            
            newdata<-df1_sel2_subj[,-1]
            svm_prediction<-predict(svm_lin1, newdata)
            
            svm_prediction <- as.data.frame(svm_prediction)
            rownames(svm_prediction)<-df1_sel2_subj$Populations
            colnames(svm_prediction)<-"SVM prediction"
            
            svm_prediction2<-predict(svm_lin1, newdata, decision.values=TRUE)
            results <- as.data.frame(attr(svm_prediction2, "decision.values"))
            rownames(results)=df1_sel2_subj$Populations

            library(plotly)
            x <- c(colnames(results))
            y <- c(as.numeric(results[1,]))
            text <- c(colnames(results))
            data <- data.frame(x, y, text)
            p <- plot_ly(data, x = ~x, y = ~y, type = 'bar', text = text,
                         marker = list(color = 'rgb(158,202,225)',
                                       line = list(color = 'rgb(8,48,107)',
                                                   width = 1.5))) %>%
                layout(title = "Decision values for the selected individual",
                       xaxis = list(title = ""),
                       yaxis = list(title = ""))
            colnames(data) <- c("Populations Comparisons", "SVM Decision Values", "text")
           
            p2<-ggplot(data=data, aes(x=x, y=y)) +
                geom_bar(stat="identity", fill="steelblue")+
                xlab("Compared Populations") + ylab("SVM Decision Values")+
                theme_minimal()
            p2
            
        } else {
           
            df1_sel2 <- df1_sel2()
            df1_sel2_subj <- df1_sel2_subj()
            Populations <- df1_sel2[,1]
            dat<-plyr::rbind.fill(df1_sel2_subj,df1_sel2)
            Populations<-dat[,1]
            dat<-dat[,-1]
            dat<-dat[,order(colnames(dat))]
            dat<-cbind(Populations,dat)
            df1_sel2_subj<-dat[seq_len(dim(df1_sel2_subj)[1]),]
            df1_sel2_subj[is.na(df1_sel2_subj)] <- 0
            df1_sel2<-dat[-seq_len(dim(df1_sel2_subj)[1]),]
            df1_sel2[is.na(df1_sel2)] <- 1
            
            pca.model <- PCAButton()$pca.model
            Populations <- PCAButton()$Populations
            
            x <- PCAButton()$plotta
            y <- PCAButton()$Populations
            dat <- cbind(y,x)
            data <- dat
            
            library(e1071)
            library(pryr)
            library(tibble)
            library(tidyr)
            library(tidyverse)
            library(gridExtra)
            library(grid)
            
            svm_lin1 <- SVMButton()$svm_lin1
            
            newdata <- as.data.frame(predict(pca.model, newdata=df1_sel2_subj[,-c(1)]))
            vec_PC <- PCAButton()$vec_PC
            colnames(newdata) <- vec_PC
            svm_prediction<-predict(svm_lin1, newdata)
            
            svm_prediction <- as.data.frame(svm_prediction)
            rownames(svm_prediction)<-df1_sel2_subj$Populations
            colnames(svm_prediction)<-"SVM prediction"
            
            svm_prediction2<-predict(svm_lin1, newdata, decision.values=TRUE)
            results <- as.data.frame(attr(svm_prediction2, "decision.values"))
            rownames(results)=df1_sel2_subj$Populations
            
            library(plotly)
            x <- c(colnames(results))
            y <- c(as.numeric(results[1,]))
            text <- c(colnames(results))
            data <- data.frame(x, y, text)
            p <- plot_ly(data, x = ~x, y = ~y, type = 'bar', text = text,
                         marker = list(color = 'rgb(158,202,225)',
                                       line = list(color = 'rgb(8,48,107)',
                                                   width = 1.5))) %>%
                layout(title = "Decision values for the selected individual",
                       xaxis = list(title = ""),
                       yaxis = list(title = ""))
            colnames(data) <- c("Populations Comparisons", "SVM Decision Values", "text")
            
            p2<-ggplot(data=data, aes(x=x, y=y)) +
                geom_bar(stat="identity", fill="steelblue")+
                xlab("Compared Populations") + ylab("SVM Decision Values")+
                theme_minimal()
            p2
            
        }
            
            list(data = data, 
                 p = p, svm_prediction = svm_prediction,
                 newdata = newdata, p2 = p2) 
      
    })
    
    output$SVM_results_prediction <- DT::renderDataTable({
        if (input$funct_SVM == "svm"){
            as.data.frame(Predict_SVMButton()$data[,c(1,2)])
        } else {
            as.data.frame(Predict_SVMButton()$data[,c(1,2)])
        }
    })
    
    # Plot of PCA scores plot 2D on unknowns
    output$SVM_prediction <- renderPlotly({
        Predict_SVMButton()$p
    })
    
   
    # Plot of PCA scores plot 2D on unknowns
    output$SVM_prediction_tab <- renderPrint({
        Predict_SVMButton()$svm_prediction
    })
    
    # Title above SVM prediction - decision values
    output$caption_SVM_prediction<-renderText({
        req(input$prediction_svm_button)
        "SVM prediction - decision values"
    })
    
    plotSVM1 <- eventReactive(input$prediction_svm_button,{
        req(input$prediction_svm_button)
        SVMButton()$a
    })
    # 
    
    plotSVM2 <- eventReactive(input$prediction_svm_button,{
        req(input$prediction_svm_button)
        Predict_SVMButton()$p2
    })
    
    prova1SVM <- eventReactive(input$prediction_svm_button,{
        prediction <- Predict_SVMButton()$svm_prediction
    })
    
    prova2SVM <- eventReactive(input$prediction_svm_button,{
        results <- Predict_SVMButton()$data[,c(1,2)]
    })
    
    prova3SVM <- eventReactive(input$prediction_svm_button,{
        newdata <- Predict_SVMButton()$newdata
    })
    # Write report for PLS-DA
    output$downloader_SVM <-
        downloadHandler(
            "results_from_shiny.pdf",
            content =
                function(file)
                {
                    rmarkdown::render(
                        input = "report_file3.Rmd",
                        output_file = "built_report_SVM.pdf",
                        params = list(plotSVM1 = plotSVM1(),
                                      plotSVM2 = plotSVM2(),
                                      prova1SVM = prova1SVM(),
                                      prova2SVM = prova2SVM(),
                                      prova3SVM = prova3SVM())
                    )
                    readBin(con = "built_report_SVM.pdf",
                            what = "raw",
                            n = file.info("built_report_SVM.pdf")[, "size"]) %>%
                        writeBin(con = file)
                }
        )

}
