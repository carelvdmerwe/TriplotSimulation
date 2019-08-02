

Install.packagesTriplot <- function()
{
    list.of.packages <- c("Matrix","caret", "polyclip","SDMTools", "rpart","shinyWidgets","Matrix","e1071","naivebayes","randomForest")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    
    library(caret)
    library(polyclip)
    library(SDMTools)
    library(rpart)
    library(shinyWidgets)
    library(MASS)
    library(Matrix)
    library(e1071)
    library(naivebayes)
    library(randomForest)
    library(Matrix)
    
    Plot.marker.new <<- source("source/Plot.marker.new.R")$value
    Draw.line2 <<- source("source/Draw.line2.R")$value
    Eigen.twosided  <<- source("source/Eigen.twosided.R")$value
    indmat <<- source("source/indmat.R")$value
    clipcords <<- source("source/clipcords.R")$value
    CVAbipl_C <<- source("source/CVAbipl_C.R")$value
    drawbipl.bagalpha_C <<- source("source/drawbipl.bagalpha_C.R")$value
    PCAbipl_C <<- source("source/PCAbipl_C.R")$value
    unioncords <<- source("source/unioncords.R")$value
    compute.bagplot_C <<- source("source/compute.bagplot_C.R")$value
    confmetrics <<- source("source/confmetrics.R")$value
    returnconfusion  <<- source("source/returnconfusion.R")$value
    bipldrawknn  <<- source("source/bipldrawknn.R")$value
    Draw.onecmline <<- source("source/Draw.onecmline.R")$value
    createdata2 <<- source("source/createdata2.R")$value
    DrawOrthogline <<- source("source/DrawOrthogline.R")$value
    
    column3C<<- read.csv("data/column3C.csv")
    graphdata<<- column3C
    legendtextin <<- c("Disk Hernia", "Spondylolisthesis", "Normal")
    bankrupt<<- read.csv("data/QualitativeBankruptcydata.csv")
    CopperFroth.datain <<- read.csv("data/CopperFroth.csv")
    Burrito.datain <<- read.csv("data/Burrito.csv")
}

matequal <<- function(x, y)
  (is.matrix(x) | is.data.frame(x)) && (is.matrix(y) | is.data.frame(y)) && dim(x) == dim(y) && all(x == y)

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################



Install.packagesTriplot()

ui <- tagList(
      navbarPage(
        theme = "yeti", 
        "Triplot",
        tabPanel("",
                 sidebarPanel(
                   radioButtons("points", "Data set to plot:", selected = "Testing",inline = TRUE,
                                choiceNames = c("Training", "Testing", "Single Point", "None"),
                                choiceValues = c("Training", "Testing", "SP", "None")),
                   tabsetPanel(
                     tabPanel("Triplot settings",
                              helpText(" "),
                              radioButtons("bipltype", "Underlying biplot used:", selected = "CVA",inline = TRUE,
                                           choices =c("PCA", "CVA", "AOD")),
                              radioButtons("method", "Underlying classification method used:", selected = "knn", 
                                           choiceNames = c("k-nearest neighbour", "Linear Discrimant Analysis","Quadratic Discrimant Analysis", "Naive Bayes with Gaussian model",
                                                           "Naive Bayes with kernel density estimates", "Multinomial", "Support Vector Machine with polynomial kernel",
                                                           "Support Vector Machine with Gaussian kernel", "CART", "Bagging", "Random Forest"),
                                           choiceValues = c("knn","lda", "qda", "nbgm", "nbkd","mn","svmpk","svmgk","cart", "bagging", "rf")),
                              sliderInput("knnin", "k used for underlying KNN:", min = 1, max = 51, value = 21, step = 5),
                              checkboxInput("innerpolybaginclude", label = strong("Inner polybag:"), value = TRUE),
                              sliderInput("innerpoly", "", min = 0, max = 99, value = 85, step = 5, post = "%"),
                              checkboxInput("outerpolybaginclude", label = strong("Outer polybag:"), value = TRUE),
                              fluidRow(
                                column(4,numericInput("outermult",label="",value = 1.5)),
                                column(8,sliderInput("outerpoly",label="", min = 0, max = 99, value = 95, step = 5, post= "%"))
                              ),
                              checkboxInput("plotbags", label = strong("Plot alpha bags used for polybag construction:"), value = FALSE),
                              pickerInput("evinput", "Eigenvectors to use:", choices = 1:6, selected = c(1,2),multiple = TRUE,options = list(`max-options` = 2)),
                              sliderInput("nbp", label = "Classification grid size:",min = 20, max = 300, step = 20, value = 200),
                              sliderInput("expin", label = "Zoom value of triplot:",min = 1, max = 8, step = 0.5, value = 1.15),
                              sliderInput("nintint", label = "Number of points in axes (use if n.int error is obtained):",min = 5, max = 200, step = 5, value = 10)),
                     
                    tabPanel("Data",
                    tabsetPanel(id = "tab",
                     tabPanel("Vertrabral",
                            helpText(" "),
                            #radioButtons("Csamp", "Test sample to plot:", selected = "PS",inline = TRUE,
                            #             choiceValues =c("PS", "SP"), choiceNames = c("Percentage Sample","Single Point")),
                            sliderInput("percentagesample3col", "Percentage test data:", min = 1, max = 50, value = 25, step=5),
                            sliderInput("randomseed3c", "Random seed used:", min = 1, max = 1000, value = 3, step=1),
                            radioButtons("CTV","Observed indicator", selected=NA,choiceNames=c("Disk Hernia (1)", "Spondylolisthesis (2)", "Normal (3)","Unknown (NA)"),
                                         choiceValues = c(1,2,3,NA),inline = TRUE),
                            sliderInput("CV1", "Pelvic incidence (V1):", min = 25, max = 135, value = 33, step = 0.01),
                            sliderInput("CV2", "Pelvic tilt (V2):", min = -10, max = 55, value = -7, step = 0.01),
                            sliderInput("CV3", "Lumbar lordosis angle (V3):", min = 10, max = 130, value = 37, step = 0.01),
                            sliderInput("CV4", "Sacral slope (V4):", min = 10, max = 125, value = 29, step = 0.01),
                            sliderInput("CV5", "Pelvic radius (V5):", min = 65, max = 170, value = 124, step = 0.01),
                            sliderInput("CV6", "Spondylolisthesis (V6):", min = -15, max = 425, value = 0, step = 0.01)),
                    tabPanel("Bankruptcy",
                             helpText(""),
                             #radioButtons("Bsamp", "Test sample to plot:", selected = "PS",inline = TRUE,
                            #              choiceValues =c("PS", "SP"), choiceNames = c("Percentage Sample","Single Point")),
                             sliderInput("percentagesampleB", "Percentage test data:", min = 1, max = 50, value = 25, step=5),
                             sliderInput("randomseedB", "Random seed used:", min = 1, max = 1000, value = 3, step=1),
                             radioButtons("BTV","Observed indicator", selected=NA,choiceNames=c("Non-Bankruptcy (1)", "Bankruptcy (2)","Unknown (NA)"),
                                          choiceValues = c(1,2,NA),inline = TRUE),
                             radioButtons("BV1","Industry risk (IR)", selected=0,choiceNames=c("Negative (-1)", "Average (0)","Positive (+1)"),
                                          choiceValues = c(-1,0,1),inline = TRUE),
                             radioButtons("BV2","Management risk (MR)", selected=0,choiceNames=c("Negative (-1)", "Average (0)","Positive (+1)"),
                                          choiceValues = c(-1,0,1),inline = TRUE),
                             radioButtons("BV3","Financial Flexibility (FF)", selected=0,choiceNames=c("Negative (-1)", "Average (0)","Positive (+1)"),
                                          choiceValues = c(-1,0,1),inline = TRUE),
                             radioButtons("BV4","Credibility (CR)", selected=0,choiceNames=c("Negative (-1)", "Average (0)","Positive (+1)"),
                                          choiceValues = c(-1,0,1),inline = TRUE),
                             radioButtons("BV5","Competitiveness (CO)", selected=0,choiceNames=c("Negative (-1)", "Average (0)","Positive (+1)"),
                                          choiceValues = c(-1,0,1),inline = TRUE),
                             radioButtons("BV6","Operating Risk (OP)", selected=0,choiceNames=c("Negative (-1)", "Average (0)","Positive (+1)"),
                                          choiceValues = c(-1,0,1),inline = TRUE)),
                    tabPanel("Copper Froth",
                             helpText(""),
                             #radioButtons("Fsamp", "Test sample to plot:", selected = "PS",inline = TRUE,
                            #              choiceValues =c("PS", "SP"), choiceNames = c("Percentage Sample","Single Point")),
                             sliderInput("percentagesampleF", "Percentage test data:", min = 1, max = 50, value = 25, step=5),
                             sliderInput("randomseedF", "Random seed used:", min = 1, max = 1000, value = 3, step=1),
                             radioButtons("FTV","Observed indicator", selected=NA,choiceNames=c("Ideal froth structure (1)", "Polyhedral bubbles (2)", "Ellipsoidal bubbles (3)", "Irregular froth structure (4)", "Depleted froth (5)","Unknown (NA)"),
                                          choiceValues = c(1,2,3,4,5,NA)),
                             sliderInput("FV1", "LNE (V1):", min = 30, max = 35, value = 32.5, step = 0.001),
                             sliderInput("FV2", "NNU (V2):", min = 6000, max = 7850, value = 6925, step = 0.01),
                             sliderInput("FV3", "SM (V3):", min = 225, max = 520, value = 300, step = 0.01),
                             sliderInput("FV4", "E_N (V4):", min = -6, max = -4, value = -5, step = 0.01),
                             sliderInput("FV5", "ENERGY (V5):", min = 1, max = 23, value = 10, step = 0.01),
                             sliderInput("FV6", "ENTROPY (V6):", min = -20, max = -1, value = -5, step = 0.01),
                             sliderInput("FV7", "INERTIA (V6):", min = 9000, max = 40000, value = 25000, step = 0.01),
                             sliderInput("FV8", "RA (V6):", min = 0.01, max = 2, value = 1, step = 0.01)),
                    
                   tabPanel("Simulation",
                            helpText(" "),
                            sliderInput("percentagesamplesim", "Percentage test data:", min = 1, max = 50, value = 25, step=5),
                            sliderInput("randomseedsim", "Random seed used for data creation:", min = 1, max = 1000, value = 3, step=1),
                            sliderInput("randomseedsimsamp", "Random seed used for test sample:", min = 1, max = 1000, value = 3, step=1),
                            radioButtons("means", "Type of Means:", selected="Sep",
                                         choiceValues = c("Sep","Overlap"),
                                         choiceNames  = c("Seperate", "Overlapping")),
                            radioButtons("cv", "Coefficient of variances:", selected="SL",
                                         choiceValues = c("SL","VH"),
                                         choiceNames  = c("Small and Low", "High and Variable")),
                            radioButtons("classsizes", "Class sizes:", selected="Even",
                                         choices = c("Even","Skewed")),
                            radioButtons("corr", "Correlation structure:", selected="Low",
                                         choiceValues = c("Low","HPos","HNeg"),
                                         choiceNames  = c("Low", "High positive", "High negative"))
                            ),
                   tabPanel("Burrito",
                            helpText(""),
                            sliderInput("percentagesampleU", "Percentage test data:", min = 1, max = 50, value = 25, step=5),
                            sliderInput("randomseedU", "Random seed used:", min = 1, max = 1000, value = 3, step=1),
                            radioButtons("UTV","Observed indicator", selected=NA,choiceNames=c("2/5", "3/5", "4/5", "5/5", "Unknown"),
                                         choiceValues = c(1,2,3,4,NA),inline = TRUE),
                            radioButtons("UV1","Beef", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV2","Pico", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV3","Guac", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV4","Cheese", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV5","Fries", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV6","Sour cream", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV7","Pork", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV8","Chicken", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV9","Shrimp", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV10","Fish", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV11","Rice", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV12","Beans", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV13","Lettuce", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV14","Tomato", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV15","Bell peper", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV16","Carrots", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV17","Cabbage", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV18","Sauce", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV19","Salsa", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV20","Cilantro", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV21","Onion", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV22","Taquito", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV23","Pineapple", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV24","Ham", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV25","Chile relleno", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV26","Egg", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV27","Mushroom", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV28","Bacon", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE),
                            radioButtons("UV29","Sushi", selected=0,choiceNames=c("Exclude", "Include"), choiceValues = c(0,1),inline = TRUE)
                            ),
                   tabPanel("Raw",
                            helpText(" ") ,
                            fileInput("file1", "Choose CSV File for training set",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            fileInput("file2", "Choose CSV File for testing set",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")))
                   
                 )))),
                 mainPanel(helpText(" "),
                              tabsetPanel(
                            tabPanel("Triplot", plotOutput(outputId = "main",height=600,width=600)),
                            tabPanel("Accuracy measures",helpText(" "),tableOutput("errormetrics")), 
                            tabPanel("Classification values", tableOutput("classvals"))))
        )
    )
)




server <- function(input,output,session) 
{
  rv <<- reactiveValues()
  output$main <- renderPlot({
    tryCatch(
      {
    if(!is.null(input$file1)) actualtraindat <<- read.csv(input$file1$datapath)
    if(!is.null(input$file2)) actualtestdat <<- read.csv(input$file2$datapath)
      },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
    )
  choice <<- input$tab
  if(choice == "Raw")
  {
  #  set.seed(3)
  #  #trainact<<-sort(samp)
  #  if(matequal(graphdata,CopperFroth.datain)==FALSE)
  #  {
  #  graphdata <<- CopperFroth.datain
  #  legendtextin <<- paste0("Group ",1:5)
  #  graphdatatest <<- CopperFroth.datain
  #  updatePickerInput(session, "evinput", choices = 1:ncol(graphdata), selected = c(1,2))
  #  }
    if(!is.null(input$file1))
    {
      #colnames(actualtraindat) <<- c("response",paste0("V",1:(dim(actualtraindat)[2]-1)))
      if(matequal(graphdata,actualtraindat)==FALSE)
      {
      graphdata <<- actualtraindat
      legendtextin <<- NULL
      graphdatatest <<- actualtraindat #matrix(rep(0,dim(graphdata)[2]),nrow=1)
      updatePickerInput(session, "evinput", choices = 1:ncol(graphdata), selected = c(1,2))
      }
    }
    if(!is.null(input$file2) & !is.null(input$file1))
    {
      #colnames(actualtestdat) <<- c("response",paste0("V",1:(dim(actualtestdat)[2]-1)))
      graphdatatest <<- actualtestdat
    }
    
  }
  
  seedrscol <<- input$randomseed3c
  seedrsB <<- input$randomseedB
  seedrsF <<- input$randomseedF
  seedrsU <<- input$randomseedU
  percentagesample3col <<- (input$percentagesample3col)/100
  percentagesampleB <<- (input$percentagesampleB)/100
  percentagesampleF <<- (input$percentagesampleF)/100
  percentagesampleU <<- (input$percentagesampleU)/100
  
  if(choice == "Vertrabral" & input$points != "SP")
  {
    set.seed(seedrscol)
    samp <- sample(1:(dim(column3C)[1]),(dim(column3C)[1])*(1-percentagesample3col))
    train3col<<-sort(samp)
    if(matequal(graphdata,column3C[train3col,])==FALSE)
    {
    graphdata <<- column3C[train3col,]
    legendtextin <<- c("Disk Hernia", "Spondylolisthesis", "Normal")
    graphdatatest <<- column3C[-train3col,]
    updatePickerInput(session, "evinput", choices = 1:ncol(graphdata), selected = c(1,2))
    }
    
  }
  
  if(choice == "Vertrabral" & input$points == "SP")
  {
    #updateRadioButtons(session, "points", selected = "Testing")
    if(matequal(graphdata,column3C)==FALSE)
    {
      graphdata <<- column3C
      legendtextin <<- c("Disk Hernia", "Spondylolisthesis", "Normal")
      updatePickerInput(session, "evinput", choices = 1:ncol(graphdata), selected = c(1,2))
    }
    graphdatatest <<- column3C[1,]
    graphdatatest[1,] <<- c(as.numeric(input$CTV), as.numeric(input$CV1), as.numeric(input$CV2), as.numeric(input$CV3), as.numeric(input$CV4), 
                            as.numeric(input$CV5), as.numeric(input$CV6))
  }

  if(choice == "Bankruptcy" & input$points == "SP")
  {
    #updateRadioButtons(session, "points", selected = "Testing")
    if(matequal(graphdata,bankrupt)==FALSE)
    {
    graphdata <<- bankrupt
    legendtextin <<- c("Non-Bankruptcy", "Bankruptcy")
    updatePickerInput(session, "evinput", choices = 1:ncol(graphdata), selected = c(1,2))
    }
    graphdatatest <<- bankrupt[1,]
    graphdatatest[1,] <<- c(as.numeric(input$BTV), as.numeric(input$BV1), as.numeric(input$BV2), as.numeric(input$BV3), as.numeric(input$BV4), 
                            as.numeric(input$BV5), as.numeric(input$BV6))
  }
  
  
  if(choice == "Bankruptcy" & input$points != "SP")
  {
    set.seed(seedrsB)
    samp <- sample(1:(dim(bankrupt)[1]),(dim(bankrupt)[1])*(1-percentagesampleB))
    trainB<<-sort(samp)
    if(matequal(graphdata,bankrupt[trainB,])==FALSE)
    {
      graphdata <<- bankrupt[trainB,]
      legendtextin <<- c("Non-Bankruptcy", "Bankruptcy")
      graphdatatest <<- bankrupt[-trainB,]
      updatePickerInput(session, "evinput", choices = 1:ncol(graphdata), selected = c(1,2))
    }
    
  }
  
  if(choice == "Copper Froth" & input$points == "SP")
  {
    if(matequal(graphdata,CopperFroth.datain)==FALSE)
    {
      graphdata <<- CopperFroth.datain
      legendtextin <<- c("Ideal froth str.", "Polyhedral bubbles", "Ellipsoidal bubbles", "Irregular froth str.", "Depleted froth")
      updatePickerInput(session, "evinput", choices = 1:ncol(graphdata), selected = c(1,2))
    }
    graphdatatest <<- CopperFroth.datain[1,]
    graphdatatest[1,] <<- c(as.numeric(input$FTV), as.numeric(input$FV1), as.numeric(input$FV2), as.numeric(input$FV3), as.numeric(input$FV4), 
                            as.numeric(input$FV5), as.numeric(input$FV6), as.numeric(input$FV7), as.numeric(input$FV8))
  }
  
  
  if(choice == "Copper Froth" & input$points != "SP")
  {
    set.seed(seedrsF)
    samp <- sample(1:(dim(CopperFroth.datain)[1]),(dim(CopperFroth.datain)[1])*(1-percentagesampleF))
    trainF<<-sort(samp)
    if(matequal(graphdata,CopperFroth.datain[trainF,])==FALSE)
    {
      graphdata <<- CopperFroth.datain[trainF,]
      legendtextin <<- c("Ideal froth str.", "Polyhedral bubbles", "Ellipsoidal bubbles", "Irregular froth str.", "Depleted froth")
      graphdatatest <<- CopperFroth.datain[-trainF,]
      updatePickerInput(session, "evinput", choices = 1:ncol(graphdata), selected = c(1,2))
    }
    
  }
  
  if(choice == "Burrito" & input$points == "SP")
  {
    if(matequal(graphdata,Burrito.datain)==FALSE)
    {
      graphdata <<- Burrito.datain
      legendtextin <<- c("2/5", "3/5", "4/5", "5/5")
      updatePickerInput(session, "evinput", choices = 1:ncol(graphdata), selected = c(1,2))
    }
    graphdatatest <<- Burrito.datain[1,]
    graphdatatest[1,] <<- c(as.numeric(input$UTV), as.numeric(input$UV1), as.numeric(input$UV2), as.numeric(input$UV3), as.numeric(input$UV4), as.numeric(input$UV5), as.numeric(input$UV6), as.numeric(input$UV7), as.numeric(input$UV8), as.numeric(input$UV9), as.numeric(input$UV10), as.numeric(input$UV11), as.numeric(input$UV12), as.numeric(input$UV13), as.numeric(input$UV14), as.numeric(input$UV15), as.numeric(input$UV16), as.numeric(input$UV17), as.numeric(input$UV18), as.numeric(input$UV19), as.numeric(input$UV20), as.numeric(input$UV21), as.numeric(input$UV22), as.numeric(input$UV23), as.numeric(input$UV24), as.numeric(input$UV25), as.numeric(input$UV26), as.numeric(input$UV27), as.numeric(input$UV28), as.numeric(input$UV29))
  }
  
  
  if(choice == "Burrito" & input$points != "SP")
  {
    set.seed(seedrsU)
    samp <- sample(1:(dim(Burrito.datain)[1]),(dim(Burrito.datain)[1])*(1-percentagesampleU))
    trainU<<-sort(samp)
    if(matequal(graphdata,Burrito.datain[trainU,])==FALSE)
    {
      graphdata <<- Burrito.datain[trainU,]
      legendtextin <<-  c("2/5", "3/5", "4/5", "5/5")
      graphdatatest <<- Burrito.datain[-trainU,]
      updatePickerInput(session, "evinput", choices = 1:ncol(graphdata), selected = c(1,2))
    }
    
  }
  
  
  ####SIMULATED DATA###############
  seedrssim <<- input$randomseedsimsamp
  seedsim <<- input$randomseedsim
  percentagesamplesim <<- (input$percentagesamplesim)/100
  
  MeansSim <<- input$means
  MeansSimA <<- c(-7.5,-2.5)
  MeansSimB <<- c(-2.5,2.5)
  MeansSimC <<- c(2.5,7.5)
  MeansSimOverlap <<- c(-5,5)
  CVarSim <<-  input$cv
  CVarSimSL <<- c(0.5,1)
  CVarSimVH <<- c(2,5)
  ClassSizeSim <<- input$classsizes
  ClassSizeSimE <<- c(1/3,1/3,1/3)
  ClassSizeSimS <<- c(0.6,0.4,0.2)
  CorrelationSim <<- input$corr
  CorrelationSimL <<- c(-0.3,0.3)
  CorrelationSimHP <<- c(0.5,0.9)
  CorrelationSimHN <<- c(-0.9,-0.5)
  
  #DataSimMat <<- as.data.frame(matrix(rep(NA, (1)*(3*4+3*4+3+6*3)),nrow=(1)))
  

  if(choice == "Simulation")
  {
    set.seed(seedsim)
    if(MeansSim == "Sep")  Meansimin <<- c(runif(4,MeansSimA[1],MeansSimA[2]), runif(4,MeansSimB[1],MeansSimB[2]), runif(4,MeansSimC[1],MeansSimC[2]))
    set.seed(seedsim)
    if(MeansSim=="Overlap")  Meansimin <<- runif(3*4,MeansSimOverlap[1],MeansSimOverlap[2])
    set.seed(seedsim)
    if(CVarSim=="SL") CVsimin <<- runif(3*4,CVarSimSL[1],CVarSimSL[2])
    set.seed(seedsim)
    if(CVarSim=="VH") CVsimin <<- runif(3*4,CVarSimVH[1],CVarSimVH[2])
    stdevsin <<- abs((Meansimin * CVsimin))
    set.seed(seedsim)
    if(ClassSizeSim == "Even") ClassSizeSimin <<- ClassSizeSimE[sample(1:3,3,replace=F)]
    set.seed(seedsim)
    if(ClassSizeSim == "Skewed") ClassSizeSimin <<- ClassSizeSimS[sample(1:3,3,replace=F)]
    set.seed(seedsim)
    if(CorrelationSim=="Low")  CorrSimin <<- runif(6*3,CorrelationSimL[1],CorrelationSimL[2])
    set.seed(seedsim)
    if(CorrelationSim=="HPos") CorrSimin <<- runif(6*3,CorrelationSimHP[1],CorrelationSimHP[2])
    set.seed(seedsim)
    if(CorrelationSim=="HNeg") CorrSimin <<- runif(6*3,CorrelationSimHN[1],CorrelationSimHN[2])
    DataSimVEC <<- c(Meansimin, stdevsin, ClassSizeSimin,CorrSimin)
    datasimcreate <<- createdata2(Meansin = list(as.numeric(DataSimVEC[1:4]),as.numeric(DataSimVEC[5:8]), as.numeric(DataSimVEC[9:12])), 
                                  varin = list(as.numeric(DataSimVEC[13:16]),as.numeric(DataSimVEC[17:20]), as.numeric(DataSimVEC[21:24])),
                                  portin = as.numeric(DataSimVEC[25:27]), 
                                  corrin = list(as.numeric(DataSimVEC[28:33]),as.numeric(DataSimVEC[34:39]), as.numeric(DataSimVEC[40:45])), seedin = seedsim)
    
    set.seed(seedrssim)
    samp <- sample(1:(dim(datasimcreate)[1]),(dim(datasimcreate)[1])*(1-percentagesamplesim))
    trainsim<<-sort(samp)
    if(matequal(graphdata,datasimcreate[trainsim,])==FALSE)
    {
    graphdata <<- datasimcreate[trainsim,]
    legendtextin <<- NULL
    graphdatatest <<- datasimcreate[-trainsim,]
    updatePickerInput(session, "evinput", choices = 1:ncol(graphdata), selected = c(1,2))
    }
  }
  
  ####INPUTS TO BIPLOT##############
  
  alphaintriplot <<- (input$innerpoly)/100
  knnintriplot <<- input$knnin
  methodstriplot <<- input$method
  bipltype <<- input$bipltype
  
  plotouter <<- input$outerpolybaginclude
  plotinner <<-input$innerpolybaginclude
  plotbags<<- T
  plotcont <<- T
  nolines <<- !(input$plotbags) #make sure everyting else is true if this is to be True
  nospots <<- (input$points == "Testing" | input$points == "None" | input$points == "SP")
  nospotstest <<- (input$points == "Training" | input$points == "None")
  
  expinputinput <<- input$expin
  evininput <<- as.numeric(input$evinput)
  hullmultininput <<- c(input$outermult, (input$outerpoly)/100)
  gridininput <<- input$nbp
  nintinput <<- input$nintint
  
  visualsettings <<- c(plotouter, plotinner, plotbags, plotcont, nolines, nospots, nospotstest)

  rv$biplout <<- bipldrawknn(datatrain = graphdata, datatest = graphdatatest, alphin = alphaintriplot, 
                         type = bipltype, kin = knnintriplot, visualsettings,method=methodstriplot,
                         expininput = expinputinput, evin = evininput, hullmultin = hullmultininput, gridin = gridininput,
                         nintint = nintinput, legendtextinp = legendtextin)
  })
  ###ERROR MATRIX CALCULATIONS#########
  
  output$errormetrics <- renderTable({

    ResultsConfusion <- matrix(rep(0,17*1),ncol=1)
    rownames(ResultsConfusion) <- c("Prevalance (+/-)","Accuracy (+)","Precision (+)","False discovery rate (-)","False omission rate (-)","Negative predicted value (+)",
                                    "True positive rate (+)","False positive rate (-)","False negative rate (-)", "True negative rate (+)","Positive Likelihood ratio (+)",
                                    "Negative likelihood ratio (-)","Diagnostic odds ratio (+)","F1 Score (+)","Total Accuracy (+)","Non-Classifiable (+/-)","Total Misclass (+)")
    colnames(ResultsConfusion) <- c("Triplot")
    
    ###TRIPLOT ERROR CALCULATION#########
    
    biploutput <<- rv$biplout
    a<- as.factor(graphdatatest[,1]) #true test values 
    b<- as.factor(biploutput$tempoutput)   #this provides the classified vector for the triplot
    newconfout <- rbind((confusionMatrix(b,a)$table),table(a[is.na(b)]))
    ResultsConfusion[,1] <- round(confmetrics(newconfout),4)*100
    classpercentage <- as.numeric(as.vector(rv$biplout$ValdateV[6]))
    
    
    ###BLACK BOX ERROR CALCULATION#######
    
    if(methodstriplot == "knn") Errs <- returnconfusion(sprintf("KNN with k=%i", knnintriplot), 
                                                        "response ~ .","knn", tuneGrid = data.frame(k = c(knnintriplot)),datainp = graphdata , datatest = graphdatatest,
                                                    NApercentage = classpercentage)
    if(methodstriplot == "lda") Errs <- returnconfusion("Linear Discrimant Analysis", "response ~ . ", "lda",datainp = graphdata , datatest = graphdatatest, NApercentage = classpercentage)
    
    if(methodstriplot == "qda") Errs <- returnconfusion("Quadratic Discrimant Analysis", "response ~ . ", "qda",datainp = graphdata , datatest = graphdatatest, NApercentage = classpercentage)
  
    if(methodstriplot == "nbgm") Errs <- returnconfusion("Naive Bayes with Gaussian model", "response ~ .", "naive_bayes",
                                                         tuneGrid = data.frame(usekernel = c(FALSE), laplace = c(0),adjust=c(0)),datainp = graphdata , datatest = graphdatatest,
                                                         NApercentage = classpercentage)
    if(methodstriplot == "nbkd") Errs <- returnconfusion("Naive Bayes with kernel density estimates", "response ~ .", "naive_bayes",
                                                         tuneGrid = data.frame(usekernel = c(TRUE), laplace = c(0),adjust=c(0)),datainp = graphdata , datatest = graphdatatest,
                                                         NApercentage = classpercentage)
    if(methodstriplot == "mn") Errs <- returnconfusion("Multinomial", "response ~ .", "multinom",datainp = graphdata , datatest = graphdatatest,
                                                       NApercentage = classpercentage) 
    if(methodstriplot == "svmpk") Errs <- returnconfusion("Support Vector Machine with polynomial kernel", 
                                                          "response ~ .", "svmPoly",datainp = graphdata , datatest = graphdatatest,
                                                          NApercentage = classpercentage)
    if(methodstriplot == "svmgk") Errs <- returnconfusion("Support Vector Machine with Gaussian kernel", 
                                                          "response ~ .", "svmRadial",datainp = graphdata , datatest = graphdatatest,
                                                          NApercentage = classpercentage)
    if(methodstriplot == "cart") Errs <- returnconfusion("CART", "response ~ .", "rpart", 
                                                         control = rpart.control(minsplit = 5, cp = 0), tuneGrid = data.frame(cp = .02),datainp = graphdata , datatest = graphdatatest,
                                                         NApercentage = classpercentage)
    if(methodstriplot == "bagging") Errs <- returnconfusion("Bagging", "response ~ .", "treebag", 
                                                            control = rpart.control(minsplit = 5),datainp = graphdata , datatest = graphdatatest,
                                                            NApercentage = classpercentage)
    if(methodstriplot == "rf") Errs <- returnconfusion("Random Forest", "response ~ .", "rf",
                                                       tuneLength = 1, 
                                                       control = rpart.control(minsplit = 5),datainp = graphdata , datatest = graphdatatest,
                                                       NApercentage = classpercentage)
    ResultsConfusion <- cbind(ResultsConfusion, round(Errs,4)*100)
    colnames(ResultsConfusion)[2] <- "Black box"
    Resultsdisplay <- ResultsConfusion[c(15,	16,	17,	3,	4,	5,	6,	7,	9,	8,	10),]
    Resultsdisplay
  },
  rownames  = TRUE, align = 'lcc')
  
  output$classvals <- renderTable({rv$biplout$datatestout }, rownames = TRUE)
}

shinyApp(ui=ui, server = server)


