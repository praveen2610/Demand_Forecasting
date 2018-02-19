library(e1071)
library(ggplot2)
library(GGally)
library(dplyr)
library(tree)
library(rpart)
library(rpart.plot)
library(lubridate)
library(shiny)
 shinyServer (function(input,output){
  
   #prediction reader dataset
   pravDataset<- read.csv('F:/My Assignments/project_Data_Binarised_playarea_V3.csv',header = T,stringsAsFactors = F)
  
   #Shiny reader dataset
   inputDataset2 <- read.csv('F:/My Assignments/project/DemandForecastingSystem/project_Data_Binarised_playarea_V3.csv',header = T,stringsAsFactors = F)
  
   testset<- read.csv('F:/My Assignments/test.csv')
  
  
  
  
  
   #use only numeric fields
   AttributesIntrainset<- c()
   for(i in 1:dim(pravDataset)[2])
   {
 	if(is.numeric(pravDataset[,i])|| is.integer(pravDataset[,i]))
 	{
   	AttributesIntrainset[i]=T
 	}
 	else
 	{
   	AttributesIntrainset[i]=F
 	}
   }
   updatedDatasetWithOnlyNumericValues <- na.omit(pravDataset[,AttributesIntrainset])
  
	
   set.seed(2)
   #split data into train and test
   train <- sample(dim(updatedDatasetWithOnlyNumericValues)[1],
               	dim(updatedDatasetWithOnlyNumericValues)[1]*0.70)
   temp_train <- updatedDatasetWithOnlyNumericValues[train,]
  
   temp_test <- updatedDatasetWithOnlyNumericValues[-train,]
  
   #declare function to calculate  root mean square error
  
   rmse <- function(error)
   {                            	
 	sqrt(mean(error^2))
   }
  
   #Linear  model
  
   #lmmodel <- lm(QUANTITYORDERED~QTR_ID+MONTH_ID+YEAR_ID+PRODUCTLINE
	#         	+CITY+PRODUCTCODE_Numeric+COUNTRY +TERRITORY+DAYS+DATE , temp_train)
  
   #predict from model
  # predictedlmQTY <- predict(lmmodel, temp_train)
  
   #calculate error
  # error <- temp_train$QUANTITYORDERED - predictedlmQTY
   #calculate rootmeansquare error
#   lmPredictionRMSE <- rmse(error) 
#   print(lmPredictionRMSE)
 	
	#tuneResult <- tune(svm, QUANTITYORDERED~QTR_ID+MONTH_ID+YEAR_ID+PRODUCTLINE
 	#             	+CITY+PRODUCTCODE_Numeric +COUNTRY +TERRITORY+DEALSIZE+DAYS+DATE,  data = temp_train,
  	#            	ranges = list(epsilon = 0.12, cost = 128))
	tuneResult <- tune(svm, QUANTITYORDERED~MONTH_ID+YEAR_ID+PRODUCTLINE
                   	+CITY+PRODUCTCODE_Numeric +DATE,  data = temp_train,
                   	ranges = list(epsilon = 0.12, cost = 128))
	
 	# ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(2:9)))
   tunedModel <- tuneResult$best.model
   predictedtunedModel <- predict(tunedModel, temp_train)
   error <- temp_train$QUANTITYORDERED - predictedtunedModel
   svrPredictionpredictedtunedModel <- rmse(error)
   #deployment and testing
 
   predictedQTYTestSet <- predict(tunedModel, testset)
  
  
  
  #Citylist dropdown
  cityList <- inputDataset2[row.names(unique(inputDataset2[,c("CITY.ID","CITY.NAME")])),]
  cityNames <- (cityList$CITY.NAME)
  #print (cityNames)
  output$cityOutput <- renderUI({
	selectInput("city_list", "City", (cityNames))
  })
 
  #productlist Dropdown
  productlist<-inputDataset2[row.names(unique(inputDataset2[,c("PRODUCTCODE_Numeric","PRODUCTCODE")])),]
  ProductNames <- (productlist$PRODUCTCODE)
  output$productcodeOutput <- renderUI({
	selectInput("produccode_list", "productcode", (ProductNames))
  })
  #Territory Dropdown
  territorylist<-inputDataset2[row.names(unique(inputDataset2[,c("TERRITORY_ID","TERRITORY")])),]
  TerritoryNames <- (territorylist$TERRITORY)
  output$territoryoutput <- renderUI({
	selectInput("TERRITORY_LIST", "TERRITORY", (TerritoryNames))
  })
  #ProductLine DropDown
  productlinelist<-inputDataset2[row.names(unique(inputDataset2[,c("PRODUCTLINE","PRODUCTLINE_ID")])),]
  ProductLineNames <- (productlinelist$PRODUCTLINE)
  output$ProductLineOutput <- renderUI({
	selectInput("PRODUCTLINE_LIST", "PRODUCTLINE", (ProductLineNames))
  })
 
 
  citySelection <- 1
 
 	output$cityid<-renderText({
  	if(input$action==0)
    	return()
  	else{
    	uniqueCityList<-inputDataset2[row.names(unique(inputDataset2[,c("CITY.ID","CITY.NAME")])),]
    	citySelection <- subset(uniqueCityList,uniqueCityList$CITY.NAME==input$city_list)
    	paste(citySelection$CITY.NAME)
  	}
  	
  }) 
	territorySelection <- 1
	
	output$territoryid<-renderText({
  	if(input$action==0)
    	return()
  	else{
    	print(input$TERRITORY_LIST)
    	print('here')
    	uniqueTerritoryList<-inputDataset2[row.names(unique(inputDataset2[,c("TERRITORY_ID","TERRITORY")])),]
    	territorySelection <- subset(uniqueTerritoryList,uniqueTerritoryList$TERRITORY==input$TERRITORY_LIST)
    	#print(currentSelection)
    	paste(territorySelection$TERRITORY)
  	}
  	
	}) 
	
	productcodeSelection <- 1
	output$productcodeop<-renderText({
  	if(input$action==0)
    	return()
  	else{
    	uniqueProductcodeList<-inputDataset2[row.names(unique(inputDataset2[,c("PRODUCTCODE","PRODUCTCODE_Numeric")])),]
    	productcodeSelection <- subset(uniqueProductcodeList,uniqueProductcodeList$PRODUCTCODE==input$produccode_list)
    	print(productcodeSelection)
    	paste(productcodeSelection$PRODUCTCODE)
  	}
  	
	}) 
	
 	
 	productlineSelection <- 1
 	output$productlineop<-renderText({
  	if(input$action==0)
    	return()
  	else{
    	uniqueProductLineList<-inputDataset2[row.names(unique(inputDataset2[,c("PRODUCTLINE","PRODUCTLINE_ID")])),]
    	productlineSelection <- subset(uniqueProductLineList,uniqueProductLineList$PRODUCTLINE==input$PRODUCTLINE_LIST)
    	print(unique(productlineSelection))
    	paste(unique(productlineSelection$PRODUCTLINE))
    	print(input$Month)
    	
    	testset$MONTH_ID =input$Month
    	print(input$TERRITORY_LIST)
    	
    	testset$YEAR_ID =input$YEAR
    	#print(input$PRODUCTLINE_LIST)
    	
    	
    	TList<-inputDataset2[row.names(unique(inputDataset2[,c("TERRITORY_ID","TERRITORY")])),]
    	TSelected <- subset(TList,TList$TERRITORY==input$TERRITORY_LIST)
    	print(TSelected$TERRITORY_ID)
    	#testset$TERRITORY=TSelected$TERRITORY_ID
    	
    	CList<-inputDataset2[row.names(unique(inputDataset2[,c("CITY.ID","CITY.NAME")])),]
    	CSelection<- subset(CList,CList$CITY.NAME==input$city_list)
    	print(CSelection$CITY.ID)
    	#testset$CITY =CSelection$CITY.ID
    	
    	
     	
    	PLList<-inputDataset2[row.names(unique(inputDataset2[,c("PRODUCTLINE","PRODUCTLINE_ID")])),]
    	PLSelection <- subset(PLList,PLList$PRODUCTLINE==input$PRODUCTLINE_LIST)
    	print(unique(PLSelection))
    	print(PLSelection$PRODUCTLINE_ID)
    	print(unique(PLSelection$PRODUCTLINE_ID))
    	#testset$PRODUCTLINE =PLSelection$PRODUCTLINE_ID
    	
    	PCList<-inputDataset2[row.names(unique(inputDataset2[,c("PRODUCTCODE","PRODUCTCODE_Numeric")])),]
    	pcSelection <- subset(PCList,PCList$PRODUCTCODE==input$produccode_list)
    	print(pcSelection$PRODUCTCODE_Numeric)
    	
    	#testset$PRODUCTCODE_Numeric =pcSelection$PRODUCTCODE_Numeric
    	print(testset)
     	#testset$DAYS
      	
    	
    	#testset$CITY <-numeric(testset$CITY)
    	#testset$PRODUCTCODE_Numeric <-numeric(testset$PRODUCTCODE_Numeric)
    	
    	#testset$PRODUCTLINE <-numeric(testset$PRODUCTLINE)
   	
    	print(testset)
    	
    	#predictedQTYTestSet <- predict(tunedModel, testset)
    	#print(predictedQTYTestSet)
  	}
  	
	})
	#CitySubset<- subset(StoreCitySubset,inputDataset2$CITY.NAME==input$city_list)
	#print("CITYIDDDDDDDD")
  
	
 
})

