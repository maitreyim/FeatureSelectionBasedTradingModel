# ==========================================================================#
#-- Project Name :  Multivariate Stock Price Movement Model                                                    
#-- Task         :  Write a Pairs Trading Strategy     
#-- version      :  4.0
#-- Date         :  25/MARCH/2014
#-- Author       :  Maitreyi Mandal   
#-- SVN Directory:  \xxxx           
# ==========================================================================#
# Code to charge data frame from the data streamed from db
# DataFrame size
#Defining Global Values
zd<-NULL
newdata<-NULL
frameSize <- 5000 # FrameSize
nSymbols <- length(symbolVec)#TickerId
SFAS<-0 # Counter to keep track of number of JPM Shares at the end
#PNL<-0 # Profit & Loss
#OJPM<-0
#OGS<-0
threshold<-0.08
PNL<-0
#Added extra column for time 
dfMatrix <- matrix(-1,nrow=frameSize,ncol=nSymbols+1)
singleRow <- matrix(-1,nrow=1,ncol=nSymbols)
fillCount <- 1
iterC <- 1
tradeTime <- "\"NULL\""
firstcall<-TRUE
options(fftempdir = "C:/Users/Maitreyi.Mandal/Desktop/R")
require(aod)
# Data charging function
dfChargeUsePrev <- function(tickerId, askPrice, tradeTime){
  temp <- gsub('.*:','', tradeTime)
  tradeTime <- sub(paste(':',temp,sep=""),paste(':',temp,sep=""), tradeTime)
  if(!is.na(askPrice)){
    symIndex <- which(symbolVec == tickerId)
    #First fill row no 1 in the matrix
    if(is.element(-1,as.vector(dfMatrix[1,]))){
      dfMatrix[1,symIndex] <<- askPrice
      dfMatrix[1,(nSymbols+1)] <<- as.POSIXct(strptime(tradeTime,"%Y-%m-%d %H:%M:%OS"))
      dFrame = data.frame(dfMatrix)
      colnames(dFrame) <- c(symbolVec,'Time')
      return(list("dFrame"=dFrame,"full"=FALSE))
    }
    #We have a complete row now
    else{
      if(fillCount == frameSize){
        #Pop the first element and insert at bottom
        tVec <- dfMatrix[fillCount[1],]
        dfMatrix[1:frameSize-1,] <<- dfMatrix[2:frameSize,]
        dfMatrix[frameSize,] <<- tVec
        dfMatrix[frameSize,symIndex] <<- askPrice
        dfMatrix[frameSize,(nSymbols+1)] <<- as.POSIXct(strptime(tradeTime,"%Y-%m-%d
                                                                 %H:%M:%OS"))
        dFrame <- data.frame(dfMatrix)
        colnames(dFrame) <- c(symbolVec,'Time')
        return(list("dFrame"=dFrame,"full"=TRUE))
      }
      else{
        #Add the element
        fillCount <<- fillCount + 1
        #Replicate previous value
        dfMatrix[fillCount,] <<- dfMatrix[fillCount-1,]
        dfMatrix[fillCount,symIndex] <<- askPrice
        dfMatrix[fillCount,(nSymbols+1)] <<- as.POSIXct(strptime(tradeTime,"%Y-%m-%d
                                                                 %H:%M:%OS"))
        if(fillCount == frameSize){
          dFrame <- data.frame(dfMatrix)
          colnames(dFrame) <- c(symbolVec,'Time')
          return(list("dFrame"=dFrame,"full"=TRUE))
        }
        else{
          dFrame = data.frame(dfMatrix)
          colnames(dFrame) <- c(symbolVec,'Time')
          return(list("dFrame"=dFrame,"full"=FALSE))
        }
      }
    }
  }
}

### ----------------------------------------------------------------------
###Function For Variable Ranking
###-----------------------------------------------------------------------
library(caret)
variableRanking<-function(x, y, method = c("nnet","rpart","gbm","blackboost",
                                           "glmboost","gamboost","treebag","pls","lm",
                                           "svmRadial","svmPoly","gaussprRadial","gaussprPoly","lasso",
                                           "rf","mars","enet","rvmRadial","rvmPoly","lda","multinom","rda",
                                           "fda","sddaLDA","sddaQDA","bagEarth","ctree","cforest",
                                           "nb","gpls","lvq"),
                          plot=F, ...)
{
  #Resetting an error flag
  errFlag<-0
  
  #Get the method argument in the function
  methodType<-match.arg(method)
  
  if(methodType == "mars")
  {
    methodType<-"earth"
  }
  
  tryCatch({
    modelFit<-train(x,y,method=methodType,...)
    varImportance<-varImp(modelFit)
  },error=function(e)
  {
    #message("The error is:")
    #message(e)
    errFlag<<-1
  })
  if(errFlag==1)
  {
    #message("The Tuning Parameters for the model are not set properly or the dataset provided is not proper. Please refer to the caret package and the specific model packages for details")
    return()
  }
  
  #Check for plotting a dotchart
  if(plot)
  {
    modProfile<-varImportance$importance
    featureVec<-modProfile[order(modProfile[,1],decreasing=T),1]
    names(featureVec)<-row.names(modProfile)[order(modProfile[,1],decreasing=T)]
    par(mgp=c(0.3,1.3,0))
    dotchart(rev(featureVec), cex = 0.4, main = "Variable Ranking values")
  } 
  #Return the importance values of the features in order of importance
  return(varImportance)
}

#________________________________________________________________
# Function to create a data frame consisting of all ticker information
#________________________________________________________________

MultivariateMain<-function(){
  while (TRUE){
    t1 <- as.character(format(Sys.time(),"%Y-%m-%d %H:%M:%OS6"))
    streamData <- streamDataFn()
    
    d1<-unlist(strsplit(streamData,split=",")[[1]])
    #dfMatrix10 <<- matrix(-1,nrow=frameSize10,ncol=nSymbols10+1)
    
    if(length(d1)==5 && length(streamData)!=0 && is.element(d1[1],symbolVec)){
      askPrice<-as.numeric(d1[3])
      bidPrice<-as.numeric(d1[4])
      tickerId<-d1[1]
      tradeTime<-d1[2]
      timChk<-d1[5]
    }
    ## Square off the trade before mkt closes 
    if(timChk==TRUE){
      #dfMatrix10 <<- matrix(-1,nrow=frameSize10,ncol=nSymbols10+1)
      if (SFAS!=0){
        return(paste("FAS","Shares in hand",SFAS,sep=","))
      }
      
      else{
        return("ignore")
      } 
    }
    
    else{
      
      res <-dfChargeUsePrev(tickerId,as.numeric(askPrice),tradeTime)
      if(res$full)
        
      {
        print("Data is Full")
        resFrame <- res$dFrame
        #Calculate Lagged Returns
        #write.csv(resFrame,'resFrame.csv')
        resFrame<-data.frame(resFrame,stringsAsFactors=FALSE)
        colnames(resFrame)<-c("FAS","SPY","XLE","GLD","USO","HYG","LQD","Time")
        resFrame$Time1<-as.POSIXct(as.numeric(resFrame$Time),origin = "1970-01-01")
        resFrame$sec<-(as.numeric(format(resFrame$Time1,"%S")))
        
        resFrame <- resFrame[!duplicated(resFrame$sec),]
        resFrame<- subset(resFrame, select = c(1,2,3,4,5,6,7,8))
        colnames(resFrame)<-c("FAS","SPY","XLE","GLD","USO","HYG","LQD","Sec")
        
        
        for(i in 2:nrow (resFrame))
        {
          #Calculate Return on FAS
          resFrame[i,9]=(resFrame[i,1]-resFrame[i-1,1])/resFrame[i,1]
          #Calculate Lag 1 of Return of FAS
          resFrame[i,10]=(resFrame[i,9]-resFrame[i-1,9])/resFrame[i,9]
          #Calculate Lag 2 of Return of FAS
          resFrame[i,11]=(resFrame[i,10]-resFrame[i-1,10])/resFrame[i,10]
          # Calculate the Indicator Variable
          if (resFrame[i,9]>0)
          {
            
            resFrame[i,12]=1
          }
          else
          {
            resFrame[i,12]=0
          }
          
          #Calculate Return on SPY
          resFrame[i,13]=(resFrame[i,2]-resFrame[i-1,2])/resFrame[i,2]
          #Calculate Lag 1 of Return of SPY
          resFrame[i,14]=(resFrame[i,13]-resFrame[i-1,13])/resFrame[i,13]
          #Calculate Lag 2 of Return of SPY
          resFrame[i,15]=(resFrame[i,14]-resFrame[i-1,14])/resFrame[i,14]
          
          #Calculate Return on XLE
          resFrame[i,16]=(resFrame[i,3]-resFrame[i-1,3])/resFrame[i,3]
          #Calculate Lag 1 of Return of XLE
          resFrame[i,17]=(resFrame[i,16]-resFrame[i-1,16])/resFrame[i,16]
          #Calculate Lag 2 of Return of XLE
          resFrame[i,18]=(resFrame[i,17]-resFrame[i-1,17])/resFrame[i,17]
          
          #Calculate Return on GLD
          resFrame[i,19]=(resFrame[i,4]-resFrame[i-1,4])/resFrame[i,4]
          #Calculate Lag 1 of Return of GLD
          resFrame[i,20]=(resFrame[i,19]-resFrame[i-1,19])/resFrame[i,19]
          #Calculate Lag 2 of Return of GLD
          resFrame[i,21]=(resFrame[i,20]-resFrame[i-1,20])/resFrame[i,20]
          
          
          #Calculate Return on USO
          resFrame[i,22]=(resFrame[i,5]-resFrame[i-1,5])/resFrame[i,5]
          #Calculate Lag 1 of Return of USO
          resFrame[i,23]=(resFrame[i,22]-resFrame[i-1,22])/resFrame[i,22]
          #Calculate Lag 2 of Return of USO
          resFrame[i,24]=(resFrame[i,23]-resFrame[i-1,23])/resFrame[i,23]
          
          # Calculate Ratio of HYG/LQD
          
          resFrame[i,25]=(resFrame[i,6]/resFrame[i,7])
          
          # Add Term Structure, Fama-French Factors
          
          resFrame[,26] <- rep(0.07,nrow(resFrame)) 
          resFrame[,27] <- rep(0.22,nrow(resFrame)) 
          resFrame[,28] <- rep(0.22,nrow(resFrame))
          resFrame[,29]<- rep(1.78,nrow(resFrame))
          
          colnames(resFrame)<-c("FAS","SPY","XLE","GLD","USO","HYG","LQD","Time","Ret_FAS","LAG1_FAS","LAG2_FAS","Ind","Ret_SPY","Lag1_SPY","Lag2_SPY","Ret_XLE",
                                "Lag1_XLE","Lag2_XLE","Ret_GLD","Lag1_GLD","Lag2_GLD","Ret_USO","Lag1_USO","Lag2_USO","Ratio","Risk_prem","SMB","HML","TermS")
          
          
          # Check for NA & Inf Values etc
          # NA Values
          resFrame[is.na(resFrame)] <- 0
          
          # Inf Values
          is.na(resFrame) <- do.call(cbind,lapply(resFrame, is.infinite))
          
        }
        #write.csv(resFrame,'resFrame.csv')
        resFrame<-data.frame(resFrame,stringsAsFactors=FALSE)
        
        #### Calling VarImp Function
        if (firstcall==TRUE)
        {
          y<-resFrame[,12]
          y<-as.factor(unlist(y))
          
          x <- subset(resFrame, select = c(13,14,15,16,17,18,19,20,21,22,23,24,25)) # selecting columns
          x[is.na(x)] <- 0
          
          v<-variableRanking(x, y, "nb")
          
          z<-row.names(v$importance[order(v$importance[,1],decreasing = T),])[1:3]
          zd<<-data.frame(z)
          d1<-resFrame[,grepl(paste0(zd[1,1],collapse="|"),colnames(resFrame))]
          d2<-resFrame[,grepl(paste0(zd[2,1],collapse="|"),colnames(resFrame))]
          d3<-resFrame[,grepl(paste0(zd[3,1],collapse="|"),colnames(resFrame))]
          data5<-cbind(resFrame[,12],d1,d2,d3)
          data5<-data.frame(data5)
          mylogit <- glm(data5[,1] ~ data5[,2] + data5[,3] + data5[,4], data = data5, family = "binomial")
          newdata<-data.frame(lapply(resFrame, tail, 1))
          pred<-predict(mylogit, newdata, type="response")
          d<-data.frame(pred)
          
          #print (d[,1])
          g <- roc(data5[,1] ~ d[,1], data = data5)
          plot(g)
          x<-mean(d[,1])
          print(x)
          
          firstcall<<-FALSE
        }
        
        else{
          
          ##### Run Logistic Regression
          
          d1<-resFrame[,grepl(paste0(zd[1,1],collapse="|"),colnames(resFrame))]
          d2<-resFrame[,grepl(paste0(zd[2,1],collapse="|"),colnames(resFrame))]
          d3<-resFrame[,grepl(paste0(zd[3,1],collapse="|"),colnames(resFrame))]
          data5<-cbind(resFrame[,12],d1,d2,d3)
          data5<-data.frame(data5)
          mylogit <- glm(data5[,1] ~ data5[,2] + data5[,3] + data5[,4], data = data5, family = "binomial")
          print(mylogit)
          #write.csv(resFrame,'resFrame.csv')
          newdata<<-data.frame(lapply(resFrame, tail, 1))
          #print(newdata)
          pred<-predict(mylogit, data5, type="response")
          d<-data.frame(pred)
          d[is.na(d)] <- 0
          print (d[,1])
          g <- roc(data5[,1] ~ d[,1], data = data5)
          plot(g)    
          x<-mean(d[,1])
          print(x)
          
        }
        
        
        
        if (x>0.1)
        {
          ###BUYING 
          SFAS<<-100+SFAS
          print(c("SFAS is:",SFAS))
          PNL<<--(PNL+SFAS*newdata[,1])
          print(c("PNL is:",PNL))
        }
        else{
          
          ####SELLING
          SFAS<<-(-100+SFAS)
          print(c("SFAS is:",SFAS))
          PNL<<-PNL-SFAS*newdata[,1]
          print(c("PNL is:",PNL))
          
          
        }
      }
    }
  }
}


pl<-MultivariateMain()
