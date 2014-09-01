# ==========================================================================#
#-- Project Name :  Multivariate Model Simulator Code                                                    
#-- Task         :  Generate Back Test Report Using Simulator   
#-- version      :  1.0
#-- Date         :  13/Aug/14
#-- Author       :  Manasvi    
#-- SVN Directory:  \xxxx           
# ==========================================================================#

is.installed <- function(pkg){ is.element(pkg, installed.packages()[,1])}
#Check if package var has been installed
#if(!is.installed('strucchange')){ install.packages("strucchange",repos="http://lib.stat.cmu.edu/R/CRAN")}
#Calling the Package for Testing, Monitoring, Dating Structural Changes
#library("strucchange")
library("futile.logger")
require("caret")
#require(caret)
mripPath<-getwd()
flog.threshold(INFO, name="info_consume")
flog.appender(appender.file(paste(mripPath,"/Multivariate_info.log",sep="")), name="info_consume")
flog.info("R libraries loaded succesfully", name="info_consume")
SFAS <-0 # Counter to keep track of number of AMZN Shares at the end
flog.info("counter is after SFAS",name = "info_consume")
PNL<-0 # Profit & Loss
flog.info("counter is after PNL",name = "info_consume")
#Added extra column for time 
symbolVec_sim1 <- c('FAS','SPY','XLE','GLD','USO','HYG','LQD')
symbolVec <- symbolVec_sim1
frameSize <- 1000
nSymbols <-length(symbolVec)
dfMatrix <- matrix(-1,nrow=frameSize,ncol=nSymbols+1)
flog.info("counter is after dfMatrix",name = "info_consume")
singleRow <- matrix(-1,nrow=1,ncol=nSymbols)
flog.info("counter is after singleRow",name = "info_consume")
fillCount <- 1
iterC <- 1
tradeTime <- "\"NULL\""
stoploss<-0
value<-0
firstcall<-TRUE

#--------------------------------------------------------------------------------------------#
##Data-frame Charging Code
#--------------------------------------------------------------------------------------------#
#Function for the dataframe charging code

# Data charging function
dfChargeUsePrev <- function(tickerId, askPrice, tradeTime){
  flog.info("counter is within dfchargeprev function",name = "info_consume")
  temp <- gsub('.*:','', tradeTime)
  tradeTime <- sub(paste(':',temp,sep=""),paste(':',temp,sep=""), tradeTime)
  if(!is.na(askPrice)){
    symIndex <- which(symbolVec == tickerId)
    
    #First fill row no 1 in the matrix
    if(is.element(-1,as.vector(dfMatrix[1,]))){
      flog.info("counter is within dfchargeprev func",name = "info_consume")
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
        flog.info("counter is about to leave dfchargeprev func",name = "info_consume")
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
          flog.info("counter is about to leave dfchargeprev func 2",name = "info_consume")
          return(list("dFrame"=dFrame,"full"=FALSE))
        }
      }
    }
  }
}
############################################### Variable Selection Function###########################

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

#--------------------------------------------------------------------------------------------#
##predictPrice Function
#--------------------------------------------------------------------------------------------#

predictPrice10<-function(tradeTime,bidSize,askSize, bidPrice,askPrice,symbol,timChk){
  flog.info("counter is about to predictprice10 func",name = "info_consume")
  
  #bidPrice<-as.numeric(bidPrice)
  askPrice<-as.numeric(askPrice)
  #bidSize<-as.numeric(bidSize)
  #askSize<-as.numeric(askSize)
  tickerId<-as.character(symbol)
  tradeTime1<-as.numeric(tradeTime)
  tradeTime2 <- as.character(tradeTime)
  timChk<-as.logical(timChk)
  
  
  #flog.info(paste("bidSize - ",bidSize," askSize - ",askSize,sep=""),name = "info_consume")
  flog.info(paste("Ticker Symbol - ",tickerId,sep=""),name = "info_consume")
  #flog.info(paste("TIME CHECK ---------------------------------------- ",timChk,sep=""), name="info_consume")
  flog.info(paste("bidPrice - ",bidPrice," askPrice - ",askPrice,sep=""),name = "info_consume")
  flog.info(paste("TimeStamp - ",as.POSIXct(tradeTime,origin = "1970-01-01"),sep=""), name="info_consume")
  
  
  
  ## Square off the trade before mkt closes   
  if(timChk==TRUE){
    
    if (SFAS!=0){
      res <-dfChargeUsePrev(tickerId,askPrice,tradeTime)
      if(res$full){
        flog.info(paste("FAS Shares in Hand:",SFAS,sep=""))
        newdata<-data.frame(lapply(res, tail, 1))
        PNL<<-PNL+(SFAS*resFrame[newdata,1])
        flog.info(paste("Today's PNL is:",PNL,sep=""))
        SFAS<<-0
        flog.info("End of trading Day",name = "info_consume")
      }
      else{
        
        flog.info("wait",name = "info_consume")
        
      }
    }
    
    
    else{
      flog.info("Nothing in hand",name = "info_consume")
      
    } 
  }
  
  
  else{ # Other Time of the day
    res<-dfChargeUsePrev(tickerId,askPrice,tradeTime)
    if(res$full)
    {
      flog.info("Data is Full",name = "info_consume")
      resFrame <- res$dFrame
      flog.info(paste("SFAS :",SFAS,sep=""),name = "info_consume")
      #Calculate Lagged Returns
      #write.csv(resFrame,'resFrame.csv')
      resFrame<-data.frame(resFrame)
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
      
      #### Calling VarImp Function
      if (firstcall==TRUE)
      {
        y<-resFrame[,12]
        y<-as.factor(unlist(y))
        #write.csv(y,'y.csv')
        x <- subset(resFrame, select = c(13,14,15,16,17,18,19,20,21,22,23,24,25)) # selecting columns
        x[is.na(x)] <- 0
        #write.csv(x,'x.csv')
        v<-variableRanking(x, y, "nb")
        #print(v)
        
        z<-row.names(v$importance[order(v$importance[,1],decreasing = T),])[1:3]
        print(z)
        mylogit <- glm(Ind ~ Lag2_XLE + Ret_SPY + Lag1_SPY, data = resFrame, family = "binomial")
        print(mylogit)
        newdata<-data.frame(lapply(resFrame, tail, 1))
        print(newdata)
        pred<-predict(mylogit, newdata, type="response")
        d<-data.frame(pred)
        
        print (d[,1])
        
        
        firstcall<<-FALSE
      }
      
      else{
        
        ##### Run Logistic Regression
        
        mylogit <- glm(Ind ~ Lag2_XLE + Ret_SPY + Lag1_SPY, data = resFrame, family = "binomial")
        print(mylogit)
        newdata<-data.frame(lapply(resFrame, tail, 1))
        print(newdata)
        pred<-predict(mylogit, newdata, type="response")
        d<-data.frame(pred)
        print (d[,1])
        
      }
      
      
      
      if (d[,1]>0.014)
      {
        
        SFAS<<-100+SFAS
        print(c("SFAS is:",SFAS))
        PNL<<-PNL+SFAS*newdata[,1]
        print(c("PNL is:",PNL))
      }
      else{
        SFAS<<-0+SFAS
        print(c("SFAS is:",SFAS))
        PNL<<-PNL
        print(c("PNL is:",PNL))
      }
    }
  }
}








