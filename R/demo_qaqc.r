# THIS R PROGRAM PROCESSES A CORRUPTED DATASET AND PRODUCES 
# A "CHECKED" DATAFRAME (checksDf) THAT INCLUDES APPENDED QUALITY CODES

# John Porter, 2018-11-14

corruptData<-read.csv("http://www.vcrlter.virginia.edu/jhp7e/sample_corrupted_data.csv")

# Set up time vector
corruptData$sampleDateTime<-as.POSIXct(corruptData$sampleDateTime,format="%Y-%m-%d %H:%M:%S",TZ="EST")
# set up a data frame to receive the results of quality checks
checksDf<-corruptData


### HAD SYNTAX ISSUES WITH DATE_INGEST_CHECKER #########################

### START INCOMPLETE CASES #############################################

incomplete_numeric_cases_checker <- function(x) {
  # returns 0 if case is complete, 1 if it is incomplete
  # select out only the numeric columns
  y<-x[,sapply(x,is.numeric)]
  result<-ifelse(complete.cases(y),0,1)
  return(result)
}

incomplete_character_cases_checker <- function(x) {
  # returns 0 if case is complete, 1 if it is incomplete
  # select out only the character and factor data
  y<-x[,sapply(x,function(z) any(is.character(z),is.factor(z))) ]
  # convert factors to character 
  i <- sapply(y, is.factor)
  y[i] <- lapply(y[i], as.character)
  # convert blank fields to missing
  y[]<-sapply(y,function(z) gsub("^$|^ $", NA, z))
  # complete.cases can work if none of the data are complex structures
  result<-ifelse(complete.cases(y),0,1)
  return(result)
}

incomplete_datetime_cases_checker <- function(x) {
  # returns 0 if date or datetime rows complete, 1 if it is incomplete
  # select out only the datetime and date columns
  y<-x[,sapply(x,function(z) any(grep("POSIX",class(z)[1])==1, class(z)[1]=="Date"))]
  # complete.cases can work if none of the data are complex structures
  result<-ifelse(complete.cases(y),0,1)
  return(result)
}


incomplete_cases_checker <- function(x) {
  # check for any missing data columns of any type 
  # returns 0 if case is complete, 1 if it is incomplete
  c_num<-incomplete_numeric_cases_checker(x)
  c_char<-incomplete_character_cases_checker(x)
  c_date<-incomplete_datetime_cases_checker(x)
  df<-data.frame(c_num,c_char,c_date)
  result<-ifelse(rowSums(df,na.rm=TRUE) == 0,0,1)
  return(result)
}

# Vector of incomplete cases - exclude comments - many are blank
incomplete_cases<-incomplete_cases_checker(corruptData[1:4])
  
# test it all - note column 5 (comments) has many blank fields and so is ignored
checksDf<-data.frame(checksDf,incomplete_cases)

### END INCOMPLETE CASES ###########################################

#### START CHECK PERSISTENCE TEST ##################################
check_persistence<-function(x,delta=0,window=5){
  # Description: test for sequences of constant values or slightly varying caused by sensor failures
  
  # Returns: 0 for no persistence, 1 for persistence (a sequence of constant or near constant values) detected, -1 no test run
  
  # Team members: John
  # 
  # delta is the threshold that must be exceeded by groups of observations within a window
  # default delta is 0, meaning the values must be identical
  # window defines the number of subsequent values that must be similar
  if (mode(x)!="numeric"|mode(delta)!="numeric"|mode(window)!="numeric"){
    e<-simpleError("values for x, delta and window all must be numeric")
    stop(e)
  }
  window<-as.integer(window) # must be an integer
  window<-ifelse(window < 2,2,window)
  window=window-1
  # put a dummy value in difs to be removed later
  difs<-0
  for (i in seq(1,length(x)-window)){
    windowSeq<- seq(i,i+window)
    tmp<-x[windowSeq]
    #print(tmp)
    difval<-diff(tmp,1)
    #print(difval)
    for (j in 2:window){
      difval<-max(difval,abs(diff(tmp,j)),na.rm=TRUE)
      #print(difval)
    }
    difs<-append(difs,difval)  
  }
  # get rid of dummy value in difs
  difs<-tail(difs,length(difs)-1)
  #print(length(difs))
  #print(head(difs))
  result<-ifelse(difs > delta,0,1)
  #fill in missing for the end observations
  for(i in 1:window){
    result<-append(result,-1)
  }
  result<-ifelse(is.na(result),-1,result)
  return(result)
}

persistTemp0 <-check_persistence(corruptData$Temperature_C,delta=0)
persistTemp15 <- check_persistence(corruptData$Temperature_C,delta=0.15,window=20)
persistPress0 <-check_persistence(corruptData$pressure_mb,delta=0)
persistPress15 <- check_persistence(corruptData$pressure_mb,delta=0.15, window=20)
checksDf<-data.frame(checksDf,persistTemp0,persistTemp15,persistPress0,persistPress15)

### END CHECK PERSISTENCE CHECK #################################

### START SORTED BY TIMESTAMP CHECK #################################
sorted_by_timestamp<-function(timeVector,allowTies=TRUE){
  if(mode(timeVector) != "numeric"){
    e<-simpleError(paste("Error: Input must be Date, POSIXct, POSIXlt or numeric.\n Input vector was ",mode(timeVector),sep=''))
    stop(e)
  }
  timeVec1<-tail(timeVector,-1)
  # print(head(timeVector))
  # print(head(timeVec1))
  if(allowTies==FALSE){
    result<-ifelse(head(timeVector,length(timeVector)-1) < timeVec1,0,1)
  }else{
    result<-ifelse(head(timeVector,length(timeVector)-1)<=timeVec1,0,1)
  }
  result<-ifelse(is.na(result),-1,result)
  result<-append(result,-1)
  return(result)
}


# Description: Check that data are sorted by timestamp

# Returns a vector of flags. 0=no problem, 1= out of order, -1= test could not be run (e.g., there is nothing to compare the last line to)

# Team members: John

timeStampOrderCheck<-sorted_by_timestamp(corruptData$sampleDateTime)
checksDf<-data.frame(checksDf,timeStampOrderCheck)
## Note ordering should be bad when the stations change

### END SORTED BY TIMESTAMP CHECK #################################

### START REMOVE SPACES  ################################# 

  # Description: Check character data for inadvertant spaces and remove spaces
  
  # Returns: A data frame without unwanted white spaces
  
  # Team members:  Kristin
  
  remove_spaces <- function(df) {
    if(class(df) != "data.frame"){
      e<-simpleError(paste("Input must be a data frame. You provided a ",class(df),sep=''))
      stop(e)
    }
    for (i in names(df)) {
      if(class(df[, i]) %in% c("factor", "character")){
        df[, i] <- trimws(df[, i])
      }
    }
    return(df)
  }
  
  checksDf<-remove_spaces(checksDf)
  ## Show differences - note type changes from factor to character
  levels(corruptData$Station)
  levels(as.factor(checksDf$Station))
  
  ### END REMOVE SPACES  #################################
  
  ### START GAP CHECK  ###################################
  library(tidyverse)
  # for some reason gap_checker fails if tidyverse isn't loaded
  
  gap_checker <- function(dat, datetimeVar, gapThreshold) {
    
    # Description: checks for gaps in time sequences larger than a threshold provided by the user
    
    # Returns: for each row in the target data frame, returns 0, 1, or -1 (passed, failed, not evaluated)
    
    # Team members: Celeste, stevan
    
    # if (!is.POSIXct(dat[[datetimeVar]])) {
    #   stop("variable to check must be of type POSIXct")
    # }
    
    datetimeVec <- dat[[datetimeVar]]
    timeGap <- datetimeVec - lag(datetimeVec)
    sequenceGap <- timeGap > gapThreshold
    sequenceGap <- ifelse(is.na(sequenceGap), -1,
                          ifelse(sequenceGap == TRUE, 1,
                                 ifelse(sequenceGap == FALSE, 0, NA)
                          )
    )
    
    return(sequenceGap)
    
  }
  # I am guessing the gapThreshold is in Seconds? - so used 1 hour
  timeGapCheck<-gap_checker(corruptData,"sampleDateTime",3600)
  checksDf<-data.frame(checksDf,timeGapCheck)
  
  ### END GAP CHECK  ###################################
  
  ### START NUMERIC STEP CHECK  ################################### 
  numeric_step_checker <- function(x,stepMax=NA,verbose=FALSE) {
    # if stepMax isn't specified, try to define a step based on 
    # the variance of the data stream. Default is 1.96*standard deviation
    # of the sequential differences
    # NOTE if the default step is used, about 5% of the data will be flagged
    # even if there are no real problems with the data
    stepMax<-ifelse(is.na(stepMax),(1.96*sd(diff(x,lag=1),na.rm=TRUE)),stepMax)
    
    isStep <- abs(diff(x,lag=1)) > stepMax
    result<-ifelse(isStep,1,0)
    # make the result vector length the same as x
    result<-append(-1,result)
    result<-ifelse(is.na(result),-1,result)
    if (verbose){
      print(paste("Looking for jumps or drops in the data > ",stepMax,sep=''))
      print("Frequency of result codes:")
      print(summary(as.factor(result)))
    }
    return(result)
  }
  
  # Description: Flags time series (e.g., sensor data) for unexpected
  # jumps (increase or decrease) between sequential observations . 
  
  # Inputs: vector to be examined, optional size of step to be flagged
  
  # Returns: vector containing 0 for no steps, 1 for steps, -1 for test not possible
  
  # Tests:
  
  # Examples:
  
  # Team members: John
  
  stepCheckTemp<-numeric_step_checker(corruptData$Temperature_C,verbose=TRUE)
  stepCheckPres<-numeric_step_checker(corruptData$pressure_mb,stepMax=10,verbose=TRUE)
  checksDf<-data.frame(checksDf,stepCheckTemp,stepCheckPres)
  
  ### END NUMERIC STEP CHECK  ################################### 
  
### START NUMERIC SPIKE CHECK  ################################### 
  
  numeric_spike_checker<-function(x,spikeMin=NA,verbose=FALSE)
    {
      # if spikeMin isn't specified, try to define a spike minimum based on 
      # the variance of the data stream. Default is 1.96*standard deviation
      # of the sequential differences
      # NOTE if the default step is used, about 5% of the data will be flagged
      # even if there are no real problems with the data
      spikeMin<-ifelse(is.na(spikeMin),(1.96*sd(diff(x,lag=1),na.rm=TRUE)),spikeMin)
      # use 3 element moving window to detect spikes
      lag1<-c(NA,head(x,-1)) # lag-1
      lagMinus1<-c(tail(x,-1),NA) # lag+1
      s<-cbind(lag1,lagMinus1,x)
      #calculate the median
      medianVec=apply(s[,c("lag1","lagMinus1","x")],1,median)
      isSpikeVal<- ((abs(x-medianVec) > spikeMin) 
                    &(!is.na(lag1))&(!is.na(lagMinus1)))
      result<-ifelse(isSpikeVal,1,0)
      result<-ifelse(is.na(result),-1,result)
      if(verbose){
        print(paste("Looking for jumps or drops in the data > ",spikeMin,sep=''))
        print("Frequency of result codes:")
        print(summary(as.factor(result)))
      }
      return(result)
    }
  
  spikeCheckTemp<-numeric_spike_checker(corruptData$Temperature_C,spikeMin=3,verbose=TRUE)
  spikeCheckPres<-numeric_spike_checker(corruptData$pressure_mb,verbose=TRUE)
  checksDf<-data.frame(checksDf,spikeCheckTemp,spikeCheckPres)
  
  ### END NUMERIC SPIKE CHECK  ################################### 
  
