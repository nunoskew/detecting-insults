# Cosine similarity to compare new features with targetvariable
                                        #How to use: train <- readDataTrain()
readDataTrain <- function()
  {
    traini <- read.table(file="train.csv",header=TRUE,sep=",")
    traini[,3] <- as.character(traini[,3])
    traini
  }

#How to use: test <- readDataTest()
readDataTest <- function()
  {
    testi <- read.table(file="test.csv",header=TRUE,sep=",")
    testi[,2] <- as.character(testi[,2])
    testi
  }

#How to use: test <- readDataTestWithSolutions()
readDataTestWithSolutions <- function()
{
  testisol <- read.csv(file="test_with_solutions.csv",header=TRUE,sep=",")
  testisol[,3] <- as.character(testisol[,3])
  testisol=testisol[,-4]
  testisol
}


experimentServer <- function()
  {
    require(DMwR)
    require(Metrics)
    require(gbm)
    load("bigramTrainTop1000WithoutStop_dtmTrain.Rdata")
    load("targetVariable.Rdata")
    dtmTrain$targetVariable <- targetVariable
    indres <- makeExperiment(dtmTrain)
  }
#How to use:  countmeanwordtrain <- countMedianWordLength(train,3)
#How to use:  countmeanwordtest <- countMedianWordLength(test,2)
countMedianWordLength <- function(data,comment)
  {
    countmeanwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        countmeanwordtrain[i] <- median(sapply(strsplit(data[i,comment]," "),nchar))
      }
    countmeanwordtrain
  }

#How to use: countwordtrain <- countMedianWordLength(train,3)
#How to use: countwordtest <- countMedianWordLength(test,2)
countWords <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        countwordtrain[i] <- sapply(strsplit(data[i,comment]," "),length)
      }
    countwordtrain
  }

#How to use: countinitialcapstrain  <- countInitialCaps(train,3)
#How to use: countinitialcapstest  <- countInitialCaps(test,2)
countInitialCaps <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        
        countwordtrain[i] <- length(which(unlist(gregexpr("^[[:upper:]][^A-Z]",strsplit(data[i,comment]," ")[[1]]))==1))
      }
    countwordtrain
  }

#How to use: countyouaretrain  <- countYouArePostStem(train,3)
#How to use: countyouaretest  <- countYouArePostStem(test,2)
countYouArePostStem <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        ifelse(gregexpr("your|you ar",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("your|you ar",data[i,comment])[[1]]))
        #countwordtrain[i] <- length(which(unlist(gregexpr("your|you ar",strsplit(data[i,comment]," ")[[1]]))==1))
      }
    countwordtrain
  }

#How to use: countyouaretrain  <- countYouArePreStem(train,3)
#How to use: countyouaretest  <- countYouArePreStem(test,2)
countYouArePreStem <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        ifelse(gregexpr("are|you'r",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("are|you'r",data[i,comment])[[1]]))
        #countwordtrain[i] <- length(which(unlist(gregexpr("your|you ar",strsplit(data[i,comment]," ")[[1]]))==1))
      }
    countwordtrain
  }

#How to use: countyoutrain  <- countYou(train,3)
#How to use: countyoutest  <- countYou(test,2)
countYou <- function(data,comment)
{
  countwordtrain <- rep(0,nrow(data))
  for(i in 1:nrow(data))
  {
    print(i/nrow(data)*100)
    ifelse(gregexpr("\\byou\\b",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("are|you'r",data[i,comment])[[1]]))
    #countwordtrain[i] <- length(which(unlist(gregexpr("your|you ar",strsplit(data[i,comment]," ")[[1]]))==1))
  }
  countwordtrain
}

#How to use: countlikeyoutrain  <- countLikeYou(train,3)
#How to use: countlikeyoutest  <- countLikeYou(test,2)
countLikeYou <- function(data,comment)
{
  countwordtrain <- rep(0,nrow(data))
  for(i in 1:nrow(data))
  {
    print(i/nrow(data)*100)
    ifelse(gregexpr("like you",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("are|you'r",data[i,comment])[[1]]))
    #countwordtrain[i] <- length(which(unlist(gregexpr("your|you ar",strsplit(data[i,comment]," ")[[1]]))==1))
  }
  countwordtrain
}
#How to use: countistrain  <- countIs(train,3)
#How to use: countistest  <- countIs(test,2)
countIs <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        ifelse(gregexpr("[^A-Za_z]is[^A-Za_z]",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("[^A-Za_z]is[^A-Za_z]",data[i,comment])[[1]]))
        #countwordtrain[i] <- length(which(unlist(gregexpr("your|you ar",strsplit(data[i,comment]," ")[[1]]))==1))
      }
    countwordtrain
  }

#How to use: countfucktrain  <- countFuck(train,3)
#How to use: countfucktest  <- countFuck(test,2)
countFuck <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        ifelse(gregexpr("fuck",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("fuck",data[i,comment])[[1]]))
        #countwordtrain[i] <- length(which(unlist(gregexpr("your|you ar",strsplit(data[i,comment]," ")[[1]]))==1))
      }
    countwordtrain
  }

#How to use: countracetrain  <- countRace(train,3)
#How to use: countracetest  <- countRace(test,2)
countRace <- function(data,comment)
{
  countwordtrain <- rep(0,nrow(data))
  for(i in 1:nrow(data))
  {
    print(i/nrow(data)*100)
    ifelse(gregexpr("negro|nigg|white|black|spic|latino|wop|hispanic|chink",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("fuck",data[i,comment])[[1]]))
    #countwordtrain[i] <- length(which(unlist(gregexpr("your|you ar",strsplit(data[i,comment]," ")[[1]]))==1))
  }
  countwordtrain
}

#How to use: countreligiontrain  <- countReligion(train,3)
#How to use: countreligiontest  <- countReligion(test,2)
countReligion <- function(data,comment)
{
  countwordtrain <- rep(0,nrow(data))
  for(i in 1:nrow(data))
  {
    print(i/nrow(data)*100)
    ifelse(gregexpr("jew|christian|arab|muslim|god",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("fuck",data[i,comment])[[1]]))
    #countwordtrain[i] <- length(which(unlist(gregexpr("your|you ar",strsplit(data[i,comment]," ")[[1]]))==1))
  }
  countwordtrain
}

#How to use: countsextrain  <- countSex(train,3)
#How to use: countsextest  <- countSex(test,2)
countSex <- function(data,comment)
{
  countwordtrain <- rep(0,nrow(data))
  for(i in 1:nrow(data))
  {
    print(i/nrow(data)*100)
    ifelse(gregexpr("men|man|woman|women",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("fuck",data[i,comment])[[1]]))
    #countwordtrain[i] <- length(which(unlist(gregexpr("your|you ar",strsplit(data[i,comment]," ")[[1]]))==1))
  }
  countwordtrain
}

#How to use: countsexorientationtrain  <- countSexOrientation(train,3)
#How to use: countsexorientationtest  <- countSexOrientation(test,2)
countSexOrientation <- function(data,comment)
{
  countwordtrain <- rep(0,nrow(data))
  for(i in 1:nrow(data))
  {
    print(i/nrow(data)*100)
    ifelse(gregexpr("homo|hetero|straight|gay|lesbian",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("fuck",data[i,comment])[[1]]))
    #countwordtrain[i] <- length(which(unlist(gregexpr("your|you ar",strsplit(data[i,comment]," ")[[1]]))==1))
  }
  countwordtrain
}
#How to use: countcapstrain  <- countCaps(train,3)
#How to use: countcapstest  <- countCaps(test,2)
countCaps <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        
        ifelse(gregexpr("[A-Z]",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("[A-Z]",data[i,comment])[[1]]))
      }
    countwordtrain
  }

#How to use: countletterstrain  <- countLetters(train,3)
#How to use: countletterstest  <- countLetters(test,2)
countLetters <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        
        ifelse(gregexpr("[[:alpha:]]",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("[[:alpha:]]",data[i,comment])[[1]]))
      }
    countwordtrain
  }

#How to use: outliers  <- getOutliers(train,3,countslashtrain,countnumberstrain)
getOutliers <- function(data,comment,countslashtrain,countnumberstrain)
  {
    # the 6 is justified cause is the first coefficient that produces a vector with only 0's in train[which(f>(mean(f)+(6*sd(f)))),c(1)]
    f <- (countslashtrain+1)/(countnumberstrain+1)
    outliers <- which(f>(mean(f)+(6*sd(f))))
    outliers
    #outliers
    #data <- data[-outliers,]
    #data
  }

#How to use: train  <- removeJibberish(train,3,countslashtrain,countnumberstrain)
removeJibberish <- function(data,comment,countslashtrain,countnumberstrain)
  {
    # the 6 is justified cause is the first coefficient that produces a vector with only 0's in train[which(f>(mean(f)+(6*sd(f)))),c(1)]
    f <- (countslashtrain+1)/(countnumberstrain+1)
    outliers <- which(f>(mean(f)+(6*sd(f))))
    #outliers
    data <- data[-outliers,]
    data
  }

#How to use: countnumberstrain  <- countNumbers(train,3)
#How to use: countnumberstest  <- countNumbers(test,2)
countNumbers <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        
        ifelse(gregexpr("[[:digit:]]",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("[[:digit:]]",data[i,comment])[[1]]))
      }
    countwordtrain
  }

#How to use: countslashtrain  <- countSlash(train,3)
#How to use: countslashtest  <- countSlash(test,2)
countSlash <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        countwordtrain[i] <- length(attributes(gregexpr("\\\\+",data[i,comment])[[1]])[1][[1]])
        #ifelse(gregexpr("\\\\",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("\\\\",data[i,comment])[[1]]))
      }
    countwordtrain
  }

#How to use: countotherspecialchartrain  <- countOtherSpecialChar(train,3)
#How to use: countotherspecialchartest  <- countOtherSpecialChar(test,2)
countOtherSpecialChar <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        #print(gregexpr("[#|\\$|\\@|\\*|\\%|\\&]{2,}",data[i,comment]))
        ifelse(attributes(gregexpr("[#|$|@|*|%|&]{2,}",data[i,comment])[[1]])[1][[1]]==-1,countwordtrain[i] <-0,countwordtrain[i] <- sum(attributes(gregexpr("[#|$|@|*|%|&]{2,}",data[i,comment])[[1]])[1][[1]]))
      }
    countwordtrain
  }


#How to use: countexclamationtrain  <- countExclamation(train,3)
#How to use: countcexclamationtest  <- countExclamation(test,2)
countExclamation <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        
        ifelse(gregexpr("\\!",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("\\!",data[i,comment])[[1]]))
      }
    countwordtrain
  }

#How to use: count3pointstrain  <- count3Points(train,3)
#How to use: count3pointstest  <- count3Points(test,2)
count3Points <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        
        countwordtrain[i] <- length(which(attributes(gregexpr("\\.{2,}",data[i,comment])[[1]])[1][[1]]>1))
      }
    countwordtrain
  }

#How to use: countpolitetrain  <- countPolite(train,3)
#How to use: countpolitetest  <- countPolite(test,2)
countPolite <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        
        ifelse(gregexpr("\\,|\\.|\\'|\\:",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("\\,|\\.|\\'|\\:",data[i,comment])[[1]]))
      }
    countwordtrain
  }

#How to use: countinterrogationtrain  <- countInterrogation(train,3)
#How to use: countinterrogationtest  <- countInterrogation(test,2)
countInterrogation <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        
        ifelse(gregexpr("\\?",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("\\?",data[i,comment])[[1]]))
      }
    countwordtrain
  }

#How to use: countpuncttrain  <- countPunctuation(train,3)
#How to use: countpuncttest  <- countPuncuation(test,2)
countPunctuation <- function(data,comment)
  {
    countwordtrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        
        ifelse(gregexpr("[[:punct:]]",data[i,comment])[[1]][1]==-1,countwordtrain[i] <-0,countwordtrain[i] <- length(gregexpr("[[:punct:]]",data[i,comment])[[1]]))
      }
    countwordtrain
  }

#How to use: countcharstrain <- countChars(train,3)
#How to use: countcharstest <- countChars(test,2)
countChars <- function(data,comment)
  {
    counttrain <- rep(0,nrow(data))
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        counttrain[i] <- nchar(data[i,comment])
      }
    counttrain
  }
cv.lm <- function(form,train,test,...) {
m <- lm(form,train,...)
p <- predict(m,test)
predicted<-(pmax(p, 0))
predicted<-(pmin(p, 1))
print(auc(resp(form, test),p))
auc(resp(form, test),p) 
}
cv.svm <- function(form,train,test,...) {
require(e1071)
require(Metrics)
m <- svm(form,train,...)
p <- predict(m,test)
predicted<-(pmax(p, 0))
predicted<-(pmin(p, 1))
print(auc(resp(form, test),p))
auc(resp(form, test),p) 
}

cv.rf <- function(form,train,test,...) {
require(randomForest)
require(Metrics)
m <- randomForest(form,train,...)
p <- predict(m,test)
predicted<-(pmax(p, 0))
predicted<-(pmin(p, 1))
#predicted <- rank(predicted)/length(predicted)
print(auc(resp(form, test),p))
auc(resp(form, test),p)
}
cv.gbm500 <- function (form, train, test, ...) 
{
  require(gbm)
  require(Metrics)
  m <- gbm(targetVariable ~ ., data = train, ..., verbose = T, n.trees = 500)
  p <- predict(m, test, type = "response", n.trees = 500)
  predicted<-(pmax(p, 0))
  predicted<-(pmin(p, 1))
  #predicted <- rank(predicted)/length(predicted)
  print(auc(resp(form, test),p))
  auc(resp(form, test),p)
}
# How to use: DSs <- buildDataset(dtmTrain)
buildDataset <- function(train)
  {
    require(DMwR)
    DSs <- sapply("targetVariable",function(x,names.attrs) {
      f <- as.formula(paste(x,"~ ."))
      dataset(f,train[,c(names.attrs,x)],x)
    },names(train)[1:(ncol(train)-1)])
    DSs
  }
# How to use: indres <- makeExperiment(dtmTrain)
makeExperiment <- function(dtmTrain)
  {
    require(DMwR)
    DSs <- buildDataset(dtmTrain)
    indres <- experimentalComparison(DSs,c(variants("cv.gbm500",distribution=c("gaussian"),shrinkage=c(0.01),n.minobsinnode=c(5),bag.fraction=c(0.5),interaction.depth=c(14))),cvSettings(1,5,1234))
    indres
  }

# How to use: indres <- makeExperimentRF(dtmTrain)
makeExperimentRF <- function(dtmTrain)
  {
    require(DMwR)
    DSs <- buildDataset(dtmTrain)
    indres <- experimentalComparison(DSs,c(variants("cv.rf",mtry=3,ntrees=300)),cvSettings(1,5,1234))
    indres
  }
# How to use: runModel(dtmTrain,dtmTest,"capsAndLetterCountNewData.csv")
runModel <- function(dtmTrain,dtmTest)
  {
    require(gbm)
    m <- gbm(targetVariable~. ,data = dtmTrain, verbose = T, n.trees = 500,distribution=c("gaussian"),shrinkage=c(0.01),n.minobsinnode=c(5),bag.fraction=c(0.5),interaction.depth=c(14))
    
    m_result <- predict(m, dtmTest, type="response", n.trees = 500)
    predicted <- m_result
    predicted<-(pmax(predicted, 0))
    predicted<-(pmin(predicted, 1))
    predicted
    #write.table(predicted,file=filename,col.names=TRUE,row.names=FALSE,quote=FALSE)
  }

# How to use: runModel(dtmTrain,dtmTest,"capsAndLetterCountNewData.csv")
runModelRF <- function(dtmTrain,dtmTest)
  {
    require(randomForest)
    m <- randomForest(targetVariable~. ,data = dtmTrain,mtry=3,ntrees=300)
    
    m_result <- predict(m, dtmTest, type="response", ntrees = 300)
    predicted <- m_result
    predicted<-(pmax(predicted, 0))
    predicted<-(pmin(predicted, 1))
    predicted
    #write.table(predicted,file=filename,col.names=TRUE,row.names=FALSE,quote=FALSE)
  }


# How to use: visited <- consecutiveWords(train,3)
consecutiveWords <- function(data,comment)
  {
    visited <- NULL
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        a <- strsplit(data[i,comment]," ")[[1]]
        #print(a)
        for(j in 1:(length(a)-1))
          {
            
            nami <- paste(c(a[j],a[j+1]),collapse="|")
            iman <- paste(c(a[j+1],a[j]),collapse="|")
            if((!nami%in%visited) & (!iman%in%visited))
              {
                visited <- c(visited,nami)
              }
            
          }
      }
    visited
  }

#How to use : revVisited <- invertConsecutiveWords(visited)
invertConsecutiveWords <- function(visited)
  {
    revVisited <- sapply(visited,function(x){paste(strsplit(x,"|",fixed=TRUE)[[1]][2],strsplit(x,"|",fixed=TRUE)[[1]][1],collapse="|",sep="|")})
    revVisited
  }

# How to use: cWordsTrain <- modifyBigramToConsecutiveBigram(visited,mtxTrain)
modifyBigramToConsecutiveBigram <- function(visited,mtx)
{
  revVisited <- invertConsecutiveWords(visited)
  cWords <- mtx[,intersect(colnames(mtx),revVisited)]
  cWords <- cbind(cWords,mtx[,intersect(colnames(mtx),visited)])
  cWords
}

# How to use: train <- stemData(train,3)
stemData <- function(data,comment)
  {
    require(Snowball)
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        data[i,comment] <- paste(SnowballStemmer(strsplit(data[i,comment]," ")[[1]]),collapse=" ")
      }
    data
    
    
  }
# How to use : train <- removeExtraSpace(train,3)
removeExtraSpace <- function(data,comment)
{
  for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        data[i,comment] <- gsub("[[:blank:]]"," ",data[i,comment])
      }
    data
}
# How to use : train <- removeFunnyCharacters(train,3)
removeFunnyCharacters <- function(data,comment)
  {
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        data[i,comment] <- gsub("xe2|x80|x90|x99|x9c|x9d|x94|x93|xc2|xbb|xab|xa9|xae|xa0|x82|x84|xa2|xac|xa6"," ",data[i,comment])
      }
    data
  }

# How to use : train <- removeStopWords(train,3,"normal")
removeStopWords <- function(data,comment,option="dtm",smartstopwords)
  {
    require(tm)
    load("smartstopwords.Rdata")
    tbl <- smartstopwords
    if(option=="dtm")
      {
        data <- data[,-match(intersect(tbl,colnames(data)),colnames(data))]
      }
    else
      {
        if(option=="normal")
          {
            for(i in 1:nrow(data))
              {
                print(i/nrow(data)*100)
                data[i,comment] <- as.character(removeWords(PlainTextDocument(data[i,comment]),tbl))
              }
          }
      }
    data
  }

#How to use: train <- removeEndLine(train,3)
removeEndLine <- function(data,comment)
  {
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        data[i,comment] <- gsub("\\\\n"," ",data[i,comment])
      }
    data
  }



cv.lm <- function(form,train,test,...) {
m <- lm(form,train,...)
p <- predict(m,test)
predicted<-(pmax(p, 0))
predicted<-(pmin(p, 1))
print(auc(resp(form, test),p))
auc(resp(form, test),p) 
}

#How to use: train <- removeNumbers(train,3)
removeNumbers <- function(data,comment)
  {
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        data[i,comment] <- gsub("[[:digit:]]"," ",data[i,comment])
      }
    data
  }
# How to use: train <- removePunctuation(train,3)
removePunctuation <- function(data,comment)
  {
    for(i in 1:nrow(data))
      {
        print(i/nrow(data)*100)
        data[i,comment] <- gsub("[[:punct:]]"," ",data[i,comment])
      }
    data
  }
# How to use: train <- toLower(train,3)
toLower <- function(data,comment)
  {
    data[,comment] <- tolower(data[,comment])
    data
  }

preProcessFormatText <- function(data,comment)
  {
    data <- toLower(data,comment)
    data <- removePunctuation(data,comment,"normal")
  }

# How to use: res <- checkBigrams(dtmTrain)
checkBigrams <- function(train)
  {
    b <- apply(train,1,function(x){which(x!=0)})
    res <- NULL
    visited <- NULL
    cont <- 1
    for(i in 1:length(b))
      {
        print(i/length(b)*100)
        if(length(b[[i]])>=2)
          {
            combi <- combn(length(b[[i]]),2)
            #print(combi)
            nami <- apply(combi,2,function(x){paste(colnames(train)[x[1]],colnames(train)[x[2]],sep="|")})
            #print(class(nami))
            colnames(combi) <- nami
            for(j in 1:ncol(combi))
              {
                #print("OLA MAE")
                if((!paste(colnames(train)[combi[1,j]],colnames(train)[combi[2,j]],sep="|")%in%colnames(res))&(!paste(colnames(train)[combi[2,j]],colnames(train)[combi[1,j]],sep="|")%in%colnames(res)))
                  {
                    res <- cbind(res,combi[,j])
                    colnames(res)[cont] <- paste(colnames(train)[combi[1,j]],colnames(train)[combi[2,j]],sep="|")
                    cont <- cont+1
                  }
                #if(any(duplicated(cbind(res,combi[,j]),MARGIN=2))==FALSE)
                #  {
                #    res <- cbind(res,combi[,j])
                #  }
              }
          }
      }
    res
  }




#How to use : mtx <- buildBigramMatrix(res,dtmTrain)
buildBigramMatrix <- function(res,train)
  {
    mtx <- matrix(0,nrow=nrow(train),ncol=ncol(res))
    for(i in 1:ncol(res))
      {
        print(i/ncol(res)*100)
        mtx[,i] <- train[,res[1,i]]+train[,res[2,i]]
        
      }
    colnames(mtx) <- colnames(res)
    #a <- apply(res,2,function(x){train[,x[1]]+train[,x[2]]})
    #mtx <- as.data.frame(mtx)
    mtx
  }

# How to use: dtmTrain <- insertVariablesTrain(dtmTrain,c("countcapstrain","countletterstrain","countmeanwordtrain","countotherspecialchartrain","countyouaretrain"),newfeatures,targetVariable)
insertVariablesTrain <- function(dtmTrain,fNames,newfeatures,targetVariable)
  {
    #load("newfeatures.Rdata")
    #load("targetVariable.Rdata")
    
    dtmTrain <- cbind(dtmTrain,newfeatures[,fNames],targetVariable)
    dtmTrain
  }
# How to use: dtmTest <- insertVariablesTest(dtmTest,c("countcapstrain","countletterstrain","countmeanwordtrain","countotherspecialchartrain","countyouaretrain"),newfeaturestest)
insertVariablesTest <- function(dtmTest,fNames,newfeatures)
  {
    #load("countcapstest.Rdata")
    #load("countletterstest.Rdata")
    #dtmTest$upperCount <- countcapstest
    #dtmTest$letterNumber <- countletterstest
    dtmTest <- cbind(dtmTest,newfeatures[,fNames])
    dtmTest
  }

## How to use : train<-normalProcedure()
normalProcedure <- function()
  {
train <- readDataTrain()


train <- removeFunnyCharacters(train,3)
train <- removeEndLine(train,3)
# REMOVE JIBBERISH
countletterstrain  <- countLetters(train,3)
countslashtrain  <- countSlash(train,3)
#train  <- removeJibberish(train,3,countslashtrain,countletterstrain)
countnumberstrain  <- countNumbers(train,3)
countslashtrain  <- countSlash(train,3)
countinterrogationtrain  <- countInterrogation(train,3)
countexclamationtrain  <- countExclamation(train,3)
count3pointstrain  <- count3Points(train,3)
countotherspecialchartrain  <- countOtherSpecialChar(train,3)
countpolitetrain  <- countPolite(train,3)
#COUNT NUMBERS
train <- removeNumbers(train,3)
countcapstrain  <- countCaps(train,3)
countinitialcapstrain  <- countInitialCaps(train,3)
## COUNT CAPS E INITIALCAPS
#train <- removeExtraSpace(train,3)
train <- toLower(train,3)
countfucktrain  <- countFuck(train,3)

countyoutrain  <- countYou(train,3)
countsexorientationtrain=countSexOrientation(train,3)
countracetrain=countRace(train,3)
countreligiontrain=countReligion(train,3)
countlikeyoutrain=countLikeYou(train,3)
countyouaretrain  <- countYouArePreStem(train,3)
# COUNT OTHER SPECIAL CHAR INTERROGATION EXCLAMATION POLITE 3 POINTS
train <- removePunctuation(train,3)




## COUNT LETTERS MEAN WORD LEN

#train <- removeStopWords(train,3,"normal")
countcharstrain <- countChars(train,3)
countmeanwordtrain <- countMedianWordLength(train,3)
countletterstrain  <- countLetters(train,3)

train <- stemData(train,3)

train
  }

## How to use : test<-normalProcedureTest()
normalProcedureTest <- function()
  {
test <- readDataTest()
test <- removeFunnyCharacters(test,2)
test <- removeEndLine(test,2)
#countletterstest  <- countLetters(test,2)
#countslashtest <- countSlash(test,3)
# REMOVE JIBBERISH
countnumberstrain  <- countNumbers(test,2)
countslashtrain  <- countSlash(test,2)
countinterrogationtrain  <- countInterrogation(test,2)
countexclamationtrain  <- countExclamation(test,2)
count3pointstrain  <- count3Points(test,2)
countotherspecialchartrain  <- countOtherSpecialChar(test,2)
countpolitetrain  <- countPolite(test,2)

test <- removeNumbers(test,2)
countcapstrain  <- countCaps(test,2)
countinitialcapstrain  <- countInitialCaps(test,2)
#train <- removeExtraSpace(train,3)
test  <- toLower(test,2)
countfucktest  <- countFuck(test,2)
countyoutest  <- countYou(test,2)
countlikeyoutest=countLikeYou(test,2)
countsexorientationtest=countSexOrientation(test,2)
countracetest=countRace(test,2)


countreligiontest=countReligion(test,2)



countyouaretest <- countYouArePreStem(test,2)
test <- removePunctuation(test,2)
countcharstrain <- countChars(test,2)
countmeanwordtrain <- countMedianWordLength(test,2)
countletterstrain  <- countLetters(test,2)

#test <- removeStopWords(test,2,"normal")
test <- stemData(test,2)

test
  }
## How to use : dtmTrain<-createDtmTrain(train)
createDtmTrain <- function(train)
  {
    require(tm)
    training <- Corpus(VectorSource(train[,3]))
    funs <- list(stripWhitespace)
    #docsTrain <- tm_map(training,FUN=tm_reduce,tmFuns=funs)
    weight <- function(x){ weightTfIdf(x,normalize=TRUE)}
    dtmTrain <- DocumentTermMatrix(training,control=list(weighting=weight,wordLengths=c(1,Inf)))
    #dtmTrain <- as.data.frame(as.matrix(dtmTrain))
    dtmTrain
  }
## How to use : dtmTrain<-createDtmTrainTf(train)
createDtmTrainTf <- function(train)
  {
    require(tm)
    training <- Corpus(VectorSource(train[,3]))
    funs <- list(stripWhitespace)
    docsTrain <- tm_map(training,FUN=tm_reduce,tmFuns=funs)
    weight <- function(x){ weightTf(x)}
    dtmTrain <- DocumentTermMatrix(docsTrain,control=list(weighting=weight))
    #dtmTrain <- as.data.frame(as.matrix(dtmTrain))
    dtmTrain
  }

## How to use : dtmTest<-createDtmTest(test)
createDtmTest <- function(test)
  {
    require(tm)
    testing <- Corpus(VectorSource(test[,2]))
    funs <- list(stripWhitespace)
    #docsTest <- tm_map(testing,FUN=tm_reduce,tmFuns=funs)
    weight <- function(x) {weightTfIdf(x,normalize=TRUE)}
    dtmTest <- DocumentTermMatrix(testing,control=list(weighting=weight,wordLengths=c(1,Inf)))
    #dtmTest <- as.data.frame(as.matrix(dtmTest))
    dtmTest
  }

## How to use : dtmTest<-createDtmTestTf(test)
createDtmTestTf <- function(test)
{
  require(tm)
  testing <- Corpus(VectorSource(test[,2]))
  funs <- list(stripWhitespace)
  docsTest <- tm_map(testing,FUN=tm_reduce,tmFuns=funs)
  weight <- function(x) weightTf(x)
  dtmTest <- DocumentTermMatrix(docsTest,control=list(weighting=weight))
  #dtmTest <- as.data.frame(as.matrix(dtmTest))
  dtmTest
}
#how to use : l<-ldaFS(train,numtopics=4)
ldaFS <- function(train,numtopics=4)
  {
    require(topicmodels)
    dtmTrain<-createDtmTrainTf(train)
    dtmTrain <- as.matrix(dtmTrain)
    print(dim(dtmTrain))
    removed <- which(rowSums(dtmTrain)==0)
    dtmTrain <- dtmTrain[-removed,]
    dtmTrain=as.DocumentTermMatrix(dtmTrain,weighting=weightTf)
    l <- LDA(dtmTrain,numtopics)
    print("LDA DONE")
    l
  }
#How to use: topic <- extractTopic(l)
extractTopic <- function(l)
{
  topic <- topics(l)
  topic
}
#How to use: term <- extractTerm(l,numterms=25)
extractTerm <- function(l,numterms=25)
{
  term <- terms(l,numterms)
  dim(term) <- NULL
  
  term
}
                                        # How to use: indres <- insertFeaturesAndMakeExp(train,term)
insertFeaturesAndMakeExp <- function(train,term)
  {
    dups <- term[which(duplicated(term))]
    
    term <- term[which(!duplicated(term))]
    dtmTrain<-createDtmTrain(train)
    dtmTrain <- as.matrix(dtmTrain)
    dtmTrain <- dtmTrain[,term]
    
    #dtmTrain <- dtmTrain[,-match(dups,colnames(dtmTrain))]
    dtmTrain <- as.data.frame(dtmTrain)
    print("FINAL MATRIX READY")
    load("targetVariable.Rdata")
    #dtmTrain$Topic <- topic
    dtmTrain$targetVariable <- targetVariable
    indres <- makeExperiment(dtmTrain)
    dtmTrain
}
# How to use: indres<-expLDA(train,numterms=25,numtopics=4)
expLDA <- function(train,numterms=25,numtopics=4)
  {
l<-ldaFS(train,numtopics=4)
term <- extractTerm(l,numterms)
#topic <- extractTopic(l)
indres <- insertFeaturesAndMakeExp(train,term)
indres
  }

findClusters <- function(train,numclusters=6)
  {
    require(cluster)
    d <- dist(train[,-ncol(train)])
    avgS <- c()
    for(k in 2:numclusters)
      {
        cl <- kmeans(train[,-ncol(train)],centers=k,iter.max=200)
        s <- silhouette(cl$cluster,d)
        avgS <- c(avgS,mean(s[,3]))
        
      }
    plot(2:numclusters,avgS,type="b",main="Average Silhouette Coeffiecient",xlab="nr. clusters")
              
  }

printWords <- function(c,dtmTrain)
{
  for(i in 1:length(c))
    {
      num <- c[i]
      print(dtmTrain[num,which(dtmTrain[num,-ncol(dtmTrain)]!=0)])
    }
}
#How to use: predicted <- exactExp(dtmTrain,dtmTest,insults)
exactExp <- function(dtmTrain,dtmTest,insults)
  {
    require(gbm)
    require(Metrics)
    m <- gbm(targetVariable~. ,data = dtmTrain, verbose = T, n.trees = 500,distribution=c("gaussian"),shrinkage=c(0.01),n.minobsinnode=c(5),bag.fraction=c(0.5),interaction.depth=c(14))
    m_result <- predict(m, dtmTest, type="response", n.trees = 500)
    predicted <- m_result
    predicted<-(pmax(predicted, 0))
    predicted<-(pmin(predicted, 1))
    print(auc(insults,predicted))
    results <- auc(insults,predicted)
    predicted
  }
#How to use: predicted <- exactExpNB(dtmTrain,dtmTest,insults)
exactExpNB <- function(dtmTrain,dtmTest,insults)
  {
    require(e1071)
    require(Metrics)
    m <- naiveBayes(targetVariable~. ,data = dtmTrain)
    answers <- predict(m, dtmTest, type="raw",laplace=1)
    answers <- answers[,"1"]
    predicted<-answers
    print(auc(insults,predicted))
    results <- auc(insults,predicted)
    predicted
  }

#How to use: predictedRF <- exactExpRF(dtmTrain,dtmTest,insults)
exactExpRF <- function(dtmTrain,dtmTest,insults)
  {
    require(randomForest)
    require(Metrics)
    m <- randomForest(targetVariable~. ,data = dtmTrain,mtry=3,ntrees=300)
    m_result <- predict(m, dtmTest, type="response", ntrees = 300)
    predicted <- m_result
    predicted<-(pmax(predicted, 0))
    predicted<-(pmin(predicted, 1))
    print(auc(insults,predicted))
    results <- auc(insults,predicted)
    predicted
  }

#How to use: predictedLM <- exactExpLM(dtmTrain,dtmTest,insults)
exactExpLM <- function(dtmTrain,dtmTest,insults)
  {
    require(Metrics)
    m <- lm(targetVariable~. ,data = dtmTrain)
    m_result <- predict(m, dtmTest)
    predicted <- m_result
    predicted<-(pmax(predicted, 0))
    predicted<-(pmin(predicted, 1))
    print(auc(insults,predicted))
    results <- auc(insults,predicted)
    predicted
  }

#How to use: predictedTREE <- exactExpTREE(dtmTrain,dtmTest,insults)
exactExpTREE <- function(dtmTrain,dtmTest,insults)
{
  require(DMwR)
  require(Metrics)
  m <- rpartXse(targetVariable~. ,data = dtmTrain,se=1)
  m_result <- predict(m, dtmTest)
  predicted <- m_result
  predicted<-(pmax(predicted, 0))
  predicted<-(pmin(predicted, 1))
  print(auc(insults,predicted))
  results <- auc(insults,predicted)
  predicted
}

#How to use: predictedM5 <- exactExpM5(dtmTrain,dtmTest,insults)
exactExpM5 <- function(dtmTrain,dtmTest,insults)
{
  require(RWeka)
  require(Metrics)
  m <- M5P(targetVariable~. ,data = dtmTrain)
  m_result <- predict(m, dtmTest)
  predicted <- m_result
  predicted<-(pmax(predicted, 0))
  predicted<-(pmin(predicted, 1))
  print(auc(insults,predicted))
  results <- auc(insults,predicted)
  predicted
}

#How to use: predictedKKNN <- exactExpKKNN(dtmTrain,dtmTest,insults)
exactExpKKNN <- function(dtmTrain,dtmTest,insults)
{
  require(kknn)
  require(Metrics)
  m <- kknn(targetVariable~. ,dtmTrain,dtmTest,distance=1,kernel="gaussian",k=10)
  m_result <- m$fitted.values
  predicted <- m_result
  predicted<-(pmax(predicted, 0))
  predicted<-(pmin(predicted, 1))
  print(auc(insults,predicted))
  results <- auc(insults,predicted)
  predicted
}



#write(paste(as.character(names(difs)),"Predicted:",as.character(solutions[as.integer(names(difs))]),"Actual:",as.character(insults[as.integer(names(difs))]),test[as.integer(names(difs)),2],"\n\n\n"),file="testing.txt")

#How to use: dtmBinTrain <- binify(dtmTrain)
binify <- function(dtm)
  {
    dtmTrainBin <- as.matrix(dtm)
    dim(dtmTrainBin) <- NULL
    dtmTrainBin[which(dtmTrainBin!=0)] <- 1
    dtmTrainBin=as.character(dtmTrainBin)
    #dtmTrainBin=as.factor(dtmTrainBin)             
dtmTrainBin <- matrix(dtmTrainBin,nrow=nrow(dtm),ncol=ncol(dtm))
    dtmTrainBin <- as.data.frame(dtmTrainBin,stringsAsFactors=TRUE)
    
    colnames(dtmTrainBin) <- colnames(dtm)
    dtmTrainBin
  }

rankzor <- function(solutions,L=20)
  {
    bgns <- seq(1,length(solutions),L)
    ends <- seq((L),length(solutions),(L))
    res <- c()
    for(i in 1:length(solutions))
      {
        res <- c(res,rank(solutions[bgns[i]:ends[i]]))
      }
  }
#How to use: l= buildData()
buildData <- function()
{
  
  train<-normalProcedure()
  test<-normalProcedureTest()
  dtmTrain<-createDtmTrain(train)
  dtmTest<-createDtmTest(test)
  dtmTrain=as.data.frame(as.matrix(dtmTrain))
  dtmTest=as.data.frame(as.matrix(dtmTest))
  dtmTrain=dtmTrain[,intersect(colnames(dtmTrain),colnames(dtmTest))]
  dtmTest=dtmTest[,colnames(dtmTrain)]
  testSums=colSums(dtmTest)
  testSums=testSums[order(testSums,decreasing=TRUE)]
  fs=names(which(testSums>=mean(testSums)))
#  fs=names(testSums)[1:1500] #COM ISTO CONSIGO 0,896 EM GBM SEM COUNTFUCK
  dtmTrain=dtmTrain[,fs]
  dtmTest=dtmTest[,fs]
  load("targetVariable.Rdata")
  load("newfeaturestest.Rdata")
  load("newfeatures.Rdata")
  load("insults.Rdata")
  #res <- checkBigrams(dtmTrain)
  #visited <- consecutiveWords(train,3)
  #restest <- checkBigrams(dtmTest)
  #visitedtest <- consecutiveWords(test,2)
  #mtxTrain <- buildBigramMatrix(res,dtmTrain)
  #mtxTest <- buildBigramMatrix(restest,dtmTest)
  #cWordsTrain <- modifyBigramToConsecutiveBigram(visited,mtxTrain)
  #cWordsTest <- modifyBigramToConsecutiveBigram(visitedtest,mtxTest)
  #cWordsTrain=cWordsTrain[,intersect(colnames(cWordsTrain),colnames(cWordsTest))]
  #cWordsTest=cWordsTest[,colnames(cWordsTrain)]
  
  a=list(dtmTrain,dtmTest)
  a
  
}

load("newfeatures.Rdata")
load("newfeaturestest.Rdata")
load("targetVariable.Rdata")
load("insults.Rdata")
#How to use: ensemble=winTournament("theInsults.csv")
winTournament <- function(dtmTrain,dtmTest,filename)
{
  l=buildData()
  dtmTrain=l[[1]]
  dtmTest=l[[2]]
  colnames(dtmTest)[c(17,20,45,304,260,827)]=c("ini","fori","ifi","whilei","nexti","breaki")
  colnames(dtmTrain)[c(17,20,45,304,260,827)]=c("ini","fori","ifi","whilei","nexti","breaki")
  
  dtmBinTrain=binify(dtmTrain)
  dtmBinTest=binify(dtmTest)
  dtmTrain <- insertVariablesTrain(dtmTrain,c("countcapstrain","countletterstrain","countmeanwordtrain","countotherspecialchartrain","countyouaretrain","countfucktrain","countracetrain"),newfeatures,targetVariable)
  dtmTest <- insertVariablesTest(dtmTest,c("countcapstrain","countletterstrain","countmeanwordtrain","countotherspecialchartrain","countyouaretrain","countfucktrain","countracetrain"),newfeaturestest)
  dtmBinTrain <- insertVariablesTrain(dtmBinTrain,c("countcapstrain","countletterstrain","countmeanwordtrain","countotherspecialchartrain","countyouaretrain","countfucktrain","countracetrain"),newfeatures,targetVariable)
  dtmBinTest <- insertVariablesTest(dtmBinTest,c("countcapstrain","countletterstrain","countmeanwordtrain","countotherspecialchartrain","countyouaretrain","countfucktrain","countracetrain"),newfeaturestest)
  #dtmBinTrainClass=dtmBinTrain
  #dtmBinTrainClass$targetVariable=as.factor(as.character(dtmBinTrain$targetVariable))
  #m=AUCRF(targetVariable~.,dtmBinTrainClass,mtry=3,ntrees=300)
  #fs=m$Xopt
  #dtmTrainFS=dtmTrain
  #dtmTestFS=dtmTest
  #dtmBinTrainFS=dtmBinTrain
  #dtmBinTRainFS=dtmBinTest
  #dtmTrainFS=dtmTrainFS[,c(fs,"targetVariable")]
  #dtmBinTrainFS=dtmBinTrainFS[,c(fs,"targetVariable")]
  #dtmTestFS=dtmTestFS[,fs]
  #dtmBinTestFS=dtmBinTestFS[,fs]
  
  
  predicted_gbm=runModel(dtmTrain,dtmTest)
  predicted_rf=runModelRF(dtmTrain,dtmTest)
  predicted_gbm_bin=runModel(dtmBinTrain,dtmBinTest)
  predicted_rf_bin=runModelRF(dtmBinTrain,dtmBinTest)
  #predicted_lm=exactExpLM(dtmTrain,dtmTest,insults)
  #predicted_lm_bin=exactExpLM(dtmBinTrain,dtmBinTest,insults)
  ensemble=rbind(predicted_gbm,predicted_rf,predicted_gbm_bin,predicted_rf_bin)#,predicted_lm,predicted_lm_bin)
  finalpred=apply(ensemble,2,mean)
  print(auc(insults,finalpred))
  write.table(finalpred,file=filename,col.names=TRUE,row.names=FALSE,quote=FALSE)
  ensemble
  
  
  
}

interestingBigrams <- function(words,bigrams)
{
  
  extrafeatures <- colnames(bigrams)[grep(words,colnames(bigrams))]
  extrafeatures
}

bigramWords <- function(extrafeatures)
{
  
  features=paste(extrafeatures,collapse="|")
  fs=unlist(strsplit(features,split="|",fixed=TRUE))
  fs=fs[-which(duplicated(fs))]
  fs
}

writeAnalysisFile <- function(filename,difs,solutions,insults,test)
{
  write(paste(as.character(names(difs)),"Predicted:",as.character(solutions[as.integer(names(difs))]),"Actual:",as.character(insults[as.integer(names(difs))]),test[as.integer(names(difs)),2],"\n\n\n"),file=filename)
}

#fs=interestingBigrams(words="\\bar\\b|\\ba\\b|\\bbyou\\b|\\byour\\b|\\bwe\\b|\\bi\\b|\\bthey\\b|\\bthem\\b",bigrams=mtxTrain)
#features=paste(fs,collapse="|")
#fags=unlist(strsplit(fs,split="|",fixed=TRUE))
#fags=fags[-(which(duplicated(fags)))]

# Melhor resultado ateh agora 0.9072 sem race , religion e fuck feito com media de gbmbin,gbm,rfbin,rf
#extrafeatures <- colnames(bigrams)[grep("you|your|all|are|being",colnames(bigrams))]
#> dtmTrain <- cbind(dtmTrain,bigrams[,extrafeatures])
#> dim(dtmTrain)
#[1] 3947 1466
#> dtmTest <- dtmTest[,-c(1001:ncol(dtmTest))]
#> originaldtmTest <- dtmTest
#> load("intersectedBigramsTest_dtmTest.Rdata")
#> bigramsTest <- dtmTest
#> dtmTest <- originaldtmTest
#> dtmTest <- cbind(dtmTest,bigramsTest[,extrafeatures])
#> dim(dtmTest)
#[1] 2647 1466
