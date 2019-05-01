library(shiny)

library("xts")
#library("TSA")
library("lubridate")
#library("sas7bdat")
library(reshape2)
library(ggplot2)
library(data.table)


## useful funcitons
  # standardize vetor to mean 0, var 1
stdFcn = function(x){return((x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))}

  # smooth data by k-days
kSmooth = function(data.xts, k){
  # k-day smoothing for data in xts form
  # takes the average of the current and previous k-1 days, even if some of those days are missing data
  beginDate = index(data.xts)[1] # first date in xts data
  xtsDates = index(data.xts)[index(data.xts)-beginDate >= (k-1)] # dates to interate over
  newData = matrix(NA, nrow=length(xtsDates), ncol=ncol(data.xts)) # create new data frame of smoothed data
  colnames(newData) = names(data.xts) # same column names as original data
  for(date in xtsDates){
    date = as.Date(date)
    subset = data.xts[date-index(data.xts)>=0 & date-index(data.xts)<=(k-1),] # data of current and previous k-1 days
    newData[which(xtsDates==date),] = colMeans(subset, na.rm=TRUE) # means of each variable, ignoring NA's
  }
  rownames(newData) = as.Date(xtsDates)
  dataNew.xts = xts(newData, order.by=as.Date(as.numeric(rownames(newData))))
  return(dataNew.xts)
}

lLag = function(data.xts, l, vars){
  xtsLags = data.xts[,which(names(data.xts) %in% vars)]
  xtsStay = data.xts[,-which(names(data.xts) %in% vars)]
  index(xtsLags) = index(xtsLags) + l
  xtsMerge = merge(xtsLags, xtsStay)
  return(xtsMerge)
}

  # get sentiment by day based on method
sentMethod = function(nPosWords, nNegWords, nWords, method){
  if(method==1){ return(ifelse(nWords==0, NA, (nPosWords-nNegWords)/nWords)) }
  if(method==2){ return(ifelse(nNegWords==0, NA, nPosWords/nNegWords)) }
  if(method==3){ return(ifelse(nNegWords+nPosWords==0, NA, nPosWords/(nPosWords+nNegWords))) }
}

  # comovement
scor = function(x, y){
  # x, y vectors
  # output percent of time x and y move in the same direction
  xChanges = x[-1] - x[-length(x)]
  yChanges = y[-1] - y[-length(y)]
  return(sum(sign(xChanges)==sign(yChanges), na.rm=TRUE) / sum(is.na(sign(xChanges)==sign(yChanges))==FALSE))
  #return(cor(sign(xChanges), sign(yChanges)))
}

scorMat = function(M){
  # M = matrix to take scar of each variable
  M = as.matrix(M)
  dim = ncol(M)
  Mscors = matrix(NA, nrow=dim, ncol=dim)
  for(i in 1:dim){
    for(j in 1:dim){
      Mscors[i,j] = scor(M[,i], M[,j])
    }
  }
  rownames(Mscors) = colnames(M)
  colnames(Mscors) = colnames(M)
  return(Mscors)
}

  # add and subtract one month from a date
addMonth = function(date){
  return(date %m+% months(1))
}
subtractMonth = function(date){
  return(date %m-% months(1))
}
relevantDays = function(date, beginDate, endDate){
  # gets all month days from beingDate to endDate
  days = c()
  day = date
  if(day >=beginDate & day <=endDate) days = c(days, day)
  # get all days from beginDate to date
  while(day > beginDate){
    day = subtractMonth(day)
    if(day >= beginDate & day <= endDate){
      days = c(days, day)
    }
  }
  # get all days from date to endDate
  day = date
  while(day < endDate){
   day = addMonth(day) 
   if(day >=beginDate & day <= endDate){
     days = c(days, day)
   }
  }
  return(as.Date(days)[order(as.Date(days))])
}

#setwd('C:/Users/Robyn/Box Sync/Documents/School/Michigan/Research/Twitter/Scripts/category app')

## read in Survey Data
dailySurvey = fread('allSurveyData.csv')
dailySurvey$Time = as.Date(dailySurvey$Time, format='%m/%d/%Y')
dailySurvey.xts = xts(dailySurvey[,-c(1:3)], order.by=as.Date(dailySurvey$Time))

## read in Twitter Data
dailyTwitter = fread('dailyTwitter.csv')
dailyTwitter$date = as.Date(dailyTwitter$date)
dailyTwitter.xts = xts(dailyTwitter[,-c(1:2)], order.by=as.Date(dailyTwitter$date))

## link up Twitter and survey data, smooth
allData = merge(dailySurvey.xts, dailyTwitter.xts)
#allData = allData[index(allData)<as.Date('2014-06-28'),]


ui <- fluidPage(
  titlePanel('Relationship between Sentiment of Jobs Tweets by Category and Index of Consumer Sentiment'),
  sidebarLayout(
    sidebarPanel(
                 # beginning and ending dates input:
                 h3('Dates:'),
                 dateInput('beginDate', label=h4('Begin Date'), value=index(allData)[1]),
                 dateInput('endDate', label=h4('End Date'), value=index(allData)[nrow(allData)]),
                 
                 h3('Variables:'),
                 # which survey questions to include:
                 checkboxGroupInput('surveyQuestions', label=h4('Survey Questions'), choices=list('ICS', 'ICC', 'ICE', 'PAGO', 'PEXP', 'BUS12', 'BUS5', 'DUR'),
                                    selected=c('ICS')),
                 h5('PAGO: "We are interested in how people are getting along financially these days. Would you say that you (and your family living there) are better off or worse off financially than you were a year ago?"'),
                 h5('PEXP: "Now looking ahead--do you think that a year from now you (and your family living there) will be better off financially, or worse off, or just about the same as now?"'),
                 h5('BUS12: "Now turning to business conditions in the country as a whole--do you think that during the next twelve months we will have good times financially, or bad times, or what?"'),
                 h5('BUS5: "Looking ahead, which would you say is more likely--that in the country as a whole we will have continuous good times during the next five years or so, or that we will have periods of widespread unemployment or depression, or what?"'),
                 h5('DUR: "About the big things people buy for their homes--such as furniture, a refrigerator, stove, television, and things like that. Generally speaking, do you think now is a good or bad time for people to buy major household items?"'),
                 # individual categories
                 checkboxGroupInput('twitterCats', label=h4('Twitter Categories'), choices=list('All'='all', 'News/Politics'='np', 'Personal'='pers', 'Ads'='advert', 'Other'='other', 'Junk'='junk'),
                                    selected=c('all')),
                 # combination category
                 checkboxGroupInput('combo', label=h4('Category Combination'), choices=list('News/Politics'='np', 'Personal'='pers', 'Ads'='advert', 'Other'='other', 'Junk'='junk'),
                                    selected=c('np', 'pers', 'other')),
                 
                 h3('Sentiment:'),
                 # method of finding sentiment--dictionary and method
                 radioButtons('DBML', label=h4('Sentiment Method'), choices=list('Dictionary Based'='DB', 'Vader'='VD', 'Textblob'='TB')),
                 h4('If dictionary based:'),
                 radioButtons('dict', label=h4('Dictionary'), choices=list('Lexicoder'='LC', 'Liu Hu'='LH', 'OpinionFinder'='OF')),
                 radioButtons('countPN', label=h4('How to count positives, negatives'), choices=list('Number of words'='total', 'Number of tweets'='I')),
                 radioButtons('sentMeth', label=h4('How to calculate sentiment'), choices=list('(pos-neg)/(#Words or #Tweets)'=1, 'pos/neg'=2, 'pos/(pos+neg)'=3)),
                 
                 #lag
                 numericInput('lag', label=h4('Survey shift'), value=0),
                 h5('Shifts survey data. For example if lag=2 and ICS=80.45 on 1/3/08, ICS is shifted forward 2 days, so now
                    ICS=80.45 on 1/5/08. Twitter variables remain the same.'),
                 
                 h3('Correlation:'), 
                 # smoothing
                 numericInput('k', label=h4('Smoothing'), value=30),
                 # rolling correlation window
                 numericInput('rollCorWindow', label=h4('Rolling Correlation Window'), value=365),
                 
                 h3('Comovement:'),
                 # rolling comovement time unit
                 radioButtons('metUnit', label=h4('Comovement Time Unit'), choices=list('Days', 'Weeks', 'Months')),
                 dateInput('unitBegin', label=h4('Being Comovement Day'), value=as.Date('2017-01-01')),
                 h5('Use for day of week/month to start on. For example, to measure percent of time two variables move together on
                    a weekly basis, as measured from Friday through the next Thursday, choose any Friday on the calendar. To measure
                    on a monthly basis, measured from the 1st through the end of the month, choose the 1st of any month 
                    on the calendar.'),
                 # rolling comovement window
                 numericInput('metWindow', label=h4('Comovement Window'), value=365),
                 h5('For rolling comovement plot, choose how many time units to use.')
                 ),
    mainPanel(
              h2('Timeplot of Twitter Sentiment and Consumer Confidence'), plotOutput('timePlot'),
              # correlations
              h2('Correlations using smoothed data:'), tableOutput('corMat'), plotOutput('rollingCorPlot'),
              # comovement
              h2('Comovement:'), tableOutput('scorMatrix'), plotOutput('rollingMetricPlot')
    )
  )
)

server <- function(input, output, session) {
  
  # restricts to columns of relevant variables
  newDataVars = reactive({
    vars = c()
    # get sentiment for combo categories (if combo variables selected)
    if(length(input$combo)>0){
      nComboTweets = rowSums(subset(dailyTwitter, select=paste0(input$combo, 'WordsI')))
      # if machine learning method:
      if(input$DBML!="DB"){
        comboSent = rep(NA, nrow(dailyTwitter))
        nComboTweets = replace(nComboTweets, nComboTweets==0, NA)
        for(cat in input$combo){
          catProp = subset(dailyTwitter, select=paste0(cat, 'WordsI')) / nComboTweets
          comboSent = rowSums(cbind(comboSent, subset(dailyTwitter, select=paste0(cat, input$DBML))*catProp), na.rm=TRUE)
        }
        comboSent = replace(comboSent, is.na(nComboTweets), NA)
      }
      # else if dictionary-based:
      else{
        comboPos = rep(0, nrow(dailyTwitter)); comboNeg = rep(0, nrow(dailyTwitter)); comboWords = rep(0, nrow(dailyTwitter))
        for(cat in input$combo){
          comboPos = rowSums(cbind(comboPos, subset(dailyTwitter, select=paste0(cat, input$dict, 'pos', ifelse(input$countPN=='I', 'I', '')))))
          comboNeg = rowSums(cbind(comboNeg, subset(dailyTwitter, select=paste0(cat, input$dict, 'neg', ifelse(input$countPN=='I', 'I', '')))))
          comboWords = rowSums(cbind(comboWords, subset(dailyTwitter, select=paste0(cat, 'Words', ifelse(input$countPN=='I', 'I', '')))))
        }
        comboSent = sentMethod(comboPos, comboNeg, comboWords, input$sentMeth)
        comboSent = replace(comboSent, nComboTweets==0, NA)
      }
      combo.xts = xts(comboSent, order.by=dailyTwitter$date)
      names(combo.xts) = c('combo')
      dat = merge(allData, combo.xts)
      dat = data.frame(dat)
      names(dat) = c(names(allData), 'combo')
      vars = c(vars, 'combo')
    } else{ # if no combo variables selected
      dat = data.frame(allData)
    }
    # dat = data frame of allData and combo variable if selected
    
    # sentiment for individual categories based on dictionary, method
    for(cat in input$twitterCats){
      if(input$DBML=='DB'){ # dictionary-based
        dat$new = sentMethod(dat[,which(names(dat)==paste0(cat, input$dict, 'pos', ifelse(input$countPN=='I', 'I', '')))],
                             dat[,which(names(dat)==paste0(cat, input$dict, 'neg', ifelse(input$countPN=='I', 'I', '')))],
                             dat[,which(names(dat)==paste0(cat, 'Words', ifelse(input$countPN=='I', 'I', '')))], input$sentMeth)
      }
      else{ # vader/textblob
        dat$new = dat[,which(names(dat)==paste0(cat, input$DBML))]
      }
      names(dat)[ncol(dat)] = paste0(cat, 'Sent')
      vars = c(vars, paste0(cat, 'Sent'))
    }
    
    # create data frame of only checked survey and twitter variables
    vars = c(vars, input$surveyQuestions)
    datSub = subset(dat, select=vars)
    datSub
  })
  
  # add lag
  newDataLag = reactive({
    dataNew = newDataVars() # use data frame of wanted variables
    dataNew.xts = xts(dataNew, order.by = as.Date(rownames(dataNew))) # convert to xts format
    datSubSmoothLag.xts = lLag(dataNew.xts, input$lag, input$surveyQuestions) # add lag specified in input
    datSubSmoothLag.xts # outputs xts of lagged data
  })
  
  # smooth
  newDataSmoothed = reactive({
    datSub = newDataLag() # use xts of already lagged data
    #print('before smooth'); print(head(datSub)); print(class(datSub)); print(head(index(datSub))); print(dim(datSub))
    datSubSmooth.xts = kSmooth(datSub, input$k) # smoothed by parameter in input
    #print('after smooth'); print(head(datSubSmooth.xts))
    data.frame(datSubSmooth.xts) # output dataframe of smoothed and lagged data for correct variables
  })
  
  # restricts to rows within the specified date range
  newData = reactive({
    datSubSmooth = newDataSmoothed() # use data frame of smoothed and lagged data
    rows = which(as.Date(rownames(datSubSmooth))>=input$beginDate & as.Date(rownames(datSubSmooth))<=input$endDate)
    datFinal = datSubSmooth[rows,]
    datFinal # output data frame
  })
  
  
  ## comovement stuuuuuuff
  newMetData = reactive({
    dat = newDataLag() # xts of lagged data
    if(input$metUnit=='Days'){
      newData = data.frame(dat) # don't smooth at all
    }
    
    else if(input$metUnit=='Weeks'){
      weekSmooth = kSmooth(dat, 7) # smooth 7 days
      dayOfWeek = weekdays(input$unitBegin) # find weekday of interest
      print(dayOfWeek)
      weekSmooth = weekSmooth[which(weekdays(index(weekSmooth))==dayOfWeek),] # restrict to days that match the weekday of interest
      newData = data.frame(weekSmooth)
    }
    
    else{ # if input$metUnit=='Months'
      monthDays = relevantDays(input$unitBegin, input$beginDate, input$endDate)
      monthDays = c(monthDays, as.Date(addMonth(monthDays[length(monthDays)])))
      newData = data.frame(dat[1,])
      for(i in 1:(length(monthDays)-1)){
        day = as.Date(monthDays[i])
        temp = dat[index(dat)>=day & index(dat)<as.Date(monthDays[i+1]),]
        newData = rbind(newData, colSums(temp, na.rm=TRUE)/nrow(temp))
        #rownames(newData)[nrow(newData)] = day
      }
      newData = newData[-1,]
      rownames(newData) = monthDays[1:nrow(newData)]
    }
    data.frame(newData) # output data frame of data restricted to correct dates
    #print(dim(newData))
  })

  #### outputs:
  output$timePlot = renderPlot({
    df = newData() # use date-restricted data
    #print('df1'); print(head(df))
    df = data.frame(apply(df, 2, stdFcn))
    #for(i in 1:ncol(df)){
    #  df[,i] = stdFcn(df[,i]) # standardize each column
    #}
    df$date = as.Date(rownames(df))
    #print('df2'); print(df[200:205,])
    df = melt(df, id.vars='date', variable.name='variable') # convert data frame so it's easier to plot
    ggplot(df, aes(date, value)) + geom_line(aes(colour=variable)) + xlab('Date') + ylab('Standardized Sentiment')
  })
  
  output$corMat = renderTable({
    round(cor(newData(), use='pairwise.complete'), 2) # table of correlations between each variable
  }, rownames=TRUE)
  
  output$scorMatrix = renderTable({ # table of new 'correlation' metric
    data = data.frame(newMetData()) # use data frame smoothed correctly/correct dates
    #print(class(data))
    data = data[which(as.Date(rownames(data))>=input$beginDate & as.Date(rownames(data))<=input$endDate),] # restrict to dates of interest
    #print(scor(data[,1], data[,2]))
    #scorMat(as.matrix(data)) # get matrix of new 'correlations'
    #print(as.matrix(data)[1:3, 1:3]); print(dim(data))
    dataMat = as.matrix(data)
    #print(scor(dataMat[,1], dataMat[,2]))
    matTest = matrix(NA, nrow=ncol(dataMat), ncol=ncol(dataMat))
    for(i in 1:ncol(dataMat)){
      for(j in 1:ncol(dataMat)){
        matTest[i,j] = scor(dataMat[,i], dataMat[,j])
      }
    }
    rownames(matTest) = colnames(dataMat); colnames(matTest) = colnames(dataMat)
    matTest
  }, rownames=TRUE)
  
  output$rollingCorPlot = renderPlot({
    df =  newDataSmoothed()
    rollingWindow = input$rollCorWindow
    surveyVars = input$surveyQuestions
    twitterVars = names(df)[-which(names(df) %in% surveyVars)]
    if(length(surveyVars)*length(twitterVars)==0){ # if not enough variables to do pairwise correlations
      plot(0, 0, type='n', main='Need more variable for rolling correlation plot')
    }
    else{ # have enough twitter,ICS variables to do pairwise correlations
      corArray = array(NA, dim=c(ncol(df), ncol(df), nrow(df)-rollingWindow))
      for(i in 1:dim(corArray)[3]){
        temp = df[i:(i+rollingWindow),]
        corArray[,,i] = cor(temp, use='pairwise.complete')
      }
      cors = c(); vars = c(); startDates = c()
      surveyIndex = which(names(df) %in% surveyVars)
      twitterIndex = which(names(df) %in% twitterVars)
      rollingData = data.frame()
      for(sIn in surveyIndex){
        for(tIn in twitterIndex){
          cors = c(cors, corArray[sIn, tIn,])
          surveyVar = names(df)[sIn]; twitterVar = names(df)[tIn]
          vars = c(vars, rep(paste0(surveyVar, '...', twitterVar), dim(corArray)[3]))
          startDates = c(startDates, as.Date(rownames(df))[1:dim(corArray)[3]])
        }
      }
      rollingData = data.frame(as.Date(startDates), cors, vars)
      names(rollingData) = c('date', 'correlation', 'vars')
      ggplot(rollingData, aes(x=date, y=correlation)) + geom_line(aes(colour=vars)) +
        xlim(input$beginDate, input$endDate-rollingWindow) + xlab('Start Date') + ylab('Correlation') + 
        ggtitle('Rolling Correlation') + ylim(-1, 1)
    }
  })

  
  output$rollingMetricPlot = renderPlot({
    df =  data.frame(newMetData())
    rollingWindow = input$metWindow
    surveyVars = input$surveyQuestions
    twitterVars = names(df)[-which(names(df) %in% surveyVars)]
    if(length(surveyVars)*length(twitterVars)==0){ # if not enough variables to do pairwise correlations
      plot(0, 0, type='n', main='Need more variable for rolling correlation plot')
    }
    else{
      corArray = array(NA, dim=c(ncol(df), ncol(df), nrow(df)-rollingWindow))
      for(i in 1:dim(corArray)[3]){
        temp = df[i:(i+rollingWindow),]
        corArray[,,i] = scorMat(as.matrix(temp))
      }
      cors = c(); vars = c(); startDates = c()
      surveyIndex = which(names(df) %in% surveyVars)
      twitterIndex = which(names(df) %in% twitterVars)
      rollingData = data.frame()
      for(sIn in surveyIndex){
        for(tIn in twitterIndex){
          cors = c(cors, corArray[sIn, tIn,])
          surveyVar = names(df)[sIn]; twitterVar = names(df)[tIn]
          vars = c(vars, rep(paste0(surveyVar, '...', twitterVar), dim(corArray)[3]))
          startDates = c(startDates, as.Date(rownames(df))[1:dim(corArray)[3]])
        }
      }
      rollingData = data.frame(as.Date(startDates), cors, vars)
      names(rollingData) = c('date', 'correlation', 'vars')
      ggplot(rollingData, aes(x=date, y=correlation)) + geom_line(aes(colour=vars)) +
        xlim(input$beginDate, rollingData$date[nrow(rollingData)]) + xlab('Start Date') + ylab('Proportion') + 
        ggtitle('Rolling Comovement') + ylim(0, 1)
    }
  })
}

shinyApp(ui = ui, server = server)




