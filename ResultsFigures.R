## Figures and results for 'Social Media as an Alternative to Surveys of Opinions about the Economy'

library(xts)
library(ggplot2)
library(readr)
library(GGally)
library(data.table)
library(fields)
library(plotly)
library(htmlwidgets)

########## Functions
## Smoothing
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

## Lag
lLag = function(data.xts, l, vars){
  xtsLags = data.xts[,which(names(data.xts) %in% vars)]
  xtsStay = data.xts[,-which(names(data.xts) %in% vars)]
  index(xtsLags) = index(xtsLags) + l
  xtsMerge = merge(xtsLags, xtsStay)
  return(xtsMerge)
}


########## Read in Data
twitterData = read_csv('jobsTwitterData.csv')

dailySurvey = fread('allSurveyData.csv')
dailySurvey$Time = as.Date(dailySurvey$Time, format='%m/%d/%Y')
dailySurvey.xts = xts(dailySurvey[,-c(1:3)], order.by=as.Date(dailySurvey$Time))


########## Replicating O'Connor et. al. and sensitivity analysis results
# restrict to tweets from 2007-2014
twitter = twitterData[twitterData$date>=as.Date('2007-01-01') & twitterData$date<=as.Date('2014-12-31'),]

## create data frame for each "jobs" Twitter category
dailytwitter_all = data.frame()
dailytwitter_np = data.frame()
dailytwitter_pers = data.frame()
dailytwitter_ads = data.frame()
dailytwitter_junk = data.frame()
dailytwitter_other = data.frame()
for(day in as.Date('2007-01-01'):as.Date('2014-12-31')){
  print(as.Date(day))
  dayTemp = twitter[twitter$date==day,]
  # all
  pos = sum(dayTemp$OFpos>0); neg = sum(dayTemp$OFneg>0); twts = nrow(dayTemp)
  pos_wds = sum(dayTemp$OFpos); neg_wds = sum(dayTemp$OFneg); wds = sum(dayTemp$nWords)
  temp.df = data.frame('date'=day, 'M1'=pos/neg, 'M2'=(pos-neg)/twts, 'M3'=pos/(pos+neg), 
                       'VD'=mean(dayTemp$vader), 'TB'=mean(dayTemp$textblob),
                       'M1_wds'=pos_wds/neg_wds, 'M2_wds'=(pos_wds-neg_wds)/wds, 'M3_wds'=pos_wds/(pos_wds+neg_wds),
                       'M1_t_LC'=ifelse(sum(dayTemp$LCneg>0)>0, sum(dayTemp$LCpos>0)/sum(dayTemp$LCneg>0), NA),
                       'M1_t_LH'=ifelse(sum(dayTemp$LHneg>0)>0, sum(dayTemp$LHpos>0)/sum(dayTemp$LHneg>0), NA),
                       'M1_w_LC'=ifelse(sum(dayTemp$LCneg)>0, sum(dayTemp$LCpos)/sum(dayTemp$LCneg), NA),
                       'M1_w_LH'=ifelse(sum(dayTemp$LHneg)>0, sum(dayTemp$LHpos)/sum(dayTemp$LHneg), NA))
  dailytwitter_all = rbind(dailytwitter_all, temp.df)
  # news/politics
  dayTemp_np = dayTemp[dayTemp$class=='newsPolitics',]
  pos_np = sum(dayTemp_np$OFpos>0); neg_np = sum(dayTemp_np$OFneg>0); twts_np = nrow(dayTemp_np)
  pos_wds = sum(dayTemp_np$OFpos); neg_wds = sum(dayTemp_np$OFneg); wds = sum(dayTemp_np$nWords)
  temp.np = data.frame('date'=day, 'M1'=ifelse(neg_np>0, pos_np/neg_np, NA), 
                       'M2'=ifelse(twts_np>0, (pos_np-neg_np)/twts_np, NA), 
                       'M3'=ifelse(pos_np+neg_np>0, pos_np/(pos_np+neg_np), NA),
                       'VD' = mean(dayTemp_np$vader), 'TB'=mean(dayTemp_np$textblob),
                       'M1_wds'=ifelse(neg_wds>0, pos_wds/neg_wds, NA), 
                       'M2_wds'=(pos_wds-neg_wds)/wds, 
                       'M3_wds'=ifelse(pos_wds+neg_wds>0, pos_wds/(pos_wds+neg_wds), NA),
                       'M1_t_LC'=ifelse(sum(dayTemp_np$LCneg>0)>0, sum(dayTemp_np$LCpos>0)/sum(dayTemp_np$LCneg>0), NA),
                       'M1_t_LH'=ifelse(sum(dayTemp_np$LHneg>0)>0, sum(dayTemp_np$LHpos>0)/sum(dayTemp_np$LHneg>0), NA),
                       'M1_w_LC'=ifelse(sum(dayTemp_np$LCneg)>0, sum(dayTemp_np$LCpos)/sum(dayTemp_np$LCneg), NA),
                       'M1_w_LH'=ifelse(sum(dayTemp_np$LHneg)>0, sum(dayTemp_np$LHpos)/sum(dayTemp_np$LHneg), NA))
  dailytwitter_np = rbind(dailytwitter_np, temp.np)
  # personal
  dayTemp_pers = dayTemp[dayTemp$class=='personal',]
  pos_pers = sum(dayTemp_pers$OFpos>0); neg_pers = sum(dayTemp_pers$OFneg>0); twts_pers = nrow(dayTemp_pers)
  pos_wds = sum(dayTemp_pers$OFpos); neg_wds = sum(dayTemp_pers$OFneg); wds = sum(dayTemp_pers$nWords)
  temp.pers = data.frame('date'=day, 'M1'=ifelse(neg_pers>0, pos_pers/neg_pers, NA), 
                         'M2'=ifelse(twts_pers>0, (pos_pers-neg_pers)/twts_pers, NA), 
                         'M3'=ifelse(neg_pers+pos_pers>0, pos_pers/(pos_pers+neg_pers), NA),
                         'VD' = mean(dayTemp_pers$vader), 'TB'=mean(dayTemp_pers$textblob),
                         'M1_wds'=ifelse(neg_wds>0, pos_wds/neg_wds, NA), 
                         'M2_wds'=(pos_wds-neg_wds)/wds, 
                         'M3_wds'=ifelse(pos_wds+neg_wds>0, pos_wds/(pos_wds+neg_wds), NA),
                         'M1_t_LC'=ifelse(sum(dayTemp_pers$LCneg>0)>0, sum(dayTemp_pers$LCpos>0)/sum(dayTemp_pers$LCneg>0), NA),
                         'M1_t_LH'=ifelse(sum(dayTemp_pers$LHneg>0)>0, sum(dayTemp_pers$LHpos>0)/sum(dayTemp_pers$LHneg>0), NA),
                         'M1_w_LC'=ifelse(sum(dayTemp_pers$LCneg)>0, sum(dayTemp_pers$LCpos)/sum(dayTemp_pers$LCneg), NA),
                         'M1_w_LH'=ifelse(sum(dayTemp_pers$LHneg)>0, sum(dayTemp_pers$LHpos)/sum(dayTemp_pers$LHneg), NA))
  dailytwitter_pers = rbind(dailytwitter_pers, temp.pers)
  # advertisements
  dayTemp_ads = dayTemp[dayTemp$class=='advert',]
  pos_ads = sum(dayTemp_ads$OFpos>0); neg_ads = sum(dayTemp_ads$OFneg>0); twts_ads = nrow(dayTemp_ads)
  pos_wds = sum(dayTemp_ads$OFpos); neg_wds = sum(dayTemp_ads$OFneg); wds = sum(dayTemp_ads$nWords)
  temp.ads = data.frame('date'=day, 'M1'=ifelse(neg_ads>0, pos_ads/neg_ads, NA), 
                        'M2'=ifelse(twts_ads>0, (pos_ads-neg_ads)/twts_ads, NA), 
                        'M3'=ifelse(pos_ads+neg_ads>0, pos_ads/(pos_ads+neg_ads), NA),
                        'VD' = mean(dayTemp_ads$vader), 'TB'=mean(dayTemp_ads$textblob),
                        'M1_wds'=ifelse(neg_wds>0, pos_wds/neg_wds, NA), 
                        'M2_wds'=(pos_wds-neg_wds)/wds, 
                        'M3_wds'=ifelse(pos_wds+neg_wds>0, pos_wds/(pos_wds+neg_wds), NA),
                        'M1_t_LC'=ifelse(sum(dayTemp_ads$LCneg>0)>0, sum(dayTemp_ads$LCpos>0)/sum(dayTemp_ads$LCneg>0), NA),
                        'M1_t_LH'=ifelse(sum(dayTemp_ads$LHneg>0)>0, sum(dayTemp_ads$LHpos>0)/sum(dayTemp_ads$LHneg>0), NA),
                        'M1_w_LC'=ifelse(sum(dayTemp_ads$LCneg)>0, sum(dayTemp_ads$LCpos)/sum(dayTemp_ads$LCneg), NA),
                        'M1_w_LH'=ifelse(sum(dayTemp_ads$LHneg)>0, sum(dayTemp_ads$LHpos)/sum(dayTemp_ads$LHneg), NA))
  dailytwitter_ads = rbind(dailytwitter_ads, temp.ads)
  # junk
  dayTemp_junk = dayTemp[dayTemp$class=='junk',]
  pos_junk = sum(dayTemp_junk$OFpos>0); neg_junk = sum(dayTemp_junk$OFneg>0); twts_junk = nrow(dayTemp_junk)
  pos_wds = sum(dayTemp_junk$OFpos); neg_wds = sum(dayTemp_junk$OFneg); wds = sum(dayTemp_junk$nWords)
  temp.junk = data.frame('date'=day, 
                         'M1'=ifelse(neg_junk>0, pos_junk/neg_junk, NA),
                         'M2'=ifelse(twts_junk>0, (pos_junk-neg_junk)/twts_junk, NA), 
                         'M3'=ifelse(pos_junk+neg_junk>0, pos_junk/(pos_junk+neg_junk), NA),
                         'VD' = mean(dayTemp_junk$vader), 'TB'=mean(dayTemp_junk$textblob),
                         'M1_wds'=ifelse(neg_wds>0, pos_wds/neg_wds, NA), 
                         'M2_wds'=(pos_wds-neg_wds)/wds, 
                         'M3_wds'=ifelse(pos_wds+neg_wds>0, pos_wds/(pos_wds+neg_wds), NA),
                         'M1_t_LC'=ifelse(sum(dayTemp_junk$LCneg>0)>0, sum(dayTemp_junk$LCpos>0)/sum(dayTemp_junk$LCneg>0), NA),
                         'M1_t_LH'=ifelse(sum(dayTemp_junk$LHneg>0)>0, sum(dayTemp_junk$LHpos>0)/sum(dayTemp_junk$LHneg>0), NA),
                         'M1_w_LC'=ifelse(sum(dayTemp_junk$LCneg)>0, sum(dayTemp_junk$LCpos)/sum(dayTemp_junk$LCneg), NA),
                         'M1_w_LH'=ifelse(sum(dayTemp_junk$LHneg)>0, sum(dayTemp_junk$LHpos)/sum(dayTemp_junk$LHneg), NA))
  dailytwitter_junk = rbind(dailytwitter_junk, temp.junk)
  # other
  dayTemp_other = dayTemp[dayTemp$class=='other',]
  pos_other = sum(dayTemp_other$OFpos>0); neg_other = sum(dayTemp_other$OFneg>0); twts_other = nrow(dayTemp_other)
  pos_wds = sum(dayTemp_other$OFpos); neg_wds = sum(dayTemp_other$OFneg); wds = sum(dayTemp_other$nWords)
  temp.other = data.frame('date'=day, 'M1'=ifelse(neg_other>0, pos_other/neg_other, NA), 
                          'M2'=ifelse(twts_other>0, (pos_other-neg_other)/twts_other, NA), 
                          'M3'=ifelse(pos_other+neg_other>0, pos_other/(pos_other+neg_other), NA),
                          'VD' = mean(dayTemp_other$vader), 'TB'=mean(dayTemp_other$textblob),
                          'M1_wds'=ifelse(neg_wds>0, pos_wds/neg_wds, NA), 
                          'M2_wds'=(pos_wds-neg_wds)/wds, 
                          'M3_wds'=ifelse(pos_wds+neg_wds>0, pos_wds/(pos_wds+neg_wds), NA),
                          'M1_t_LC'=ifelse(sum(dayTemp_other$LCneg>0)>0, sum(dayTemp_other$LCpos>0)/sum(dayTemp_other$LCneg>0), NA),
                          'M1_t_LH'=ifelse(sum(dayTemp_other$LHneg>0)>0, sum(dayTemp_other$LHpos>0)/sum(dayTemp_other$LHneg>0), NA),
                          'M1_w_LC'=ifelse(sum(dayTemp_other$LCneg)>0, sum(dayTemp_other$LCpos)/sum(dayTemp_other$LCneg), NA),
                          'M1_w_LH'=ifelse(sum(dayTemp_other$LHneg)>0, sum(dayTemp_other$LHpos)/sum(dayTemp_other$LHneg), NA))
  dailytwitter_other = rbind(dailytwitter_other, temp.other)
}

## correlations for each method using 30-day smoothing and 50-day lag
dailytwitter_all.xts = xts(dailytwitter_all[,-1], order.by=as.Date(dailytwitter_all$date))
twittermerged_all = merge(dailytwitter_all.xts, dailySurvey.xts$ICS)
twittermerged_all_lag = lLag(twittermerged_all, -50, c('ICS'))
twittermerged_all_lag_smooth = kSmooth(twittermerged_all_lag, 30)
cors_all = cor(twittermerged_all_lag_smooth[index(twittermerged_all_lag_smooth)<=as.Date('2009-12-31') &
                              index(twittermerged_all_lag_smooth)>=as.Date('2008-01-01'),], use='pairwise.complete')[,1]

dailytwitter_np.xts = xts(dailytwitter_np[,-1], order.by=as.Date(dailytwitter_np$date))
twittermerged_np = merge(dailytwitter_np.xts, dailySurvey.xts$ICS)
twittermerged_np_lag = lLag(twittermerged_np, -50, c('ICS'))
twittermerged_np_lag_smooth = kSmooth(twittermerged_np_lag, 30)
cors_np = cor(twittermerged_np_lag_smooth[index(twittermerged_np_lag_smooth)<=as.Date('2009-12-31') &
                             index(twittermerged_np_lag_smooth)>=as.Date('2008-01-01'),], use='pairwise.complete')[,1]

dailytwitter_pers.xts = xts(dailytwitter_pers[,-1], order.by=as.Date(dailytwitter_pers$date))
twittermerged_pers = merge(dailytwitter_pers.xts, dailySurvey.xts$ICS)
twittermerged_pers_lag = lLag(twittermerged_pers, -50, c('ICS'))
twittermerged_pers_lag_smooth = kSmooth(twittermerged_pers_lag, 30)
cors_pers = cor(twittermerged_pers_lag_smooth[index(twittermerged_pers_lag_smooth)<=as.Date('2009-12-31') &
                               index(twittermerged_pers_lag_smooth)>=as.Date('2008-01-01'),], use='pairwise.complete')[,1]

dailytwitter_ads.xts = xts(dailytwitter_ads[,-1], order.by=as.Date(dailytwitter_ads$date))
twittermerged_ads = merge(dailytwitter_ads.xts, dailySurvey.xts$ICS)
twittermerged_ads_lag = lLag(twittermerged_ads, -50, c('ICS'))
twittermerged_ads_lag_smooth = kSmooth(twittermerged_ads_lag, 30)
cors_ads = cor(twittermerged_ads_lag_smooth[index(twittermerged_ads_lag_smooth)<=as.Date('2009-12-31') &
                              index(twittermerged_ads_lag_smooth)>=as.Date('2008-01-01'),], use='pairwise.complete')[,1]

dailytwitter_junk.xts = xts(dailytwitter_junk[,-1], order.by=as.Date(dailytwitter_junk$date))
twittermerged_junk = merge(dailytwitter_junk.xts, dailySurvey.xts$ICS)
twittermerged_junk_lag = lLag(twittermerged_junk, -50, c('ICS'))
twittermerged_junk_lag_smooth = kSmooth(twittermerged_junk_lag, 30)
cors_junk = cor(twittermerged_junk_lag_smooth[index(twittermerged_junk_lag_smooth)<=as.Date('2009-12-31') &
                               index(twittermerged_junk_lag_smooth)>=as.Date('2008-01-01'),], use='pairwise.complete')[,1]

dailytwitter_other.xts = xts(dailytwitter_other[,-1], order.by=as.Date(dailytwitter_other$date))
twittermerged_other = merge(dailytwitter_other.xts, dailySurvey.xts$ICS)
twittermerged_other_lag = lLag(twittermerged_other, -50, c('ICS'))
twittermerged_other_lag_smooth = kSmooth(twittermerged_other_lag, 30)
cors_other = cor(twittermerged_other_lag_smooth[index(twittermerged_other_lag_smooth)<=as.Date('2009-12-31') &
                                index(twittermerged_other_lag_smooth)>=as.Date('2008-01-01'),], use='pairwise.complete')[,1]

cors08_09 = data.frame(rbind(cors_all, cors_np, cors_pers, cors_ads, cors_junk, cors_other))

print('Table 1')
print(round(cors08_09[,c('M1', 'M2', 'M3')], 2))

print('Table 2')
print(round(cors08_09[,c('M1', 'M1_wds', 'M1_t_LC', 'M1_w_LC', 'M1_t_LH', 'M1_w_LH')], 2))

print('Table 3')
print(round(cors08_09[,c('VD', 'TB')], 2))

## by year using O'Connor et al sentiment method, Vader, and Textblob sentiment methods
all08 = round(cor(twittermerged_all_lag_smooth[index(twittermerged_all_lag_smooth)<=as.Date('2008-12-31') &
                              index(twittermerged_all_lag_smooth)>=as.Date('2008-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5 = data.frame('category'='all', 'method'=1, 'yr08'=all08[1,2], 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)
tables5 = rbind(tables5, data.frame('category'='all', 'method'=2, 'yr08'=all08[1,3], 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA))
tables5 = rbind(tables5, data.frame('category'='all', 'method'=3, 'yr08'=all08[1,4], 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA))

all09 = round(cor(twittermerged_all_lag_smooth[index(twittermerged_all_lag_smooth)<=as.Date('2009-12-31') &
                              index(twittermerged_all_lag_smooth)>=as.Date('2009-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr09 = c(all09[1,2:4])
all10 = round(cor(twittermerged_all_lag_smooth[index(twittermerged_all_lag_smooth)<=as.Date('2010-12-31') &
                              index(twittermerged_all_lag_smooth)>=as.Date('2010-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr10 = c(all10[1,2:4])
all11 = round(cor(twittermerged_all_lag_smooth[index(twittermerged_all_lag_smooth)<=as.Date('2011-12-31') &
                              index(twittermerged_all_lag_smooth)>=as.Date('2011-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr11 = c(all11[1,2:4])
all12 = round(cor(twittermerged_all_lag_smooth[index(twittermerged_all_lag_smooth)<=as.Date('2012-12-31') &
                              index(twittermerged_all_lag_smooth)>=as.Date('2012-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr12 = c(all12[1,2:4])
all13 = round(cor(twittermerged_all_lag_smooth[index(twittermerged_all_lag_smooth)<=as.Date('2013-12-31') &
                              index(twittermerged_all_lag_smooth)>=as.Date('2013-01-01'),c(1,2,5,6)], use='pairwise.complete'), 3)
tables5$yr13 = c(all13[1,2:4])
all14 = round(cor(twittermerged_all_lag_smooth[index(twittermerged_all_lag_smooth)<=as.Date('2014-06-27') &
                              index(twittermerged_all_lag_smooth)>=as.Date('2014-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr14 = c(all14[1,2:4])

np08 = round(cor(twittermerged_np_lag_smooth[index(twittermerged_np_lag_smooth)<=as.Date('2008-12-31') &
                             index(twittermerged_np_lag_smooth)>=as.Date('2008-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5 = rbind(tables5, data.frame('category'=rep('np',3), 'method'=1:3, 'yr08'=c(np08[1,2:4]), 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA))
np09 = round(cor(twittermerged_np_lag_smooth[index(twittermerged_np_lag_smooth)<=as.Date('2009-12-31') &
                             index(twittermerged_np_lag_smooth)>=as.Date('2009-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr09[4:6] = np09[1,2:4]
np10 = round(cor(twittermerged_np_lag_smooth[index(twittermerged_np_lag_smooth)<=as.Date('2010-12-31') &
                             index(twittermerged_np_lag_smooth)>=as.Date('2010-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr10[4:6] = np10[1,2:4]
np11 = round(cor(twittermerged_np_lag_smooth[index(twittermerged_np_lag_smooth)<=as.Date('2011-12-31') &
                             index(twittermerged_np_lag_smooth)>=as.Date('2011-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr11[4:6] = np11[1,2:4]
np12 = round(cor(twittermerged_np_lag_smooth[index(twittermerged_np_lag_smooth)<=as.Date('2012-12-31') &
                             index(twittermerged_np_lag_smooth)>=as.Date('2012-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr12[4:6] = np12[1,2:4]
np13 = round(cor(twittermerged_np_lag_smooth[index(twittermerged_np_lag_smooth)<=as.Date('2013-12-31') &
                             index(twittermerged_np_lag_smooth)>=as.Date('2013-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr13[4:6] = np13[1,2:4]
np14 = round(cor(twittermerged_np_lag_smooth[index(twittermerged_np_lag_smooth)<=as.Date('2014-12-31') &
                             index(twittermerged_np_lag_smooth)>=as.Date('2014-06-27'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr14[4:6] = np14[1,2:4]

pers08 = round(cor(twittermerged_pers_lag_smooth[index(twittermerged_pers_lag_smooth)<=as.Date('2008-12-31') &
                               index(twittermerged_pers_lag_smooth)>=as.Date('2008-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5 = rbind(tables5, data.frame('category'=rep('pers',3), 'method'=1:3, 'yr08'=c(pers08[1,2:4]), 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA))
pers09 = round(cor(twittermerged_pers_lag_smooth[index(twittermerged_pers_lag_smooth)<=as.Date('2009-12-31') &
                               index(twittermerged_pers_lag_smooth)>=as.Date('2009-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr09[7:9] = pers09[1,2:4]
pers10 = round(cor(twittermerged_pers_lag_smooth[index(twittermerged_pers_lag_smooth)<=as.Date('2010-12-31') &
                               index(twittermerged_pers_lag_smooth)>=as.Date('2010-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr10[7:9] = pers10[1,2:4]
pers11 = round(cor(twittermerged_pers_lag_smooth[index(twittermerged_pers_lag_smooth)<=as.Date('2011-12-31') &
                               index(twittermerged_pers_lag_smooth)>=as.Date('2011-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr11[7:9] = pers11[1,2:4]
pers12 = round(cor(twittermerged_pers_lag_smooth[index(twittermerged_pers_lag_smooth)<=as.Date('2012-12-31') &
                               index(twittermerged_pers_lag_smooth)>=as.Date('2012-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr12[7:9] = pers12[1,2:4]
pers13 = round(cor(twittermerged_pers_lag_smooth[index(twittermerged_pers_lag_smooth)<=as.Date('2013-12-31') &
                               index(twittermerged_pers_lag_smooth)>=as.Date('2013-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr13[7:9] = pers13[1,2:4]
pers14 = round(cor(twittermerged_pers_lag_smooth[index(twittermerged_pers_lag_smooth)<=as.Date('2014-12-31') &
                               index(twittermerged_pers_lag_smooth)>=as.Date('2014-06-27'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr14[7:9] = pers14[1,2:4]

ads08 = round(cor(twittermerged_ads_lag_smooth[index(twittermerged_ads_lag_smooth)<=as.Date('2008-12-31') &
                              index(twittermerged_ads_lag_smooth)>=as.Date('2008-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5 = rbind(tables5, data.frame('category'=rep('ads',3), 'method'=1:3, 'yr08'=c(ads08[1,2:4]), 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA))
ads09 = round(cor(twittermerged_ads_lag_smooth[index(twittermerged_ads_lag_smooth)<=as.Date('2009-12-31') &
                              index(twittermerged_ads_lag_smooth)>=as.Date('2009-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr09[10:12] = ads09[1,2:4]
ads10 = round(cor(twittermerged_ads_lag_smooth[index(twittermerged_ads_lag_smooth)<=as.Date('2010-12-31') &
                              index(twittermerged_ads_lag_smooth)>=as.Date('2010-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr10[10:12] = ads10[1,2:4]
ads11 = round(cor(twittermerged_ads_lag_smooth[index(twittermerged_ads_lag_smooth)<=as.Date('2011-12-31') &
                              index(twittermerged_ads_lag_smooth)>=as.Date('2011-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr11[10:12] = ads11[1,2:4]
ads12 = round(cor(twittermerged_ads_lag_smooth[index(twittermerged_ads_lag_smooth)<=as.Date('2012-12-31') &
                              index(twittermerged_ads_lag_smooth)>=as.Date('2012-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr12[10:12] = ads12[1,2:4]
ads13 = round(cor(twittermerged_ads_lag_smooth[index(twittermerged_ads_lag_smooth)<=as.Date('2013-12-31') &
                              index(twittermerged_ads_lag_smooth)>=as.Date('2013-01-01'),c(1,2,5,6)], use='pairwise.complete'),2)
tables5$yr13[10:12] = ads13[1,2:4]
ads14 = round(cor(twittermerged_ads_lag_smooth[index(twittermerged_ads_lag_smooth)<=as.Date('2014-12-31') &
                              index(twittermerged_ads_lag_smooth)>=as.Date('2014-06-27'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr14[10:12] = ads14[1,2:4]

junk08 = round(cor(twittermerged_junk_lag_smooth[index(twittermerged_junk_lag_smooth)<=as.Date('2008-12-31') &
                               index(twittermerged_junk_lag_smooth)>=as.Date('2008-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5 = rbind(tables5, data.frame('category'=rep('junk',3), 'method'=1:3, 'yr08'=c(junk08[1,2:4]), 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA))
junk09 = round(cor(twittermerged_junk_lag_smooth[index(twittermerged_junk_lag_smooth)<=as.Date('2009-12-31') &
                               index(twittermerged_junk_lag_smooth)>=as.Date('2009-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr09[13:15] = junk09[1,2:4]
junk10 = round(cor(twittermerged_junk_lag_smooth[index(twittermerged_junk_lag_smooth)<=as.Date('2010-12-31') &
                               index(twittermerged_junk_lag_smooth)>=as.Date('2010-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr10[13:15] = junk10[1,2:4]
junk11 = round(cor(twittermerged_junk_lag_smooth[index(twittermerged_junk_lag_smooth)<=as.Date('2011-12-31') &
                               index(twittermerged_junk_lag_smooth)>=as.Date('2011-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr11[13:15] = junk11[1,2:4]
junk12 = round(cor(twittermerged_junk_lag_smooth[index(twittermerged_junk_lag_smooth)<=as.Date('2012-12-31') &
                               index(twittermerged_junk_lag_smooth)>=as.Date('2012-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr12[13:15] = junk12[1,2:4]
junk13 = round(cor(twittermerged_junk_lag_smooth[index(twittermerged_junk_lag_smooth)<=as.Date('2013-12-31') &
                               index(twittermerged_junk_lag_smooth)>=as.Date('2013-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr13[13:15] = junk13[1,2:4]
junk14 = round(cor(twittermerged_junk_lag_smooth[index(twittermerged_junk_lag_smooth)<=as.Date('2014-12-31') &
                               index(twittermerged_junk_lag_smooth)>=as.Date('2014-06-27'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr14[13:15] = junk14[1,2:4]

other08 = round(cor(twittermerged_other_lag_smooth[index(twittermerged_other_lag_smooth)<=as.Date('2008-12-31') &
                                index(twittermerged_other_lag_smooth)>=as.Date('2008-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5 = rbind(tables5, data.frame('category'=rep('other',3), 'method'=1:3, 'yr08'=c(other08[1,2:4]), 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA))
other09 = round(cor(twittermerged_other_lag_smooth[index(twittermerged_other_lag_smooth)<=as.Date('2009-12-31') &
                                index(twittermerged_other_lag_smooth)>=as.Date('2009-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr09[16:18] = other09[1,2:4]
other10 = round(cor(twittermerged_other_lag_smooth[index(twittermerged_other_lag_smooth)<=as.Date('2010-12-31') &
                                index(twittermerged_other_lag_smooth)>=as.Date('2010-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr10[16:18] = other10[1,2:4]
other11 = round(cor(twittermerged_other_lag_smooth[index(twittermerged_other_lag_smooth)<=as.Date('2011-12-31') &
                                index(twittermerged_other_lag_smooth)>=as.Date('2011-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr11[16:18] = other11[1,2:4]
other12 = round(cor(twittermerged_other_lag_smooth[index(twittermerged_other_lag_smooth)<=as.Date('2012-12-31') &
                                index(twittermerged_other_lag_smooth)>=as.Date('2012-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr12[16:18] = other12[1,2:4]
other13 = round(cor(twittermerged_other_lag_smooth[index(twittermerged_other_lag_smooth)<=as.Date('2013-12-31') &
                                index(twittermerged_other_lag_smooth)>=as.Date('2013-01-01'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr13[16:18] = other13[1,2:4]
other14 = round(cor(twittermerged_other_lag_smooth[index(twittermerged_other_lag_smooth)<=as.Date('2014-12-31') &
                                index(twittermerged_other_lag_smooth)>=as.Date('2014-06-27'),c(1,2,5,6)], use='pairwise.complete'), 2)
tables5$yr14[16:18] = other14[1,2:4]

print('Table 5')
print(tables5[tables5$method==1,])
print(tables5[tables5$method==2,])
print(tables5[tables5$method==3,])


### Collective vs. Self question
## Collective Question
table6 = data.frame('category'='all', 'yr08'=NA, 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)

twittermerged_all_coll = merge(dailytwitter_all.xts, dailySurvey.xts$BUS12)
twittermerged_all_lag_coll = lLag(twittermerged_all_coll, -50, c('BUS12'))
twittermerged_all_lag_smooth_coll = kSmooth(twittermerged_all_lag_coll, 30)

table6$yr08 = with(twittermerged_all_lag_smooth_coll[index(twittermerged_all_lag_smooth_coll) %in% as.Date('2008-01-01'):as.Date('2008-12-31'),], 
     round(cor(BUS12, M1_w_LC), 2))
table6$yr09 = with(twittermerged_all_lag_smooth_coll[index(twittermerged_all_lag_smooth_coll) %in% as.Date('2009-01-01'):as.Date('2009-12-31'),], 
     round(cor(BUS12, M1_w_LC), 2))
table6$yr10 = with(twittermerged_all_lag_smooth_coll[index(twittermerged_all_lag_smooth_coll) %in% as.Date('2010-01-01'):as.Date('2010-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
table6$yr11 = with(twittermerged_all_lag_smooth_coll[index(twittermerged_all_lag_smooth_coll) %in% as.Date('2011-01-01'):as.Date('2011-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
table6$yr12 = with(twittermerged_all_lag_smooth_coll[index(twittermerged_all_lag_smooth_coll) %in% as.Date('2012-01-01'):as.Date('2012-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
table6$yr13 = with(twittermerged_all_lag_smooth_coll[index(twittermerged_all_lag_smooth_coll) %in% as.Date('2013-01-01'):as.Date('2013-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
table6$yr14 = with(twittermerged_all_lag_smooth_coll[index(twittermerged_all_lag_smooth_coll) %in% as.Date('2014-01-01'):as.Date('2014-06-27'),], 
                   round(cor(BUS12, M1_w_LC), 2))

twittermerged_np_coll = merge(dailytwitter_np.xts, dailySurvey.xts$BUS12)
twittermerged_np_lag_coll = lLag(twittermerged_np_coll, -50, c('BUS12'))
twittermerged_np_lag_smooth_coll = kSmooth(twittermerged_np_lag_coll, 30)

newrow = data.frame('category'='np', 'yr08'=NA, 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)

newrow$yr08 = with(twittermerged_np_lag_smooth_coll[index(twittermerged_np_lag_smooth_coll) %in% as.Date('2008-01-01'):as.Date('2008-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr09 = with(twittermerged_np_lag_smooth_coll[index(twittermerged_np_lag_smooth_coll) %in% as.Date('2009-01-01'):as.Date('2009-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr10 = with(twittermerged_np_lag_smooth_coll[index(twittermerged_np_lag_smooth_coll) %in% as.Date('2010-01-01'):as.Date('2010-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr11 = with(twittermerged_np_lag_smooth_coll[index(twittermerged_np_lag_smooth_coll) %in% as.Date('2011-01-01'):as.Date('2011-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr12 = with(twittermerged_np_lag_smooth_coll[index(twittermerged_np_lag_smooth_coll) %in% as.Date('2012-01-01'):as.Date('2012-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr13 = with(twittermerged_np_lag_smooth_coll[index(twittermerged_np_lag_smooth_coll) %in% as.Date('2013-01-01'):as.Date('2013-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr14 = with(twittermerged_np_lag_smooth_coll[index(twittermerged_np_lag_smooth_coll) %in% as.Date('2014-01-01'):as.Date('2014-06-27'),], 
                   round(cor(BUS12, M1_w_LC), 2))
table6 = rbind(table6, newrow)

twittermerged_pers_coll = merge(dailytwitter_pers.xts, dailySurvey.xts$BUS12)
twittermerged_pers_lag_coll = lLag(twittermerged_pers_coll, -50, c('BUS12'))
twittermerged_pers_lag_smooth_coll = kSmooth(twittermerged_pers_lag_coll, 30)

newrow = data.frame('category'='pers', 'yr08'=NA, 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)

newrow$yr08 = with(twittermerged_pers_lag_smooth_coll[index(twittermerged_pers_lag_smooth_coll) %in% as.Date('2008-01-01'):as.Date('2008-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr09 = with(twittermerged_pers_lag_smooth_coll[index(twittermerged_pers_lag_smooth_coll) %in% as.Date('2009-01-01'):as.Date('2009-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr10 = with(twittermerged_pers_lag_smooth_coll[index(twittermerged_pers_lag_smooth_coll) %in% as.Date('2010-01-01'):as.Date('2010-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr11 = with(twittermerged_pers_lag_smooth_coll[index(twittermerged_pers_lag_smooth_coll) %in% as.Date('2011-01-01'):as.Date('2011-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr12 = with(twittermerged_pers_lag_smooth_coll[index(twittermerged_pers_lag_smooth_coll) %in% as.Date('2012-01-01'):as.Date('2012-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr13 = with(twittermerged_pers_lag_smooth_coll[index(twittermerged_pers_lag_smooth_coll) %in% as.Date('2013-01-01'):as.Date('2013-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr14 = with(twittermerged_pers_lag_smooth_coll[index(twittermerged_pers_lag_smooth_coll) %in% as.Date('2014-01-01'):as.Date('2014-06-27'),], 
                   round(cor(BUS12, M1_w_LC), 2))
table6 = rbind(table6, newrow)

twittermerged_ads_coll = merge(dailytwitter_ads.xts, dailySurvey.xts$BUS12)
twittermerged_ads_lag_coll = lLag(twittermerged_ads_coll, -50, c('BUS12'))
twittermerged_ads_lag_smooth_coll = kSmooth(twittermerged_ads_lag_coll, 30)

newrow = data.frame('category'='ads', 'yr08'=NA, 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)

newrow$yr08 = with(twittermerged_ads_lag_smooth_coll[index(twittermerged_ads_lag_smooth_coll) %in% as.Date('2008-01-01'):as.Date('2008-12-31'),], 
                   round(cor(BUS12, M1_w_LC, use='pairwise.complete'), 2))
newrow$yr09 = with(twittermerged_ads_lag_smooth_coll[index(twittermerged_ads_lag_smooth_coll) %in% as.Date('2009-01-01'):as.Date('2009-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr10 = with(twittermerged_ads_lag_smooth_coll[index(twittermerged_ads_lag_smooth_coll) %in% as.Date('2010-01-01'):as.Date('2010-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr11 = with(twittermerged_ads_lag_smooth_coll[index(twittermerged_ads_lag_smooth_coll) %in% as.Date('2011-01-01'):as.Date('2011-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr12 = with(twittermerged_ads_lag_smooth_coll[index(twittermerged_ads_lag_smooth_coll) %in% as.Date('2012-01-01'):as.Date('2012-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr13 = with(twittermerged_ads_lag_smooth_coll[index(twittermerged_ads_lag_smooth_coll) %in% as.Date('2013-01-01'):as.Date('2013-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr14 = with(twittermerged_ads_lag_smooth_coll[index(twittermerged_ads_lag_smooth_coll) %in% as.Date('2014-01-01'):as.Date('2014-06-27'),], 
                   round(cor(BUS12, M1_w_LC), 2))
table6 = rbind(table6, newrow)

twittermerged_junk_coll = merge(dailytwitter_junk.xts, dailySurvey.xts$BUS12)
twittermerged_junk_lag_coll = lLag(twittermerged_junk_coll, -50, c('BUS12'))
twittermerged_junk_lag_smooth_coll = kSmooth(twittermerged_junk_lag_coll, 30)

newrow = data.frame('category'='junk', 'yr08'=NA, 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)

newrow$yr08 = with(twittermerged_junk_lag_smooth_coll[index(twittermerged_junk_lag_smooth_coll) %in% as.Date('2008-01-01'):as.Date('2008-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr09 = with(twittermerged_junk_lag_smooth_coll[index(twittermerged_junk_lag_smooth_coll) %in% as.Date('2009-01-01'):as.Date('2009-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr10 = with(twittermerged_junk_lag_smooth_coll[index(twittermerged_junk_lag_smooth_coll) %in% as.Date('2010-01-01'):as.Date('2010-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr11 = with(twittermerged_junk_lag_smooth_coll[index(twittermerged_junk_lag_smooth_coll) %in% as.Date('2011-01-01'):as.Date('2011-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr12 = with(twittermerged_junk_lag_smooth_coll[index(twittermerged_junk_lag_smooth_coll) %in% as.Date('2012-01-01'):as.Date('2012-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr13 = with(twittermerged_junk_lag_smooth_coll[index(twittermerged_junk_lag_smooth_coll) %in% as.Date('2013-01-01'):as.Date('2013-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr14 = with(twittermerged_junk_lag_smooth_coll[index(twittermerged_junk_lag_smooth_coll) %in% as.Date('2014-01-01'):as.Date('2014-06-27'),], 
                   round(cor(BUS12, M1_w_LC), 2))
table6 = rbind(table6, newrow)

twittermerged_other_coll = merge(dailytwitter_other.xts, dailySurvey.xts$BUS12)
twittermerged_other_lag_coll = lLag(twittermerged_other_coll, -50, c('BUS12'))
twittermerged_other_lag_smooth_coll = kSmooth(twittermerged_other_lag_coll, 30)

newrow = data.frame('category'='other', 'yr08'=NA, 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)

newrow$yr08 = with(twittermerged_other_lag_smooth_coll[index(twittermerged_other_lag_smooth_coll) %in% as.Date('2008-01-01'):as.Date('2008-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr09 = with(twittermerged_other_lag_smooth_coll[index(twittermerged_other_lag_smooth_coll) %in% as.Date('2009-01-01'):as.Date('2009-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr10 = with(twittermerged_other_lag_smooth_coll[index(twittermerged_other_lag_smooth_coll) %in% as.Date('2010-01-01'):as.Date('2010-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr11 = with(twittermerged_other_lag_smooth_coll[index(twittermerged_other_lag_smooth_coll) %in% as.Date('2011-01-01'):as.Date('2011-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr12 = with(twittermerged_other_lag_smooth_coll[index(twittermerged_other_lag_smooth_coll) %in% as.Date('2012-01-01'):as.Date('2012-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr13 = with(twittermerged_other_lag_smooth_coll[index(twittermerged_other_lag_smooth_coll) %in% as.Date('2013-01-01'):as.Date('2013-12-31'),], 
                   round(cor(BUS12, M1_w_LC), 2))
newrow$yr14 = with(twittermerged_other_lag_smooth_coll[index(twittermerged_other_lag_smooth_coll) %in% as.Date('2014-01-01'):as.Date('2014-06-27'),], 
                   round(cor(BUS12, M1_w_LC), 2))
table6 = rbind(table6, newrow)


## Personal Question
table6b = data.frame('category'='all', 'yr08'=NA, 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)

twittermerged_all_pers = merge(dailytwitter_all.xts, dailySurvey.xts$PEXP)
twittermerged_all_lag_pers = lLag(twittermerged_all_pers, -50, c('PEXP'))
twittermerged_all_lag_smooth_pers = kSmooth(twittermerged_all_lag_pers, 30)

table6b$yr08 = with(twittermerged_all_lag_smooth_pers[index(twittermerged_all_lag_smooth_pers) %in% as.Date('2008-01-01'):as.Date('2008-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
table6b$yr09 = with(twittermerged_all_lag_smooth_pers[index(twittermerged_all_lag_smooth_pers) %in% as.Date('2009-01-01'):as.Date('2009-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
table6b$yr10 = with(twittermerged_all_lag_smooth_pers[index(twittermerged_all_lag_smooth_pers) %in% as.Date('2010-01-01'):as.Date('2010-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
table6b$yr11 = with(twittermerged_all_lag_smooth_pers[index(twittermerged_all_lag_smooth_pers) %in% as.Date('2011-01-01'):as.Date('2011-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
table6b$yr12 = with(twittermerged_all_lag_smooth_pers[index(twittermerged_all_lag_smooth_pers) %in% as.Date('2012-01-01'):as.Date('2012-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
table6b$yr13 = with(twittermerged_all_lag_smooth_pers[index(twittermerged_all_lag_smooth_pers) %in% as.Date('2013-01-01'):as.Date('2013-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
table6b$yr14 = with(twittermerged_all_lag_smooth_pers[index(twittermerged_all_lag_smooth_pers) %in% as.Date('2014-01-01'):as.Date('2014-06-27'),], 
                   round(cor(PEXP, M1_w_LC), 2))

twittermerged_np_pers = merge(dailytwitter_np.xts, dailySurvey.xts$PEXP)
twittermerged_np_lag_pers = lLag(twittermerged_np_pers, -50, c('PEXP'))
twittermerged_np_lag_smooth_pers = kSmooth(twittermerged_np_lag_pers, 30)

newrow = data.frame('category'='np', 'yr08'=NA, 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)

newrow$yr08 = with(twittermerged_np_lag_smooth_pers[index(twittermerged_np_lag_smooth_pers) %in% as.Date('2008-01-01'):as.Date('2008-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr09 = with(twittermerged_np_lag_smooth_pers[index(twittermerged_np_lag_smooth_pers) %in% as.Date('2009-01-01'):as.Date('2009-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr10 = with(twittermerged_np_lag_smooth_pers[index(twittermerged_np_lag_smooth_pers) %in% as.Date('2010-01-01'):as.Date('2010-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr11 = with(twittermerged_np_lag_smooth_pers[index(twittermerged_np_lag_smooth_pers) %in% as.Date('2011-01-01'):as.Date('2011-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr12 = with(twittermerged_np_lag_smooth_pers[index(twittermerged_np_lag_smooth_pers) %in% as.Date('2012-01-01'):as.Date('2012-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr13 = with(twittermerged_np_lag_smooth_pers[index(twittermerged_np_lag_smooth_pers) %in% as.Date('2013-01-01'):as.Date('2013-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr14 = with(twittermerged_np_lag_smooth_pers[index(twittermerged_np_lag_smooth_pers) %in% as.Date('2014-01-01'):as.Date('2014-06-27'),], 
                   round(cor(PEXP, M1_w_LC), 2))
table6b = rbind(table6b, newrow)

twittermerged_pers_pers = merge(dailytwitter_pers.xts, dailySurvey.xts$PEXP)
twittermerged_pers_lag_pers = lLag(twittermerged_pers_pers, -50, c('PEXP'))
twittermerged_pers_lag_smooth_pers = kSmooth(twittermerged_pers_lag_pers, 30)

newrow = data.frame('category'='pers', 'yr08'=NA, 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)

newrow$yr08 = with(twittermerged_pers_lag_smooth_pers[index(twittermerged_pers_lag_smooth_pers) %in% as.Date('2008-01-01'):as.Date('2008-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr09 = with(twittermerged_pers_lag_smooth_pers[index(twittermerged_pers_lag_smooth_pers) %in% as.Date('2009-01-01'):as.Date('2009-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr10 = with(twittermerged_pers_lag_smooth_pers[index(twittermerged_pers_lag_smooth_pers) %in% as.Date('2010-01-01'):as.Date('2010-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr11 = with(twittermerged_pers_lag_smooth_pers[index(twittermerged_pers_lag_smooth_pers) %in% as.Date('2011-01-01'):as.Date('2011-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr12 = with(twittermerged_pers_lag_smooth_pers[index(twittermerged_pers_lag_smooth_pers) %in% as.Date('2012-01-01'):as.Date('2012-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr13 = with(twittermerged_pers_lag_smooth_pers[index(twittermerged_pers_lag_smooth_pers) %in% as.Date('2013-01-01'):as.Date('2013-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr14 = with(twittermerged_pers_lag_smooth_pers[index(twittermerged_pers_lag_smooth_pers) %in% as.Date('2014-01-01'):as.Date('2014-06-27'),], 
                   round(cor(PEXP, M1_w_LC), 2))
table6b = rbind(table6b, newrow)

twittermerged_ads_pers = merge(dailytwitter_ads.xts, dailySurvey.xts$PEXP)
twittermerged_ads_lag_pers = lLag(twittermerged_ads_pers, -50, c('PEXP'))
twittermerged_ads_lag_smooth_pers = kSmooth(twittermerged_ads_lag_pers, 30)

newrow = data.frame('category'='ads', 'yr08'=NA, 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)

newrow$yr08 = with(twittermerged_ads_lag_smooth_pers[index(twittermerged_ads_lag_smooth_pers) %in% as.Date('2008-01-01'):as.Date('2008-12-31'),], 
                   round(cor(PEXP, M1_w_LC, use='pairwise.complete'), 2))
newrow$yr09 = with(twittermerged_ads_lag_smooth_pers[index(twittermerged_ads_lag_smooth_pers) %in% as.Date('2009-01-01'):as.Date('2009-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr10 = with(twittermerged_ads_lag_smooth_pers[index(twittermerged_ads_lag_smooth_pers) %in% as.Date('2010-01-01'):as.Date('2010-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr11 = with(twittermerged_ads_lag_smooth_pers[index(twittermerged_ads_lag_smooth_pers) %in% as.Date('2011-01-01'):as.Date('2011-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr12 = with(twittermerged_ads_lag_smooth_pers[index(twittermerged_ads_lag_smooth_pers) %in% as.Date('2012-01-01'):as.Date('2012-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr13 = with(twittermerged_ads_lag_smooth_pers[index(twittermerged_ads_lag_smooth_pers) %in% as.Date('2013-01-01'):as.Date('2013-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr14 = with(twittermerged_ads_lag_smooth_pers[index(twittermerged_ads_lag_smooth_pers) %in% as.Date('2014-01-01'):as.Date('2014-06-27'),], 
                   round(cor(PEXP, M1_w_LC), 2))
table6b = rbind(table6b, newrow)

twittermerged_junk_pers = merge(dailytwitter_junk.xts, dailySurvey.xts$PEXP)
twittermerged_junk_lag_pers = lLag(twittermerged_junk_pers, -50, c('PEXP'))
twittermerged_junk_lag_smooth_pers = kSmooth(twittermerged_junk_lag_pers, 30)

newrow = data.frame('category'='junk', 'yr08'=NA, 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)

newrow$yr08 = with(twittermerged_junk_lag_smooth_pers[index(twittermerged_junk_lag_smooth_pers) %in% as.Date('2008-01-01'):as.Date('2008-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr09 = with(twittermerged_junk_lag_smooth_pers[index(twittermerged_junk_lag_smooth_pers) %in% as.Date('2009-01-01'):as.Date('2009-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr10 = with(twittermerged_junk_lag_smooth_pers[index(twittermerged_junk_lag_smooth_pers) %in% as.Date('2010-01-01'):as.Date('2010-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr11 = with(twittermerged_junk_lag_smooth_pers[index(twittermerged_junk_lag_smooth_pers) %in% as.Date('2011-01-01'):as.Date('2011-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr12 = with(twittermerged_junk_lag_smooth_pers[index(twittermerged_junk_lag_smooth_pers) %in% as.Date('2012-01-01'):as.Date('2012-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr13 = with(twittermerged_junk_lag_smooth_pers[index(twittermerged_junk_lag_smooth_pers) %in% as.Date('2013-01-01'):as.Date('2013-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr14 = with(twittermerged_junk_lag_smooth_pers[index(twittermerged_junk_lag_smooth_pers) %in% as.Date('2014-01-01'):as.Date('2014-06-27'),], 
                   round(cor(PEXP, M1_w_LC), 2))
table6b = rbind(table6b, newrow)

twittermerged_other_pers = merge(dailytwitter_other.xts, dailySurvey.xts$PEXP)
twittermerged_other_lag_pers = lLag(twittermerged_other_pers, -50, c('PEXP'))
twittermerged_other_lag_smooth_pers = kSmooth(twittermerged_other_lag_pers, 30)

newrow = data.frame('category'='other', 'yr08'=NA, 'yr09'=NA, 'yr10'=NA, 'yr11'=NA, 'yr12'=NA, 'yr13'=NA, 'yr14'=NA)

newrow$yr08 = with(twittermerged_other_lag_smooth_pers[index(twittermerged_other_lag_smooth_pers) %in% as.Date('2008-01-01'):as.Date('2008-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr09 = with(twittermerged_other_lag_smooth_pers[index(twittermerged_other_lag_smooth_pers) %in% as.Date('2009-01-01'):as.Date('2009-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr10 = with(twittermerged_other_lag_smooth_pers[index(twittermerged_other_lag_smooth_pers) %in% as.Date('2010-01-01'):as.Date('2010-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr11 = with(twittermerged_other_lag_smooth_pers[index(twittermerged_other_lag_smooth_pers) %in% as.Date('2011-01-01'):as.Date('2011-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr12 = with(twittermerged_other_lag_smooth_pers[index(twittermerged_other_lag_smooth_pers) %in% as.Date('2012-01-01'):as.Date('2012-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr13 = with(twittermerged_other_lag_smooth_pers[index(twittermerged_other_lag_smooth_pers) %in% as.Date('2013-01-01'):as.Date('2013-12-31'),], 
                   round(cor(PEXP, M1_w_LC), 2))
newrow$yr14 = with(twittermerged_other_lag_smooth_pers[index(twittermerged_other_lag_smooth_pers) %in% as.Date('2014-01-01'):as.Date('2014-06-27'),], 
                   round(cor(PEXP, M1_w_LC), 2))
table6b = rbind(table6b, newrow)

print('Table 6')
print(table6)
print(table6b)


### Comovement
scor = function(x, y){
  # x, y vectors
  # output percent of time x and y move in the same direction
  xChanges = x[-1] - x[-length(x)]
  yChanges = y[-1] - y[-length(y)]
  return(sum(sign(xChanges)==sign(yChanges), na.rm=TRUE) / sum(is.na(sign(xChanges)==sign(yChanges))==FALSE))
}

table4 = data.frame('cat'=c('all', 'np', 'pers', 'ads', 'junk', 'other'), 'daily'=NA)
# daily
com_daily_all = twittermerged_all_lag[index(twittermerged_all_lag) %in% as.Date('2008-01-01'):as.Date('2009-12-31'), c("ICS","M1")]
table4$daily[1] = round(scor(as.vector(com_daily_all$ICS), as.vector(com_daily_all$M1)), 2)

com_daily_np = twittermerged_np_lag[index(twittermerged_np_lag) %in% as.Date('2008-01-01'):as.Date('2009-12-31'), c("ICS","M1")]
table4$daily[2] = round(scor(as.vector(com_daily_np$ICS), as.vector(com_daily_np$M1)), 2)

com_daily_pers = twittermerged_pers_lag[index(twittermerged_pers_lag) %in% as.Date('2008-01-01'):as.Date('2009-12-31'), c("ICS","M1")]
table4$daily[3] = round(scor(as.vector(com_daily_pers$ICS), as.vector(com_daily_pers$M1)), 2)

com_daily_ads = twittermerged_ads_lag[index(twittermerged_ads_lag) %in% as.Date('2008-01-01'):as.Date('2009-12-31'), c("ICS","M1")]
table4$daily[4] = round(scor(as.vector(com_daily_ads$ICS), as.vector(com_daily_ads$M1)), 2)

com_daily_junk = twittermerged_junk_lag[index(twittermerged_junk_lag) %in% as.Date('2008-01-01'):as.Date('2009-12-31'), c("ICS","M1")]
table4$daily[5] = round(scor(as.vector(com_daily_junk$ICS), as.vector(com_daily_junk$M1)), 2)

com_daily_other = twittermerged_other_lag[index(twittermerged_other_lag) %in% as.Date('2008-01-01'):as.Date('2009-12-31'), c("ICS","M1")]
table4$daily[6] = round(scor(as.vector(com_daily_other$ICS), as.vector(com_daily_other$M1)), 2)

# weekly
weekComFcn = function(dat){
  weekSmooth = kSmooth(dat, 7) # smooth 7 days
  dayOfWeek = weekdays(as.Date('2017-01-01')) # find weekday of interest
  weekSmooth = weekSmooth[which(weekdays(index(weekSmooth))==dayOfWeek),] # restrict to days that match the weekday of interest
  newData = data.frame(weekSmooth)
  return(round(scor(as.vector(newData$ICS), as.vector(newData$M1)), 2))
}

table4$week = NA
table4$week[1] = weekComFcn(com_daily_all)
table4$week[2] = weekComFcn(com_daily_np)
table4$week[3] = weekComFcn(com_daily_pers)
table4$week[4] = weekComFcn(com_daily_ads)
table4$week[5] = weekComFcn(com_daily_junk)
table4$week[6] = weekComFcn(com_daily_other)

# monthly
  # 1st
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

monthDays1 = relevantDays(as.Date('2017-01-01'), as.Date('2008-01-01'), as.Date('2009-12-31'))
monthDays1 = c(monthDays1, as.Date(addMonth(monthDays1[length(monthDays1)])))

monthlyComFcn = function(dat, monthDays){
  newData = data.frame(dat[1,])
  for(i in 1:(length(monthDays)-1)){
    day = as.Date(monthDays[i])
    temp = dat[index(dat)>=day & index(dat)<as.Date(monthDays[i+1]),]
    newData = rbind(newData, colSums(temp, na.rm=TRUE)/nrow(temp))
  }
  newData = newData[-1,]
  return(round(scor(as.vector(newData$ICS), as.vector(newData$M1)), 2))
}

table4$month1 = NA
table4$month1[1] = monthlyComFcn(com_daily_all, monthDays1)
table4$month1[2] = monthlyComFcn(com_daily_np, monthDays1)
table4$month1[3] = monthlyComFcn(com_daily_pers, monthDays1)
table4$month1[4] = monthlyComFcn(com_daily_ads, monthDays1)
table4$month1[5] = monthlyComFcn(com_daily_junk, monthDays1)
table4$month1[6] = monthlyComFcn(com_daily_other, monthDays1)

# monthly-2nd
monthDays2 = relevantDays(as.Date('2017-01-02'), as.Date('2008-01-01'), as.Date('2009-12-31'))
monthDays2 = c(monthDays2, as.Date(addMonth(monthDays2[length(monthDays2)])))

table4$month2 = NA
table4$month2[1] = monthlyComFcn(com_daily_all, monthDays2)
table4$month2[2] = monthlyComFcn(com_daily_np, monthDays2)
table4$month2[3] = monthlyComFcn(com_daily_pers, monthDays2)
table4$month2[4] = monthlyComFcn(com_daily_ads, monthDays2)
table4$month2[5] = monthlyComFcn(com_daily_junk, monthDays2)
table4$month2[6] = monthlyComFcn(com_daily_other, monthDays2)

# monthly-4th
monthDays4 = relevantDays(as.Date('2017-01-04'), as.Date('2008-01-01'), as.Date('2009-12-31'))
monthDays4 = c(monthDays4, as.Date(addMonth(monthDays4[length(monthDays4)])))

table4$month4 = NA
table4$month4[1] = monthlyComFcn(com_daily_all, monthDays4)
table4$month4[2] = monthlyComFcn(com_daily_np, monthDays4)
table4$month4[3] = monthlyComFcn(com_daily_pers, monthDays4)
table4$month4[4] = monthlyComFcn(com_daily_ads, monthDays4)
table4$month4[5] = monthlyComFcn(com_daily_junk, monthDays4)
table4$month4[6] = monthlyComFcn(com_daily_other, monthDays4)

print('Table 4')
print(table4)

## sensitivity analysis to smoothing and lag
ks = 1:100
ls = -100:100

all_kl_sens_data = cbind(twittermerged_all$M1, twittermerged_all$ICS)
all_kl_sens_data = all_kl_sens_data[index(all_kl_sens_data)<=as.Date('2010-01-01') & 
                                      index(all_kl_sens_data)>=as.Date('2007-01-01'),]

kl_sensitivity = matrix(NA, ncol=length(ls), nrow=length(ks))
colnames(kl_sensitivity) = ls; rownames(kl_sensitivity) = ks
for(l in ls){
  # lag data
  lagged = lLag(all_kl_sens_data, l, c('ICS'))
  for(k in ks){
    # smooth data
    lagged_smoothed = kSmooth(lagged, k)
    # calculate correlation
    corr = cor(lagged_smoothed[index(lagged_smoothed)<=as.Date('2009-12-31') &
                                 index(lagged_smoothed)>=as.Date('2008-01-01'),], use='pairwise.complete')[2,1]
    # add to matrix
    kl_sensitivity[which(ks==k),which(ls==l)] = corr
  }
  #print(l)
}

fig2 = plot_ly(type='contour', z=kl_sensitivity, x=ls, y=ks, contours=list(showlabels=TRUE), colors=colorRamp(c('white', 'gray30'))) %>%
  colorbar(title='Correlation')
htmlwidgets::saveWidget(as_widget(fig2), 'fig2.html')

plot_ly(type='contour', z=kl_sensitivity, x=ls, y=ks, contours=list(showlabels=TRUE))



##### Correlation matrix for various senitment methods
dailytwitter_all_ignored = dailytwitter_all[!(apply(dailytwitter_all,1,function(x) is.na(sum(x))|is.infinite(sum(x)))),]
round(cor(na.omit(dailytwitter_all_ignored[dailytwitter_all_ignored$date>=13879,-1])), 3)

round(cor(dailytwitter_all_ignored[as.Date(dailytwitter_all_ignored$date)<as.Date('2010-01-01') &
                           as.Date(dailytwitter_all_ignored$date)>as.Date('2008-01-01'),-1]), 3)


########## Other Figures
## plot of proportion of each tweet category by year
# color
twitterData$category = ifelse(twitterData$class=='junk', 'Irrelevant',
                              ifelse(twitterData$class=='personal', 'Personal',
                                     ifelse(twitterData$class=='advert', 'Advertisement',
                                            ifelse(twitterData$class=='other', 'Other', 'News/Politics'))))
ggplot(twitterData[twitterData$date<=as.Date('2014-06-27') & substr(twitterData$date, 1, 4)>=2008,], 
       aes(x=substr(date, 1, 4), fill=category)) + geom_bar(position='fill') + xlab('Year') + ylab('Proportion')
# grayscale
ggplot(twitterData[twitterData$date<=as.Date('2014-06-27') & substr(twitterData$date, 1, 4)>=2008,], 
       aes(x=substr(date, 1, 4), fill=category)) + geom_bar(position='fill') + xlab('Year') + ylab('Proportion') + scale_fill_grey(start = 0, end = .9) + theme_bw()


## how proportion of junk and news/politics tweets changes by day
dayProps = data.frame()
for(day in unique(twitterData$date)){
  temp = twitterData[twitterData$date==day,]
  propJunk = sum(temp$class=='junk') / nrow(temp)
  propNP = sum(temp$class=='newsPolitics') / nrow(temp)
  temp.df = data.frame('date'=as.Date(day), 'propJunk'=propJunk, 'propNP'=propNP)
  dayProps = rbind(dayProps, temp.df)
}

ggplot(dayProps, aes(x=date, y=propJunk)) + geom_point() + xlab('Date') + ylab('Proportion of Irrelevant Tweets') +
  xlim(as.Date('2008-01-01'), as.Date('2014-06-27'))
ggplot(dayProps, aes(x=date, y=propNP)) + geom_point() + xlab('Date') + ylab('Proportion of News/Politics Tweets') +
  xlim(as.Date('2008-01-01'), as.Date('2014-06-27'))
