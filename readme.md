# Readme

This GitHub contains data and scripts for reproducing results found in "Social Media as an Alternative to Surveys of Opinions About the Economy" by Conrad, Gagnon-Bartsch, Ferg, Schober, Pasek, and Hou in Social Science Computer Review, found at https://journals.sagepub.com/doi/10.1177/0894439319875692

In this paper we perform a sensitivity analysis on the relationship between consumer confidence and sentiment of tweets containing the word "jobs".

This GitHub repository contains two main R scripts: one for replicating most results found in the above mentioned paper, and one for creating a shiny app to see how changing various parameters affects the resulting observed relationships. All of the results in the paper can be replicated either by using the R script, the Shiny App, or both. A dockerhub site to run the Shiny App and reproduce results can be found at https://hub.docker.com/r/johanngb/twitter .

For privacy reasons we do not make tweet text or idenfitying data of tweets publicly available.

## Variables

Consumer confidence, as measured by the Survey of Consumers, is determined by five questions asked to respondents:
- PAGO: "We are interested in how people are getting along financially these days. Would you say that you (and your family living there) are better off or worse off financially than you were a year ago?"
- PEXP: "Now looking ahead--do you think that a year from now you (and your family living there) will be better off financially, or worse off, or just about the same as now?"
- BUS12: "Now turning to business conditions in the country as a whole--do you think that during the next twelve months we will have good times financially, or bad times, or what?"
- BUS5: "Looking ahead, which would you say is more likely--that in the country as a whole we will have continuous good times during the next five years or so, or that we will have periods of widespread unemployment or depression, or what?"
- DUR: "About the big things people buy for their homes--such as furniture, a refrigerator, stove, television, and things like that. Generally speaking, do you think now is a good or bad time for people to buy major household items?"

ICS is the main measure of consumer confidence we consider in the paper. ICS takes into account responses to each of the above five questions: (PAGO+PEXP+BUS12+BUS5+DUR)/6.7558 + 2

The Survey of Consumers also creates the measures ICC and ICE:

ICC = (PAGO + DUR)/2.6424 + 2

ICE = (PEXP+BUS12+BUS5)/4.1134 + 2

We create an algorithm to categorize "jobs" tweets into one of five categories. Further details of these categories, algorithm used to sort tweets, and example tweets from each category can be found in the paper. The five categories are:
- News/Politics
- Personal
- Ads
- Other
- Junk/Irrelevant

Sentiment of tweets can be calculated using the following methods:
- Vader
- TextBlob
- Lexicoder dictionary
- Liu-Hu dictionary
- OpinionFinder dictionary

Using the dictionary-based methods, we can count either the total number of positive and negative words in a given time period, or the total number of positive and negative tweets in a given time period. (A tweet is considered positve [negative] if it contains at least one positive [negative] word).

Using the number of positives and negatives, overall sentiment for a given day can be calculated one of three ways:
- (# positive - # negative)/# total
- (# positive)/(# negative)
- (# positive)/(# positive + # negative)

We include various levels of smoothing k for Twitter sentiment, where the smoothed daily sentiment for a given day is the average of that day and the previous k-1 days.

We also include a lag L, indicating whether Twitter sentiment leads or lags consumer confidence by L days.

## Reproducibility

The file 'ResultsFigures.R' reproduces most of the results and figures found in the above paper. This script uses the data sets 'allDataSurvey.csv' and 'jobsTwitterData.csv'. This script replicates Tables 1, 2, 3, and 5, and Figure 2. The dockerhub site https://hub.docker.com/r/johanngb/twitter runs this code and returns tables and figures presented in the paper.

'allDataSurvey.csv' is a csv file containing daily mean survey responses for the five questions making up the ICS.

'jobsTwitterData.csv' is a csv file of "jobs" tweets used in the analysis. Because of its size, this csv file is found under Releases. Each row is a single tweet, with columns being the date the tweet was sent, the category the tweet was classified into, sentiment as given by Vader and TextBlob, and number of positive and negative words from the OpinionFinder, Liu-Hu, and Lexicoder dictionaries.

'ResultsFigures.R' requires the following packages to be installed in R: xts, ggplot2, readr, GGally, data.table, fields, plotly, htmlwidgets.

We also include code and data to reproduce Figure 1. These files are in Releases: https://github.com/robynferg/Twitter_ICS/releases/tag/0.0.0

The script 'MakeFig1.R' is a script that recreates Figure 1 using 'scaData.csv', 'scadates.sas7bdat', and 'TwitterFig1.Rdata'. This figure first appeared in "A 'collective-vs-self' hypothesis for when Twitter and survey data tell the same story" presented at AAPOR 2015, by Conrad, Schober, Pasek, Guggenheim, Lampe, and Hou.

'scaDates.csv' is a csv file of individual responses from the Survey of Consumers. 'scadates.sas7bdat' contains the dates that these responses were recorded. 

'TwitterFig1.Rdata' contains "jobs" Twitter sentiment for the analysis used in the analysis.

## Shiny App
We also include a Shiny app. Using this Shiny app, various parameters can be manipulated to see how the resulting relationships between consumer confidence and sentiment of "jobs" tweets changes. Tables 4 and 6 can be replicated using the Shiny App.

The shiny app makes use of two data sets: 'allDataSurvey.csv' and 'dailyTwitter.csv'.

'allDataSurvey.csv' is a csv file containing daily mean survey responses for the five questions making up the ICS.

'dailyTwitter.csv' is a csv file containing the sentiment of tweets for each category of "jobs" tweets, with each row being a single day.

To run the shiny app, save 'allDataSurvey.csv', 'dailyTwitter.csv', and the script from 'app.R' in the same folder. Run 'app.R' in R to display shiny app. The shiny app requires the following packages to be installed in R before running: shiny, xts, lubridate, reshape2, ggplot2, data.table.

With the shiny app we can also track a combination of the above "jobs" tweets categories. For example, if we wanted to completely remove junk/irrelevant tweets from the analysis, we would check all boxes except 'junk' under 'Category Combination'. This shows up as 'combo' in plots and tables. Sentiment of all "jobs" tweets is denoted as 'allSent'.

The first figure is a timeplot showing the sentiment over time for each of the "jobs" categories selected and each survey question selected.

The next table contains correlations between the "jobs" categories and survey responses.

To see how these correlations change over time, we next display rolling correlations between the "jobs" categories and survey questions. For a chosen rolling correlation window, we calculate the correlation between the two time series for that length of time. For example, if we are tracking the correlation between ICS and all "jobs" tweets with a rolling correlation window of 365 days, the point of the graph at 1/1/2008 is the correlation between sentiment of all "jobs" tweets and ICS from 1/1/2008 throough 1/1/2009.

We next calculate comovement between the time series. Comovement is defined as the proportion of time two time series move in the same direction from one time period to the next. We allow comovement to be calculated on the daily, weekly, and monthly scale. Rolling comovement is similar to rolling correlation.

Many of the results from the paper can be reproduced using this Shiny App, for example:
- To replicate the first column of Table 1, we select tweets from 1/1/2008 through 12/31/2009, select the ICS survey question, sentiment of each category of "jobs" tweets, use the dictionary-based method of OpinionFinder, counting the number of tweets, calculating sentiment as pos/neg, a -50 day survey shift, and smoothing of 30 days.
- We can replicate the correlation between sentiment of all "jobs" tweets and ICS using Vader (as in Table 3), by selecting tweets from 1/1/2008 through 12/31/2009, select the ICS survey questions, sentiment of all "jobs" tweets, Vader sentiment method, survey shift of -50, and smoothing of 30

