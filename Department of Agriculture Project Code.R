


low_income_gdpcap = read.csv('low-income-gdp-per-capita.csv')

#low_income_gdpcap = c('Burundi', 'Central African Republic', 'Madagascar', 'Syrian Arab Republic', 'Congo, Dem. Rep.',
#                         'Niger', 'Sudan', 'Gambia, The', 'Rwanda', 'Uganda')
'low_income_gdpcap = gdpcap[is.element(gdpcap$Country.Name, low_income_gdpcap),]
low_income_gdpcap[is.na(low_income_gdpcap)] = 0
low_income_gdpcap = t(low_income_gdpcap)'
#install.packages('janitor')
'library(janitor)
library(dplyr)
low_income_gdpcap = row_to_names(low_income_gdpcap, row_number = 1)
low_income_gdpcap = low_income_gdpcap[-c(1, 2, 3),]
low_income_gdpcap = data.frame(apply(low_income_gdpcap, 2, function(x) as.numeric(x)))'
low_income_gdpcap$Average1 = rowMeans(low_income_gdpcap[,-1])

low_income_age = read.csv('low-income-age.csv')
low_income_age$Average = rowMeans(low_income_age[,-1])

low_income_no_educ = read.csv('low-income-no-education.csv')
low_income_no_educ$Average = rowMeans(low_income_no_educ[,-1],na.rm = TRUE)

low_income_economic_df = cbind(low_income_age$Year, low_income_gdpcap$Average, low_income_age$Average, low_income_no_educ$Average)
colnames(low_income_economic_df) = c('Year', 'Average GDP Per Capita', 'Average Median Age', 'Average % of 15+ Pop. with no education')

## ok I got the economic indicators all organized finally... now I have to do the same with the
## crop production data

low_income_milk = read.csv('low-income-milk-supply.csv')
low_income_milk$Average = rowMeans(low_income_milk[,-1],na.rm = TRUE)

low_income_rice = read.csv('low-income-rice-supply.csv')
low_income_rice$Average = rowMeans(low_income_rice[,-1],na.rm = TRUE)

low_income_potatoes = read.csv('low-income-potatoes-supply.csv')
low_income_potatoes$Average = rowMeans(low_income_potatoes[,-1],na.rm = TRUE)

low_income_wheat = read.csv('low-income-wheat-supply.csv')
low_income_wheat$Average = rowMeans(low_income_wheat[,-1],na.rm = TRUE)

low_income_maize = read.csv('low-income-maize-supply.csv')
low_income_maize$Average = rowMeans(low_income_maize[,-1],na.rm = TRUE)


low_income_crops = cbind(low_income_milk$Year, low_income_milk$Average, low_income_maize$Average, low_income_wheat$Average,
                         low_income_potatoes$Average, low_income_rice$Average)
colnames(low_income_crops) = c('Year', 'Average Milk Supply', 'Average Maize Supply', 'Average Wheat Supply',
                               'Average Potato Supply', 'Average Rice Supply')


## okey dokey, got all the low income data, now it /may/ be helpful to impute data for
## the average % of the population that had no education since we're missing most years.
## I'll do "MICE," but in this case all that means is regressing that variable on the rest of the
## variables in my dataset, then filling in the missing values with predicted values.

## first, join all data in one weird df
all_low_income_data = as.data.frame(cbind(low_income_crops, low_income_economic_df[,-1]))
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

all_low_income_data[is.nan(all_low_income_data)] = NA
all_low_income_data = all_low_income_data[12:71,]
#install.packages('mice')
'library(mice)
imputed_data = mice(all_low_income_data)'
exog = all_low_income_data[,-c(1,ncol(all_low_income_data))]
exog = as.matrix(exog)
mice_model = lm(all_low_income_data$`Average % of 15+ Pop. with no education` ~ exog)
coefficients = mice_model[['coefficients']]
imputed_no_educ = coefficients[1] + coefficients[2] * all_low_income_data[,2] + coefficients[3] * all_low_income_data[,3] +
  coefficients[4] * all_low_income_data[,4] + coefficients[5] * all_low_income_data[,5] + coefficients[6] * all_low_income_data[,6] +
  coefficients[7] * all_low_income_data[,7] + coefficients[8] * all_low_income_data[,8]
print(imputed_no_educ)
low_income_is_na = is.na(all_low_income_data$`Average % of 15+ Pop. with no education`)
all_low_income_data$`Average % of 15+ Pop. with no education`[low_income_is_na] = imputed_no_educ[low_income_is_na]
## successful imputation! Nice. Ok, so now we need to find which crop supply series has the biggest
## positive impact on the economic indicators we have available... using the ARDL structure we
## usually use? And just basing our estimation of "positive impact" on the coefficients of the
## crop supply in question? But won't we need to 

## Just checked the data again and found one year with exactly 0 as the "Average GDP Per Capita,"
## so let me just fix that really quick with another imputation for that observation only
all_low_income_data$`Average GDP Per Capita`[which(all_low_income_data$`Average GDP Per Capita`==0)] = NA
exog = all_low_income_data[,-c(1,ncol(all_low_income_data)-2)]
exog = as.matrix(exog)
mice_model = lm(all_low_income_data$`Average GDP Per Capita` ~ exog)
coefficients = mice_model[['coefficients']]
imputed_gdp_per_cap = coefficients[1] + coefficients[2] * exog[,1] + coefficients[3] * exog[,2] +
  coefficients[4] * exog[,3] + coefficients[5] * exog[,4] + coefficients[6] * exog[,5] +
  coefficients[7] * exog[,6] + coefficients[8] * exog[,7]
print(imputed_gdp_per_cap)
low_income_is_na = is.na(all_low_income_data$`Average GDP Per Capita`)
all_low_income_data$`Average GDP Per Capita`[low_income_is_na] = imputed_gdp_per_cap[low_income_is_na]

## Ok turns out I filled NA values in the GDP per capita with zeros earlier in the
## process, so I undid that and ran everything and I've still got some missing data
## but mostly not missing data, so I think it wouldn't hurt to impute a couple more times
## using the data I have to fill in the remaining NA values for both no_educ and gdp_per_cap;

## first no_educ (one more time)
## remove no_educ AND gdp_per_cap this time
exog = all_low_income_data[,-c(1,ncol(all_low_income_data), ncol(all_low_income_data)-2)]
exog = as.matrix(exog)
mice_model = lm(all_low_income_data$`Average % of 15+ Pop. with no education` ~ exog)
coefficients = mice_model[['coefficients']]
imputed_no_educ = coefficients[1] + coefficients[2] * exog[,1] + coefficients[3] * exog[,2] +
  coefficients[4] * exog[,3] + coefficients[5] * exog[,4] + coefficients[6] * exog[,5] +
  coefficients[7] * exog[,6]
print(imputed_no_educ)
low_income_is_na = is.na(all_low_income_data$`Average % of 15+ Pop. with no education`)
all_low_income_data$`Average % of 15+ Pop. with no education`[low_income_is_na] = imputed_no_educ[low_income_is_na]

## now again, one more time for gdp_per_cap
exog = all_low_income_data[,-c(1,ncol(all_low_income_data)-2)]
exog = as.matrix(exog)
mice_model = lm(all_low_income_data$`Average GDP Per Capita` ~ exog)
coefficients = mice_model[['coefficients']]
imputed_gdp_per_cap = coefficients[1] + coefficients[2] * exog[,1] + coefficients[3] * exog[,2] +
  coefficients[4] * exog[,3] + coefficients[5] * exog[,4] + coefficients[6] * exog[,5] +
  coefficients[7] * exog[,6] + coefficients[8] * exog[,7]
print(imputed_gdp_per_cap)
low_income_is_na = is.na(all_low_income_data$`Average GDP Per Capita`)
all_low_income_data$`Average GDP Per Capita`[low_income_is_na] = imputed_gdp_per_cap[low_income_is_na]


## Ok, NOW I can get onto the analysis part (or at least the first analysis part).
## We want to find the crop with the highest impact on economic well being, so we're
## actually going to construct a VaR model with each crop and the 3 economic indicators we
## have available, then determine based on those results which crop is best.

## first let's test the stationarity of each time series:
library(tseries)
for (col in c(2:ncol(all_low_income_data)-1)) {
  print(colnames(all_low_income_data)[col+1])
  print(adf.test(na.omit(all_low_income_data[,col+1])))
  print('\n')
}

time = c(1:nrow(all_low_income_data))
plot(time, all_low_income_data$`Average Milk Supply`, type='l',col='black',main='Average Milk Supply',ylab='KG Per Capita Per Year',xlab='Years since 1961')
plot(time, all_low_income_data$`Average Maize Supply`, type='l',col='black',main='Average Maize Supply',ylab='KG Per Capita Per Year',xlab='Years since 1961')
plot(time, all_low_income_data$`Average Wheat Supply`, type='l',col='black',main='Average Wheat Supply',ylab='KG Per Capita Per Year',xlab='Years since 1961')
plot(time, all_low_income_data$`Average Potato Supply`, type='l',col='black',main='Average Potato Supply',ylab='KG Per Capita Per Year',xlab='Years since 1961')
plot(time, all_low_income_data$`Average Rice Supply`, type='l',col='black',main='Average Rice Supply',ylab='KG Per Capita Per Year',xlab='Years since 1961')

plot(time, all_low_income_data$`Average GDP Per Capita`, type='l',col='black',main='Average GDP Per Capita',ylab='Current US Dollars',xlab='Years since 1961')
plot(time, all_low_income_data$`Average Median Age`, type='l',col='black',main='Average Median Age',ylab='Years',xlab='Years since 1961')
plot(time, all_low_income_data$`Average % of 15+ Pop. with no education`, type='l',col='black',main='Average % of the Population 15 Years or Older With No Education',ylab='Percentage',xlab='Years since 1961')

## seems like everything is stationary! so no VaR issues are afoot
#install.packages('vars')
library(vars)
## first milk...
x = all_low_income_data[,-c(1, 3, 4, 5, 6)]
#plot.ts(x,main='',ylab=c('bruh','bruh','bruh','bruh'))
## plot.ts doesn't work how I want it to, so I'll just plot things individually eventually
fitvar1 = VAR(na.omit(x),p=1)
summary(fitvar1)
## a higher milk supply in year (x-1) leads to...
## higher GDP/capita (at no significant level), lower median age (at the 10% level), and
## more uneducated people over the age of 15 (at no significant level) in the year x.
## try with maize now
x = all_low_income_data[,-c(1, 2, 4, 5, 6)]
fitvar1 = VAR(na.omit(x),p=1)
summary(fitvar1)
## a higher maize supply in year (x-1) leads to...
## lower GDP/capita (at less than 1% level), lower median age (at less than 1% level), and
## more uneducated people over the age of 15 (at the 5% level) in the year x... what??
## now for wheat...
x = all_low_income_data[,-c(1, 2, 3, 5, 6)]
fitvar1 = VAR(na.omit(x),p=1)
summary(fitvar1)
## a higher wheat supply in year (x-1) leads to...
## lower GDP/capita (at less than 1% level), lower median age (at no significant level),
## and more uneducated people over the age of 15 (at the 5% level) in year x.
## now potatoes...
x = all_low_income_data[,-c(1, 2, 3, 4, 6)]
fitvar1 = VAR(na.omit(x),p=1)
summary(fitvar1)
## a higher potato supply in year (x-1) leads to...
## higher GDP/capita (at 5% level), higher median age (at 5% level), and
## fewer uneducated people over the age of 15 (no significant level) in year x.
## and finally rice...
x = all_low_income_data[,-c(1, 2, 3, 4, 5)]
fitvar1 = VAR(na.omit(x),p=1)
summary(fitvar1)
## a higher supply of rice in year (x-1) leads to...
## lower GDP/capita (at 0.1% level), lower median age (at 1% level),
## and more uneducated people over the age of 15 (at no significant level) in year x.

## so I think more (maize -- on second thought, this is... not the case for whatever reason. I messed
## up the aggregation of the gdp/capita data and didn't align them to the correct years, which led to
## the literal opposite results of what the correct data gives... so...) potatoes seem to have the 
## best effect on an economy, despite the lower median age result.
## None of these crop supplies seem to have a notable effect on education rates, which may make sense since
## not only is food a bit less directly related to how often people attend school than it's related to people's
## physical health (median age) and having sufficient energy to work (GDP/capita), but it's also largely imputed data!
## and that could totally have some dampening effect on the amount of information we can truly extract from this analysis.
## Anyway, what I would like to do now is find the US's exports of maize to these 10 countries, then construct a VaR
## with US maize exports, GDP/capita, median age, and education levels for each of the 10 countries + those same series for the
## US in each VaR (so a total of 7 variables in each VaR, repeated 10 times for each of the poorer countries used here).


## well ok before I do that I actually have to impute some data for the US education series!
## totally forgot about that, so let's put all the data into one dataframe and do that

us_data = read.csv('us-economic-df.csv')
low_income_age = read.csv('low-income-age.csv')
low_income_no_educ = read.csv('low-income-no-education.csv')
low_income_gdpcap = read.csv('low-income-gdp-per-capita.csv')


all_data = cbind(low_income_age, low_income_gdpcap[,-1], low_income_no_educ[,-1],
                 us_data[,-1])

## so now I actually have to impute the no_educ data for each country... so maybe
## it's a good idea to just do that as part of the steps for every analysis, especially
## since we aren't using Burundi data in the Gambia analysis (for example).

## well, in fact, we don't have potato export data on a few countries, one of which is Burundi.
## so now we gotta get rid of those countries!
## in particular we're missing potato export data on...
## Burundi, Uganda, Madagascar, Syria, the Central African Republic, and Rwanda
all_data = all_data[,-c(2, 12, 22, 3, 13, 23, 4, 14, 24, 5, 15, 25, 10, 20, 30, 11, 21, 31)]

## so start by bringing in export data
potatoes = read.csv('US Potato Exports transposed.csv')
potatoes$Congo = potatoes$Congo..Brazzaville. + potatoes$Congo..Kinshasa.
## let's start with the congo
congo_analysis = cbind(all_data[18:73,c(2, 6, 10, ncol(all_data)-2, ncol(all_data)-1, ncol(all_data))], potatoes$Congo)
## including potato exports produces singular fit for this model, so we'll just ignore them while imputing
exog = congo_analysis[,-c(3, 5, 7)]
exog = as.matrix(exog)
mice_model = lm(congo_analysis$US...of.Pop.15..with.no.education ~ exog, singular.ok = TRUE)
coefficients = mice_model[['coefficients']]
imputed_no_educ = coefficients[1] + coefficients[2] * exog[,1] + coefficients[3] * exog[,2] +
  coefficients[4] * exog[,3] + coefficients[5] * exog[,4]
print(imputed_no_educ)
imputed_no_educ[which(imputed_no_educ<0)] = 0
print(imputed_no_educ)
us_no_educ_na = is.na(congo_analysis$US...of.Pop.15..with.no.education)
congo_analysis$US...of.Pop.15..with.no.education[us_no_educ_na] = imputed_no_educ[us_no_educ_na]

## repeat for congo data
exog = congo_analysis[,-c(3, 7)]
exog = as.matrix(exog)
mice_model = lm(congo_analysis$Democratic.Republic.of.Congo...of.Pop.15..with.no.education ~ exog, singular.ok = TRUE)
coefficients = mice_model[['coefficients']]
imputed_no_educ = coefficients[1] + coefficients[2] * exog[,1] + coefficients[3] * exog[,2] +
  coefficients[4] * exog[,3] + coefficients[5] * exog[,4] + coefficients[6] * exog[,5]
print(imputed_no_educ)
imputed_no_educ[which(imputed_no_educ<0)] = 0
print(imputed_no_educ)
no_educ_na = is.na(congo_analysis$Democratic.Republic.of.Congo...of.Pop.15..with.no.education)
congo_analysis$Democratic.Republic.of.Congo...of.Pop.15..with.no.education[no_educ_na] = imputed_no_educ[no_educ_na]

time = c(1:nrow(congo_analysis))
plot(time, congo_analysis$Democratic.Republic.of.Congo.Median.Age,type='l',col='black',main='DRC Median Age',ylab='Years',xlab='Years since 1967')
plot(time, congo_analysis$Congo..Dem..Rep..GDP.Per.Capita,type='l',col='black',main='DRC GDP per Capita',ylab='Current US Dollars',xlab='Years since 1967')
plot(time, congo_analysis$Democratic.Republic.of.Congo...of.Pop.15..with.no.education,type='l',col='black',main='DRC % of Popoulation 15+ With No Education',ylab='Percentage',xlab='Years since 1967')
plot(time, congo_analysis$`potatoes$Congo`,type='l',col='black',main='US Potato Exports to the DRC',ylab='Thousands of Current US Dollars',xlab='Years since 1967')

## now we do the VaR!
x = congo_analysis
fitvar1 = VAR(na.omit(x),p=1)
summary(fitvar1)



############

niger_analysis = cbind(all_data[18:73,c(3, 7, 11, ncol(all_data)-2, ncol(all_data)-1, ncol(all_data))], potatoes$Niger)
## including potato exports produces singular fit for this model, so we'll just ignore them while imputing
exog = niger_analysis[,-c(3, 5, 7)]
exog = as.matrix(exog)
mice_model = lm(niger_analysis$US...of.Pop.15..with.no.education ~ exog, singular.ok = TRUE)
coefficients = mice_model[['coefficients']]
imputed_no_educ = coefficients[1] + coefficients[2] * exog[,1] + coefficients[3] * exog[,2] +
  coefficients[4] * exog[,3] + coefficients[5] * exog[,4]
print(imputed_no_educ)
imputed_no_educ[which(imputed_no_educ<0)] = 0
print(imputed_no_educ)
us_no_educ_na = is.na(niger_analysis$US...of.Pop.15..with.no.education)
niger_analysis$US...of.Pop.15..with.no.education[us_no_educ_na] = imputed_no_educ[us_no_educ_na]

## repeat for niger data
exog = niger_analysis[,-c(3, 7)]
exog = as.matrix(exog)
mice_model = lm(niger_analysis$Niger...of.Pop.15..with.no.education ~ exog, singular.ok = TRUE)
coefficients = mice_model[['coefficients']]
imputed_no_educ = coefficients[1] + coefficients[2] * exog[,1] + coefficients[3] * exog[,2] +
  coefficients[4] * exog[,3] + coefficients[5] * exog[,4] + coefficients[6] * exog[,5]
print(imputed_no_educ)
imputed_no_educ[which(imputed_no_educ<0)] = 0
print(imputed_no_educ)
no_educ_na = is.na(niger_analysis$Niger...of.Pop.15..with.no.education)
niger_analysis$Niger...of.Pop.15..with.no.education[no_educ_na] = imputed_no_educ[no_educ_na]

time = c(1:nrow(niger_analysis))
plot(time, niger_analysis$Niger.Median.Age,type='l',col='black',main='Niger Median Age',ylab='Years',xlab='Years since 1967')
plot(time, niger_analysis$Niger.GDP.Per.Capita,type='l',col='black',main='Niger GDP per Capita',ylab='Current US Dollars',xlab='Years since 1967')
plot(time, niger_analysis$Niger...of.Pop.15..with.no.education,type='l',col='black',main='Niger % of Popoulation 15+ With No Education',ylab='Percentage',xlab='Years since 1967')
plot(time, niger_analysis$`potatoes$Niger`,type='l',col='black',main='US Potato Exports to the Niger',ylab='Thousands of Current US Dollars',xlab='Years since 1967')

## now we do the VaR!
x = niger_analysis
fitvar1 = VAR(na.omit(x),p=1)
summary(fitvar1)



##################

gambia_analysis = cbind(all_data[18:73,c(4, 8, 12, ncol(all_data)-2, ncol(all_data)-1, ncol(all_data))], potatoes$Gambia..The)
## including potato exports produces singular fit for this model, so we'll just ignore them while imputing
exog = gambia_analysis[,-c(3, 5, 7)]
exog = as.matrix(exog)
mice_model = lm(gambia_analysis$US...of.Pop.15..with.no.education ~ exog, singular.ok = TRUE)
coefficients = mice_model[['coefficients']]
imputed_no_educ = coefficients[1] + coefficients[2] * exog[,1] + coefficients[3] * exog[,2] +
  coefficients[4] * exog[,3] + coefficients[5] * exog[,4]
print(imputed_no_educ)
imputed_no_educ[which(imputed_no_educ<0)] = 0
print(imputed_no_educ)
us_no_educ_na = is.na(gambia_analysis$US...of.Pop.15..with.no.education)
gambia_analysis$US...of.Pop.15..with.no.education[us_no_educ_na] = imputed_no_educ[us_no_educ_na]

## repeat for gambia data
exog = gambia_analysis[,-c(3, 7)]
exog = as.matrix(exog)
mice_model = lm(gambia_analysis$Gambia...of.Pop.15..with.no.education ~ exog, singular.ok = TRUE)
coefficients = mice_model[['coefficients']]
imputed_no_educ = coefficients[1] + coefficients[2] * exog[,1] + coefficients[3] * exog[,2] +
  coefficients[4] * exog[,3] + coefficients[5] * exog[,4] + coefficients[6] * exog[,5]
print(imputed_no_educ)
imputed_no_educ[which(imputed_no_educ<0)] = 0
print(imputed_no_educ)
no_educ_na = is.na(gambia_analysis$Gambia...of.Pop.15..with.no.education)
gambia_analysis$Gambia...of.Pop.15..with.no.education[no_educ_na] = imputed_no_educ[no_educ_na]

time = c(1:nrow(gambia_analysis))
plot(time, gambia_analysis$Gambia.Median.Age,type='l',col='black',main='Gambia Median Age',ylab='Years',xlab='Years since 1967')
plot(time, gambia_analysis$Gambia..The.GDP.Per.Capita,type='l',col='black',main='Gambia GDP per Capita',ylab='Current US Dollars',xlab='Years since 1967')
plot(time, gambia_analysis$Gambia...of.Pop.15..with.no.education,type='l',col='black',main='Gambia % of Popoulation 15+ With No Education',ylab='Percentage',xlab='Years since 1967')
plot(time, gambia_analysis$`potatoes$Gambia..The`,type='l',col='black',main='US Potato Exports to the Gambia',ylab='Thousands of Current US Dollars',xlab='Years since 1967')


## now we do the VaR!
x = gambia_analysis
fitvar1 = VAR(na.omit(x),p=1)
summary(fitvar1)


########################
potatoes$Sudan = potatoes$Sudan... + potatoes$South.Sudan...
sudan_analysis = cbind(all_data[18:73,c(5, 9, 13, ncol(all_data)-2, ncol(all_data)-1, ncol(all_data))], potatoes$Sudan)
## including potato exports produces singular fit for this model, so we'll just ignore them while imputing
exog = sudan_analysis[,-c(3, 5, 7)]
exog = as.matrix(exog)
mice_model = lm(sudan_analysis$US...of.Pop.15..with.no.education ~ exog, singular.ok = TRUE)
coefficients = mice_model[['coefficients']]
imputed_no_educ = coefficients[1] + coefficients[2] * exog[,1] + coefficients[3] * exog[,2] +
  coefficients[4] * exog[,3] + coefficients[5] * exog[,4]
print(imputed_no_educ)
imputed_no_educ[which(imputed_no_educ<0)] = 0
print(imputed_no_educ)
us_no_educ_na = is.na(sudan_analysis$US...of.Pop.15..with.no.education)
sudan_analysis$US...of.Pop.15..with.no.education[us_no_educ_na] = imputed_no_educ[us_no_educ_na]

## repeat for sudan data
exog = sudan_analysis[,-c(3, 7)]
exog = as.matrix(exog)
mice_model = lm(sudan_analysis$Sudan...of.Pop.15..with.no.education ~ exog, singular.ok = TRUE)
coefficients = mice_model[['coefficients']]
imputed_no_educ = coefficients[1] + coefficients[2] * exog[,1] + coefficients[3] * exog[,2] +
  coefficients[4] * exog[,3] + coefficients[5] * exog[,4] + coefficients[6] * exog[,5]
print(imputed_no_educ)
imputed_no_educ[which(imputed_no_educ<0)] = 0
print(imputed_no_educ)
no_educ_na = is.na(sudan_analysis$Sudan...of.Pop.15..with.no.education)
sudan_analysis$Sudan...of.Pop.15..with.no.education[no_educ_na] = imputed_no_educ[no_educ_na]

time = c(1:nrow(sudan_analysis))
plot(time, sudan_analysis$Sudan.Median.Age,type='l',col='black',main='Sudan Median Age',ylab='Years',xlab='Years since 1967')
plot(time, sudan_analysis$Sudan.GDP.Per.Capita,type='l',col='black',main='Sudan GDP per Capita',ylab='Current US Dollars',xlab='Years since 1967')
plot(time, sudan_analysis$Sudan...of.Pop.15..with.no.education,type='l',col='black',main='Sudan % of Popoulation 15+ With No Education',ylab='Percentage',xlab='Years since 1967')
plot(time, sudan_analysis$`potatoes$Sudan`,type='l',col='black',main='US Potato Exports to the Sudan',ylab='Thousands of Current US Dollars',xlab='Years since 1967')


## now we do the VaR!
x = sudan_analysis
fitvar1 = VAR(na.omit(x),p=1)
summary(fitvar1)


time = c(1:nrow(sudan_analysis))
plot(time, sudan_analysis$US.Median.Age,type='l',col='black',main='US Median Age',ylab='Years',xlab='Years since 1967')
plot(time, sudan_analysis$US.GDP.Per.Capita,type='l',col='black',main='US GDP per Capita',ylab='Current US Dollars',xlab='Years since 1967')
plot(time, sudan_analysis$US...of.Pop.15..with.no.education,type='l',col='black',main='US % of Popoulation 15+ With No Education',ylab='Percentage',xlab='Years since 1967')
