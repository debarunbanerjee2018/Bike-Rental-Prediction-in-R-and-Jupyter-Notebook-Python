#Clean the environment
rm(list = ls())

#Set working directory
setwd("Z:/Edwisor Project 3")

#Loading all libraries
libraries = c("caret","plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","corrgram","DataCombine")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)
library('forecast')
library('tseries')

#Read the csv file
day = read.csv(file = "day.csv", header = T, sep = ",", na.strings = c(" ", "", "NA"))

########################################EXPLORE THE DATA########################################
#First few rows
head(day)

#Dimensions of data
dim(day)

#Column names
names(day)

#Structure of variables
str(day)

########################################FEATURE ENGINEERING########################################
# Droping instant and Date column and tranforming the datatypes
day$dteday = NULL
day$instant = NULL
day$season = as.factor(day$season)
day$yr = as.factor(day$yr)
day$mnth = as.factor(day$mnth)
day$holiday = as.factor(day$holiday)
day$weekday = as.factor(as.character(day$weekday))
day$workingday = as.factor(as.character(day$workingday))
day$weathersit = as.factor(day$weathersit)
day$casual = as.numeric(day$casual)
day$registered = as.numeric(day$registered)
day$cnt = as.numeric(day$cnt)

########################################MISSING VALUES########################################
# We don't have any missing values in our dataset
missing_values = sapply(day, function(x){sum(is.na(x))})
missing_values
########################################EXPLORE USING GRAPHS########################################
#Check the distribution of categorical Data using bar graph
bar1 = ggplot(data = day, aes(x = season)) + geom_bar() + ggtitle("Count of Season")
bar2 = ggplot(data = day, aes(x = weathersit)) + geom_bar() + ggtitle("Count of Weather")
bar3 = ggplot(data = day, aes(x = holiday)) + geom_bar() + ggtitle("Count of Holiday")
bar4 = ggplot(data = day, aes(x = workingday)) + geom_bar() + ggtitle("Count of Working day")
# ## Plotting plots together
gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)

#Check the normality of numerical data using histogram
hist1 = ggplot(data = day, aes(x =temp)) + ggtitle("Distribution of Temperature") + geom_histogram(bins = 25)
hist2 = ggplot(data = day, aes(x =hum)) + ggtitle("Distribution of Humidity") + geom_histogram(bins = 25)
hist3 = ggplot(data = day, aes(x =atemp)) + ggtitle("Distribution of Feel Temperature") + geom_histogram(bins = 25)
hist4 = ggplot(data = day, aes(x =windspeed)) + ggtitle("Distribution of Windspeed") + geom_histogram(bins = 25)
gridExtra::grid.arrange(hist1,hist2,hist3,hist4,ncol=2)

#Check the distribution of numerical data using scatterplot
scat1 = ggplot(data = day, aes(x =temp, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike COunt")
scat2 = ggplot(data = day, aes(x =hum, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point(color="red") + xlab("Humidity") + ylab("Bike COunt")
scat3 = ggplot(data = day, aes(x =atemp, y = cnt)) + ggtitle("Distribution of Feel Temperature") + geom_point() + xlab("Feel Temperature") + ylab("Bike COunt")
scat4 = ggplot(data = day, aes(x =windspeed, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point(color="red") + xlab("Windspeed") + ylab("Bike COunt")
gridExtra::grid.arrange(scat1,scat2,scat3,scat4,ncol=2)

#Check for outliers in data using boxplot
day_copy = day
cnames = colnames(day[,c("temp","atemp","windspeed","hum")])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = day)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn3,gn2,gn4,ncol=2)

#Replace all outlier data with NA
outlier_contains_columns = colnames(day[,c("windspeed","hum")])
for(i in outlier_contains_columns){
  val = day[,i][day[,i] %in% boxplot.stats(day[,i])$out]
  print(paste(i,length(val)))
  day[,i][day[,i] %in% val] = NA
}
#Check number of missing values
sapply(day,function(x){sum(is.na(x))})
#treating these missing values by imputing through KNN as we have very less observations for our dataset
#Compute the NA values using KNN imputation
train_control <- trainControl(method = "repeatedcv", repeats = 30)
knnFit <- train(cnt ~ ., data = day_copy, 
                method = "knn", trControl = train_control, tuneLength = 10)
knnFit
#Use plots to see optimal number of clusters:
#Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
plot(knnFit)
day = knnImputation(data = day, k = 5)


df = day[,c("temp","atemp","hum","windspeed","casual","registered")]
vifcor(df)
df1 = day[,c("temp","hum","windspeed","casual","registered")]
vifcor(df1)
df2 = day[,c("temp","hum","windspeed","casual")]
vifcor(df2)
df3 = day[,c("temp","hum","windspeed")]
vifcor(df3)

#Check for collinearity using corelation graph
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrgram(day, order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot",method="number",addCoef.col = "black")


day <- subset(day, select = c(cnt,temp,hum,windspeed,yr,mnth,holiday,weekday,workingday,weathersit))
rmExcept(keepers = "day")

#Divide the data into train and test
set.seed(123)
train_index = sample(1:nrow(day), 0.8 * nrow(day))
train = day[train_index,]
test = day[-train_index,]  

########################################DECISION TREE########################################
#rpart for regression
dt_model = rpart(cnt ~ ., data = train, method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model, test[,-1])

#Create dataframe for actual and predicted values
df = data.frame("actual"=test[,1], "pred"=dt_predictions)
head(df)

#calculate MAPE
regr.eval(trues = test[,1], preds = dt_predictions, stats = c("mae","mse","rmse","mape"))

#calculate MAPE
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}
MAPE(test[,1], dt_predictions)
########################################RANDOM FOREST########################################

#Train the data using random forest
rf_model = randomForest(cnt~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-1])

#Create dataframe for actual and predicted values
df = cbind(df,rf_predictions)
head(df)

#Calculate MAPE
regr.eval(trues = test[,1], preds = rf_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,1],rf_predictions)

########################################LINEAR REGRESSION########################################

#Train the data using linear regression
lr_model = lm(formula = cnt~., data = train)

#Check the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model, test[,-1])

#Create dataframe for actual and predicted values
df = cbind(df,lr_predictions)
head(df)

#Calculate MAPE
regr.eval(trues = test[,1], preds = lr_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,1], lr_predictions)

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

#Predict a sample data
predict(lr_model,test[1,])

################################ARIMA Time Series Analysis##############################

#taking only the date and count variable.

day <- subset(day, select = c(cnt,dteday))

day$Date = as.Date(day$dteday)

ggplot(day, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Count") + xlab("")


count_ts = ts(day[, c('cnt')]);

day$clean_count = tsclean(count_ts)

ggplot() +
  geom_line(data = day, aes(x = date, y = clean_count)) + ylab('Bike Count')

# calculating the weekly Moving average
day$count_ma = ma(day$clean_count, order=7)
#Calculating Monthly Moving Average
day$count_ma30 = ma(day$clean_count, order=30)

#PLoting the Data to create Line Graph
ggplot() +
  geom_line(data = day, aes(x = date, y = clean_count, colour = "Counts")) +
  geom_line(data = day, aes(x = date, y = count_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = day, aes(x = date, y = count_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bike Count')


count_ma = ts(na.omit(day$count_ma), frequency=30)
#Decomposition of data into four parts
decomp = stl(count_ma, s.window="periodic")
# taking the seanoal count
deseasonal_cnt <- seasadj(decomp)
#Poting the decomposed parts
plot(decomp)

#Testing if data is stationary or not
adf.test(count_ma, alternative = "stationary")

Acf(count_ma, main='')

Pacf(count_ma, main='')

# Differentiating the Deseasonal Count
count_d1 = diff(deseasonal_cnt, differences = 1)
#PLoting the differentiated data and checking the stationarity
plot(count_d1)
adf.test(count_d1, alternative = "stationary")



Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')
# We can make the ARIMA as we have got the data as Stationary format
auto.arima(deseasonal_cnt, seasonal=FALSE)


#fitting the ARIMA model
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
#PLoting the Residuals
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

# fitting the arima by seasonal interval

fit2 = arima(deseasonal_cnt, order=c(1,1,7))

fit2
#Checking the reseduals of the ARIMA model
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

# Monthly Forcasting
fcast <- forecast(fit2, h=30)
# PLoting the forecast
plot(fcast)
