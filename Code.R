#Downloading and installing packages, this a could take a few minutes

if(!require(caret)) install.packages("caret",repos ="http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse",repos ="http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot",repos ="http://cran.us.r-project.org")
install.packages("rJava", repos = "https://cran.rstudio.com")
install.packages("extraTrees",repos ="https://cran.r-project.org/")


# Please, make sure you have the right version of Java (32 or 64 bit) to match architectures with R, 
# otherwise the console will show an error

library(rJava)

# This instruction solves memory problems for "extraTrees" package

options( java.parameters = "-Xmx4g" ) 
library(extraTrees)

library(lubridate)

library(creditmodel)

# Create "temp" file and download database

temp <- tempfile()
download.file("https://raw.githubusercontent.com/GuidoRetegui/bike_rental_hour_saegus/master/bike_rental_hour_saegus.csv", temp)

brhs <- read.csv(temp)

str(brhs) # Structure of the database

head(brhs) # First rows of the database


# Data Cleaning


#Remove last day from the dataset

for (i in 52249:52272){ 
  brhs <- brhs[-c(52249),]
}


#delete "°C" from the variable 'temp'
brhs$temp <- lapply(brhs$temp, gsub, pattern='°C', replacement = '') 
#change variable's type to numeric
brhs$temp <- as.numeric(brhs$temp) 

#delete "°C" from the variable 'atemp'
brhs$atemp <- lapply(brhs$atemp, gsub, pattern='°C', replacement = '')
#change variable's type to numeric
brhs$atemp <- as.numeric(brhs$atemp) 

#delete "°C" from the variable 'dewpoint'
brhs$dewpoint <- lapply(brhs$dewpoint, gsub, pattern='°C', replacement = '') 
#change variable's type to numeric
brhs$dewpoint <- as.numeric(brhs$dewpoint)

#delete "%" from the variable 'hum'
brhs$hum <- lapply(brhs$hum, gsub, pattern='%', replacement = '') 
#change variable's type to numeric
brhs$hum <- as.numeric(brhs$hum) 

#delete " km/h" from the variable 'windspeed'
brhs$windspeed <- lapply(brhs$windspeed, gsub, pattern=' km/h', replacement = '') 
#change variable's type to numeric
brhs$windspeed <- as.numeric(brhs$windspeed)

#delete " mb" from the variable 'pressure'
brhs$pressure <- lapply(brhs$pressure, gsub, pattern='mb', replacement = '')
#change variable's type to numeric
brhs$pressure <- as.numeric(brhs$pressure) 

# Change "fog" to a dummy variable

for (i in 1:52248) {
  if (brhs$fog[i] == "true" & !is.na(brhs$fog[i])) {
    brhs$fog[i] = 1
  }
  else{
    brhs$fog[i] = 0
  }
}

#change variable's type to numeric

brhs$fog <- as.numeric(brhs$fog)

# Create the "weekdayname" column

brhs <- mutate(brhs, weekdayname = strftime(brhs$dteday, "%A"))

# Create the "weekdaynum" column. Weekdaynum as a decimal number (1-7, Monday is 1).

brhs <- mutate(brhs, weekdaynum = strftime(brhs$dteday, "%u"))

#change variable's type to numeric

brhs$weekdaynum <- as.numeric(brhs$weekdaynum)

# Create the column "lcnt" with the cnt's logarithm

brhs <- mutate(brhs, lcnt = log(brhs$cnt)) 

# Create the column "year"

brhs <- mutate(brhs, year = year(brhs$dteday)) 

# Create the column "workingday_dummy"

brhs <- mutate(brhs, workingday_dummy = 0) 

#Assigning "Workingday_dummy" column the value "1" when it is a workingday.

for (i in 1:52248) {
  if (brhs$workingday[i] == "True" & !is.na(brhs$workingday[i])) {
    brhs$workingday_dummy[i] = 1
  }
}

# Check first variables of the "new" database 

head(brhs)



# Data analysis


# Summary of total rents per day and mean per hour

brhs %>% group_by(dteday) %>% summarize (rents = sum(cnt), mean = mean(cnt))

# Accumulated rents per day, differentiated by "registered" and "casual" variables

sum_day_cnt <- brhs %>% group_by(dteday) %>% summarize(day_reg = sum(registered), day_casual = sum(casual), day_count = sum(cnt))
sum_day_cnt

# Mean of rents per day, differentiated by "registered" and "casual" variables

sum_day_cnt %>% summarize(reg_p_day = mean(day_reg), cas_p_day = mean(day_casual), cnt_p_day = mean(day_count))


# Total rents, differentiated by "registered" and "casual" variables

brhs %>% summarize(registered = sum(registered), casual = sum(casual), cnt= sum(cnt))


# Summary of principal variables
 
brhs %>% summarize (temp = mean(temp), atemp = mean(atemp)) 
brhs %>% summarize (dewpoint = mean(dewpoint), hum = mean(hum))
brhs %>% summarize (windspeed = mean(windspeed), pressure = mean(pressure))


# Hours with fog vs hours without fog, and its probability

brhs %>% summarize(fog = sum(brhs$fog==1), no_fog = sum(brhs$fog==0), 
                   Pr_fog = fog/(fog+no_fog), Pr_no_fog = no_fog/(fog+no_fog)) 



# Rents by hour.

brhs %>% group_by(hr) %>% 
  summarize(cnt=mean(cnt)) %>% 
  ggplot(aes(hr, cnt)) + 
  geom_point () + 
  geom_line()


# Rents by hour, differentiated by workingday

brhs %>% group_by(hr, workingday) %>% 
  summarize(cnt=mean(cnt), .groups = 'drop') %>% 
  ggplot(aes(hr, cnt, col = workingday, group = workingday)) + 
  geom_point () + 
  geom_line()



# Rents by hour, differentiated by day

brhs %>% group_by(hr, weekdayname) %>% 
  summarize(cnt=mean(cnt), .groups = 'drop') %>% 
  ggplot(aes(hr, cnt, col = weekdayname)) + 
  geom_point () + 
  geom_line()


# cnt histogram

brhs %>% ggplot(aes(cnt)) + geom_histogram(binwidth = 50, color="red") 

# Logarithm of cnt histogram

brhs %>% ggplot(aes(lcnt)) + geom_histogram(binwidth = 0.175, color="blue") 

#Select variables to analyse

M <- brhs %>% select(cnt, temp, atemp, dewpoint, hum, windspeed, pressure, casual, registered)

# Make the correlation of the selected variables

A <- cor(M)

# Show the correlation graph

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(A, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=90, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank", 
)



# Model Evaluation


# Define Root Mean Squared Logarithmic Error (RMSLE)
RMSLE <- function(true, pred){
  sqrt(mean((log(pred+1) - log(true+1))^2))
}

# Copy CNT values in "Y" and split to train (0.7) and test (0.3)

Y <- as.data.frame(brhs$cnt)

Y <- train_test_split(Y, prop = 0.7, split_type = "Random")

# Simplest Model 

mu <- mean(Y$train)
mu

RMSLE_SM <- RMSLE(mu, Y$test)
RMSLE_SM

#MODEL 2

X <- brhs # Assign database to X

#Delete some non-numeric columns of X

X <- select(X, -c(dteday, cnt, lcnt, fulldate, weathersit, wea_desc, 
                  workingday, weekdayname, weekdaynum, workingday_dummy)) 

#Split to train (0.7) and test (0.3)

X <- train_test_split(X, prop = 0.7, split_type = "Random")

# Extra Trees function

ex <- extraTrees(X$train,Y$train)

y_predict <- predict(ex, X$test)


#Calculate the residuals

residuals <- Y$test-y_predict

# Calculate the correlation between Y$test and residuals

r <- cor(Y$test, residuals)

# Calculate standard deviation of Y$test

s <- sd(Y$test)

# Calculate standard deviation of residuals

u <- sd(residuals)

#Graph of Number of observations with residuals

ggplot(data = NULL, aes(Y$test, residuals)) + geom_point (alpha = 0.5) + 
  geom_abline(intercept = mean(residuals)-((r*u/s)*mean(Y$test)), 
              slope = r*u/s, color = "green")

#RMSLE for Model 2

RMSLE2 <- RMSLE(Y$test, y_predict)
RMSLE2


# Model 3


X <- brhs # Assign database to X

#Delete the same non-numeric columns of X as the first model, except for "workingday_dummy"

X <- select(X, -c(dteday, cnt, lcnt, fulldate, weathersit, wea_desc, 
                  weekdayname, weekdaynum, workingday))

#Split to train (0.7) and test (0.3)

X <- train_test_split(X, prop = 0.7, split_type = "Random")

# Extra Trees function

ex <- extraTrees(X$train,Y$train)

y_predict <- predict(ex, X$test)

#Calculate the residuals

residuals <- Y$test-y_predict

# Calculate the correlation between Y$test and residuals

r <- cor(Y$test, residuals)

# Calculate standard deviation of Y$test

s <- sd(Y$test)

# Calculate standard deviation of residuals

u <- sd(residuals)

#Graph of Number of observations with residuals

ggplot(data = NULL, aes(Y$test, residuals)) + geom_point (alpha = 0.5) + 
  geom_abline(intercept = mean(residuals)-((r*u/s)*mean(Y$test)), 
              slope = r*u/s, color = "green")

#RMSLE for Model 3

RMSLE3 <- RMSLE(Y$test, y_predict)
RMSLE3

# Model 4


X <- brhs # Assign database to X

#Delete the same non-numeric columns of X as the first model, 
#except for "workingday_dummy" and "weekdaynum"

X <- select(X, -c(dteday, cnt, lcnt, fulldate, weathersit, 
                  wea_desc, weekdayname, workingday)) 

#Split to train (0.7) and test (0.3)

X <- train_test_split(X, prop = 0.7, split_type = "Random")

# Extra Trees function

ex <- extraTrees(X$train,Y$train)

y_predict <- predict(ex, X$test)

#Calculate the residuals

residuals <- Y$test-y_predict

# Calculate the correlation between Y$test and residuals

r <- cor(Y$test, residuals)

# Calculate standard deviation of Y$test

s <- sd(Y$test)

# Calculate standard deviation of residuals

u <- sd(residuals)

#Graph of Number of observations with residuals

ggplot(data = NULL, aes(Y$test, residuals)) + geom_point (alpha = 0.5) + 
  geom_abline(intercept = mean(residuals)-((r*u/s)*mean(Y$test)), 
              slope = r*u/s, color = "green")

#RMSLE for Model 4

RMSLE4 <- RMSLE(Y$test, y_predict)
RMSLE4

# Comparing Models Results

tibble (Model_1 = RMSLE_SM, Model_2 = RMSLE2, Model_3 = RMSLE3, Model_4 = RMSLE4)
