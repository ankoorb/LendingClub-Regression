# Session info
sessionInfo()

# Set working director
setwd("/Users/Ankoor/Documents/Git/LendingClub-Regression")

# Get file form the internet
fileUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv"
download.file(fileUrl, destfile = "loansData.csv", method = "curl")

# Date of download
dateDownloaded <- date()

# Load data to R
loanData <- read.csv("loansData.csv")

# Display data: column names and data type
sapply(loanData, class)

# Display first few rows of data, last rows -> tail()
head(loanData)

## Check for missing data
# Identifies total no. of missing values
sum(is.na(loanData)) 

# Identifies column names with missing data
names(loanData[, !complete.cases(t(loanData))]) # t() -> transpose

# Data cleanup
# Remove rows with NA's
loanData <- loanData[complete.cases(loanData),]

# Remove column with xx% to numeric variables
loanData$Interest.Rate <- as.numeric(strsplit(as.character(loanData$Interest.Rate),"%"))
loanData$Debt.To.Income.Ratio <- as.numeric(strsplit(as.character(loanData$Debt.To.Income.Ratio),
                                                     "%"))

# Remove column with xx month to numeric variables
loanData$Loan.Length <- as.numeric(strsplit(as.character(loanData$Loan.Length)," months"))

# Mean of FICO Range (simple approach)
# loanData$FICO.Range <- as.numeric(substring(loanData$FICO.Range, 1, 3)) + 2

# Mean of FICO Range (Another approach)
meanFICO <- function(x) (as.numeric(substr(x, 1, 3)) + as.numeric(substr(x, 5, 7)))/2
loanData$FICO.Mean <- sapply(loanData$FICO.Range, meanFICO)

# Remove monthly income outlier
loanData <- loanData[which(loanData$Monthly.Income < 50000),]

# Info and plots
summary(loanData$Interest.Rate)
quantile(loanData$Interest.Rate)


## Exploratory Data Analysis
# Set plot display to 1 row and 3 graphs
par(mfrow = c(1, 3))

# Plot interest rate and check for normal distribution
boxplot(loanData$Interest.Rate, col = "yellow", xlab = "Interest Rate", main = "Boxplot")
hist(loanData$Interest.Rate, col = "blue", xlab = "Interest Rate", main = "Histogram")
qqnorm(loanData$Interest.Rate)
qqline(loanData$Interest.Rate, col = "red", lwd = 1.5)

# Boxplot using 2 variables
par(mfrow = c(1, 3))
boxplot(loanData$Interest.Rate ~ loanData$Loan.Length, col = "orange", varwidth = TRUE, xlab = "Loan Length")
boxplot(loanData$Interest.Rate ~ loanData$Loan.Purpose, col = "green", varwidth = TRUE, xlab = "Loan Purpose")
boxplot(loanData$Interest.Rate ~ loanData$Home.Ownership, col = "gray", varwidth = TRUE, xlab = "Home Ownership")

par(mfrow = c(1, 3))
boxplot(loanData$Interest.Rate ~ loanData$Employment.Length, col = "blue", varwidth = TRUE, xlab = "Employment Length")
boxplot(loanData$Interest.Rate ~ loanData$Inquiries.in.the.Last.6.Months, xlab = "# of Inquiries in last 6 months",col = "red", varwidth = TRUE)
boxplot(loanData$Interest.Rate ~ loanData$Open.CREDIT.Lines, col = "purple", varwidth = TRUE, xlab = "# of Open Credit Lines")

# Interest rate and Mean FICO score
par(mfrow = c(1,1))
boxplot(loanData$Interest.Rate ~ loanData$FICO.Mean, col = "blue", varwidth = TRUE, xlab = "Mean FICO Score")

# Interest rate and FICO Range
par(mfrow = c(1,1))
boxplot(loanData$Interest.Rate ~ loanData$FICO.Mean, col = "yellow", varwidth = TRUE, xlab = "FICO Range")

# Interest rate and State
par(mfrow = c(1,1))
boxplot(loanData$Interest.Rate ~ loanData$State, col = "green", xlab = "State")

# Fitting a simple linear regression model to predict interest rate based on mean fico score
meanFicoLM <- lm(loanData$Interest.Rate ~ loanData$FICO.Mean)
summary(meanFicoLM)

# Fitting all independent variables to predict interest rate
testLM <- lm(loanData$Interest.Rate ~ ., data = loanData)
summary(testLM)

# Fitting model with significant independent variables: Amount.Requested,
# Amount.Funded.By.Investors, Loan.Length, Monthly.Income, Open.CREDIT.Lines, 
# Inquiries.in.the.Last.6.Months, FICO.Mean
linearModel <- lm(loanData$Interest.Rate ~ loanData$Amount.Requested + loanData$Loan.Length +
                          loanData$Amount.Funded.By.Investors + loanData$Monthly.Income +
                          loanData$Open.CREDIT.Lines + loanData$Inquiries.in.the.Last.6.Months +
                          loanData$FICO.Mean)
summary(linearModel)

# Display 95% Confidence Interval
confint(linearModel)

# Plot Residuals check fitting problems
par(mfrow = c(1,2))
hist(linearModel$residuals, col = "azure", xlab = "Residuals") # Colors: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
qqnorm(linearModel$residuals)
qqline(linearModel$residuals, col = "blue", lwd = 2) # lwd -> line width

par(mfrow = c(1, 1))
plot(loanData$Interest.Rate, linearModel$residuals, pch = 20) # pch -> plotting character
abline(c(0, 0), col = "red",  lwd =3)

plot(linearModel$fitted, linearModel$residuals, col = "blue", pch = 20)

par(mfrow = c(2, 2))
plot(linearModel)

# Comparing residuals
par(mfrow = c (1, 2))
plot(loanData$Interest.Rate, meanFicoLM$residuals, pch = 19, xlab = "Interest Rate",
     ylab = "Residual (Actual - Predicted)", main = "Mean FICO Linear Model")
abline(c(0,0), col = "blue", lwd = 3)

plot(loanData$Interest.Rate, linearModel$residuals, pch = 19, xlab = "Interest Rate",
     ylab = "Residual (Actual - Predicted)", main = "Significant Factor Linear Model")
abline(c(0,0), col = "blue", lwd = 3)

########### Machine Learning ############
# Trick to replace or impute missing value (NA's) with mean values
# loanData$Monthly.Income[is.na(loanData$Monthly.Income)] <- mean(loanData$Monthly.Income)

## Training and Testing Data Sets: Training = 80%, Testing = 20%
# Set the seed to make partition reproducible
set.seed(15)
sampleSize <- ceiling(nrow(loanData)*0.8)
position <- sample(nrow(loanData), sampleSize)
train <- loanData[position, ]
test <- loanData[-position, ]

# Removing dependent variable from test data
trueValues <- test$Interest.Rate
test$Interest.Rate <- NULL 

# Fit significant factors in training data
trainLM <- lm(Interest.Rate ~ Amount.Requested + Loan.Length + Amount.Funded.By.Investors + 
                      Monthly.Income + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months +
                      FICO.Mean, data = train)


# Predict interest rates
interestRateHat <- predict(trainLM, data = test)

# Calculate Root Mean Squared Error
RMSE <- sqrt(mean(trueValues - interestRateHat)^2)
RMSEpercent <- RMSE/mean(trueValues) * 100
