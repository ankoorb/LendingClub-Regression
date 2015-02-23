# Session info
sessionInfo()

# Set working director
setwd("/Users/Ankoor/Documents/Git/LendingClub-Regression")

# Get file form the internet
fileUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv"
download.file(fileUrl, destfile = "./data/loansData.csv", method = "curl") # "curl" for Mac (https problem)
list.files("./data") # Lists file in a particular directory

# Date of download
dateDownloaded <- date()

# Load data to R
loanData <- read.csv("loansData.csv")

# Other cool way to load data: loanData <- read.csv(file.choose())

# Structure of Data
str(loanData)

# Display data: column names and data type
sapply(loanData, class)

# Display first few rows of data, last rows -> tail()
head(loanData)

## Check for missing data

# Identifies total no. of missing values
sum(is.na(loanData)) 

# Identifies column names with missing data
names(loanData[, !complete.cases(t(loanData))]) # t() -> transpose

## Data cleanup

# Remove rows with NA's
loanData <- loanData[complete.cases(loanData),]

# Remove column with xx% to numeric variables
loanData$Interest.Rate <- as.numeric(strsplit(as.character(loanData$Interest.Rate),"%"))
loanData$Debt.To.Income.Ratio <- as.numeric(strsplit(as.character(loanData$Debt.To.Income.Ratio),
                                                     "%"))

# Remove column with xx month to numeric variables
# loanData$Loan.Length <- as.numeric(strsplit(as.character(loanData$Loan.Length)," months"))

# Mean of FICO Range (simple approach)
# loanData$FICO.Range <- as.numeric(substring(loanData$FICO.Range, 1, 3)) + 2

# Mean of FICO Range (Another approach)
# loanData$FICO <- paste(loanData$FICO_range_low, loanData$FICO_range_high, sep = "-")
meanFICO <- function(x) (as.numeric(substr(x, 1, 3)) + as.numeric(substr(x, 5, 7)))/2
loanData$FICO.Mean <- sapply(loanData$FICO.Range, meanFICO)

# Remove monthly income outlier
loanData <- loanData[which(loanData$Monthly.Income < 50000), ]

# Info and plots
summary(loanData$Interest.Rate)
quantile(loanData$Interest.Rate)


# Set plot display to 1 row and 3 graphs
par(mfrow = c(1, 2))

# Plot interest rate and check for normal distribution
boxplot(loanData$Interest.Rate, col = "yellow", xlab = "Interest Rate", main = "Boxplot")
hist(loanData$Interest.Rate, col = "blue", xlab = "Interest Rate", main = "Histogram")
qqnorm(loanData$Interest.Rate)
qqline(loanData$Interest.Rate, col = "red", lwd = 1.5)

# Boxplot using 2 variables
par(mfrow = c(1, 3))
boxplot(loanData$Interest.Rate ~ loanData$Loan.Length, col = "orange", varwidth = TRUE, xlab = "Loan Length")
boxplot(loanData$Interest.Rate ~ loanData$Loan.Purpose, col = "green", varwidth = TRUE, xlab = "Loan Purpose", , ylab = "Interest Rate")
boxplot(loanData$Interest.Rate ~ loanData$Home.Ownership, col = "orange", varwidth = TRUE, xlab = "Home Ownership", ylab = "Interest Rate")

par(mfrow = c(1, 3))
boxplot(loanData$Interest.Rate ~ loanData$Employment.Length, col = "blue", varwidth = TRUE, xlab = "Employment Length", ylab = "Interest Rate")
boxplot(loanData$Interest.Rate ~ loanData$Inquiries.in.the.Last.6.Months, xlab = "# of Inquiries in last 6 months",col = "red", varwidth = TRUE, ylab = "Interest Rate")
boxplot(loanData$Interest.Rate ~ loanData$Open.CREDIT.Lines, col = "purple", varwidth = TRUE, xlab = "# of Open Credit Lines", ylab = "Interest Rate")

# Interest rate and Mean FICO score
par(mfrow = c(1,1))
boxplot(loanData$Interest.Rate ~ loanData$FICO.Mean, col = "blue", varwidth = TRUE, xlab = "Mean FICO Score", ylab = "Interest Rate")

# Interest rate and FICO Range
par(mfrow = c(1,1))
boxplot(loanData$Interest.Rate ~ loanData$FICO.Mean, col = "yellow", varwidth = TRUE, xlab = "FICO Range", ylab = "Interest Rate")

# Interest rate and State
par(mfrow = c(1,1))
boxplot(loanData$Interest.Rate ~ loanData$State, col = "green", xlab = "State", ylab = "Interest Rate")

## Logistic Regression
# What is the probability of getting a loan of $x1, from the lending club at 10 % with a FICO score of x2 and monthly income of $x3
# Simple cutoff: Prob > 0.50 -> Get lone, otherwise no loan!
# Interest.Rate = a0 + a1 * FICO.Mean + a2 * Amount.Requested + a3 * Monthly.Income

# First add an indicator variable which indicates whether interest rate is <= 10
loanData$Indicator <- loanData$Interest.Rate <= 10
head(loanData)
inspect.true <- loanData[loanData$Interest.Rate <=10,] # test to see if interest rate <= true?
inspect.false <- loanData[!loanData$Interest.Rate <= 10,] # test to see if interest rate <= false?
summary(loanData)
sapply(loanData, sd)

# Fit a logit model using glm
logitModel <- glm(Indicator ~ FICO.Mean + Amount.Requested + Monthly.Income, data = loanData, family = "binomial")
summary(logitModel)
confint(logitModel)
par(mfrow = c(2, 2))
plot(logitModel)

# Odds ratio
exp(coef(logitModel))

# Odds ratio and 95% CI
exp(cbind(OR = coef(logitModel), confint(logitModel)))


#########################################
test <- loanData
predict(logitModel, type = "response")

boxplot(predict(logitModel, type = "response") ~ loanData$Indicator, col = "blue", varwidth = TRUE)
boxplot(predict(logitModel) ~ loanData$Indicator, col = "blue")

logitModel$fitted


# Choosing a cutoff(re-substitution)
xx <- seq(0, 1, length = 20)
err <- rep(NA, 20)

for (i in 1:length(xx)){
        err[i] <- sum((predict(logitModel, type = "response") > xx[i]) != loanData$Indicator)
}

plot(xx, err, pch = 19, col = "red", xlab = "Cutoff", ylab = "Error")

# This gives Cutoff to be approximately 0.4


## Testing how model performed
inspect.true$Indicator <- NULL
inspect.true$Indicator <- predict(logitModel, newdata = inspect.true, type = "response")
for (i in 1:nrow(inspect.true)){
        if (inspect.true$Indicator[i] >= 0.4){
                inspect.true$PredictedIndicator[i] = 1
        }
        else{
                inspect.true$PredictedIndicator[i] = 0
        }
}

table(inspect.true$PredictedIndicator)


inspect.false$Indicator <- NULL
inspect.false$Indicator <- predict(logitModel, newdata = inspect.false, type = "response")
for (i in 1:nrow(inspect.false)){
        if (inspect.false$Indicator[i] >= 0.4){
                inspect.false$PredictedIndicator[i] = 1
        }
        else{
                inspect.false$PredictedIndicator[i] = 0
        }
}

table(inspect.false$PredictedIndicator)


logitInverse <- function(x){1 / (1 + exp(-x))}
logitInverse(predict(logitModel, type = "response"))

# Model Performance
Performance <- predict(logitModel, type = "response") > 0.4
table(loanData$Indicator, Performance)


summary(logitModel)

## Testing
FICO.Mean <- 750
Amount.Requested <- 15000
Monthly.Income <- 6500

missX <- data.frame(FICO.Mean = 750, Amount.Requested = 15000, Monthly.Income = 6500)
predict(logitModel, newdata = missX, type = "response")


test <- logitModel <- glm(Indicator ~ FICO.Mean + Amount.Requested, data = loanData, family = "binomial")
summary(test)

FICO.Mean <- 720
Amount.Requested <- 10000
temp <- data.frame(FICO.Mean, Amount.Requested)
logitInverse(predict(test, newdata = temp, type = "response"))



