library(mice)
library(party)
library(tidyverse)
#library(MASS)
library(VIM)
library(pls)
library(glmnet)
library(caret)
library(earth)
library(car)
library(partykit)
Train<-read.csv("C:/Users/Tushar/Downloads/Train.csv/Train.csv",na.strings = c("","NA"))
Test<-read.csv("C:/Users/Tushar/Downloads/Train.csv/Test.csv",na.strings = c("","NA"))
TrainNumeric <- Train %>%
  select_if(is.numeric)

#TrainDiscrete <- Train %>%
#transmute_if(is.character,as_factor)%>%
#select(-date)
TrainDiscrete <- Train[, sapply(Train, function(x) !is.numeric(x))]%>%
  #select_if(~ !is.numeric(.) && . != "date")
  select(-date)
#select_if(is.character)



#For Test Data
TestNumeric <- Test %>%
  select_if(is.numeric)

TestDiscrete <- Test[, sapply(Test, function(x) !is.numeric(x))]%>%
  #select_if(~ !is.numeric(.) && . != "date")
  select(-date)

#TestDiscrete <- Test %>%
#transmute_if(is.character, as_factor)
#select_if(is.character)

glimpse(TrainNumeric)
glimpse(TrainDiscrete)

Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}
Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}
Q0<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[1]
}
Q4<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[5]
}

TrainNumericSummary <- function(x){
  c(length(x), n_distinct(x),((n_distinct(x)/length(x))*100), sum(is.na(x)),((sum(is.na(x))/length(x))*100), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}

TrainNumericTableSummary <- TrainNumeric %>%
  summarize(across(everything(), TrainNumericSummary))
#test
TestNumericTableSummary <- TestNumeric %>%
  summarize(across(everything(), TrainNumericSummary))

# View the structure of 'numericSummary'
#glimpse(numericSummary)
TrainNumericTableSummary <-cbind(
  stat=c("n","unique","Unique_percentage","missing","missing_Percentage", "mean","min","Q1","median","Q3","max","sd"),
  TrainNumericTableSummary)
#test
TestNumericTableSummary <-cbind(
  stat=c("n","unique","Unique_percentage","missing","missing_Percentage", "mean","min","Q1","median","Q3","max","sd"),
  TrainNumericTableSummary)

#Final Numeric Summary
TrainNumericSummaryFinal <- TrainNumericTableSummary %>%
  pivot_longer("sessionId":"revenue", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)%>% 
  #mutate(missing_pct = 100*missing/n,
  #unique_pct = 100*unique/n) %>%
  select(variable, n, missing,  unique, everything())

#test
TestNumericSummaryFinal <- TestNumericTableSummary %>%
  pivot_longer("sessionId":"revenue", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)%>% 
  #mutate(missing_pct = 100*missing/n,
  #unique_pct = 100*unique/n) %>%
  select(variable, n, missing,  unique, everything())

library(knitr)
options(digits=3)
options(scipen=99)
TrainNumericSummaryFinal %>% kable()
TestNumericSummaryFinal %>% kable()

#Data Report for Non-Numeric Table
#This function will work for Every column
getmodes <- function(v,type=1) {
  if(sum(is.na(v))==length(v)){
    return(NA)
  }
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}

getmodesCnt <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl)) #1st mode freq
  }
  else if (type==2) {
    return (max(tbl[-m1])) #2nd mode freq
  }
  else if (type==-1) {
    return (min(tbl)) #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}

# This function will run the get modes individually for every column and display
getmodes_df <- function(df, type = 1) {
  modes_list <- list()  # Create an empty list to store modes for each column
  
  for (col in colnames(df)) {
    modes_list[[col]] <- getmodes(df[[col]], type)  # Apply getmodes to each column
  }
  
  return(modes_list)
}

#getmodes_df(TrainDiscrete,type=1)
#getmodes_df(TrainDiscrete,type=2)
#getmodes_df(TrainDiscrete,type=-1)

TrainDiscreteSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)),  getmodes(x, type=1), getmodesCnt(x, type =1),
    getmodes(x, type=2), getmodesCnt(x, type =2), getmodes(x, type= -1), getmodesCnt(x, type = -1))
}

result1 <- lapply(TrainDiscrete, TrainDiscreteSummary)
result_matrix <- do.call(cbind, result1)

# Convert the matrix into a dataframe
TrainDiscreteTableSummary <- as.data.frame(result_matrix)

#test
result2 <- lapply(TestDiscrete, TrainDiscreteSummary)
result_matrix <- do.call(cbind, result2)

# Convert the matrix into a dataframe
TestDiscreteTableSummary <- as.data.frame(result_matrix)

# Assign the first vector as column names
#colnames(result_df) <- result_df[1, ]
#result_df <- as.data.frame(do.call(cbind, result1))

#TrainDiscreteTableSummary <- TrainDiscrete %>%
#summarize(across(everything(), TrainDiscreteSummary))



TrainDiscreteTableSummary <-cbind(
  stat=c("n","unique","missing","1st mode", "first_mode_freq", "2nd mode", "second_mode_freq", "least common", "least common freq"),
  TrainDiscreteTableSummary)
#glimpse(DiscreteFactorSummary)
#test
TestDiscreteTableSummary <-cbind(
  stat=c("n","unique","missing","1st mode", "first_mode_freq", "2nd mode", "second_mode_freq", "least common", "least common freq"),
  TrainDiscreteTableSummary)

DiscreteFactorSummaryFinal <- TrainDiscreteTableSummary %>%
  pivot_longer("channelGrouping":"adwordsClickInfo.isVideoAd", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(across(c(2,3,4,6,8,10), as.double), missing_pct = 100*missing/n,
         unique_pct = 100*unique/n, freq_ratio = as.numeric(first_mode_freq) / as.numeric(second_mode_freq))%>%
  select(variable, n, missing, missing_pct, unique, unique_pct, freq_ratio, everything())

#test
TestDiscreteFactorSummaryFinal <- TestDiscreteTableSummary %>%
  pivot_longer("channelGrouping":"adwordsClickInfo.isVideoAd", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(across(c(2,3,4,6,8,10), as.double), missing_pct = 100*missing/n,
         unique_pct = 100*unique/n, freq_ratio = as.numeric(first_mode_freq) / as.numeric(second_mode_freq))%>%
  select(variable, n, missing, missing_pct, unique, unique_pct, freq_ratio, everything())

library(knitr)
options(digits=3)
options(scipen=99)
#DiscreteFactorSummaryFinal %>% kable()


#beginning of Data Preparation and Preprocessing
#Since we have to do preprocessing for both Test Data and Training Data we'll create functions

ggplot(data = DiscreteFactorSummaryFinal,mapping = ( aes(x = variable, y =missing_pct, fill=missing_pct>80 ))) +
  geom_bar(stat="identity") +
  labs(
    title = "Missing Value Percentage by Column",
    x = "Column",
    y = "Missing Percentage"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

ggplot(data = TrainNumericSummaryFinal,mapping = ( aes(x = variable, y =missing_Percentage, fill=missing_Percentage>75 ))) +
  geom_bar(stat="identity") +
  labs(
    title = "Missing Value Percentage by Column",
    x = "Column",
    y = "Missing Percentage"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability



#removing columns with more than 80% missing values
columns_to_remove <- c("adContent", "adwordsClickInfo.adNetworkType", "adwordsClickInfo.gclId","adwordsClickInfo.isVideoAd","campaign","adwordsClickInfo.slot","adwordsClickInfo.page","keyword")
Train_preprocess <- Train %>%
  select(-one_of(columns_to_remove))

#Removing columns for test
Test_preprocess <- Test %>%
  select(-one_of(columns_to_remove))


#impfun<- function(df,col_name, target_colname){
#housingPMM.imp <- housingData
# missing <- is.na(data[[col_name]])
#housingPMM.imp[missing, col_name] <- mice.impute.pmm(col_name, !missing, target_colname)
#}
#preprocessing Steps 2 Imputung the numeric columns with mean
#Imputed_column <- impfun(Train,Train$bounces,Train$sessionId)
Train_preprocess.imp<-Train_preprocess
columns_to_impute <- c("bounces", "newvisits", "pageviews","newVisits")

for (col_name in columns_to_impute) {
  missing <- is.na(Train_preprocess[[col_name]])
  if (sum(missing) > 0) {
    Train_preprocess.imp[missing, col_name] <- mice.impute.pmm(
      Train_preprocess.imp[[col_name]], 
      !missing, 
      Train_preprocess.imp$custId
    )
  }
}

#test
Test_preprocess.imp<-Test_preprocess
columns_to_impute <- c("bounces", "newvisits", "pageviews","newVisits")

for (col_name in columns_to_impute) {
  missing <- is.na(Test_preprocess[[col_name]])
  if (sum(missing) > 0) {
    Test_preprocess.imp[missing, col_name] <- mice.impute.pmm(
      Test_preprocess.imp[[col_name]], 
      !missing, 
      Test_preprocess.imp$custId
    )
  }
}

#install.packages("party")
library(party)


#so We'll first impute columns with missing values percent of less than 10% by replacing them with mode of the column But 
#if missing values is greater than 7000, that is roughly 10% of the Total column then we'll replace NA values with Other
# and create a seperate level of category for the misiing values
Train_preprocess.imp1<- Train_preprocess.imp
col_names<- names(Train_preprocess.imp1)
for (col in col_names)
{
  #print(Train_preprocess.imp1[[col]])
  missing <- is.na(Train_preprocess.imp1[[col]])
  if(is.character(Train_preprocess.imp1[[col]])){
    if (sum(missing) < 7000 & sum(missing) > 0) {
      Train_preprocess.imp1[[col]][is.na(Train_preprocess.imp1[[col]])] <- getmodes(Train_preprocess.imp1[[col]])
    }
    else{
      Train_preprocess.imp1[[col]][is.na(Train_preprocess.imp1[[col]])] <- "Other"
      
    }
  }
}
#printing the number of missing values in the entire table
total_missing_values<-sum(is.na(Train_preprocess.imp))

#test
Test_preprocess.imp1<- Test_preprocess.imp
col_names<- names(Test_preprocess.imp1)
for (col in col_names)
{
  #print(Train_preprocess.imp1[[col]])
  missing <- is.na(Test_preprocess.imp1[[col]])
  if(is.character(Test_preprocess.imp1[[col]])){
    if (sum(missing) < 7000 & sum(missing) > 0) {
      Test_preprocess.imp1[[col]][is.na(Test_preprocess.imp1[[col]])] <- getmodes(Test_preprocess.imp1[[col]])
    }
    else{
      Test_preprocess.imp1[[col]][is.na(Test_preprocess.imp1[[col]])] <- "Other"
      
    }
  }
}
#printing the number of missing values in the entire table
total_missing_values<-sum(is.na(Test_preprocess.imp))

#Preprocessing step 3:Converting all characters to Factor Variables
char_vars <- sapply(Train_preprocess.imp1, is.character)

Train_preprocess.imp1[char_vars] <- lapply(Train_preprocess.imp1[char_vars], as.factor)

#test
char_vars <- sapply(Test_preprocess.imp1, is.character)

Test_preprocess.imp1[char_vars] <- lapply(Test_preprocess.imp1[char_vars], as.factor)

#preprocessing step 4:Removing Outliers


#performing PCA
#Hpca <-prcomp(TrainNumeric,scale. = TRUE)

#Hpca

#Plottinh LDA
#lda_result <- lda(revenue ~ ., data = Train_preprocess.imp1)
# Print the LDA results
#lda_result

#<-Glass[, sapply(Glass, is.numeric)]
corMat <- cor(TrainNumeric)
corMat
#SessionID and cust Id have high correlation so we cannot use them together to avoid multicollinearity

#Applying Log Transformation on Revenue
Train_predicted<-Train_preprocess.imp1 %>%
  group_by(custId)%>%
  summarize(revenue=log(sum(revenue+1)))%>%
  as_tibble()

#Grouping the entire table on the basis of  Cust ID
TrainGroupedData<-Train_preprocess.imp1 %>%
  group_by(custId)%>%
  summarize(visitNumber=max(visitNumber),timeSinceLastVisit=mean(timeSinceLastVisit),continent=getmodes(continent),
            subContinent=getmodes(subContinent),country=getmodes(country),region=getmodes(region),city=getmodes(city),
            metro=getmodes(metro),channelGrouping=getmodes(channelGrouping),visitStartTime=min(visitStartTime),
            browser=getmodes(browser), operatingSystem=getmodes(operatingSystem),isMobile=getmodes(isMobile),
            deviceCategory=getmodes(deviceCategory),networkDomain=getmodes(networkDomain),topLevelDomain=getmodes(topLevelDomain),
            source=getmodes(source),medium=getmodes(medium),isTrueDirect=getmodes(isTrueDirect),
            referralPath=getmodes(referralPath),pageviews=sum(pageviews),bounces=getmodes(bounces),newVisits=getmodes(newVisits),
            revenue=log(sum(revenue+1)))%>%
  as_tibble()
char_vars <- sapply(TrainGroupedData, is.character)

TrainGroupedData[char_vars] <- lapply(TrainGroupedData[char_vars], as.factor)

#test
TestGroupedData<-Test_preprocess.imp1 %>%
  group_by(custId)%>%
  summarize(visitNumber=max(visitNumber),timeSinceLastVisit=mean(timeSinceLastVisit),continent=getmodes(continent),
            subContinent=getmodes(subContinent),country=getmodes(country),region=getmodes(region),city=getmodes(city),
            metro=getmodes(metro),channelGrouping=getmodes(channelGrouping),visitStartTime=min(visitStartTime),
            browser=getmodes(browser), operatingSystem=getmodes(operatingSystem),isMobile=getmodes(isMobile),
            deviceCategory=getmodes(deviceCategory),networkDomain=getmodes(networkDomain),topLevelDomain=getmodes(topLevelDomain),
            source=getmodes(source),medium=getmodes(medium),isTrueDirect=getmodes(isTrueDirect),
            referralPath=getmodes(referralPath),pageviews=sum(pageviews),bounces=getmodes(bounces),newVisits=getmodes(newVisits))%>%
  as_tibble()
char_vars <- sapply(TestGroupedData, is.character)

TestGroupedData[char_vars] <- lapply(TestGroupedData[char_vars], as.factor)
#Train_preprocess.impM<- as.matrix(Train_preprocess.imp1)
ctrl <- trainControl(
  method = "cv",       # Cross-validation method ("cv" for k-fold cross-validation)
  number = 5,          # Number of folds (5 for 5-fold cross-validation)
  verboseIter = TRUE,  # Display progress
  summaryFunction = defaultSummary  # Use default summary function
)

# Fit your model with 5-fold cross-validation
OlsCVmodel <- train(
  revenue ~ visitNumber+timeSinceLastVisit+pageviews+channelGrouping,     # Specify the formula (dependent variable ~ predictor variables)
  data = TrainGroupedData,   # Specify your dataset
  method = "lm",      # Specify the modeling method (e.g., linear regression)
  trControl = ctrl,
  metric="RMSE"# Use the training control settings created earlier
)
cvsummary<-summary(OlsCVmodel)
cvsummary

CV_RMSE <- OlsCVmodel$results$RMSE
CV_R2<- cvsummary$r.squared
cat("CVRMSE:", CV_RMSE , "\n")

cat("CV R-squared:",CV_R2  , "\n")

#PLS
ctrl <- trainControl(
  method = "cv",         # Cross-validation method (e.g., k-fold)
  number = 5,           # Number of folds
  savePredictions = TRUE, # Save predictions for final model
  
)

# Perform hyperparameter tuning with cross-validation
set.seed(123)  # For reproducibility
ncompnum<-(seq(1,4,1))
pls_model <- train(
  revenue ~ visitNumber+timeSinceLastVisit+pageviews+channelGrouping,     # Specify the formula (dependent variable ~ predictor variables)
  data = TrainGroupedData,                          # Your response variable
  method = "pls",              # Machine learning method (Ridge Regression)
  trControl = ctrl,               # Cross-validation control
  #ncomp=5,
  tuneGrid = expand.grid(ncomp = ncompnum),
  metric="RMSE", 
  preProc=c("center","scale"))

pls_model$results
summary(pls_model)



#Creating Lasso model
lambda_val=seq(0,1,0.1)
ctrl <- trainControl(
  method = "cv",         # Cross-validation method (e.g., k-fold)
  number = 10,           # Number of folds
  savePredictions = TRUE, # Save predictions for final model
  
)

# Perform hyperparameter tuning with cross-validation
set.seed(123)  # For reproducibility
Lasso_model <- train(
  revenue ~ visitNumber+timeSinceLastVisit+pageviews+channelGrouping,     # Specify the formula (dependent variable ~ predictor variables)
  data = TrainGroupedData,                          # Your response variable
  method = "glmnet",              # Machine learning method (Ridge Regression)
  trControl = ctrl,               # Cross-validation control
  tuneGrid = expand.grid(lambda=lambda_val, alpha=1),
  metric="RMSE",
  preProc=c("center","scale")
)

Lasso_model$results

#Ridge
lambda_val=seq(0,1,0.1)
ctrl <- trainControl(
  method = "cv",         # Cross-validation method (e.g., k-fold)
  number = 10,           # Number of folds
  savePredictions = TRUE, # Save predictions for final model
  repeats=5
)

# Perform hyperparameter tuning with cross-validation
set.seed(123)  # For reproducibility
ridge_model <- train(
  revenue ~ visitNumber+timeSinceLastVisit+pageviews+channelGrouping,     # Specify the formula (dependent variable ~ predictor variables)
  data = TrainGroupedData,                          # Your response variable
  method = "glmnet",              # Machine learning method (Ridge Regression)
  trControl = ctrl,               # Cross-validation control
  tuneGrid = expand.grid(lambda=lambda_val, alpha=0),
  metric="RMSE",
  preProc=c("center","scale"))

summary(ridge_model)
ridge_model$results

#pcrmodel
ctrl <- trainControl(
  method = "cv",         # Cross-validation method (e.g., k-fold)
  number = 5,           # Number of folds
  savePredictions = TRUE,  # Save predictions for final model
  summaryFunction = defaultSummary 
)

# Perform hyperparameter tuning with cross-validation
set.seed(123)  # For reproducibility
pcr_model <- train(
  revenue ~ visitNumber+timeSinceLastVisit+pageviews+channelGrouping,     # Specify the formula (dependent variable ~ predictor variables)
  data = TrainGroupedData,                          # Your response variable
  method = "pcr",                 # Machine learning method (PCR)
  trControl = ctrl,               # Cross-validation control
  tuneLength = 3,                 # Number of principal components to consider
  preProc=c("center","scale")
  
)
pcr_model$results

#Marsmodel
ctrl <- trainControl(
  method = "LOOCV",              # Cross-validation method (e.g., k-fold)
  number = 10,                # Number of folds
  #savePredictions = "final" ,  # Save predictions for final model
  #summaryFunction = twoClassSummary,
  
)

set.seed(123)  # For reproducibility
mars_model <- train(
  revenue ~ visitNumber+timeSinceLastVisit+pageviews+channelGrouping,     # Specify the formula (dependent variable ~ predictor variables)
  data = TrainGroupedData,                      # Your response variable
  method = "earth",           # Machine learning method (MARS)
  trControl = ctrl,           # Cross-validation control
  #metric="RMSE",
  preProc=c("center","scale"),
  tuneGrid = expand.grid(degree=1:4,nprune=10:30)
)
mars_model
summary(mars_model)
marspredictions <- predict(mars_model, newdata = TestGroupedData)
summary(marspredictions)

TestGroupedData1<- TestGroupedData %>%
  mutate(predRevenue=marspredictions)

FinalPredictions<- TestGroupedData1 %>%
  select(custId, predRevenue)

write.csv(FinalPredictions, file = "fileprediction.csv", row.names = FALSE)


