
library(ggplot2)
library(gridExtra)
library(dplyr)
library(ggthemes)
library(numform)
library(treemapify)
library(timeDate)
library(lubridate)
library(reshape2)
library(ca)
library(skimr)
library(janitor)
library(flextable)
library(shiny)
library(leaflet)
library(maps)
library(RColorBrewer)
library(scales)
library(readr)
library(forecast)
library(caret)
library(knitr)
library(ROCR)
library(ROCit)
library(nortest)
library(pROC)
library(glmnet)
library(car)
library(nnet)
library(rpart)
library(rpart.plot)
library(pander)
library(e1071)
library(randomForest)
library(fpc)
project_theme <- theme(
  panel.background = element_rect(fill = "#FFFBDC"),  # Light yellow background
  panel.grid.major = element_line(color = "#FFE4A1"), # Light orange major grid lines
  panel.grid.minor = element_blank(), # Remove minor grid lines
  plot.title = element_text(size = 18, hjust = 0.5, color = "darkblue"),  # Title color and size
  axis.title = element_text(size = 16, color = "darkblue"),  # Axis title color and size
  axis.text = element_text(size = 14, color = "black"),   # Axis text color and size
  legend.title = element_text(size = 16, color = "darkblue"), # Legend title color and size
  legend.text = element_text(size = 14, color = "black"),   # Legend text color and size
  legend.background = element_rect(fill = "#FFFBDC"),  # Legend background color
  plot.background = element_rect(fill = "#FFFBDC")   # Background color of the entire plot
)
data_path <- 'Global YouTube Statistics.csv'
raw_data <- read.csv(data_path,encoding = "UTF-8")
# Part3 - Data Cleaning and Preparation
## 3.1 Data Cleaning
### 3.1.1 Check column names and make adjustments
raw_data <- janitor::clean_names(raw_data)
raw_data <- raw_data %>%
  rename(
    #More descriptive names
    video_views_last_30_days = video_views_for_the_last_30_days,
    country_abbr = abbreviation,
    #Shorter names
    monthly_earnings_low = lowest_monthly_earnings,
    monthly_earning_high = highest_monthly_earnings,
    yearly_earning_low = lowest_yearly_earnings,
    yearly_earning_high = highest_yearly_earnings,
    subs_last_30_days = subscribers_for_last_30_days,
    tertiary_edu_enrollment = gross_tertiary_education_enrollment
  )

raw_data[raw_data == 'nan' | raw_data == "NaN" | raw_data == 0] <- NA
#Numeric columns
raw_data <- raw_data |>
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))


change_to_median_cols <- c("video_views",
"uploads","video_views_rank","country_rank","channel_type_rank","video_views_last_30_days","monthly_earnings_low",	"monthly_earning_high","yearly_earning_low",	 "yearly_earning_high","subs_last_30_days","tertiary_edu_enrollment","population","unemployment_rate","urban_population")



for (col in change_to_median_cols) {
    median_val <- median(raw_data[[col]], na.rm = TRUE)
    raw_data[[col]][is.na(raw_data[[col]])] <- median_val
}
rm(col,median_val,change_to_median_cols)

character_columns <- c("category", "country","country_abbr","channel_type")
  
for (col in character_columns) {
  raw_data[[col]][is.na(raw_data[[col]])] <- "Unknown"
}
rm(col,character_columns)

raw_data <- raw_data %>%
  filter(!is.na(created_year) & !is.na(created_month) & !is.na(created_date) & created_year >= 2005)

raw_data_unique <- unique(raw_data)
raw_data <- raw_data_unique
rm(raw_data_unique)

numeric_vars <- raw_data[sapply(raw_data,is.numeric)]
categorical_vars <- raw_data[sapply(raw_data,function(x) class(x) %in% c('factor','character'))]
numeric_vars_name <- names(numeric_vars)
categorical_vars_name <- names(categorical_vars)

# Define a function to detect outliers in the dataset
detect_outliers <- function(data, method = "zscore", threshold = 3) {
  # Initialize an empty dataframe to store detected outliers
  outliers <- data.frame()
  # Loop over each column of the dataset
  for (col in colnames(data)) {
    # Check if the column is numeric as outliers can be detected only in numeric columns
    if (is.numeric(data[[col]])) {
      # If method is zscore, then use the z-score method to detect outliers
      if (method == "zscore") {
        z <- (data[[col]] - mean(data[[col]])) / sd(data[[col]])
        # If the z-score value exceeds the threshold, consider it as an outlier
        outliers <- rbind(outliers, data[abs(z) > threshold, ])
      # Else, if method is iqr, then use the Interquartile Range (IQR) method to detect outliers
      } else if (method == "iqr") {
        Q1 <- quantile(data[[col]], 0.25)
        Q3 <- quantile(data[[col]], 0.75)
        IQR <- Q3 - Q1
        # If the value is outside the range defined by Q1 - 1.5*IQR and Q3 + 1.5*IQR, consider it as an outlier
        outliers <- rbind(outliers, data[data[[col]] < (Q1 - 1.5 * IQR) | data[[col]] > (Q3 + 1.5 * IQR), ])
      }
    }
  }
  return(outliers)
}
# Use the detect_outliers function to detect outliers in columns 1 to 18 of the Youtube_cleaned dataset
find_outliers <- detect_outliers(raw_data[numeric_vars_name])
# Print the detected outliers


#### 3.1.5.3 Winsorize outliers

# Function to truncate values in specified columns that exceed a given percentile.
truncate_tail <- function(data, column_names, percentile = 95) {
  
  # For each column, replace values above the specified percentile with the value at that percentile.
  for (col in column_names) {
    q <- quantile(data[[col]], probs = percentile / 100, na.rm = TRUE)
    data[complete.cases(data) & data[[col]] > q, col] <- q
  }
  
  return(data)
}

# Apply the function to the first 18 columns of Youtube_cleaned.
raw_data <- truncate_tail(raw_data, numeric_vars_name, percentile = 95)


# Function to compute z-scores and detect outliers based on a specified threshold
detect_outliers_zscore <- function(data, column_names, threshold = 3) {
  outliers_count <- sapply(column_names, function(col_name) {
    # Calculate z-scores for each specified column
    z_scores <- (data[[col_name]] - mean(data[[col_name]], na.rm = TRUE)) / sd(data[[col_name]], na.rm = TRUE)
    # Count the number of absolute z-scores that exceed the provided threshold
    sum(abs(z_scores) > threshold, na.rm = TRUE)
  })
  return(outliers_count)
}

# Detect outliers in the data after truncation using the function above
outliers_count_after_truncate <- detect_outliers_zscore(raw_data, numeric_vars_name)
rm(outliers_count_after_truncate,numeric_vars,categorical_vars)

factor_columns <- c("category", "country","country_abbr", "channel_type")
#Use mutate adjust factor_columns, change then to factor type
raw_data <- raw_data %>%
  mutate(across(all_of(factor_columns), as.factor))
rm(factor_columns)

youtube_data <- raw_data
clustering_data <- raw_data

### 3.1.8 Add new columns
youtube_data$created_date <- as.integer(as.character(raw_data$created_date))

# Check for any non-integer values in 'created_date'
if (any(!is.finite(youtube_data$created_date))) {
  stop("The 'created_date' column contains non-integer or NA values.")
}

# Define a vector of month names
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Create a vector of two-digit month numbers using sprintf
month_numbers <- sprintf("%02d", 1:12)

# Create a mapping between month names and month numbers
month_mapping <- setNames(month_numbers, month_names)

# Update the 'full_date' column by combining year, month, and day
youtube_data$full_time <- as.Date(paste(raw_data$created_year, 
                                        raw_data$created_month %>% match(month_names) %>% month_mapping[.],
                                        sprintf("%02d", youtube_data$created_date), 
                                        sep = "-"), 
                                  format = "%Y-%m-%d")
rm(month_mapping, month_names, month_numbers)
youtube_data$subs_engagement <- youtube_data$video_views/youtube_data$subscribers

youtube_data$uploads_frequency <- youtube_data$uploads/(2023-youtube_data$created_year)

youtube_data$average_views_per_video <- youtube_data$video_views/youtube_data$uploads

youtube_data$urban_rate <- youtube_data$urban_population/youtube_data$population


youtube_data$age <- 2022-youtube_data$created_year

youtube_data$edu_ratio <- youtube_data$tertiary_edu_enrollment/youtube_data$population



youtube_data$top_channel_flag <- as.numeric(youtube_data$channel_type_rank <1000)


youtube_data$video_views_increasing_rate <- (youtube_data$video_views_last_30_days/youtube_data$video_views)


youtube_data$monthly_earning <-( youtube_data$monthly_earning_high+youtube_data$monthly_earnings_low)/2
youtube_data$yearly_earning <-( youtube_data$yearly_earning_low+youtube_data$yearly_earning_high)/2



detect_unusual_rows <- function(data) {
  unusual_rows <- data[data$monthly_earning > data$yearly_earning, c("monthly_earning", "yearly_earning","monthly_earnings_low","monthly_earning_high","yearly_earning_low","yearly_earning_high")]
  print(unusual_rows)
  cat("The number of unreasonable rows are", nrow(unusual_rows), "rows\n")
}
detect_unusual_rows(youtube_data)



youtube_data <- youtube_data[youtube_data$monthly_earning <= youtube_data$yearly_earning, ]


## 3.2 Data Preparation

### 3.2.1 Object and features
gdp_data_path <- "API_NY.GDP.PCAP.CD_DS2_en_csv_v2_5871588.csv"
gdp_data <- read_csv(gdp_data_path, skip = 4, show_col_types = FALSE) %>% 
  select(-c("Country Code", "Indicator Name", "Indicator Code","...68"))

colnames(gdp_data)[colnames(gdp_data) == "Country Name"] <- "Country.Name"

gdp_data <- as.data.frame(gdp_data)

gdp_data <- gdp_data %>%
  mutate(gdp_per_capita = `2022`)


(diff_countries = setdiff(youtube_data$country, gdp_data$Country.Name))
gdp_data$Country.Name[gdp_data$Country.Name == "Egypt, Arab Rep."] <- "Egypt"
gdp_data$Country.Name[gdp_data$Country.Name == "Korea, Rep."] <- "South Korea"
gdp_data$Country.Name[gdp_data$Country.Name == "Russian Federation"] <- "Russia"
gdp_data$Country.Name[gdp_data$Country.Name == "Turkiye"] <- "Turkey"
gdp_data$Country.Name[gdp_data$Country.Name == "Venezuela, RB"] <- "Venezuela"


replace_na_with_global_median <- function(data) {
  years <- colnames(data)[-1]  
  global_median_gdp <- numeric(length(years))
  
  # change GDP variable into numeric
  for (i in 2:ncol(data)) {
    data[, i] <- as.numeric(data[, i])
  }
  
  # Calculate global median GDP for each year
  for (i in 1:length(years)) {
    global_median_gdp[i] <- median(data[!is.na(data[, years[i]]), years[i]], na.rm = TRUE)
  }
  
  data[, years] <- t(apply(data[, years], 1, function(row) {
    is_na <- is.na(row)
    row[is_na] <- global_median_gdp[is_na]
    return(row)
  }))
  
  return(data)
}


gdp_data <- replace_na_with_global_median(gdp_data)





# Divide the per capital GDP value in those years of channel age by the channel age in these years
calculate_channel_age_adjusted_income <- function(youtube_data, gdp_data) {
  
  adjusted_income <- numeric(nrow(youtube_data))
  
  # Iterate through each row of youtube_data
  for (i in 1:nrow(youtube_data)) {
    
    country <- as.character(youtube_data$country[i])
    created_year <- as.numeric(as.character(youtube_data$created_year[i]))  
    
    # Find the corresponding country row in gdp_data
    gdp_row <- gdp_data[gdp_data$`Country.Name` == country, ]
    created_year_column <- which(names(gdp_row) == created_year)
    
    if (nrow(gdp_row) == 0) {
      adjusted_income[i] <- NA
    } else {
      gdp_values <- as.numeric(gdp_row[ , (created_year_column + 1):64])
      
      value1 <- sum(gdp_values, na.rm = TRUE)
      
      value2 <- 1 / youtube_data$age[i]
      
      adjusted_income[i] <- youtube_data$yearly_earning[i] / value2
    }
  }
  
  youtube_data$channel_age_adjusted_income <- adjusted_income
  
  return(youtube_data)
}


youtube_data <- calculate_channel_age_adjusted_income(youtube_data, gdp_data)


median_value <- median(youtube_data$channel_age_adjusted_income, na.rm = TRUE)


for (i in 1:length(youtube_data$channel_age_adjusted_income)) {
  if (is.na(youtube_data$channel_age_adjusted_income[i])) {
    youtube_data$channel_age_adjusted_income[i] <- median_value
  }
}


gdp_2022 <- gdp_data[,c("Country.Name","gdp_per_capita")]

youtube_data <- merge(youtube_data,gdp_2022, by.x = "country",by.y = "Country.Name", all.x = TRUE)
rm(gdp_2022)


median_gdp <- median(youtube_data$gdp_per_capita,na.rm = TRUE)
youtube_data$gdp_per_capita[is.na(youtube_data$gdp_per_capita)] <- median_gdp
rm(median_gdp)






youtube_data$normalized_earning <- as.numeric(youtube_data$yearly_earning/youtube_data$gdp_per_capita)


summary(youtube_data$normalized_earning)


#Kernel Density Estimation
plot(density(youtube_data$normalized_earning), main="Kernel Density Estimation of Normalized Earnings", xlab="Normalized Earnings")

# Shapiro-Wilk test
shapiro_test <- shapiro.test(youtube_data$normalized_earning)
print(shapiro_test)
# Kolmogorov-Smirnov test
ks_test <- ks.test(youtube_data$normalized_earning, "pnorm", mean(youtube_data$normalized_earning), sd(youtube_data$normalized_earning))
print(ks_test)
# Anderson-Darling test
ad_test <- ad.test(youtube_data$normalized_earning)
print(ad_test)








summary(youtube_data$normalized_earning)
# Shapiro-Wilk test
shapiro_test <- shapiro.test(youtube_data$normalized_earning)
print(shapiro_test)

# Kolmogorov-Smirnov test
ks_test <- ks.test(youtube_data$normalized_earning, "pnorm", mean(youtube_data$normalized_earning), sd(youtube_data$normalized_earning))
print(ks_test)

# Anderson-Darling test
ad_test <- ad.test(youtube_data$normalized_earning)
print(ad_test)
# Density plot
plot(density(youtube_data$normalized_earning), main="Kernel Density Estimation of Normalized Earnings", xlab="Normalized Earnings")

rm(ad_test,ks_test,shapiro_test)




earning_threshold <- median(youtube_data$normalized_earning)+sd(youtube_data$normalized_earning)
print(earning_threshold)


### 3.2.2 Tranform categrical variables


print(table(youtube_data$country))

youtube_data$country_status <- factor(ifelse(youtube_data$country %in% c("Japan","Germany","Canada","United States", "United Kingdom","Denmark", "Australia","China","South Korea", "Switzerland", "Sweden", "Finland", "France", "Norway", "Singapore", "Netherlands", "Italy", "Brazil", "Belgium", "Greece", "Iceland", "Luxembourg", "Spain"), "Developed", ifelse(youtube_data$country == "Unknown", "Unknown", "Developing")))

print(table(youtube_data$channel_type))
print(table(youtube_data$channel_type) < 20)

youtube_data$channel_type_status <- factor(ifelse(youtube_data$channel_type %in% c("Animals","Autos","Nonprofit","Sports","Tech","Unknown"),"Others",as.character(youtube_data$channel_type)))

### 3.2.3 Remove unrelevant columns


youtube_data <- youtube_data[
  c(
    "normalized_earning",
    "subscribers","video_views","uploads","channel_type_status","created_year","category",
    "subs_engagement","uploads_frequency","average_views_per_video","urban_rate","age","edu_ratio","top_channel_flag","video_views_increasing_rate","channel_age_adjusted_income","country_status"
    )
  ]


### 3.2.4 Feature Engineering


youtube_data$earning_class <- ifelse(youtube_data$normalized_earning > 6.2, 1, 0)
youtube_data <- subset(youtube_data, select = -normalized_earning)

names(youtube_data)


### 3.2.5 Training and Testing dataset 

# Set a seed for reproducibility
set.seed(562816)

train_index <- createDataPartition(youtube_data$earning_class, p = 0.8, list = FALSE, times = 1)

# Subset the original dataset to create training and testing datasets based on the indices generated above.
youtube_train <- youtube_data[train_index, ]
youtube_test  <- youtube_data[-train_index, ]

# Identify and store the names of all independent variables (i.e., all variables excluding the target 'earning_class').
independent_variables <- setdiff(colnames(youtube_data), "earning_class")

# From the list of independent variables, identify and store the names of numeric and integer type variables.
numeric_independent_variables <- independent_variables[sapply(youtube_data[, independent_variables], class) %in% c("numeric", "integer")]

# Similarly, from the list of independent variables, identify and store the names of factor and character type variables.
categorical_independent_variables <- independent_variables[sapply(youtube_data[, independent_variables], class) %in% c("factor", "character")]

# Part4 Classification

## 4.1 Dependent Variable & Independent Variable
## 4.2 Single Variable Classification 
### 4.2.1 Function build up

#Define target column and positive label
target_column <- 'earning_class'
positive_label <- '1'

Single_variable_model <- function(target_column, feature_column, prediction_column){
  
  # Ensure that the input vectors are of the same length
  if (!(length(target_column) == length(feature_column) && length(target_column) == length(prediction_column))) {
    stop("All input vectors must have the same length")
  }

  # Small constant to avoid division by zero
  epsilon <- 1.0e-3
  
  # Calculate the overall probability of the positive class
  probility_of_positive <- sum(target_column == positive_label) / length(target_column)
  
  # For NAs in feature_column, compute the distribution of target_column
  na_table <- table(as.factor(target_column[is.na(feature_column)]))
  
  # For NAs in feature_column, compute the conditional probability of the positive class
  probility_of_positive_with_na <- ifelse(is.na((na_table / sum(na_table))[positive_label]), probility_of_positive, (na_table / sum(na_table))[positive_label])
  
  # Create a table to represent the relationship between different values of feature_column and target_column
  value_table <- table(as.factor(target_column), feature_column, useNA = "ifany")
  
  # Compute conditional probabilities for each value of feature_column
  probility_of_positive_with_val <- (value_table[positive_label, ] + epsilon * probility_of_positive) / (colSums(value_table) + epsilon)
  
  # Generate predictions based on conditional probabilities
  predictions <- probility_of_positive_with_val[prediction_column]
  
  # Handle NA values in prediction_column
  predictions[is.na(prediction_column)] <- probility_of_positive_with_na
  
  # Handle values in prediction_column that didn't appear in the training data
predictions[is.na(predictions)] <- probility_of_positive

# Return predictions
return(predictions)
}



AUC_calculator <- function(predcol, outcol) {
  perf <- performance(prediction(predcol, outcol == positive_label), 'auc')
  return(as.numeric(perf@y.values))
}


# 将数值型列离散化的函数
discretizeVariable <- function(column) {
  quantiles <- quantile(column, probs = seq(0, 1, 0.1), na.rm = TRUE)
  discrete_column <- cut(column, unique(quantiles))
  return(discrete_column)
}



# 单一变量预测的函数（使用离散化的数值特征）
SingleVariablePredictNumeric <- function(target_column, feature_column, prediction_column, subset) {
  feature_discrete <- discretizeVariable(feature_column[subset])
  prediction_discrete <- discretizeVariable(prediction_column[subset])
  return(Single_variable_model(target_column[subset], feature_discrete, prediction_discrete))
}


### 4.2.2 Analyze Variables

#### 4.2.2.1 Analyzing Categorical variables:


for (var in categorical_independent_variables) {
  prediction_probs <- Single_variable_model(target_column = youtube_train$earning_class, 
                                            feature_column = youtube_train[, var],
                                            prediction_column = youtube_train[, var])
  auc <- AUC_calculator(prediction_probs, youtube_train$earning_class)
  if (auc >= 0.1) {
    print(sprintf("%s - AUC: %4.3f", var, auc))
  }
}


# 对每个数值型特征进行处理
for (var in numeric_independent_variables) {
  pred_col_name <- paste('pred', var, sep = '_')
  youtube_train[, pred_col_name] <- SingleVariablePredictNumeric(youtube_train$earning_class, youtube_train[, var], youtube_train[, var])
  auc_train <- AUC_calculator(youtube_train[, pred_col_name], youtube_train$earning_class)
  if (auc_train >= 0.55) {
    print(sprintf("%s: AUC: %4.3f", var, auc_train))
  }
}



# 单一变量预测的函数（使用离散化的数值特征）

# 对每个数值型特征进行处理
vars <- numeric_independent_variables 

for (var in vars) {
  aucs <- rep(0, 100)
  
  for (rep in 1:length(aucs)) {
    useForCalRep <- rbinom(n=nrow(youtube_train), size=1, prob=0.1) > 0
    
    predRep <- SingleVariablePredictNumeric(youtube_train$earning_class, 
                                            youtube_train[, var], 
                                            youtube_train[, var], 
                                            !useForCalRep)  # Pass the subset to the function
    
    # Only calculate AUC for the subset used for prediction
    actual_subset_labels = youtube_train$earning_class[!useForCalRep]
    aucs[rep] <- AUC_calculator(predRep, actual_subset_labels)
  }
  
  print(sprintf("%s: mean: %4.3f; sd: %4.3f", var, mean(aucs), sd(aucs)))
}






### 4.2.3 ROC plot

library(ROCit)

plot_roc <- function(predcol, outcol, colour_id=2, overlaid=FALSE) {
  ROCit_obj <- rocit(score = predcol, class = outcol == positive_label)
  par(new = overlaid)
  plot(ROCit_obj, col = c(colour_id, 1), legend = FALSE, YIndex = FALSE, values = FALSE)
}



library(ggplot2)
library(gridExtra)

var1 <- categorical_independent_variables[1]
var2 <- categorical_independent_variables[2]

prediction_probs1 <- Single_variable_model(target_column = youtube_train$earning_class, 
                                           feature_column = youtube_train[, var1],
                                           prediction_column = youtube_train[, var1])

prediction_probs2 <- Single_variable_model(target_column = youtube_train$earning_class, 
                                           feature_column = youtube_train[, var2],
                                           prediction_column = youtube_train[, var2])

fig1 <- ggplot(youtube_train) + geom_density(aes(x = prediction_probs1, color = as.factor(earning_class)))
fig2 <- ggplot(youtube_train) + geom_density(aes(x = prediction_probs2, color = as.factor(earning_class)))

grid.arrange(fig1, fig2, ncol=2)



## 4.3 Null model
### 4.3.1 Build Null Model


# Calculate the number of positive class instances (Npos)
Npos <- sum(youtube_train[, target_column] == 1)
cat("Number of positive class (target_column == 1) in youtube_train:", Npos, "rows\n")

# Calculate the number of negative class instances (Nneg)
Nneg <- nrow(youtube_train) - Npos
cat("Number of negative class (target_column == 0) in youtube_train:", Nneg, "rows\n")

# Calculate the Null Model prediction, which is the proportion of positive class
pred.Null <- Npos / (Npos + Nneg)
cat("Proportion of target_column == 1 in youtube_train (Null Model prediction):", pred.Null, "\n")

# Create a vector of Null Model predictions with the same length as the dataset
null_model_predictions <- rep(pred.Null, nrow(youtube_train))

# Calculate the Null Model AUC using the pROC package
library(pROC) 
roc_obj <- roc(youtube_train[, target_column], null_model_predictions)
null_model_auc <- auc(roc_obj)

cat("Null Model AUC:", null_model_auc, "\n")


## 4.4 Feature Selection


feature_combine1 <- c(categorical_independent_variables,numeric_independent_variables)


### 4.4.1 Likelihood and deviance


positive_label <- '1'
# Define a function to compute log likelihood 
logLikelihood <- function(ytrue, ypred, epsilon=1e-6) {
  sum(ifelse(ytrue==positive_label, log(ypred+epsilon), log(1-ypred-epsilon)), na.rm=T)
}
# Compute the likelihood of the Null model on the Training model
logNull <- logLikelihood(youtube_train[,target_column], sum(youtube_train[,target_column]==positive_label)/nrow(youtube_train))
cat('NULL model\'s log likelihood is',logNull)

#### 4.4.1.1 Deviance of Categorical Variables

# By adjusting the value of minDrop, we can control how strictly the variables are selected
selCatVars <- c()
minDrop <- 10 
for (v in categorical_independent_variables) {
  pi <- paste( v, sep='')
  binary_var <- ifelse(youtube_train$v == "desired_level", 1, 0)
  devDrop <- 2 * (logLikelihood(youtube_train[, target_column], binary_var) - logNull)
  if (devDrop >= minDrop) {
    print(sprintf("%s, deviance reduction: %g", pi, devDrop))
    selCatVars <- c(selCatVars, pi)
  }
}

#### 4.3.2.2 Deviance of Numeric Variables

selNumVars <- c()
minDrop <- 10 
for (v in numeric_independent_variables) {
  pi <- paste(v, sep='')
  epsilon <- 1e-6  # Small constant to avoid division by zero
  devDrop <- 2 * (logLikelihood(youtube_train[, target_column], youtube_train[, pi], epsilon) - logNull)
  if (devDrop >= minDrop) {
    print(sprintf("%s, deviance reduction: %g", pi, devDrop))
    selNumVars <- c(selNumVars, pi)
  }
}
feature_combine2 <- c("channel_type_status","country_status","video_views","average_views_per_video","subscribers","channel_age_adjusted_income")


### 4.4.2 Chi-Square Test
analyze_features <- function(data, target_column, categorical_features, numeric_features) {
  results <- data.frame(Feature = character(0), Analysis = character(0), P_Value = numeric(0))
  
  # Analyze categorical features
  for (feature in categorical_features) {
    cross_table <- table(data[, feature], data[, target_column])
    chi_square <- chisq.test(cross_table)
    
    result <- data.frame(
      Feature = feature,
      Analysis = "Chi-Square Test",
      P_Value = chi_square$p.value
    )
    
    results <- rbind(results, result)
  }
  
  # Analyze numerical features
  for (feature in numeric_features) {
    anova_result <- aov(data[, target_column] ~ data[, feature])
    
    result <- data.frame(
      Feature = feature,
      Analysis = "ANOVA",
      P_Value = summary(anova_result)[[1]][["Pr(>F)"]]
    )
    
    results <- rbind(results, result)
  }
  
  return(results)
}

results <- analyze_features(youtube_data, target_column, categorical_independent_variables, numeric_independent_variables)

print(results)


feature_combine3 <- c("category","subscribers","created_year","average_views_per_video","urban_rate","age","edu_ratio","top_channel_flag","channel_age_adjusted_income")



## 4.5 Multi-Variable Classification
### 4.5.1 Build the model performacne measure function and plotting ROC function
#### 4.5.1.1 Calculate the Model's accuracy, precision, recall, and f1 score

# ytrue should be a vector containing 1s (or TRUE) and 0s (or FALSE);
# ypred should be a vector containing the predicted probability values for the target class.
# Both ytrue and ypred should have the same length.
performanceMeasures <- function(ytrue, ypred, model.name = "model", threshold=0.5) {
  # compute the normalised deviance
  dev.norm <- -2 * logLikelihood(ytrue, ypred)/length(ypred)
  # compute the confusion matrix
  confmatrix <- table(actual = ytrue, predicted = ypred >= threshold)
  accuracy <- sum(diag(confmatrix)) / sum(confmatrix)
  precision <- confmatrix[2, 2] / sum(confmatrix[, 2])
  recall <- confmatrix[2, 2] / sum(confmatrix[2, ])
  f1 <- 2 * precision * recall / (precision + recall)
  data.frame(model = model.name, precision = precision, recall = recall, f1 = f1, dev.norm = dev.norm)
}

# pander formating
panderOpt <- function(){
  library(pander)
  # setting up Pander Options
  panderOptions("plain.ascii", TRUE)
  panderOptions("keep.trailing.zeros", TRUE)
  panderOptions("table.style", "simple")
}

# Prettier Performance Table Function
pretty_perf_table <- function(model, xtrain, ytrain,
                              xtest, ytest, threshold=0.5) {
  # Option setting for Pander
  panderOpt()
  perf_justify <- "lrrrr"
  
  # call the predict() function to do the predictions
  pred_train <- predict(model, newdata=xtrain)
  pred_test <- predict(model, newdata=xtest)
  
  # comparing performance on training vs. test
  trainperf_df <- performanceMeasures(
    ytrain, pred_train, model.name=paste(substitute(model), ", training"), threshold=threshold)
  testperf_df <- performanceMeasures(
    ytest, pred_test, model.name=paste(substitute(model), ", testing"), threshold=threshold)
  
  # combine the two performance data frames using rbind()
  perftable <- rbind(trainperf_df, testperf_df)
  pandoc.table(perftable, justify = perf_justify)
}
# calculate each models' LogLikelihood value
calculateLogLikelihood <- function(model, x, y, epsilon = 1e-6) {
  
  predicted_probabilities <- predict(model, newdata = x)
  
  log_likelihood <- sum(ifelse(y == 1, log(predicted_probabilities + epsilon), log(1 - predicted_probabilities + epsilon)))
  
  return(log_likelihood)
}


#### 4.5.1.2 Plot the ROC

library(ROCit)
plot_roc <- function(predcol1, outcol1, predcol2, outcol2){
  roc_1 <- rocit(score=predcol1, class=outcol1==positive_label)
  roc_2 <- rocit(score=predcol2, class=outcol2==positive_label)
  plot(roc_1, col = c("blue","green"), lwd = 3,
       legend = FALSE,YIndex = FALSE, values = TRUE, asp=1)
  lines(roc_2$TPR ~ roc_2$FPR, lwd = 3,
        col = c("red","green"), asp=1)
  legend("bottomright", col = c("blue","red", "green"),
         c("Test Data", "Training Data", "Null Model"), lwd = 2)
}
plot_roc3 <- function(predcol, outcol) {
  roc <- rocit(score = predcol, class = outcol == positive_label)
  plot(roc, col = c("blue", "green"), lwd = 3,
       legend = FALSE, YIndex = FALSE, values = TRUE, asp = 1)
  legend("bottomright", col = c("blue", "green"),
         c("Test Data", "Training Data"), lwd = 2)
}


### 4.5.2 Descision Tree Classifier
#### 4.5.2.1 Build Descision Tree Model for all features
formula_Var1 <- paste(target_column,'> 0 ~ ',paste(c(categorical_independent_variables,numeric_independent_variables), collapse=' + '), sep='')
tmodel1 <- rpart(formula_Var1, data=youtube_train)

# visualization decision tree 
rpart.plot(tmodel1)

# train set AUC score
tmodel1_train_auc <- AUC_calculator(predict(tmodel1, newdata=youtube_train), youtube_train[,target_column])
cat('tmodel1\'s AUC score on the training set is',tmodel1_train_auc, "\n")

# test set AUC score
tmodel1_test_auc <- AUC_calculator(predict(tmodel1, newdata=youtube_test), youtube_test[,target_column])
cat('tmodel1\'s AUC score on the testing set is',tmodel1_test_auc)

# performance table
pretty_perf_table(tmodel1,youtube_train[c(categorical_independent_variables,numeric_independent_variables)],youtube_train[,target_column] == positive_label,youtube_test[c(categorical_independent_variables,numeric_independent_variables)],youtube_test[,target_column] == positive_label)

# log likelihood
cat("tmodel1 Log Likelihood:", calculateLogLikelihood(tmodel1, youtube_test, positive_label, epsilon = 1e-6), "\n")
cat("NULL model Log Likelihood:",logNull)

#Print the ROC plot
pred_test_roc_tmodel1 <- predict(tmodel1, newdata=youtube_test)
pred_train_roc_tmodel1 <- predict(tmodel1, newdata=youtube_train)
plot_roc(pred_test_roc_tmodel1, youtube_test[[target_column]],
         pred_train_roc_tmodel1, youtube_train[[target_column]])

#### 4.5.2.2 Build Descision Tree Model for feature combination 2

formula_Var2 <- paste(target_column,'> 0 ~ ', paste(feature_combine2, collapse=' + '), sep='')
tmodel2 <- rpart(formula_Var2, data=youtube_train)

# visualization decision tree 
rpart.plot(tmodel2)


# train set AUC score
tmodel2_train_auc <- AUC_calculator(predict(tmodel2, newdata=youtube_train), youtube_train[,target_column])
cat('tmodel2\'s AUC score on the training set is',tmodel2_train_auc, "\n")

# test set AUC score
tmodel2_test_auc <- AUC_calculator(predict(tmodel2, newdata=youtube_test), youtube_test[,target_column])
cat('tmodel2\'s AUC score on the testing set is',tmodel2_test_auc)

# performance table
pretty_perf_table(tmodel2,youtube_train[feature_combine2],youtube_train[,target_column] == positive_label,youtube_test[feature_combine2],youtube_test[,target_column] == positive_label)

# log likelihood
cat("tmodel2 Log Likelihood:", calculateLogLikelihood(tmodel2, youtube_test, positive_label, epsilon = 1e-6), "\n")
cat("NULL model Log Likelihood:",logNull)

# Print the ROC plot
pred_test_roc_tmodel2 <- predict(tmodel2, newdata=youtube_test)
pred_train_roc_tmodel2 <- predict(tmodel2, newdata=youtube_train)
plot_roc(pred_test_roc_tmodel2, youtube_test[[target_column]],
         pred_train_roc_tmodel2, youtube_train[[target_column]])

#### 4.5.2.3 Build Descision Tree Model for feature combination 3

formula_Var3 <- paste(target_column,'> 0 ~ ', paste(feature_combine3, collapse=' + '), sep='')
tmodel3 <- rpart(formula_Var3, data=youtube_train)

# visualization decision tree
rpart.plot(tmodel3)

# train set AUC score 
tmodel3_train_auc <- AUC_calculator(predict(tmodel3, newdata=youtube_train), youtube_train[,target_column])
cat('tmodel3\'s AUC score on the training set is',tmodel3_train_auc, "\n")

# test set AUC score
tmodel3_test_auc <- AUC_calculator(predict(tmodel3, newdata=youtube_test), youtube_test[,target_column])
cat('tmodel3\'s AUC score on the testing set is',tmodel3_test_auc)

# performance table
pretty_perf_table(tmodel3,youtube_train[feature_combine3],youtube_train[,target_column] == positive_label,youtube_test[feature_combine3],youtube_test[,target_column] == positive_label)

# log likelihood
cat("tmodel3 Log Likelihood:", calculateLogLikelihood(tmodel3, youtube_test, positive_label, epsilon = 1e-6), "\n")
cat("NULL model Log Likelihood:",logNull)

# Print the ROC plot
pred_test_roc_tmodel3 <- predict(tmodel3, newdata=youtube_test)
pred_train_roc_tmodel3 <- predict(tmodel3, newdata=youtube_train)
plot_roc(pred_test_roc_tmodel3, youtube_test[[target_column]],
         pred_train_roc_tmodel3, youtube_train[[target_column]])

### 4.5.3 Logistic Regression classifier

#### 4.5.3.1 Build Logistic Regression model for all features
lrmodel1 <- glm(as.formula(paste("earning_class ~ ", paste(feature_combine1, collapse = ' + '))), data = youtube_train, family = "binomial")

# train set AUC score 
lrmodel1_train_auc <- AUC_calculator(predict(lrmodel1, newdata=youtube_train), youtube_train[,target_column])
cat('lrmodel1\'s AUC score on the training set is',lrmodel1_train_auc, "\n")

# test set AUC score
lrmodel1_test_auc <- AUC_calculator(predict(lrmodel1, newdata=youtube_test), youtube_test[,target_column])
cat('lrmodel1\'s AUC score on the testing set is',lrmodel1_test_auc)

# performance table
pretty_perf_table(lrmodel1,youtube_train[feature_combine1],youtube_train[,target_column] == positive_label,youtube_test[feature_combine1],youtube_test[,target_column] == positive_label)

# log likelihood
cat("lrmodel1 Log Likelihood:", calculateLogLikelihood(lrmodel1, youtube_test, positive_label, epsilon = 1e-6), "\n")
cat("NULL model Log Likelihood:",logNull)

# print ROC plot
pred_test_roc_lrmodel1 <- predict(lrmodel1, newdata=youtube_test)
pred_train_roc_lrmodel1 <- predict(lrmodel1, newdata=youtube_train)
plot_roc(pred_test_roc_lrmodel1, youtube_test[[target_column]],
         pred_train_roc_lrmodel1, youtube_train[[target_column]])

#### 4.5.3.2 Build Logistic Regression model for feature combination 2

lrmodel2 <- glm(as.formula(paste("earning_class ~ ", paste(feature_combine2, collapse = ' + '))), data = youtube_train, family = "binomial")

# train set AUC score 
lrmodel2_train_auc <- AUC_calculator(predict(lrmodel2, newdata=youtube_train), youtube_train[,target_column])
cat('lrmodel2\'s AUC score on the training set is',lrmodel2_train_auc, "\n")

# test set AUC score
lrmodel2_test_auc <- AUC_calculator(predict(lrmodel2, newdata=youtube_test), youtube_test[,target_column])
cat('lrmodel2\'s AUC score on the testing set is',lrmodel2_test_auc)

# performance table
pretty_perf_table(lrmodel2,youtube_train[feature_combine2],youtube_train[,target_column] == positive_label,youtube_test[feature_combine2],youtube_test[,target_column] == positive_label)

# log likelihood
cat("lrmodel2 Log Likelihood:", calculateLogLikelihood(lrmodel2, youtube_test, positive_label, epsilon = 1e-6), "\n")
cat("NULL model Log Likelihood:",logNull)

# print ROC plot
pred_test_roc_lrmodel2 <- predict(lrmodel2, newdata=youtube_test)
pred_train_roc_lrmodel2 <- predict(lrmodel2, newdata=youtube_train)
plot_roc(pred_test_roc_lrmodel2, youtube_test[[target_column]],
         pred_train_roc_lrmodel2, youtube_train[[target_column]])

#### 4.5.3.3 Build Logistic Regression model for feature combination 3

lrmodel3 <- glm(as.formula(paste("earning_class ~ ", paste(feature_combine3, collapse = ' + '))), data = youtube_train, family = "binomial")

# train set AUC score 
lrmodel3_train_auc <- AUC_calculator(predict(lrmodel3, newdata=youtube_train), youtube_train[,target_column])
cat('lrmodel3\'s AUC score on the training set is',lrmodel3_train_auc, "\n")

# test set AUC score
lrmodel3_test_auc <- AUC_calculator(predict(lrmodel3, newdata=youtube_test), youtube_test[,target_column])
cat('lrmodel3\'s AUC score on the testing set is',lrmodel3_test_auc)

# performance table
pretty_perf_table(lrmodel3,youtube_train[feature_combine3],youtube_train[,target_column] == positive_label,youtube_test[feature_combine3],youtube_test[,target_column] == positive_label)

# log likelihood
cat("lrmodel3 Log Likelihood:", calculateLogLikelihood(lrmodel3, youtube_test, positive_label, epsilon = 1e-6), "\n")
cat("NULL model Log Likelihood:",logNull)

# print ROC plot
pred_test_roc_lrmodel3 <- predict(lrmodel3, newdata=youtube_test)
pred_train_roc_lrmodel3 <- predict(lrmodel3, newdata=youtube_train)
plot_roc(pred_test_roc_lrmodel3, youtube_test[[target_column]],
         pred_train_roc_lrmodel3, youtube_train[[target_column]])


### 4.5.4 SVM classifier
library(e1071)
svm_model <- svm(as.formula(paste("earning_class ~ ", paste(numeric_independent_variables, collapse = ' + '))), data = youtube_train, kernel = "radial")

# train set AUC score 
svm_model_train_auc <- AUC_calculator(predict(svm_model, newdata=youtube_train), youtube_train[,target_column])
cat('svm_model\'s AUC score on the training set is',svm_model_train_auc, "\n")

# test set AUC score
svm_model_test_auc <- AUC_calculator(predict(svm_model, newdata=youtube_test), youtube_test[,target_column])
cat('svm_model\'s AUC score on the testing set is',svm_model_test_auc)

# performance table
pretty_perf_table(svm_model,youtube_train[numeric_independent_variables],youtube_train[,target_column] == positive_label,youtube_test[numeric_independent_variables],youtube_test[,target_column] == positive_label)

# log likelihood
cat("svm_model Log Likelihood:", calculateLogLikelihood(svm_model, youtube_test, positive_label, epsilon = 1e-6), "\n")
cat("NULL model Log Likelihood:",logNull)

# print ROC plot
pred_test_roc_svm_model <- predict(svm_model, newdata=youtube_test)
pred_train_roc_svm_model <- predict(svm_model, newdata=youtube_train)
plot_roc(pred_test_roc_svm_model, youtube_test[[target_column]],
         pred_train_roc_svm_model, youtube_train[[target_column]])
### 4.5.5 Random Forests classifier
#### 4.5.5.1 Build Random Forests model for feature combination 1
rfmodel1 <- randomForest(as.formula(paste("earning_class ~ ", paste(feature_combine1, collapse = ' + '))), data = youtube_train, ntree = 100)


# train set AUC score 
rfmodel1_train_auc <- AUC_calculator(predict(rfmodel1, newdata=youtube_train), youtube_train[,target_column])
cat('rfmodel1\'s AUC score on the training set is',rfmodel1_train_auc, "\n")

# test set AUC score
rfmodel1_test_auc <- AUC_calculator(predict(rfmodel1, newdata=youtube_test), youtube_test[,target_column])
cat('rfmodel1\'s AUC score on the testing set is',rfmodel1_test_auc)

# performance table
pretty_perf_table(rfmodel1,youtube_train[feature_combine1],youtube_train[,target_column] == positive_label,youtube_test[feature_combine1],youtube_test[,target_column] == positive_label)

# log likelihood
cat("rfmodel1 Log Likelihood:", calculateLogLikelihood(rfmodel1, youtube_test, positive_label, epsilon = 1e-6), "\n")
cat("NULL model Log Likelihood:",logNull)

# print ROC plot
pred_test_roc_rfmodel1 <- predict(rfmodel1, newdata=youtube_test)
pred_train_roc_rfmodel1 <- predict(rfmodel1, newdata=youtube_train)
plot_roc(pred_test_roc_rfmodel1, youtube_test[[target_column]],
         pred_train_roc_rfmodel1, youtube_train[[target_column]])
#### 4.5.5.2 Build Random Forests model for feature combination 2
rfmodel2 <- randomForest(as.formula(paste("earning_class ~ ", paste(feature_combine2, collapse = ' + '))), data = youtube_train, ntree = 100)

# train set AUC score 
rfmodel2_train_auc <- AUC_calculator(predict(rfmodel2, newdata=youtube_train), youtube_train[,target_column])
cat('rfmodel2\'s AUC score on the training set is',rfmodel2_train_auc, "\n")

# test set AUC score
rfmodel2_test_auc <- AUC_calculator(predict(rfmodel2, newdata=youtube_test), youtube_test[,target_column])
cat('rfmodel2\'s AUC score on the testing set is',rfmodel2_test_auc)

# performance table
pretty_perf_table(rfmodel2,youtube_train[feature_combine2],youtube_train[,target_column] == positive_label,youtube_test[feature_combine2],youtube_test[,target_column] == positive_label)

# log likelihood
cat("rfmodel2 Log Likelihood:", calculateLogLikelihood(rfmodel2, youtube_test, positive_label, epsilon = 1e-6), "\n")
cat("NULL model Log Likelihood:",logNull)

# print ROC plot
pred_test_roc_rfmodel2 <- predict(rfmodel2, newdata=youtube_test)
pred_train_roc_rfmodel2 <- predict(rfmodel2, newdata=youtube_train)
plot_roc(pred_test_roc_rfmodel2, youtube_test[[target_column]],
         pred_train_roc_rfmodel2, youtube_train[[target_column]])
#### 4.5.5.3 Build Random Forests model for feature combination 3
rfmodel3 <- randomForest(as.formula(paste("earning_class ~ ", paste(feature_combine3, collapse = ' + '))), data = youtube_train, ntree = 100)

# train set AUC score 
rfmodel3_train_auc <- AUC_calculator(predict(rfmodel3, newdata=youtube_train), youtube_train[,target_column])
cat('rfmodel3\'s AUC score on the training set is',rfmodel3_train_auc, "\n")

# test set AUC score
rfmodel3_test_auc <- AUC_calculator(predict(rfmodel3, newdata=youtube_test), youtube_test[,target_column])
cat('rfmodel3\'s AUC score on the testing set is',rfmodel3_test_auc)

# performance table
pretty_perf_table(rfmodel3,youtube_train[feature_combine3],youtube_train[,target_column] == positive_label,youtube_test[feature_combine3],youtube_test[,target_column] == positive_label)

# log likelihood
cat("rfmodel3 Log Likelihood:", calculateLogLikelihood(rfmodel3, youtube_test, positive_label, epsilon = 1e-6), "\n")
cat("NULL model Log Likelihood:",logNull)

# print ROC plot
pred_test_roc_rfmodel3 <- predict(rfmodel3, newdata=youtube_test)
pred_train_roc_rfmodel3 <- predict(rfmodel3, newdata=youtube_train)
plot_roc(pred_test_roc_rfmodel1, youtube_test[[target_column]],
         pred_train_roc_rfmodel1, youtube_train[[target_column]])

### 4.5.6 Compare all models
#### 4.5.6.1 models performace

model_data <- data.frame(
  model.type = c("Decision Tree", "Decision Tree", "Decision Tree","Logistic Regression", "Logistic Regression", "Logistic Regression","SVM", "Random Forest", "Random Forest","Random Forest"),
  model.name = c("tmodel1_train_auc", "tmodel2_train_auc", "tmodel3_train_auc", "lrmodel1_test_auc", "lrmodel2_test_auc","lrmodel3_test_auc", "svm_model_test_auc","rfmodel1_train_auc", "rfmodel2_train_auc", "rfmodel3_test_auc"),
  train.auc = c(tmodel1_train_auc,tmodel2_train_auc,tmodel3_train_auc,lrmodel1_train_auc,lrmodel2_train_auc,lrmodel3_train_auc,svm_model_train_auc,rfmodel1_train_auc,rfmodel2_train_auc,rfmodel3_train_auc)
)
print(model_data)
#### 4.5.6.2 ROC plot for each model

# Generic function to generate ROC curve plots
generate_roc_plot <- function(models, model_names, data, target_column) {
  library(ROCit)
  library(ggplot2)
  
  # Create an empty list to store prediction performance objects for each model
  model_preds <- vector("list", length(models))
  
  # Iterate over the list of models to generate prediction performance objects
  for (i in 1:length(models)) {
    pred <- prediction(predict(models[[i]], newdata = data), data[[target_column]])
    model_preds[[i]] <- pred
  }
  
  # Create an empty data frame to store ROC curve data for all models
  roc_data <- data.frame()
  
  # Iterate over each model's name and prediction performance object
  for (i in 1:length(models)) {
    model_name <- model_names[i]
    pred <- model_preds[[i]]
    
    # Extract TPR and FPR data
    perf <- performance(pred, "tpr", "fpr")
    
    # Extract TPR and FPR values and add them to the data frame
    roc_data <- rbind(roc_data, data.frame(
      model = model_name,
      fpr = unlist(perf@x.values),
      tpr = unlist(perf@y.values)
    ))
  }
  
  # Use ggplot2 to plot ROC curves for multiple models
  roc_plot <- ggplot(roc_data, aes(x = fpr, y = tpr, color = model)) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = 'dashed') +
    labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)") +
    theme_minimal()
  
  return(roc_plot)
}

# Generate plot_all
models_all <- list(tmodel1, tmodel2, tmodel3, lrmodel1, lrmodel2, lrmodel3, svm_model, rfmodel1, rfmodel2, rfmodel3)
model_names_all <- c("tmodel1", "tmodel2", "tmodel3", "lrmodel1", "lrmodel2", "lrmodel3", "SVM_model", "rfmodel1", "rfmodel2", "rfmodel3")
roc_plot_all <- generate_roc_plot(models_all, model_names_all, youtube_train, target_column)

# Generate plot_tmodel
models_tmodel <- list(tmodel1, tmodel2, tmodel3)
model_names_tmodel <- c("tmodel1", "tmodel2", "tmodel3")
roc_plot_tmodel <- generate_roc_plot(models_tmodel, model_names_tmodel, youtube_train, target_column)

# Generate plot_lrmodel
models_lrmodel <- list(lrmodel1, lrmodel2, lrmodel3)
model_names_lrmodel <- c("lrmodel1", "lrmodel2", "lrmodel3")
roc_plot_lrmodel <- generate_roc_plot(models_lrmodel, model_names_lrmodel, youtube_train, target_column)

# Generate plot_rfmodel
models_rfmodel <- list(rfmodel1, rfmodel2, rfmodel3)
model_names_rfmodel <- c("rfmodel1", "rfmodel2", "rfmodel3")
roc_plot_rfmodel <- generate_roc_plot(models_rfmodel, model_names_rfmodel, youtube_train, target_column)

print(roc_plot_all)


# Part5 Clustering





## 5.1 Data preprocessing

### Data Import


clustering_data <- clustering_data[,c("country","category","subscribers","video_views",
                                      "uploads","video_views_last_30_days","monthly_earnings_low","monthly_earning_high","yearly_earning_low","yearly_earning_high","subs_last_30_days","tertiary_edu_enrollment","population","unemployment_rate" ,"urban_population")]
                                      
# Calculate the frequency of each country
country_counts <- table(clustering_data$country)
categories_counts <- table(clustering_data$category)

# Replace countries with fewer than 5 occurrences with "World", and categories with fewer than 5 occurrences with "Others"
countries_to_replace <- names(country_counts[country_counts < 5])
categories_to_replace <- names(categories_counts[categories_counts < 5])

# Convert factor columns to character type
clustering_data$country <- as.character(clustering_data$country)
clustering_data$category <- as.character(clustering_data$category)

# Replace the names of the countries in the list with "World"
clustering_data$country[clustering_data$country %in% countries_to_replace] <- "World"
clustering_data$country[clustering_data$country == "Unknown"] <- "World"

# Replace the names of the categories in the list with "Others"
clustering_data$category[clustering_data$category %in% categories_to_replace] <- "Others"

# Convert back to factor type
clustering_data$country <- as.factor(clustering_data$country)
clustering_data$category <- as.factor(clustering_data$category)

clustering_data[sapply(clustering_data, is.numeric)] <- scale(clustering_data[sapply(clustering_data, is.numeric)])
#Extracting Numeric Data
clustering_numeric_data <- clustering_data[, sapply(clustering_data, is.numeric)]

## 5.3 Finding Best K

# Define a function to compute the squared Euclidean distance between two points x and y
sqr_euDist <- function(x, y) {
  sum((x - y)^2)
}

# Define a function to compute the Within-Cluster Sum of Squares (WSS) for a given cluster matrix
wss <- function(clustermat) {
  c0 <- colMeans(clustermat)
  sum(apply(clustermat, 1, FUN=function(row) {sqr_euDist(row, c0)}))
}

# Define a function to compute the total WSS for all clusters
wss_total <- function(scaled_df, labels) {
  wss_sum <- 0
  k <- length(unique(labels))
  for (i in 1:k)
    wss_sum <- wss_sum + wss(subset(scaled_df, labels == i))
  wss_sum
}

# Define a function to compute the Total Sum of Squares (TSS)
tss <- function(scaled_df) {
  wss(scaled_df)
}

# Define a function to compute the Calinski-Harabasz Index (CH Index) using either hierarchical clustering or k-means clustering
# The CH Index is useful for determining the optimal number of clusters
CH_index <- function(scaled_df, kmax, method="kmeans") {
  if (!(method %in% c("kmeans", "hclust")))
    stop("method must be one of c('kmeans', 'hclust')")
  npts <- nrow(scaled_df)
  wss_value <- numeric(kmax)
  wss_value[1] <- wss(scaled_df)
  
  if (method == "kmeans") {
    for (k in 2:kmax) {
      clustering <- kmeans(scaled_df, k, nstart=10, iter.max=100)
      wss_value[k] <- clustering$tot.withinss
    }
  } else {
    d <- dist(scaled_df, method="euclidean")
    pfit <- hclust(d, method="ward.D2")
    for (k in 2:kmax) {
      labels <- cutree(pfit, k=k)
      wss_value[k] <- wss_total(scaled_df, labels)
    }
  }
  
  bss_value <- tss(scaled_df) - wss_value
  B <- bss_value / (0:(kmax-1))
  W <- wss_value / (npts - 1:kmax)
  data.frame(k = 1:kmax, CH_index = B/W, WSS = wss_value)
}


## 5.4 hierarchical clustering


# Define the distance methods and linkage methods for hierarchical clustering
distance_methods <- c("manhattan", "euclidean")
linkage_methods <- c("average", "single", "ward.D2")

# Initialize a list to store the results of each clustering configuration
all_cluster_results <- list()

# Loop through each combination of distance and linkage methods
for (dist_method in distance_methods) {
  for (linkage_method in linkage_methods) {
    # Compute the distance matrix for the given distance method
    dist_matrix <- dist(clustering_numeric_data, method = dist_method)
    # Perform hierarchical clustering using the computed distance matrix and the given linkage method
    cluster_result <- hclust(dist_matrix, method = linkage_method)
    # Store the clustering result in the results list with a unique key based on the distance and linkage methods
    all_cluster_results[[paste(dist_method, linkage_method, sep="_")]] <- cluster_result
    # Plot the dendrogram for the clustering result
    plot(cluster_result, labels = FALSE, main = paste("Distance method:", dist_method, ", Linkage method:", linkage_method))
    rect.hclust(cluster_result, k=3)
  }
}

# Extract cluster memberships for the last clustering result with k=3
groups <- cutree(cluster_result, k=3)



# Perform principal component analysis (PCA) on the numeric data
princ <- prcomp(clustering_numeric_data)

# Number of components to retain for projection (in this case 2D)
nComp <- 2

# Project the data into the first two principal components
project2D <- as.data.frame(predict(princ, newdata=clustering_numeric_data)[,1:nComp])

# Combine the 2D projections with cluster groups and country information
hclust_project2D <- cbind(project2D, cluster=as.factor(groups), country=clustering_data$country)

# Define a function to compute the convex hull for each cluster in the 2D projection
find_convex_hull <- function(proj2Ddf, groups) {
  do.call(rbind,
          lapply(unique(groups),
                 FUN = function(c) {
                   f <- subset(proj2Ddf, cluster==c)
                   f[chull(f),]
                 }
          )
  )
}

# Compute the convex hull for each cluster
hclust_hull <- find_convex_hull(hclust_project2D, groups)

# Compute the CH Index for the clustering numeric data using hierarchical clustering
crit.df <- CH_index(clustering_numeric_data, 10, method="hclust")

# Handle potential missing values
crit.df <- na.omit(crit.df)




# Plot the CH index against k values
CHIndexPlot <- ggplot(crit.df, aes(x=k, y=CH_index)) +
  geom_point() + geom_line(colour="red") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="CH index") + theme(text=element_text(size=20))

# Plot the WSS against k values
WSSPlot <- ggplot(crit.df, aes(x=k, y=WSS)) +
  geom_point() + geom_line(colour="blue") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="WSS") + theme(text=element_text(size=20))

# Display both plots side by side
grid.arrange(CHIndexPlot, WSSPlot, nrow=1)



# Use ggplot visualize Clustering
ggplot(hclust_project2D, aes(x=PC1, y=PC2)) +
  geom_point(aes(shape=cluster, color=cluster)) +
  geom_text(aes(label=country, color=cluster), hjust=0, vjust=1, size=3) +
  geom_polygon(data=hclust_hull, aes(group=cluster, fill=as.factor(cluster)), alpha=0.4, linetype=0) +
  theme(text=element_text(size=20))


## 5.6 K-means Clustering


# Set the best number of clusters
kbest.p <- 3
cboot.hclust <- clusterboot(clustering_numeric_data, clustermethod=hclustCBI, method="ward.D2", k=kbest.p)

# Get the cluster labels for the data points from the clusterboot results
groups.cboot <- cboot.hclust$result$partition

# Calculate stability values for each cluster
stability_values <- 1 - cboot.hclust$bootbrd/100

# Identify and display the two most stable clusters
cat("Thus, clusters", order(-stability_values)[1], "and", order(-stability_values)[2], "are highly stable")

# Perform clustering using the k-means algorithm with the optimal value for k set to 5
optimalKValue <- 5
kMeansResult <- kmeans(clustering_numeric_data, optimalKValue, nstart=100, iter.max=100)

# Get the cluster results from k-means
clusterGroups <- kMeansResult$cluster

# Calculate CH index and ASW values for k values ranging from 1 to 10 using the fpc library
kMeansCH <- kmeansruns(clustering_numeric_data, krange=1:10, criterion="ch")
kMeansASW <- kmeansruns(clustering_numeric_data, krange=1:10, criterion="asw")

# Combine k-means CH index and ASW values into a single data frame
kMeansMetrics <- data.frame(k=1:10, CH_index=kMeansCH$crit, ASW=kMeansASW$crit)


# Plot the CH index against k values
CHIndexPlot <- ggplot(kMeansMetrics, aes(x=k, y=CH_index)) +
  geom_point() + geom_line(colour="red") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="CH index") + theme(text=element_text(size=20))

# Plot the ASW against k values
ASWPlot <- ggplot(kMeansMetrics, aes(x=k, y=ASW)) +
  geom_point() + geom_line(colour="blue") +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  labs(y="ASW") + theme(text=element_text(size=20))

# Display both plots side by side
grid.arrange(CHIndexPlot, ASWPlot, nrow=1)


# For k values ranging from 2 to 5, plot the results of the k-means clustering
plotList <- list()
kRange <- 2:5
for (currentK in kRange) {
  currentClusters <- kmeans(clustering_numeric_data, currentK, nstart=100, iter.max=100)$cluster
  kMeansProjection2D <- cbind(project2D, cluster=as.factor(currentClusters), country=clustering_data$country)
  kMeansHull <- find_convex_hull(kMeansProjection2D, currentClusters)
  
  currentPlot <- ggplot(kMeansProjection2D, aes(x=PC1, y=PC2)) +
    geom_point(aes(shape=cluster, color=cluster)) +
    geom_polygon(data=kMeansHull, aes(group=cluster, fill=cluster), alpha=0.4, linetype=0) +
    labs(title = sprintf("k = %d", currentK)) +
    theme(legend.position="none", text=element_text(size=20))
  
  plotList[[currentK - 1]] <- currentPlot
}

# Display k-means clustering results in a 2x2 grid
grid.arrange(plotList[[1]], plotList[[2]], plotList[[3]], plotList[[4]], nrow=2)







ui_model <- fluidPage(
  titlePanel("Model Selection"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model_type", "Model Type", 
                  choices = c("Decision Tree Model", "Logistic Regression Model", "SVM Model", "Random Forest Model", "All Model")
      ),
      conditionalPanel(
        condition = "input.model_type == 'Decision Tree Model'",
        selectInput("dt_model", "Decision Tree Model",
                    choices = c("All","tmodel1", "tmodel2", "tmodel3")
        )
      ),
      conditionalPanel(
        condition = "input.model_type == 'Logistic Regression Model'",
        selectInput("lr_model", "Logistic Regression Model",
                    choices = c("All","lrmodel1", "lrmodel2", "lrmodel3")
        )
      ),
      conditionalPanel(
        condition = "input.model_type == 'SVM Model'",
        selectInput("SVM_model", "SVM Model",
                    choices = c("SVM_model")
        )
      ),
      conditionalPanel(
        condition = "input.model_type == 'Random Forest Model'",
        selectInput("rf_model", "Random Forest Model",
                    choices = c("All","rfmodel1", "rfmodel2", "rfmodel3")
        )
      )
    ),
    mainPanel(
      plotOutput("roc_plot")
    )
  )
)

server_model <- function(input, output, session) {
  
  output$roc_plot <- renderPlot({
    model_type <- input$model_type
    
    if (model_type == "Decision Tree Model") {
      selected_model <- input$dt_model
      
      if (selected_model == "tmodel1") {
        plot_roc3(predict(tmodel1, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "tmodel2") {
        plot_roc3(predict(tmodel2, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "tmodel3") {
        plot_roc3(predict(tmodel3, newdata=youtube_train), youtube_train[[target_column]])
      }
      else if (selected_model == "All") {roc_plot_tmodel}
    } else if (model_type == "Logistic Regression Model") {
      selected_model <- input$lr_model
      
      if (selected_model == "lrmodel1") {
        plot_roc3(predict(lrmodel1, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "lrmodel2") {
        plot_roc3(predict(lrmodel2, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "lrmodel3") {
        plot_roc3(predict(lrmodel3, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "All") {roc_plot_lrmodel}
    } else if (model_type == "SVM Model") {
      selected_model <- input$SVM_model
      if (selected_model == "SVM_model") {
        plot_roc3(predict(svm_model, newdata=youtube_train), youtube_train[[target_column]])
      }
    } else if (model_type == "Random Forest Model") {
      selected_model <- input$rf_model
      
      if (selected_model == "rfmodel1") {
        plot_roc3(predict(rfmodel1, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "rfmodel2") {
        plot_roc3(predict(rfmodel2, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "rfmodel3") {
        plot_roc3(predict(rfmodel3, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "All") {roc_plot_rfmodel}
    } else if (model_type == "All Model"){roc_plot_all}
  })
}
shinyApp(ui = ui_model, server = server_model)


















ui_model <- fluidPage(
  titlePanel("Model Selection"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model_type", "Model Type", 
                  choices = c("Decision Tree Model", "Logistic Regression Model", "SVM Model", "Random Forest Model", "All Model")
      ),
      conditionalPanel(
        condition = "input.model_type == 'Decision Tree Model'",
        selectInput("dt_model", "Decision Tree Model",
                    choices = c("All","tmodel1", "tmodel2", "tmodel3")
        )
      ),
      conditionalPanel(
        condition = "input.model_type == 'Logistic Regression Model'",
        selectInput("lr_model", "Logistic Regression Model",
                    choices = c("All","lrmodel1", "lrmodel2", "lrmodel3")
        )
      ),
      conditionalPanel(
        condition = "input.model_type == 'SVM Model'",
        selectInput("SVM_model", "SVM Model",
                    choices = c("SVM_model")
        )
      ),
      conditionalPanel(
        condition = "input.model_type == 'Random Forest Model'",
        selectInput("rf_model", "Random Forest Model",
                    choices = c("All","rfmodel1", "rfmodel2", "rfmodel3")
        )
      )
    ),
    mainPanel(
      plotOutput("roc_plot")
    )
  )
)

server_model <- function(input, output, session) {
  
  output$roc_plot <- renderPlot({
    model_type <- input$model_type
    
    if (model_type == "Decision Tree Model") {
      selected_model <- input$dt_model
      
      if (selected_model == "tmodel1") {
        plot_roc3(predict(tmodel1, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "tmodel2") {
        plot_roc3(predict(tmodel2, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "tmodel3") {
        plot_roc3(predict(tmodel3, newdata=youtube_train), youtube_train[[target_column]])
      }
      else if (selected_model == "All") {roc_plot_tmodel}
    } else if (model_type == "Logistic Regression Model") {
      selected_model <- input$lr_model
      
      if (selected_model == "lrmodel1") {
        plot_roc3(predict(lrmodel1, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "lrmodel2") {
        plot_roc3(predict(lrmodel2, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "lrmodel3") {
        plot_roc3(predict(lrmodel3, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "All") {roc_plot_lrmodel}
    } else if (model_type == "SVM Model") {
      selected_model <- input$SVM_model
      if (selected_model == "SVM_model") {
        plot_roc3(predict(svm_model, newdata=youtube_train), youtube_train[[target_column]])
      }
    } else if (model_type == "Random Forest Model") {
      selected_model <- input$rf_model
      
      if (selected_model == "rfmodel1") {
        plot_roc3(predict(rfmodel1, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "rfmodel2") {
        plot_roc3(predict(rfmodel2, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "rfmodel3") {
        plot_roc3(predict(rfmodel3, newdata=youtube_train), youtube_train[[target_column]])
      } else if (selected_model == "All") {roc_plot_rfmodel}
    } else if (model_type == "All Model"){roc_plot_all}
  })
}



shinyApp(ui = ui_model, server = server_model)