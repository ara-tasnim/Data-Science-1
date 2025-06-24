# Import package for read excel file
install.packages("readxl")
library(readxl)   
dataset <- read_excel(path="C:/Users/NABIHA TAHSIN/Documents/Data Science/mid project 1 JN/Midterm_Dataset_Section(A).xlsx")
dataset
print(dataset, n=Inf)

summary(dataset)

#new dataset
dataset_1 <- dataset

# Check null value and count by column
is.na(dataset_1)
colSums(is.na(dataset_1))



#Handle missing value

#discard instance for "dataset_1"
cleaned_dataset_1 <- na.omit(dataset_1)
cleaned_dataset_1


#Mean for "age" attribute
mean_dataset <- dataset_1 
mean_dataset$Age[is.na(mean_dataset$Age)] <- mean(mean_dataset$Age, na.rm = TRUE)
mean_dataset

#median for "age"
median_dataset <- dataset_1 
median_dataset$Age[is.na(median_dataset$Age)] <- median(median_dataset$Age, na.rm = TRUE)
median_dataset

#mode for "age"
mode_dataset <- dataset_1 
mode_dataset$Age[is.na(mode_dataset$Age)] <- mode(mode_dataset$Age)
mode_dataset

#mode(replace with most frequent value for age)
mode_1_age <- dataset_1
most_frequent_age <- as.numeric(names(sort(table(mode_1_age$Age), decreasing = TRUE)[1]))
mode_1_age$Age[is.na(mode_1_age$Age)] <- most_frequent_age
mode_1_age

#most frequent value for "gender"
most_frequent_gender <- dataset_1
updated_gender <- names(sort(table(most_frequent_gender$Gender), decreasing = TRUE)[1])
most_frequent_gender$Gender[is.na(most_frequent_gender$Gender)] <- updated_gender
most_frequent_gender$Gender

#most frequent value for "Item Purchased"
most_frequent_item <- dataset_1
updated_item_purchased <- names(sort(table(most_frequent_item$'Item Purchased'), decreasing = TRUE)[1])
most_frequent_item$'Item Purchased'[is.na(most_frequent_item$'Item Purchased')] <- updated_item_purchased
most_frequent_item$'Item Purchased'


#most frequent value for "Frequency of Purchases"
most_frequent_purchase <- dataset_1
updated_purchase_frequency <- names(sort(table(most_frequent_purchase$'Frequency of Purchases'), decreasing = TRUE)[1])
most_frequent_purchase$'Frequency of Purchases'[is.na(most_frequent_purchase$'Frequency of Purchases')] <- updated_purchase_frequency
most_frequent_purchase$'Frequency of Purchases'

#applying top-down by forward fill
install.packages("zoo")
library(zoo)

top_down <- dataset_1
top_down$Age <- zoo::na.locf(top_down$Age, na.rm = FALSE)
top_down$Gender <- zoo::na.locf(top_down$Gender, na.rm = FALSE)
top_down$'Item Purchased' <- zoo::na.locf(top_down$'Item Purchased', na.rm = FALSE)
top_down$'Frequency of Purchases' <- zoo::na.locf(top_down$'Frequency of Purchases', na.rm = FALSE)
top_down



#down-top by backward fill
down_top <- dataset_1
down_top$Age <- zoo::na.locf(down_top$Age, fromLast = TRUE, na.rm = FALSE)
down_top$Gender <- zoo::na.locf(down_top$Gender, fromLast = TRUE, na.rm = FALSE)
down_top$'Item Purchased' <- zoo::na.locf(down_top$'Item Purchased', fromLast = TRUE, na.rm = FALSE)
down_top$'Frequency of Purchases' <- zoo::na.locf(down_top$'Frequency of Purchases', fromLast = TRUE, na.rm = FALSE)
down_top

# missing values on a graph.  
# Install and load naniar package
install.packages("naniar")
library(naniar)
# Create a data set with missing values
graph_dataset <- dataset_1

# Visualize missing values in the data set
gg_miss_var(graph_dataset)



#Imbalanced to balanced dataset

install.packages("dplyr")
library(dplyr)
table(cleaned_dataset_1$`Frequency of Purchases`)

#Find the class counts
class_counts <- table(cleaned_dataset_1$`Frequency of Purchases`)
class_counts

min_class_size <- min(class_counts)
print(paste("Smallest class size:", min_class_size))

#Perform undersampling
set.seed(42)  
undersampling_data <- cleaned_dataset_1 %>%
  group_by(`Frequency of Purchases`) %>%
  sample_n(size = min_class_size) %>%
  ungroup()

table(undersampling_data$`Frequency of Purchases`)



#Oversampling

max_class_size <- max(class_counts)
print(paste("Largest class size:", max_class_size))

set.seed(42)
oversampling_data <- cleaned_dataset_1 %>%
  group_by(`Frequency of Purchases`) %>%
  group_modify(~ .x[sample(1:nrow(.x), max_class_size, replace = TRUE), ]) %>%
  ungroup()

table(oversampling_data$`Frequency of Purchases`)



set.seed(42) 
oversampled_minority <- minority_class %>% sample_n(nrow(majority_class), replace = TRUE)

oversampling_data <- rbind(majority_class, oversampled_minority)

table(oversampling_data$'Frequency of Purchases')




#Find duplicate values
duplicate_value <- cleaned_dataset_1[duplicated(cleaned_dataset_1), ]
duplicate_value

#Remove duplicate values
remove_duplicate_value <- cleaned_dataset_1[!duplicated(cleaned_dataset_1), ]
remove_duplicate_value





#Filtering methods to filter data
filtered_data <- cleaned_dataset_1 %>% filter(Age > 25)
filtered_data

# Filter rows where Age > 30 AND Gender is "Male"
filtered_data_1 <- cleaned_dataset_1 %>% filter(Age > 30, Gender == "Male")
filtered_data_1
# Filter rows where Age > 30 OR Gender is "Male"
filtered_data_2 <- cleaned_dataset_1 %>% filter(Age > 30 | Gender == "Male")
filtered_data_2





#Conversion
converted_dataset <- dataset_1


#numerical to categorical for attribute "Age"
converted_dataset$Age_group <- cut(converted_dataset$Age, 
                                   breaks = c(0, 18, 35, 50, 100), 
                                   labels = c('Teen', 'Young Adult', 'Adult', 'Senior'))
#categorical to numerical for attribute "Gender"
converted_dataset$Gender_numeric <- as.numeric(factor(converted_dataset$Gender))
#categorical to numerical for attribute "Item Purchased"
converted_dataset$item_purchased_numeric <- as.numeric(factor(converted_dataset$'Item Purchased'))
#categorical to numerical for attribute "Category"
converted_dataset$Category_numeric <- as.numeric(factor(converted_dataset$Category))
#numerical to catagorical for attribute "Purchase amount"
converted_dataset$Purchase_amount_group <- cut(converted_dataset$`Purchase Amount (USD)`, 
                                               breaks = c(0, 40, 80, Inf), 
                                               labels = c('LOW', 'MEDIUM', 'HIGH'),
                                               right=FALSE)
#categorical to numerical for attribute "Location"
converted_dataset$location_numeric <- as.numeric(factor(converted_dataset$Location))
#categorical to numerical for attribute "Size"
converted_dataset$size_numeric <- as.numeric(factor(converted_dataset$Size))
#categorical to numerical for attribute "Color"
converted_dataset$color_numeric <- as.numeric(factor(converted_dataset$Color))
#numerical to categorical for attribute "review rating"
converted_dataset$rating_group <- cut(converted_dataset$'Review Rating', 
                                   breaks = c(0, 1, 2, 3, 4, 5), 
                                   labels = c('Very Poor', 'Poor', 'Average', 'Good', 'Excellent'))
#categorical to numerical for attribute "subscription status"
converted_dataset$subscription_status_numeric <- as.numeric(factor(converted_dataset$'Subscription Status'))
#categorical to numerical for attribute "discount applied"
converted_dataset$discount_applied_numeric <- as.numeric(factor(converted_dataset$'Discount Applied'))
#categorical to numerical for attribute "Promo code used"
converted_dataset$promo_code_numeric <- as.numeric(factor(converted_dataset$'Promo Code Used'))
#categorical to numerical for attribute "frequency of purchases"
converted_dataset$frequency_purchases_numeric <- as.numeric(factor(converted_dataset$'Frequency of Purchases'))

converted_dataset






#Normalization in one attribute
normalize <- dataset_1
normalize$Age <- (normalize$Age - min(normalize$Age, na.rm = TRUE)) / (max(normalize$Age, na.rm = TRUE) - min(normalize$Age, na.rm = TRUE))
normalize$Age
head(normalize)






#find invalid data for age
str(dataset_1)
invalid_data <- dataset_1$Age < 0 | dataset_1$Age > 100 | is.na(dataset_1$Age)
invalid_data


#Handle invalid data

#1.Remove all NA value
Remove_NA_Values <- na.omit(dataset_1$Age)
Remove_NA_Values

#2. replacing by median value
replace_by_median <- dataset_1 
replace_by_median$Age[is.na(replace_by_median$Age)] <- median(replace_by_median$Age, na.rm = TRUE)
replace_by_median

#find invalid data for gender
valid_gender <- c("Male", "Female")
invalid_gender <- !(dataset_1$Gender %in% valid_gender)
invalid_gender

#handle invalid data for gender(Impute Invalid Values)
handle_gender <- dataset_1
handle_gender$Gender[invalid_gender] <- "Unknown"
handle_gender

#handle invalid data for item purchased(by most frequent value)
handle_item <- dataset_1
final_item <- names(sort(table(handle_item$'Item Purchased'), decreasing = TRUE)[1])
handle_item$'Item Purchased'[is.na(handle_item$'Item Purchased')] <- final_item

##handle invalid data for frequency of purchase(by Remove all NA value)
handle_frequency_purchase <- na.omit(dataset_1$'Frequency of Purchases')
handle_frequency_purchase
summary(handle_frequency_purchase)



#Outliers

#Z-Score

# Calculate Z-scores for numerical columns
numerical_columns_zscore <- sapply(dataset_1, is.numeric)
z_scores <- scale(dataset_1[, numerical_columns_zscore])

# Identify outliers (Z-score > 3 or < -3)
outliers_zscore <- abs(z_scores) > 3

# Check which rows have outliers
outlier_rows_zscore <- apply(outliers_zscore, 1, any)

# View the rows with outliers
outlier_data_zscore <- dataset_1[outlier_rows_zscore, ]
outlier_data_zscore

#Remove outliers
Remove_outliers_zscore <- dataset_1[!outlier_rows_zscore, ]
Remove_outliers_zscore



#IQR

# Function to calculate IQR and identify outliers
IQR_Outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)  # 25th percentile (Q1)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)  # 75th percentile (Q3)
  IQR_value <- Q3 - Q1  # Interquartile range
  
  # Define lower and upper bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Identify outliers (values outside the lower and upper bounds)
  return(x < lower_bound | x > upper_bound)
}

# Apply the IQR method to numerical columns
numerical_columns_IQR <- sapply(dataset_1, is.numeric)  # Identify numerical columns
outliers_IQR <- sapply(dataset_1[, numerical_columns_IQR], IQR_Outliers)

# Check the rows that contain outliers
outlier_rows_IQR <- apply(outliers_IQR, 1, any)  # True if any column has an outlier
outlier_data_IQR <- dataset_1[outlier_rows_IQR, ]
outlier_data_IQR

#Handle outliers IQR

# Remove rows with outliers
IQR_no_outliers <- dataset_1[!outlier_rows_IQR, ]
IQR_no_outliers

#Boxplot

#Boxplot before removing outliers for "Age"
boxplot(dataset_1$Age, 
        main="Boxplot before removing outliers of Age", 
        ylab="Age", 
        col="lightblue", 
        border="black", 
        horizontal=TRUE)

# Boxplot after removing outliers for "Age"
boxplot_no_outliers <- dataset_1[dataset_1$Age >= 10 & dataset_1$Age <= 100, ]
boxplot(boxplot_no_outliers$Age, 
        main="Boxplot after removing outliers of Age", 
        ylab="Age", 
        col="lightgreen", 
        border="black", 
        horizontal=TRUE)
        


