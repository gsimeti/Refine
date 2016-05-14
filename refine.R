# Load packages - dplyr, tidyr, stringr, readr
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Load dataset "refine_original.csv" into data frame "refine_df"
refine_df <- read_csv("refine_original.csv")

# Check structure of refine_df
str(refine_df)

# Print refine_df
refine_df

# Make company names all lowercase
refine_df$company <- tolower(refine_df$company)

# Determine min and max string lengths in refine_df$company vector for regular expressions
min(str_length(refine_df$company))
max(str_length(refine_df$company))

# Use gsub to replace misspelled names in company vector
refine_df$company <- gsub("^p[a-z0-9[:space:]]{3,10}$", "philips", refine_df$company)
refine_df$company <- gsub("^a[a-z0-9[:space:]]{3,10}$", "akzo", refine_df$company)
refine_df$company <- gsub("^v[a-z0-9[:space:]]{3,10}$", "van houten", refine_df$company)
refine_df$company <- gsub("^u[a-z0-9[:space:]]{3,10}$", "unilever", refine_df$company)

# Check company character vector
refine_df$company

# Replace individual philips misspelling
refine_df$company[15] <- "philips"

# Separate product code and number, set as refine_df2
refine_df2 <- separate(refine_df, col = "Product code / number",
                       into = c("product_code", "product_number"), sep = "-")

# Add new column "product_category"
product_category <- vector(mode = "character", length = 25)
product_category <- rep(NA, 25)  # make product_category a vector with missing values

# Match product_category to refine_df2$product_code
for (i in 1:25) {
    if (refine_df2$product_code[i] == "p") {
        product_category[i] <- "Smartphone"
    } else if (refine_df2$product_code[i] == "v") {
        product_category[i] <- "TV"
    } else if (refine_df2$product_code[i] == "x") {
        product_category[i] <- "Laptop"
    } else if (refine_df2$product_code[i] == "q") {
        product_category[i] <- "Tablet"
    } else {
    next
    }
}

# Bind product_category to refine_df2, save as refine_df3
refine_df3 <- cbind(refine_df2, product_category)

# Merge address, city, country into full_address, save as refine_df4
refine_df4 <- unite(refine_df3, "full_address", address, city, country, sep = ", ")

# Make refine_df4$product_category a categorical variable
refine_df4$product_category <- as.factor(refine_df4$product_category)

# Create 4 company dummy variable vectors
company_philips <- as.numeric(refine_df4$company == "philips")
company_akzo <- as.numeric(refine_df4$company == "akzo")
company_van_houten <- as.numeric(refine_df4$company == "van houten")
company_unilever <- as.numeric(refine_df4$company == "unilever")

# Bind company dummy variable vectors to data frame
refine_df4 <- cbind(refine_df4, company_philips)
refine_df4 <- cbind(refine_df4, company_akzo)
refine_df4 <- cbind(refine_df4, company_van_houten)
refine_df5 <- cbind(refine_df4, company_unilever)

# Create 4 product dummy variable vectors
product_smartphone <- as.numeric(refine_df5$product_category == "Smartphone")
product_tv <- as.numeric(refine_df5$product_category == "TV")
product_laptop <- as.numeric(refine_df5$product_category == "Laptop")
product_tablet <- as.numeric(refine_df5$product_category == "Tablet")

# Bind product dummy variable vectors to data frame
refine_df5 <- cbind(refine_df5, product_smartphone)
refine_df5 <- cbind(refine_df5, product_tv)
refine_df5 <- cbind(refine_df5, product_laptop)
refine_df6 <- cbind(refine_df5, product_tablet)

# Save data frame as refine_clean.csv
write.csv(refine_df6, file = "refine_clean.csv")
