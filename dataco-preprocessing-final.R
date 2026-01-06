
# STEP 1 – Dataset Loading & Basic Inspection


# 1.1 Load required packages 
library(tidyverse)   # includes dplyr, tidyr, ggplot2, readr etc.

# 1.2 Load the main DataCo CSV 
data_raw <- DataCoSupplyChainDataset  

# 1.3 Basic size information
# Number of rows and columns
dim(data_raw)      # returns c(n_rows, n_cols)

# 1.4 First few rows
head(data_raw, 10)   # first 10 rows

# 1.5 Structure of the dataset 
str(data_raw)        # shows each column name, type, and examples

# 1.6 Summary statistics (numeric + categorical)
summary(data_raw)

# 1.7 Quick NA overview 

# Check how many missing values per column
na_counts <- colSums(is.na(data_raw))
na_counts

# Create a tidy NA table: variable, NA count, NA%
na_tbl <- data_raw |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "NA_count"
  ) |>
  mutate(
    NA_percent = round(100 * NA_count / nrow(data_raw), 2)
  ) |>
  arrange(desc(NA_count))

# Top 15 variables by missingness
head(na_tbl, 15)


# STEP 2 – DATA CLEANING (ORDER-LEVEL)
# - Remove duplicates
# - Handle missing data


# 2.0 Start from the raw data
data_clean <- data_raw

# 2.1 HANDLE DUPLICATES

# Exact duplicate rows (all columns identical)
n_original <- nrow(data_clean)
n_distinct <- nrow(distinct(data_clean))

dup_rows_count <- n_original - n_distinct
dup_rows_count   # number of exact duplicate rows

# Remove exact duplicate rows
data_clean <- distinct(data_clean)

# Check duplicates by Order Id + Order Item Id (order line identifiers)
dup_order_items <- data_clean %>%
  group_by(`Order Id`, `Order Item Id`) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

nrow(dup_order_items)  # if > 0, there are duplicated order-line IDs


# 2.2 HANDLE MISSING DATA

# Drop columns with extremely high missingness and no clear modelling use:
# - Product Description (100% NA)
# - Order Zipcode (~86% NA)
data_clean <- data_clean %>%
  select(
    -`Product Description`,
    -`Order Zipcode`
  )

# Define critical variables that must NOT be NA
# These are needed for order-level modelling with our chosen features.
critical_vars <- c(
  "Department Name",              # Department Name
  "Order Profit Per Order",       # Order Profit Per Order
  "Order Item Product Price",     # Order Item Product Price
  "Order Item Quantity",          # Order Item Quantity
  "Order Item Discount Rate",     # Order Item Discount Rate
  "Days for shipping (real)",     # Days for shipping (real)
  "Days for shipment (scheduled)",# Days for shipment (scheduled)
  "Customer Segment",             # Customer Segment
  "Order Region",                 # Order Region
  "Shipping Mode",                # Shipping Mode
  "Order Id",                     # Order Id
  "Longitude",                   
  "Customer Zipcode"              
)

# Drop rows where ANY of these critical fields is missing
data_clean <- data_clean %>%
  filter(across(all_of(critical_vars), ~ !is.na(.)))

# Re-check missing values after cleaning
na_after <- colSums(is.na(data_clean))
na_after

# 2.3 CONVERT DATE COLUMNS TO PROPER DATETIME

data_clean <- data_clean %>%
  mutate(
    # order date: try with time first
    order_date_hm = as.POSIXct(`order date (DateOrders)`,
                               format = "%m/%d/%Y %H:%M"),
    # fallback: date only
    order_date_d  = as.POSIXct(`order date (DateOrders)`,
                               format = "%m/%d/%Y"),
    # final parsed order date
    order_date_dt = dplyr::coalesce(order_date_hm, order_date_d),
    
    # shipping date: same logic
    shipping_date_hm = as.POSIXct(`shipping date (DateOrders)`,
                                  format = "%m/%d/%Y %H:%M"),
    shipping_date_d  = as.POSIXct(`shipping date (DateOrders)`,
                                  format = "%m/%d/%Y"),
    shipping_date_dt = dplyr::coalesce(shipping_date_hm, shipping_date_d)
  )


sum(is.na(data_clean$order_date_dt))
sum(is.na(data_clean$shipping_date_dt))

head(data_clean[c("order date (DateOrders)",
                  "order_date_hm", "order_date_d", "order_date_dt")], 10)

# Quick check the conversion worked
summary(data_clean$shipping_date_dt)
summary(data_clean$shipping_date_dt)

dim(data_clean)
## EDA
# Ensure Late_delivery_risk is coded as factor with readable labels
data_clean <- data_clean %>%
  mutate(
    Late_delivery_risk = factor(
      Late_delivery_risk,
      levels = c(0, 1),
      labels = c("On time", "Late")
    )
  )

# Check the distribution in counts
table(data_clean$Late_delivery_risk)

# Check the distribution in percentages
prop.table(table(data_clean$Late_delivery_risk)) * 100
## plotting
library(ggplot2)

ggplot(data_clean, aes(x = Late_delivery_risk, fill = Late_delivery_risk)) +
  geom_bar() +
  scale_fill_manual(
    values = c("On time" = "#1B9E77",  # green-ish
               "Late"    = "#D95F02")  # orange/red-ish
  ) +
  labs(
    title = "Distribution of Delivery Outcome",
    x = "Delivery Status",
    y = "Number of Orders",
    fill = "Delivery Status"
  ) +
  theme_minimal()
 
#  delay rate per Shipping Mode


shipping_delay_summary <- data_clean %>%
  group_by(Shipping.Mode) %>%
  summarise(
    total_orders = n(),
    late_orders  = sum(Late_delivery_risk == "Late"),
    late_rate    = round(100 * late_orders / total_orders, 2)
  ) %>%
  arrange(desc(late_rate))

shipping_delay_summary

colnames(data_clean)

## Late rate (%) by Shipping Mode

ggplot(shipping_delay_summary,
       aes(x = reorder(Shipping.Mode, late_rate),
           y = late_rate,
           fill = Shipping.Mode)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Late Delivery Rate by Shipping Mode",
    x = "Shipping Mode",
    y = "Late Orders (%)",
    fill = "Shipping Mode"
  ) +
  theme_minimal()

##Late rate per Region

library(dplyr)
library(ggplot2)
install.packages("scales")
library(scales)

# Region-level sales + percentage
region_sales_delay <- data_clean %>%
  group_by(Order.Region, Late_delivery_risk) %>%
  summarise(
    total_sales = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Order.Region) %>%
  mutate(
    region_total = sum(total_sales),
    pct         = total_sales / region_total
  ) %>%
  arrange(Order.Region, Late_delivery_risk) %>%
  mutate(
    cum_sales = cumsum(total_sales),
    label_pos = cum_sales - total_sales / 2   # middle of each segment
  ) %>%
  ungroup() %>%
  mutate(
    Order.Region = reorder(Order.Region, region_total)
  )

# Plot: Region-wise sales, stacked, with % labels
ggplot(region_sales_delay,
       aes(x = Order.Region,
           y = total_sales,
           fill = Late_delivery_risk)) +
  geom_col() +
  geom_text(
    aes(y = label_pos,
        label = percent(pct, accuracy = 1)),
    colour = "white",
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Region-wise Sales by Delivery Status",
    x = "Region",
    y = "Total Sales",
    fill = "Delivery Status"
  ) +
  theme_minimal()



##Market-wise Sales with % On time vs Late
# Market-level sales + percentage
market_sales_delay <- data_clean %>%
  group_by(Market, Late_delivery_risk) %>%
  summarise(
    total_sales = sum(Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Market) %>%
  mutate(
    market_total = sum(total_sales),
    pct         = total_sales / market_total
  ) %>%
  arrange(Market, Late_delivery_risk) %>%
  mutate(
    cum_sales = cumsum(total_sales),
    label_pos = cum_sales - total_sales / 2
  ) %>%
  ungroup() %>%
  mutate(
    Market = reorder(Market, market_total)
  )

# Plot: Market-wise sales, stacked, with % labels
ggplot(market_sales_delay,
       aes(x = Market,
           y = total_sales,
           fill = Late_delivery_risk)) +
  geom_col() +
  geom_text(
    aes(y = label_pos,
        label = percent(pct, accuracy = 1)),
    colour = "white",
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Market-wise Sales by Delivery Status",
    x = "Market",
    y = "Total Sales",
    fill = "Delivery Status"
  ) +
  theme_minimal()


## Shipping Time vs Delay
shipping_time_summary <- data_clean %>%
  group_by(Late_delivery_risk) %>%
  summarise(
    n_orders   = n(),
    mean_days  = round(mean(Days.for.shipping..real., na.rm = TRUE), 2),
    median_days = round(median(Days.for.shipping..real., na.rm = TRUE), 2),
    sd_days    = round(sd(Days.for.shipping..real., na.rm = TRUE), 2)
  )

shipping_time_summary


## Boxplot: actual shipping days by delivery status
ggplot(data_clean,
       aes(x = Late_delivery_risk,
           y = Days.for.shipping..real.,
           fill = Late_delivery_risk)) +
  geom_boxplot(outlier.alpha = 0.2) +
  scale_fill_manual(
    values = c(
      "On time" = "#4C72B0",   # blue
      "Late"    = "#C44E52"    # red
    )
  ) +
  labs(
    title = "Actual Shipping Days by Delivery Status",
    x = "Delivery Status",
    y = "Days for Shipping (real)",
    fill = "Delivery Status"
  ) +
  theme_minimal()

##Discount Rate vs Delivery Status
discount_summary <- data_clean %>%
  group_by(Late_delivery_risk) %>%
  summarise(
    n_orders     = n(),
    mean_disc    = round(mean(Order.Item.Discount.Rate, na.rm = TRUE), 3),
    median_disc  = round(median(Order.Item.Discount.Rate, na.rm = TRUE), 3),
    sd_disc      = round(sd(Order.Item.Discount.Rate, na.rm = TRUE), 3)
  )

discount_summary


##plot
ggplot(data_clean,
       aes(x = Late_delivery_risk,
           y = Order.Item.Discount.Rate,
           fill = Late_delivery_risk)) +
  geom_boxplot(outlier.alpha = 0.2) +
  scale_fill_manual(
    values = c(
      "On time" = "#4C72B0",   # blue
      "Late"    = "#C44E52"    # red
    )
  ) +
  labs(
    title = "Order Item Discount Rate by Delivery Status",
    x = "Delivery Status",
    y = "Discount Rate",
    fill = "Delivery Status"
  ) +
  theme_minimal()


## Order Profit Per Order vs Delivery Status
profit_summary <- data_clean %>%
  group_by(Late_delivery_risk) %>%
  summarise(
    n_orders      = n(),
    mean_profit   = round(mean(Order.Profit.Per.Order, na.rm = TRUE), 2),
    median_profit = round(median(Order.Profit.Per.Order, na.rm = TRUE), 2),
    sd_profit     = round(sd(Order.Profit.Per.Order, na.rm = TRUE), 2)
  )

profit_summary

## boxplot 
ggplot(data_clean,
       aes(x = Late_delivery_risk,
           y = Order.Profit.Per.Order,
           fill = Late_delivery_risk)) +
  geom_boxplot(outlier.alpha = 0.2) +
  scale_fill_manual(
    values = c(
      "On time" = "#4C72B0",   # blue
      "Late"    = "#C44E52"    # red
    )
  ) +
  labs(
    title = "Order Profit Per Order by Delivery Status",
    x = "Delivery Status",
    y = "Order Profit Per Order",
    fill = "Delivery Status"
  ) +
  theme_minimal()


#  Late rate by Department

dept_delay_summary <- data_clean %>%
  group_by(Department.Name) %>%
  summarise(
    total_orders = n(),
    late_orders  = sum(Late_delivery_risk == "Late"),
    late_rate    = round(100 * late_orders / total_orders, 2)
  ) %>%
  arrange(desc(late_rate))

dept_delay_summary

## plot 
ggplot(dept_delay_summary,
       aes(x = reorder(Department.Name, late_rate),
           y = late_rate,
           fill = Department.Name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Late Delivery Rate by Department",
    x = "Department",
    y = "Late Orders (%)"
  ) +
  theme_minimal()



## late rate per category
library(scales)

# Build top 10 categories by number of late orders
category_late_top10 <- data_clean %>%
  filter(Late_delivery_risk == "Late") %>%
  group_by(Category.Name) %>%
  summarise(
    late_orders = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pct = late_orders / sum(late_orders)
  ) %>%
  arrange(desc(late_orders)) %>%
  slice_head(n = 10) %>%
  # create a label with name + % for the pie
  mutate(
    label = paste0(Category.Name, " (", percent(pct, accuracy = 1), ")")
  )

category_late_top10

##plot pie chart

professional_colors <- c(
  "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
  "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
)

ggplot(category_late_top10,
       aes(x = "",
           y = pct,
           fill = Category.Name)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 3,
    colour = "white"
  ) +
  scale_fill_manual(values = professional_colors) +
  labs(
    title = "Top 10 Product Categories by Share of Late Orders",
    fill  = "Product Category"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )


## Outlier Detetction 

# Select only numeric columns
numeric_data <- data_clean %>%
  select(where(is.numeric))

# Outlier detection with boxplot.stats for each numeric variable
outlier_list <- lapply(numeric_data, function(x) boxplot.stats(x)$out)

# Count outliers per variable
outlier_counts <- sapply(outlier_list, length)

outlier_summary <- data.frame(
  Variable      = names(outlier_counts),
  Outlier_Count = as.integer(outlier_counts)
) %>%
  arrange(desc(Outlier_Count))

outlier_summary    # look at this table in console

library(dplyr)
library(stringr)

# 1.1 Keep only main business variables (no IDs)
main_vars <- c(
  "Order.Profit.Per.Order",
  "Order.Item.Profit.Ratio",
  "Order.Item.Discount",
  "Order.Item.Product.Price",
  "Product.Price",
  "Sales.per.customer",
  "Order.Item.Total",
  "Sales",
  "Longitude"   # keep if you still care about distance/location
  # add/remove names here as you like
)

outlier_summary_main <- outlier_summary %>%
  filter(Variable %in% main_vars) %>%
  arrange(desc(Outlier_Count))

outlier_summary_main

## Histogram



# Variables you want detailed histograms for
vars_to_plot <- c(
  "Order.Profit.Per.Order",
  "Order.Item.Discount",
  "Sales",
  "Order.Item.Product.Price"
)

col_original <- "#4E79A7"  # blue
col_log      <- "#59A14F"  # green

# loop over variables in chunks of 2
for (i in seq(1, length(vars_to_plot), by = 2)) {
  # subset 2 variables at a time
  vars_chunk <- vars_to_plot[i:min(i+1, length(vars_to_plot))]
  
  par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))  # 2 rows, 2 cols
  
  for (v in vars_chunk) {
    x <- data_clean[[v]]
    x_pos <- x[x > 0]
    
    hist(
      x,
      breaks = 40,
      col    = col_original,
      main   = paste("Histogram of", v),
      xlab   = v
    )
    
    hist(
      log10(x_pos),
      breaks = 40,
      col    = col_log,
      main   = paste("Log10-transformed histogram of", v),
      xlab   = paste("log10(", v, ")", sep = "")
    )
  }
}

par(mfrow = c(1, 1))  # reset


# Remove extreme negative outliers for Order Profit Per Order
data_clean <- data_clean %>%
  dplyr::filter(`Order Profit Per Order` > -2500)


dim(data_clean)


## New Variable 'WeekendOrder"
data_clean <- data_clean %>%
  mutate(
    # Day name from the final parsed order date
    Order_DayOfWeek = weekdays(order_date_dt),
    
    # Weekend flag: 1 = Saturday/Sunday, 0 = Monday–Friday
    WeekendOrder = ifelse(
      Order_DayOfWeek %in% c("Saturday", "Sunday"),
      1L,
      0L
    )
  )

# Quick check
table(data_clean$WeekendOrder)



## ProcessingDays = (shipping date – order date) in days

data_clean <- data_clean %>%
  mutate(
    Processing.Days = as.numeric(
      difftime(
        shipping_date_dt,
        order_date_dt,
        units = "days"
      )
    )
  )

# Quick check
summary(data_clean$Processing.Days)

# Check a few raw date pairs and the ProcessingTime result
data_clean %>%
  dplyr::select(
    order_date_dt,
    shipping_date_dt,
    Processing.Days
  ) %>%
  head(10)

dim(data_clean)


summary(data_clean$Processing.Days)

##distance
# install once if needed
install.packages("tidygeocoder")
install.packages("geosphere")

library(tidygeocoder)
library(geosphere)
library(dplyr)

#Extract unique delivery cities

## Cleaning city/ state/country to proper UTF-8
library(stringi)

library(dplyr)
library(stringi)

unique_locations <- data_clean %>%
  distinct(`Order City`, `Order State`, `Order Country`)

unique_locations_clean <- unique_locations %>%
  mutate(
    # create cleaned versions of the names
    Order.City    = stri_enc_toascii(`Order City`),
    Order.State   = stri_enc_toascii(`Order State`),
    Order.Country = stri_enc_toascii(`Order Country`),
    
    # build full address string
    full_address  = paste(Order.City, Order.State, Order.Country, sep = ", ")
  )



## geocode
geo_results <- unique_locations_clean %>%
  geocode(
    address = full_address,
    method = "osm",
    lat   = order_latitude,
    long  = order_longitude
  )
## adding ne
data_clean <- data_clean %>%
  left_join(
    geo_results %>%
      select(
        `Order City`,
        `Order State`,
        `Order Country`,
        order_latitude,
        order_longitude
      ),
    by = c("Order City", "Order State", "Order Country")
  )

# Quick check: how many NAs in customer_lat / customer_lon?
sum(is.na(data_clean$order_latitude))
sum(is.na(data_clean$order_longitude))


nrow(geo_results)                        # how many unique locations
sum(is.na(geo_results$order_longitude))  # how many locations have no lon
sum(!is.na(geo_results$order_longitude)) # how many locations succeeded

geo_results %>%
  dplyr::filter(is.na(order_longitude)) %>%
  dplyr::select(`Order City`, `Order State`, `Order Country`, full_address) %>%
  head(20)

names(data_clean)
# should include "order_latitude" and "order_longitude"

library(dplyr)

data_clean <- data_clean %>%
  left_join(
    geo_results %>%
      select(`Order City`, `Order Country`, order_latitude, order_longitude),
    by = c("Order City", "Order Country")
  )



# Drop rows with missing customer coords
data_clean %>%
  dplyr::select(
    `Order City`,
    `Order Country`,
    order_latitude,
    order_longitude
  ) %>%
  head(20)



n_total <- nrow(data_clean)


n_total      # total rows
n_missing_loc# rows that WOULD be dropped
n_keep       # rows that would remain


dim(data_clean)


names(data_clean)

# how many NAs in the x version
sum(is.na(data_clean$order_latitude.x))
sum(is.na(data_clean$order_longitude.x))

# how many NAs in the y version
sum(is.na(data_clean$order_latitude.y))
sum(is.na(data_clean$order_longitude.y))

n_missing_y <- sum(
  is.na(data_clean$order_latitude.y) |
    is.na(data_clean$order_longitude.y)
)

n_missing_y

data_clean <- data_clean %>%
  mutate(
    order_latitude_final  = coalesce(order_latitude.y, order_latitude.x),
    order_longitude_final = coalesce(order_longitude.y, order_longitude.x)
  )

sum(is.na(data_clean$order_latitude_final))
sum(is.na(data_clean$order_longitude_final))


n_total <- nrow(data_clean)

n_missing_loc <- sum(
  is.na(data_clean$order_latitude_final) |
    is.na(data_clean$order_longitude_final)
)

n_keep <- n_total - n_missing_loc

n_total       # total rows
n_missing_loc # rows that will be dropped
n_keep        # rows that will remain



data_clean <- data_clean %>%
  filter(
    !is.na(order_latitude_final),
    !is.na(order_longitude_final)
  )

nrow(data_clean)  # should equal n_keep


data_clean <- data_clean %>%
  filter(
    !is.na(order_latitude_final),
    !is.na(order_longitude_final)
  )


nrow(data_clean)

library(readr)

write_csv(
  data_clean,
  "data_clean_processed.csv"
)


getwd()




# Recompute Distance_km (just to be safe)
library(geosphere)
library(dplyr)

data_clean <- data_clean %>%
  mutate(
    Distance_km = geosphere::distHaversine(
      cbind(Longitude, Latitude),                     # warehouse coords
      cbind(order_longitude_final, order_latitude_final)  # order coords
    ) / 1000  # meters → km
  )

# Check Distance NAs
sum(is.na(data_clean$Distance_km))


## Final Prepriocessing

library(dplyr)

model_vars <- c(
  "WeekendOrder",
  "Processing.Days",
  "Distance_km",
  "Order Item Product Price",   # order price
  "Order Item Quantity",
  "Order Item Discount Rate",
  "Order Region",
  "Shipping Mode",
  "Late_delivery_risk"          # target
)

data_model <- data_clean %>%
  dplyr::select(all_of(model_vars))


str(data_model)
summary(data_model$Late_delivery_risk)



write_csv(
  data_model,
  "data_model.csv"
)


## One hot encoding


# This creates dummy variables for all factors except the target
# --- 0. Load packages ---
library(readr)
library(dplyr)

# --- 1. Read your existing CSV (adjust path if needed) ---
data_model <- read_csv("data_model.csv")

# --- 2. Make sure columns have the right types (no renaming) ---

data_model <- data_model %>%
  mutate(
    # numeric predictors
    WeekendOrder                 = as.numeric(WeekendOrder),
    `Processing.Days`            = as.numeric(`Processing.Days`),
    Distance_km                  = as.numeric(Distance_km),
    `Order Item Product Price`   = as.numeric(`Order Item Product Price`),
    `Order Item Quantity`        = as.numeric(`Order Item Quantity`),
    `Order Item Discount Rate`   = as.numeric(`Order Item Discount Rate`),
    
    # categorical predictors
    `Order Region`               = as.factor(`Order Region`),
    `Shipping Mode`              = as.factor(`Shipping Mode`),
    
    # target: make sure it's 0/1 then factor
    Late_delivery_risk = as.factor(
      as.numeric(as.character(Late_delivery_risk))
    )
  )



# 3. One-hot encode predictors with model.matrix() ---

# X = design matrix with dummy vars for factors (k-1 encoding), numeric columns unchanged
X <- model.matrix(
  Late_delivery_risk ~ .,
  data = data_model
)[, -1]   # drop intercept column

# y = target
y <- data_model$Late_delivery_risk

# --- 4. Combine back into a data frame and save to CSV ---

data_model_onehot <- cbind(as.data.frame(X), Late_delivery_risk = y)

write_csv(
  data_model_onehot,
  "dataco-preprocessing-final.csv"
)


