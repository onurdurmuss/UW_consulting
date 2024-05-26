# Loading necessary libraries
library(DBI)

# Database connection function
connect_to_db <- function() {
  dbConnect(odbc::odbc(), "RetailDB")
}

# Load data function
load_data <- function(con) {
  markdown_hist <- dbGetQuery(con, "
    SELECT TOP 1000 * 
    FROM BI_Sandbox.dbo.BOARD_MarkdownHistory_Modified")
  
  product_info <- dbGetQuery(con, "
    SELECT TOP 100 * 
    FROM BI.DIM.Product_Modified
    WHERE ITEM_CODE IN ('134132RSM','136154BLM','136201WTM')
    ORDER BY ITEM_CODE, BARCODE")
  
  sales_records <- dbGetQuery(con, "
    SELECT mh.ITEM_CODE, mh.DISCOUNT_PERC, mh.START_DATE,
    SUM(CASE WHEN s.TRANSACTION_DATE BETWEEN mh.DATE_BEFORE_8WK AND mh.DATE_BEFORE THEN s.QUANTITY ELSE 0 END) AS QUANTITY_8WK_BEFORE,
    SUM(CASE WHEN s.TRANSACTION_DATE BETWEEN mh.DATE_BEFORE_4WK AND mh.DATE_BEFORE THEN s.QUANTITY ELSE 0 END) AS QUANTITY_4WK_BEFORE,
    SUM(CASE WHEN s.TRANSACTION_DATE BETWEEN mh.START_DATE AND mh.DATE_AFTER_8WK THEN s.QUANTITY ELSE 0 END) AS QUANTITY_8WK_AFTER,
    SUM(CASE WHEN s.TRANSACTION_DATE BETWEEN mh.START_DATE AND mh.DATE_AFTER_4WK THEN s.QUANTITY ELSE 0 END) AS QUANTITY_4WK_AFTER
    INTO #sales_data
    FROM BI.FACT.Sales_Data s
    JOIN BI.DIM.Calendar cal ON cal.DATE_ID = s.TRANSACTION_DATE
    JOIN BI.DIM.Product_Modified prod ON prod.PRODUCT_ID = s.PRODUCT_ID
    JOIN BI.DIM.Organization org ON org.ORG_ID = s.ORG_ID
    JOIN #markdown_details mh ON mh.ITEM_CODE = prod.ITEM_CODE AND s.TRANSACTION_DATE BETWEEN mh.DATE_BEFORE_8WK AND mh.DATE_AFTER_8WK
    WHERE s.TRANSACTION_DATE >= 20200201
    AND prod.season IN ('22ZZ','22WW','23ZZ','23WW')
    AND org.DATAAREA_ID = 'GF'
    AND LEN(org.STORE_ID) = 3
    AND prod.department = 'Womens'
    GROUP BY mh.ITEM_CODE, mh.DISCOUNT_PERC, mh.START_DATE")
  
  list(markdown_hist = markdown_hist, product_info = product_info, sales_records = sales_records)
}


# Data preprocessing function
preprocess_data <- function(sales_records) {
  sales_records %>%
    filter(QUANTITY_8WK_BEFORE > 0, QUANTITY_4WK_BEFORE > 0, QUANTITY_8WK_AFTER > 0, QUANTITY_4WK_AFTER > 0) %>%
    mutate(PE_4WK_CALC = (QUANTITY_4WK_AFTER / QUANTITY_4WK_BEFORE - 1) / DISCOUNT_PERC,
           PE_8WK_CALC = (QUANTITY_8WK_AFTER / QUANTITY_8WK_BEFORE - 1) / DISCOUNT_PERC)
}

# Model training function
train_model <- function(filtered_sales_data) {
  lm(QUANTITY_4WK_AFTER ~ DISCOUNT_PERC + PE_4WK_CALC, data = filtered_sales_data)
}
# Prediction function
predict_sales <- function(model) {
  discounts <- seq(0.1, 0.5, by = 0.1)
  predicted_sales <- data.frame(DISCOUNT_PERC = discounts)
  predicted_sales$PREDICTED_QUANTITY_4WK <- predict(model, newdata = predicted_sales)
  predicted_sales
}