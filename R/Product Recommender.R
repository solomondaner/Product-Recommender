# Load Packages----
library(data.table)
library(recommenderlab)

# Read in Data----
sales_data  <- data.table :: fread("/Users/solod/Documents/Data Science Projects/Product Recommenders/Product recommender/Sales.csv")

# Scan for Common Data Quality Issues such as Feature Names, Cancelled Orders, Negative Quantity Ordered, Duplicate Rows
names(sales_data)
DataQualityIssues <- sales_data[substring(sales_data$ORDERNUMBER,1,1) == "C" | QUANTITYORDERED <= 0 | duplicated(sales_data)]

# Clean Data----
data.table::setnames(sales_data,old = c("QUANTITYORDERED","PRODUCTCODE","CUSTOMERNAME"),new =c("QuantityOrdered","ProductCode","CustomerName"))
sales_data_clean <- sales_data[substring(sales_data$ORDERNUMBER,1,1) != "C" & QuantityOrdered > 0 & !duplicated(sales_data)]

# Check Clean Data via Row Count----
sales_data[,.N]
DataQualityIssues[,.N]
sales_data_clean[,.N]

# Apply Pareto Principle to Reduce Dimensionality and Increase Runtime Performance----
# Summarize QuantityOrdered by StockCode then Sort Descending by QuantitySold
data.table::setorder(sales_data_clean,-QuantityOrdered,ProductCode)

# Add a Cumulative Sum Column
sales_data_clean[,CumQuantity := cumsum(QuantityOrdered)]

# Add a Cumulative Percent Column
sales_data_clean[,CumQuantityPercent := CumQuantity/sum(CumQuantity)]

# Filter Products that don't meet the Pareto Threshold (ie don't make up .06% of total quantities sold)
sales_data_clean_pareto = sales_data_clean[CumQuantityPercent <= .0006]

# Create A Binary Rating Matrix----
# Ensure Proper data types: CustomerName and ProductCode is chr, QuantityOrdered is int
str(sales_data_clean_pareto)

# Create a Matrix
Matrix <- data.table::dcast(sales_data_clean_pareto,CustomerName ~ ProductCode,
                           value.var = "QuantityOrdered",fun.aggregate = sum)

# Covert Quantity into Binary
for(j in 2:ncol(Matrix)){
  data.table::set(Matrix, which(Matrix[[j]] > 0), j, 1)
  data.table::set(Matrix, which(Matrix[[j]] <= 0), j, 0)
}

# Store CustomerName for Rownames
Matrix_rownames <- Matrix[["CustomerName"]]

# Remove CustomerName Column
Matrix[,CustomerName := NULL]

# Convert Data Table into a Binary Matrix
BinaryMatrix <- as.matrix(Matrix)

# Set Rownames
row.names(BinaryMatrix) <- Matrix_rownames

# Create a Binary Rating Matrix
BinaryRatingMatrix <- methods::as(object = BinaryMatrix, Class = "binaryRatingMatrix")

# Find the Winning Model----
# Evaluation Scheme
scheme <- evaluationScheme(BinaryRatingMatrix,
      method = "split",train =.75,k=1,given=3,goodRating=1)

# Store Algorithms 
algorithms <- list(
  "RandomItems"  = list(name = "RANDOM",  param = NULL),
  "PopularItems" = list(name = "POPULAR", param = NULL),
  "UserBasedCF" = list(name = "UBCF",    param = NULL),
  "ItemBasedCF" = list(name = "IBCF",    param = NULL),
  "AssociationRules" = list(
    name = "AR",
    param = list(support = 0.001, confidence = 0.05)
  ))
  
# Evauluate Predicted Ratings from each Algorithm
results <- recommenderlab::evaluate(x      = scheme,     # evaluation scheme
                                   method = algorithms, # list from above
                                   type   = "topNList", # or "ratings".
                                   n      = 1:5)       # number of recommendations

# Plot ROC Curve (run this line twice)
ROC_plot <- plot(results, legend="topright")

# Average the Confusion Matrix
store <- list()
for (i in 1:length(results)) {
  temp <- data.table(recommenderlab::avg(results)[[i]])
  temp[, model := results[[i]]@method]
  temp[, n_products := seq(1:5)]
  store[[i]] <- temp
}

# Collect Results into one Datatable
x <- rbindlist(store)

# Pick Winning Model based Max TPR for 5th Recommendation
WinningModel <- x[n_products == 5][order(-TPR)][1, "model"][[1]]

# Score The Model (Use the Winning Model to Output Product Recommendations for each Customer)----
# Setup Winning Model and Arguments
if (WinningModel == "AR") {
  recommender <- recommenderlab::Recommender(
    data = BinaryRatingMatrix,
    method = "AR",
    parameter = list(support = 0.001,
                     confidence = 0.05)
  )
} else {
  recommender <- recommenderlab::Recommender(data = BinaryRatingMatrix,
                                             method = WinningModel)
}

# Score The Model
data <- recommenderlab::predict(recommender,
                                BinaryRatingMatrix,
                                type = "topNList",
                                n = 5)

# Data Transformations
product_recommendations <- as(data, "list")
results <- data.table::data.table(data.table::melt(product_recommendations))
data.table::setcolorder(results, c(2, 1))
data.table::setnames(results,
                       c("L1", "value"),
                       c("CustomerName", "ProductCode"))

# Add ProductRank and TimeStamp
results[, ProductRank := seq_len(.N), by = c("CustomerName")]
results[, ':=' (TimeStamp = as.character(Sys.time()))]

# Export Data as CSV File
write.csv(results,file = "/Users/solod/Desktop/Output.csv", row.names = F)

