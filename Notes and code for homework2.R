#============================================================================#
#============================================================================#
#====================== Note and code for the Homework 2 SDB2 ===============#
#============================================================================#
#============================================================================#

#---------------------------------- THE ENVIRONMENT -------------------------------------#

# Clear the environment 
rm(list=ls())

# Set your environment 
setwd("/Users/helen/Documents/BFH/7. Semestre/SBD2/Homework 2")

#---------------------------------- Library and data -------------------------------------#

# Install the package and call the library
#install.packages("readxl")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("corrplot")
#install.packages("dplyr")
#install.packages("ggcorrplot")
#install.packages("qgraph)
#install.packages("Hmisc)
install.packages("lubridate")
install.packages("knitr")
library(lubridate)
library(readxl)
library(ggplot2)
library(plotly)
library(corrplot)
library(dplyr)
library(ggcorrplot)
library(Hmisc)
library(qgraph)
library(readr)
library(car)



# Import the dataset into the environment.
# Call the data set "echanges.csv".
bitcoin_price <- read.csv("exchanges.csv")
data <- bitcoin_price
View(data)


#---------------------------------- 1 Exercise -------------------------------------#

# Determine the period covered by the data
period <- range(data$Date)
period

# What types of data are included in the dataset?
# Visualization of included data types
str(data)

# How to evaluate data quality? 
# Control of missing values for data quality
missing_values <- sum(is.na(data))
missing_values

# Missing values for each column
missing_values_columns <- colSums(is.na(data))
missing_values_columns

#The data quality is very good; no data are missing! 

# Mean and median for all numeric columns in the data set without specifying individual columns 
summary(data)

# List of numeric columns in the dataframe
numeric_columns <- sapply(data, is.numeric)

# Calculation of mean and median for each numeric column
for (column in names(data[numeric_columns])) {
  mean_val <- mean(data[[column]], na.rm = TRUE)
  median_val <- median(data[[column]], na.rm = TRUE)
  cat("Column:", column, "\nMean:", mean_val, "\nMedian:", median_val, "\n\n")
}


# Which variable shows the greatest variation over the observed period?

# List of numeric columns in the dataframe
numeric_columns <- sapply(data, is.numeric)

# Initializing variables to track columns with maximum and minimum standard deviation
max_deviation <- -1
min_deviation <- Inf
col_with_max_deviation <- ""
col_with_min_deviation <- ""

# Calculating standard deviation for each numeric column
for (column in names(data[numeric_columns])) {
  deviation <- sd(data[[column]], na.rm = TRUE)
  
  # Checking for maximum deviation
  if (deviation > max_deviation) {
    max_deviation <- deviation
    col_with_max_deviation <- column
  }
  
  # Checking for minimum deviation
  if (deviation < min_deviation) {
    min_deviation <- deviation
    col_with_min_deviation <- column
  }
}

# Displaying columns with maximum and minimum deviation
cat("Column with maximum deviation:", col_with_max_deviation, "\nMaximum deviation:", max_deviation, "\n\n")
cat("Column with minimum deviation:", col_with_min_deviation, "\nMinimum deviation:", min_deviation, "\n\n")


#---------------------------------- 2 Exercise -------------------------------------#

# Changing the format of dates
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

# Assuming you have a "Date" column in the dataset
data$Date <- as.Date(data$Date)


# Create the binary variable
data$Period <- ifelse(year(data$Date) < 2017, "Before", "After")
# Selects only the columns 'Date' and 'btc_coinbase'
bitcoin_data <- data[c('Date', 'btc_coinbase')]

# The 'Date' column converses in date format
bitcoin_data$Date <- as.Date(bitcoin_data$Date)

# Add a new 'Period' column based on the date
bitcoin_data <- bitcoin_data %>%
  mutate(Period = ifelse(year(Date) < 2017, "Before", "After"))


# Filtering data for the period before and after 2017 on Coinbase for Bitcoin
before_2017_coinbase <- subset(data, Date < as.Date("2017/01/01") & btc_coinbase > 0)
after_2017_coinbase <- subset(data, Date >= as.Date("2017-01-01") & btc_coinbase > 0)
before_2017_coinbase
after_2017_coinbase

# Calculate the average price before and after 2017 on Coinbase for Bitcoin
mean_before_2017_coinbase <- mean(before_2017_coinbase$btc_coinbase)
mean_after_2017_coinbase <- mean(after_2017_coinbase$btc_coinbase)
mean_before_2017_coinbase
mean_after_2017_coinbase

# Results
cat("Mean price of Bitcoin on Coinbase before 2017:", mean_before_2017_coinbase, "\n")
cat("Mean price of Bitcoin on Coinbase after 2017:", mean_after_2017_coinbase, "\n")


# Calculate the difference between Bitcoin prices on Coinbase and Kraken
data$Difference <- data$btc_coinbase - data$btc_kraken
data$Difference

# Find the day with the largest difference
day_of_max_difference <- data$Date[which.max(data$Difference)]
# Format the date as dd/mm/yyyy
formatted_date <- format(day_of_max_difference, "%d/%m/%Y")


# Display the day with the largest difference in dd/mm/yyyy format
cat("The day with the largest difference between Coinbase and Kraken prices:", formatted_date, "\n")




#---------------------------------- 3 Exercise -------------------------------------#

# Date column is in the Date format
data$Date <- as.Date(data$Date)

# Line graph of bitcoin price on Coinbase
ggplot(data, aes(x = Date, y = btc_coinbase)) +
  geom_line(color = "red", size = 1.0) + 
  labs(title = "Bitcoin price trend on Coinbase",
       x = "Date",
       y = "Bitcoin price in dollars") +
  theme_minimal()


# Bitcoin price histogram chart on Coinbase
ggplot(data, aes(x = btc_coinbase)) +
  geom_histogram(binwidth = 500, fill = "magenta", color = "black") + 
  labs(title = "Bitcoin price distribution on Coinbase",
       x = "Bitcoin price in dollars",
       y = "Frequency") +
  theme_minimal()



# Bitcoin price boxplot on Coinbase
ggplot(data, aes(y = btc_coinbase)) +
  geom_boxplot(fill = "lightpink", color = "black") + 
  labs(title = " Bitcoin price distribution on Coinbase ",
       y = "Bitcoin price in dollars") +
  theme_minimal()

#Bitcoin price boxplot on Coinbase more accurate
ggplot(data, aes(y = btc_coinbase)) +
  geom_boxplot(fill = "lightpink", color = "black") + 
  labs(title = " Bitcoin price distribution on Coinbase ",
       y = "Bitcoin price in dollars") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 5000))


#- What can be said about the price of Bitcoin over the observed period?



#---------------------------------- 4 Exercise -------------------------------------#

# Plot and regression line plotting - Bitcoin and oil relationship
ggplot(data, aes(x = oil, y = btc_coinbase)) +
  geom_point(color = "blue") +  
  labs(title = "Relationship between the price of Bitcoin and the price of oil",
       x = "Oil price",
       y = "Bitcoin price on Coinbase") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "red")  

# What can be said about the dependence between these two assets?



#Correct columns
bitcoin_gold_data <- data[c('btc_coinbase', 'gold')]

# #Plot and regression line plotting - Bitcoin and gold relationship
ggplot(bitcoin_gold_data, aes(x = gold, y = btc_coinbase)) +
  geom_point(color = "black") +  
  labs(title = "Dependence between the price of Bitcoin and the price of gold",
       x = "Gold price",
       y = "Bitcoin price on Coinbase") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "magenta")  

# What can be said about the dependence between these two assets?



#Correct columns
bitcoin_gold_data <- data[c('btc_coinbase', 'gold')]

# Plotly
plot_ly(data = bitcoin_gold_data, x = ~gold, y = ~btc_coinbase, type = 'scatter', mode = 'markers', marker = list(color = 'blue', size = 3)) %>%
  layout(title = "Dependence between the price of Bitcoin and the price of gold",
         xaxis = list(title = "Gold price"),
         yaxis = list(title = "Bitcoin price on Coinbase"),
         showlegend = FALSE) %>%
  add_lines(x = ~gold, y = ~fitted(lm(btc_coinbase ~ gold, data = bitcoin_gold_data)), line = list(color = 'red'), inherit = FALSE)



# Multiple regression model using the price of oil, gold and the SP500
model <- lm(btc_coinbase ~ oil + gold + sp500, data = data)

# Summary of the model
summary(model)

#Descrizione dei risultati 
#Residuals: Questa sezione mostra le statistiche sui residui del modello, ovvero la differenza tra i valori osservati e quelli predetti dal modello. La colonna "Median" rappresenta la mediana dei residui, mentre "Min" e "Max" indicano il valore minimo e massimo.
#Coefficients: Questa sezione contiene gli stimatori dei coefficienti per le variabili nel modello. Ogni riga rappresenta una variabile indipendente inclusa nel modello. Gli "Estimate" sono i valori stimati per i coefficienti di ogni variabile. "Std. Error" indica la deviazione standard stimata per ciascun coefficiente. Il "t value" è il rapporto tra l'estimatore e la sua deviazione standard stimata. Infine, "Pr(>|t|)" indica il valore p, che indica la significatività statistica del coefficiente. Ad esempio, un valore p basso (come < 0.001) indica una maggiore significatività statistica.
#Residual standard error: Questo valore rappresenta la stima della deviazione standard dei residui. Indica la dispersione media dei punti dati intorno alla linea di regressione.
#Multiple R-squared: Questo valore rappresenta la proporzione di varianza nella variabile dipendente (btc_coinbase) che viene spiegata dalle variabili indipendenti (oil, gold, sp500). Un valore vicino a 1 indica che le variabili indipendenti spiegano una grande parte della variazione nella variabile dipendente.
#F-statistic: Questo valore indica se il modello nel suo complesso è statisticamente significativo. Un valore di F-statistic elevato con un valore p basso suggerisce che almeno una delle variabili indipendenti ha un effetto significativo sulla variabile dipendente.

#In questo specifico modello:
#Il prezzo dell'oro (gold) e l'indice SP500 (sp500) sembrano essere statisticamente significativi (p-value molto basso), con coefficienti positivi. Questo suggerisce che un aumento nel prezzo dell'oro o nell'indice SP500 è associato a un aumento nel prezzo del Bitcoin su Coinbase.
#Il prezzo del petrolio (oil) sembra avere un impatto meno significativo, ma comunque negativo (coefficiente negativo) sul prezzo del Bitcoin su Coinbase. Un valore p più alto suggerisce una minore significatività statistica rispetto agli altri predittori.
#La percentuale di varianza spiegata (Adjusted R-squared) è del 75%, il che indica che questo modello spiega efficacemente il 75% della variazione nel prezzo del Bitcoin su Coinbase utilizzando le variabili indipendenti considerate


# Multiple regression model
model <- lm(btc_coinbase ~ oil + gold + sp500, data = data)

# Plotting avPlots graphs for independent variables
avPlots(model)


#---------------------------------- 5 Exercise -------------------------------------#


# Select only numeric columns.
numeric_columns <- data[, sapply(data, is.numeric)]

# Calculate the correlation matrix
corr <- cor(numeric_columns, use = "complete.obs")
numeric_columns <- data[, sapply(data, is.numeric)]

#Lower half of the correlation matrix
ggcorrplot(corr, type = "lower", method = "circle")


# Calculate correlations between numerical variables
numeric_columns <- data[, sapply(data, is.numeric)]
corr <- cor(numeric_columns, use = "complete.obs")

# Interactive heat map with plotly
plot_ly(z = corr, type = "heatmap") %>%
  layout(title = "Heat map of correlations between numerical variables",
         x = colnames(corr), y = colnames(corr),
         xgap = 1, ygap = 1)
