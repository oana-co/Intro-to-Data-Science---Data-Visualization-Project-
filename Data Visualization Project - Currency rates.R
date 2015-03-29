# Data Visualization Project

install.packages("Quandl")

library(Quandl)  				
library(ggplot2)				
library(reshape2)				

# Authenticate my token

token <- 'Hz46EzY5vu7U28MQrM7v'
Quandl("BNP/USDEUR", authcode="Hz46EzY5vu7U28MQrM7v")
read.csv("https://www.quandl.com/api/v1/datasets/BNP/USDEUR.csv?auth_token=Hz46EzY5vu7U28MQrM7v")
Quandl.auth(token)	

# Build vector of currencies
currencies <- c("ARS","AUD","BRL","CAD","CHF","CNY","DKK","EUR","GBP","IDR","ILS","INR","JPY","MXN","MYR","NZD","PHP","RUB","SEK","TRY")

# Function to fetch major currency rates
rdQcurr <- function(curr){
  # Construct Quandl code for first currency
  codes <- paste("QUANDL/",curr,"USD",sep="")
  for(i in 1:length(curr)){
    if (i == 1){
      # Select the date from the first currency
      d <- Quandl(codes[1],start_date="2000-01-01",end_date="2013-06-07" )[,1]
      A <- array(0,dim=c(length(d),length(codes)))
      # Get the rate fom the first curency
      A[,1] <- Quandl(codes[1],start_date="2000-01-01",end_date="2013-06-07" )[,2]
    }
    else{
      # Just get the rates for the remaining currencies
      A[,i] <- Quandl(codes[i],start_date="2000-01-01",end_date="2013-06-07" )[,2]
    }
  }
  df <- data.frame(d,A)
  names(df) <- c("DATE",curr)
  return(df)
}

# Fetch the currency rates

rates <- rdQcurr(currencies)	

# Make DATE into type Date
rates$DATE <- as.Date(rates$DATE)		

# Pick out some rates to plot

rates4 <- rates[,c(1,3:6)]

# Shape data for plottting
meltdf <- melt(rates4,id="DATE")		

# Generate plot
ggplot(meltdf,aes(x=DATE,y=value,colour=variable,group=variable)) + 
  geom_line() +
  scale_colour_manual(values=1:22)+
  ggtitle("Major Currency Exchange Rates in USD") 