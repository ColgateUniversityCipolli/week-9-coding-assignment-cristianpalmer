# HW 9
#Load Tidyverse
library(tidyverse)

#Load Non-Linear Equation Solver
library(nleqslv)

#Load and clean the data (taken from rcode-lecture16.R)
dat.precip <- read_csv(file = "agacis.csv")
dat.precip.long <- dat.precip |>    
  dplyr::select(-Annual) |>                   # Remove annual column 
  pivot_longer(cols = c(Jan, Feb, Mar, Apr,   # pivot the column data into one col
                        May, Jun, Jul, Aug, 
                        Sep, Oct, Nov, Dec), 
               values_to = "Precipitation",   # store the values in Precipitation
               names_to = "Month") |>         # store the months in Month
  mutate(Precipitation = case_when(Precipitation == "M" ~ NA_character_,
                                   TRUE                 ~ Precipitation))|>
  mutate(Precipitation = as.numeric(Precipitation)) |>
  filter(!is.na(Precipitation))  # Remove rows with NA values



#########################################################################################################

"(1.a)  Compute the MLEs for these data using a Gamma distribution"

llgamma <- function(data, par, neg=F){
  alpha <- par[1]
  beta <- par[2]
  
  loglik <- sum(log(dgamma(x=data, shape=alpha, rate=beta)))
  
  return(ifelse(neg, -loglik, loglik))
}

(mles <- optim(par = c(1,1),
               fn = llgamma,
               data=dat.precip.long$Precipitation,
               neg=T))
alpha.hat.mle <- mles$par[1]
beta.hat.mle <- mles$par[2]

#########################################################################################################

"(1.b)  Compute the MLEs for these data using the Log-Normal distribution"

lllognormal <- function(data, par, neg=F) {
  meanlog <- par[1]
  sdlog <- par[2]
  
  loglik <- sum(log(dlnorm(x=data, meanlog=meanlog, sdlog=sdlog)))
  
  return(ifelse(neg, -loglik, loglik))
}

(mles <- optim(par = c(0, 1),
               fn = lllognormal,
               data=dat.precip.long$Precipitation,
               neg=T))

meanlog.hat.mle <- mles$par[1]
sdlog.hat.mle <- mles$par[2]

#########################################################################################################

"(1.c)  Compute the likelihood ratio to compare the Weibull and the Gamma distribution.  
Which has a better fit according to the likelhiood ratio?"






#########################################################################################################

"(1.d)  Compute the likelihood ratio to compare the Weibull and the Log-Normal distribution.  
Which has a better fit according to the likelihood ratio?"






#########################################################################################################

"(1.e)  Compute the likelihood ratio to compare the Gamma and the Log-Normal distribution.  
Which has a better fit according to the likelhiood ratio?"







