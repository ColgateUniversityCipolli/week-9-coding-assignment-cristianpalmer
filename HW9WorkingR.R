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

loglik_gamma <- -mles$value # Calculate loglik_gamma

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

loglik_lognormal <- -mles$value # Calculate loglik_lognormal

#########################################################################################################

"(1.c)  Compute the likelihood ratio to compare the Weibull and the Gamma distribution.  
Which has a better fit according to the likelhiood ratio?"

##########################################
#Maximum Likelihood of Weibull Distibution (Taken from rcode-lecture16.R)


llweibull <- function(par, data, neg=F){
  # a <- par[1]
  # sigma <- par[2]
  a <- exp(par[1]) # go from (-inf,inf) to (0,inf)
  sigma <- exp(par[2]) # go from (-inf,inf) to (0,inf)
  
  ll <- sum(log(dweibull(x=data, shape=a, scale=sigma)), na.rm=T)
  
  return(ifelse(neg, -ll, ll))
}

MLEs <- optim(fn = llweibull,
              par = c(1,1),
              data = dat.precip.long$Precipitation,
              neg=T)

(MLEs$par <- exp(MLEs$par)) # transform

loglik_weibull <- -MLEs$value # Calculate loglik_weibull
##########################################

# Compute the likelihood ratio to compare the Weibull and the Gamma distribution

(Q.WG = exp(loglik_weibull - loglik_gamma))

# Since the likelihood ratio comparing the Weibull and the Gamma distribution is less than one, it indicates that
# the Weibull distibution is a better fit than the Gamme distibution



#########################################################################################################

"(1.d)  Compute the likelihood ratio to compare the Weibull and the Log-Normal distribution.  
Which has a better fit according to the likelihood ratio?"

(Q.WLG = exp(loglik_weibull - loglik_lognormal))

# Since the likelihood ratio comparing the Weibull and the Gamma distribution is greater than one, it indicates that
# the Weibull distibution is a better fit than the Log-normal distibution.


#########################################################################################################

"(1.e)  Compute the likelihood ratio to compare the Gamma and the Log-Normal distribution.  
Which has a better fit according to the likelhiood ratio?"

(Q.GLG = exp(loglik_gamma - loglik_lognormal))

# Since the likelihood ratio comparing the Gamma and the Log-normal distribution is greater than one, it indicates that
# the Gamma distibution is a better fit than the Log-normal distibution.




