#Arrivals to an Emergency Room
library(jsonlite)
er_json <- fromJSON("C:/Users/admin/Downloads/data/er_arrivals.json")
#convert column class from character to Date
er_json$date <- as.Date(er_json$date)
#Adding num_arrivals column
num <- lapply(er_json$times, function(x) length(x))
er_json$num_arrivals <- num

library(dplyr)
days <- seq(as.Date("1963-02-01"), 
            as.Date("1964-03-31"), by="days")
checker <- function(x) na_if(!x %in% er_json$date)
new_dates <- sapply(days, checker)

#Poissonness plot
Yk <- er_json$num_arrivals
k <- seq(0,4,1)
N <- 425
Xk <- table(Yk)
Xk <- as.vector(Xk)
#lfactorial = log(factorial)
phi <-lfactorial(k) + log(Xk/N)
#expression(phi[k])
#expression(hat(lambda) == 0.56)
lm_out <- lm(phi ~ k)
coefs <- coef(lm_out)
abline(a=coefs[1], b=coefs[2],lty=2)
mean(Yk)
lm1 <- lm(phi ~ k)
slope <- unname(coef(lm1)[2])
