# Load packages
library("surveillance")

################################################################################
# IN BETWEEN HERE THE CODE IS FROM AN OLD VERSION OF THE SURVEILLANCE PACKAGE IN
# R. Slightly changed to work with out problem.
#surveillance.gvar.hyp <- read.csv("Data/hypGeomSmall.txt")[, 1]
#surveillance.gvar.z <- - c(0:1000/100, 11:100)

library("hypergeo")
surveillance.gvar.z <- - c(0:1000/100, 11:150)
surveillance.gvar.hyp <- Re(sapply(surveillance.gvar.z, function(i)
  hypergeo::hypergeo(1/3, 2/3, 5/3, i)))


#####################################################################
# compute anscombe residuals for Y ~ NegBin(mu, alpha) using hypgeom2F1 function
# E(Y)= \mu, Var(Y) = \mu + \alpha*\mu^2
#################################################################
anscombeNB <- function(y, mu, alpha = 0.1) {
  par <- lapply(1:length(y), function(i) c(y[i], mu[i]))
  one <- function(par){
    y <- par[1]
    mu <- par[2]
    hypgeom.mu <- 3/2*mu^(2/3)*hypgeom2F1special(-alpha*mu)
    up <- 3/2*y^(2/3) * hypgeom2F1special(-alpha*y) - hypgeom.mu
    down <- (mu+alpha*mu^2)^(1/6)
    return(up/down)
  }
  return(sapply(par, one))
}

#####################################################################
# function to evaluate hypgeom_2F1(1/3,2/3, 5/3, x)
# "exact" values for x = -(0:10) and linear interpolation for x =- -(10:100)
####################################################################
hypgeom2F1special <- function(x) {
  #Return the z (the approximation grid), which is closest to x
  inner <- function(x){
    idx <- which.min(abs(surveillance.gvar.z-x))
    if(x >= -10){
      return(surveillance.gvar.hyp[idx])
    }else{
      # find out interval that contains x
      if((x-surveillance.gvar.z[idx]) < 0){
        idxLow <- idx +1
        idxUp <- idx
      } else {
        idxLow <- idx
        idxUp <- idx -1
      }
      #linear interpolation: f(x)=f(x0)+(f(x1)-f(x0))/1*(x-x0)
      #if(idxLow > 1091){
      #  return(surveillance.gvar.hyp[idxUp])
      #}
      surveillance.gvar.hyp
      return(surveillance.gvar.hyp[idxLow] +
               (surveillance.gvar.hyp[idxUp] -
                  surveillance.gvar.hyp[idxLow])*(x-surveillance.gvar.z[idxLow]))
      
    }
  }
  return(sapply(x, inner))
}
################################################################################

# Load model
load("Models/hhh4Model.RData")

# Shorter name
model <- hhh4Model4.wo.strat.od

# Get observations
y <- as.numeric(model$stsObj@observed[-1, ])
test <- sapply(-phi*y, hypgeom2F1special)
# Get predicted
mu <- as.numeric(model$fitted.values)

# Get dispersion
phi <- exp(-model$coefficients["-log(overdisp)"])

# Get Anscombe residuals
ans.resid <- anscombeNB(y, mu, phi)
save(ans.resid, file="Data/AnscombeResid.Rdata")
colors <- rep("black", length(ans.resid))
colors[na.idx] <- "red"

plot(ans.resid)
# Make base r histogram
hist(ans.resid, breaks = 100, freq = FALSE)
lines(seq(-5, 5, length.out = 100), dnorm(seq(-5, 5, length.out = 100)))

plot(as.numeric(residuals(model)), col = colors)



(1/3,2/3, 5/3, x)