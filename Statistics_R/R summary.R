##########################################################
# Chapter 2: Basic R                                     #
##########################################################

# Making a logical vector
X <- 1:5
log.vec <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
X[log.vec]

# Mean of vector
mean(log.vec)

# Execute script
source('r_prog_02_script_01.R', echo=TRUE)

# Operations
# 1. Arithmetic 
# +, -, *, /, %%, %/%, ^, %*%
# 2. Scientific
# exp, sin, cos, tan, atan, log, beta, gamma, factorial
# 3. Utility
# floor, ceiling, round, signif
# 4. Statistic
# mean, var, cov, cor
# 5. Logical
# <, <=, >, >=, ==, !=, !, |, &

# Sample 1000 Uniform rv
U <- runif(1000)
mean(U)
var(U)

# Simulate exponential rv from uniform rv
transform_unif <- function(U, lambda) {
  E <- -(1/lambda) * log(U)
  E
}

transform_unif(runif(1), 2)

# Vector arithmetic
x <- c(1.2, 3, 4, 5); y <- c(1,2)
x*2 + y

# Matrix arithmetic
X <- matrix(c(1,2,3,4), nrow=2)
Y <- c(-1,-2)
X + Y

# Vectorizing scalar for functions
f <- function(x) {
  if(x < 0.5) 
    return(TRUE) 
  else 
    return(FALSE)
}
f(c(0.4, 0.6, 0.9)) # Does not do what we want it to
f <- Vectorize(f)
f(c(0.4, 0.6, 0.9))

# Generate 50% x=-1, 50% x=uniform~[0,1]
generate_f <- function(u) {
  if(u <= 0.5)
    return(-1) 
  else {
    return(runif(1))
  }
}

U <- runif(100000)
X <- sapply(U, generate_f) # use of sapply
quantile(X)

# g(x)=2x with x=max(U1,U2)
Umat <- matrix(runif(2000), ncol=2)
X <- apply(Umat, 1, max)
hist(X, freq=FALSE, col="cadetblue2")

# Demo of apply function
M <- matrix(runif(100), 10, 10)
round(apply(M, 1, mean), 3) # row-wise
round(apply(M, 2, mean), 3) # col-wise

# 'For' vs 'apply'
set.seed(13) # for reproducibility
m <- matrix(runif(100000), ncol=10)
max1 <- vector(mode="numeric", length=10000)
system.time({ # records time needed for function to run
  for(i in 1:10000) { 
    max1[i] <- max(m[i,]) }
})
system.time(max2 <- apply(m, 1, max))

# Repeat loop
count <- 0
repeat{
  count <- count + 1
  print(count)
  if(count >= 10){
    break
  }
}

# Tutorial 1 Qn 9
set.seed(13)
n.iter <- 0
repeat{
  X.Y <- runif(2, 0,5)
  n.iter <- n.iter + 1
  if(X.Y[1] < X.Y[2]) {
    print(X.Y[2])
    break
  }
}

# While loop
count <- 0
while(count < 10) {
  count <- count + 1
  print(count)
}

# Tutorial 1 Qn 10
set.seed(13)
n.iter <- 0
flag <- TRUE
while(flag){
  X.Y <- runif(2, 0,5)
  n.iter <- n.iter + 1
  if(X.Y[1] < X.Y[2]) {
    flag <- FALSE
    print(X.Y[2])
  }
}

# Basic scatter plot
set.seed(1)
X <- rnorm(100)
Y <- rnorm(100)
plot(X, Y, main='Scatter plot', 
     col='burlywood2', pch=20, 
     cex=1.9)
abline(a=0,b=0, lty=2, lwd=2, 
       col='cyan')

# Basic histogram plot
set.seed(1)
X <- rnorm(100)
hist(X, main="Histogram", freq=FALSE, breaks=15, 
     col='tomato3', border='white')
curve((1/sqrt(2*pi))* exp(-0.5*x^2), -3, 3, col='slateblue4', lwd=2, add=TRUE)

##########################################################
# Chapter 3: PRNG                                        #
##########################################################

# Frequency test
freqTest <- function(U, k) {
  n <- length(U)
  k.intervals <- seq(from=0, to=1, length=k+1)
  j.interval <- findInterval(U, k.intervals)
  f.j <- tabulate(j.interval, k)
  T1 <- (k/n)*sum((f.j - n/k)^2)
  pchisq(T1, df=k-1, lower.tail=FALSE)
}
set.seed(123)
U <- runif(10000)
hist(U, col="rosybrown2", freq=FALSE, main="Histogram from R")
freq.test <- freqTest(U, 1000) 
round(freq.test, 2)

#Auto-correlation test
autoCorrTest <- function(U, lag) {
  n <- length(U)
  h <- floor((n-1)/lag)-1
  id.vec <- 1 + (0:h)*lag
  U.sub <- U[id.vec]
  rho.hat <- (12/(h+1))*sum(U.sub[1:h]*U.sub[2:(h+1)]) - 3
  var.rho.hat <- (13*h+7)/(h+1)^2
  T3 <- rho.hat/sqrt(var.rho.hat)
  2*pnorm(abs(T3), mean=0, sd=1, lower.tail=FALSE)
}
autoCorrTest(U,1)
autoCorrTest(U,2)
autoCorrTest(U,3)
autoCorrTest(U.lcg,1)
autoCorrTest(U.lcg,2)
autoCorrTest(U.lcg,3)

# Example of a good LCG
Z <- c(0,3,2,13,4,7,6,1,8,11,10,5,12,15,14,9)
Z <- rep(Z, times=1000)
U.lcg <- Z/16
hist(U.lcg, col="rosybrown2", freq=FALSE, main="Histogram from LCG", breaks=50)
args(freqTest)
freq.test <- freqTest(U.lcg, 1000)
round(freq.test, 2)

# General LCG Code
lcg <- function(n,a,m,c,x0){
  ran <- numeric(n)
  for(i in 1:n){
    x1 <- (a*x0+c)%%m
    x0 <- x1
    ran[i] <- x1/m
  }
  return(ran)
}
U.lcg = lcg(10000,397204094,2^31-1,182361,1234)
freq.test <- freqTest(U.lcg, 1000)
round(freq.test, 2)
autoCorrTest(U.lcg,1)
autoCorrTest(U.lcg,2)
autoCorrTest(U.lcg,3)

##########################################################
# Chapter 4: Discrete rv                                 #
##########################################################

# Inversion method
p.i <- dbinom(0:10, 10, 0.3)
F.i <- cumsum(p.i)
plot(stepfun(0:10, c(0, F.i)), main="Sequential inversion method", ylab="F", xlab="i",
     verticals=FALSE, lwd=2, pch=20, cex=2, xaxt='n')
axis(1, at=0:10)
abline(h=F.i, lty=3, col='gray')
xbot <- rep(-1, 11)
ybot <- c(0, F.i[-11])
xright <- rep(-0.5, 11)
ytop <- F.i
rect(xbot, ybot, xright, ytop, col=c("chartreuse4", "steelblue"),
     border="yellow")
points(c(-0.75,-0.75), c(0.5, 0.8), col='white', pch=4, cex=1.9)
text(-0.5, c(0.5,0.8), c("Set X=3", "Set X=4"), pos=4, col='violetred', cex=2.5)

#Naive Poisson Distribution
invertPoisNaive <- function(U, lambda) {
  X <- 0
  S <- exp(-lambda)
  while(U > S) {
    X <- X + 1
    S <- S + exp(-lambda)*(lambda^X)/factorial(X)  
  }
  return(X)
}
invertPoisNaive <- Vectorize(invertPoisNaive, "U")
set.seed(2013)
system.time(out <- invertPoisNaive(runif(1e+04), 5))
set.seed(2014)
system.time(out <- invertPoisNaive(runif(1e+04), 75))

# Inversion truncation
p.i <- rep(1/10, 10)
F.i <- cumsum(p.i)
plot(stepfun(0:9 , c(0, F.i)), main="Inversion by truncation", sub="Discrete
uniform with K=10 values", ylab="F", xlab="i", verticals=FALSE, lwd=2, pch=20,
     cex=2, col="salmon3")
segments(0,0, 10, 1, col='palegreen', lwd=2)
legend("topleft", lwd=2, lty=1, col=c('salmon', 'palegreen'), legend=c('F','G'))

# Geometric distribution with inversion
q <- 0.3
i <- 1:10
p.i <- (1-q)*q^(i-1)
F.i <- cumsum(p.i)
plot(stepfun(1:10 , c(0, F.i)), main="Inversion by truncation",
     sub="Geometric(p)", ylab="F", xlab="i", verticals=FALSE, lwd=2, pch=20,
     cex=2, col="salmon3")
curve(1 - q^(x-1), from=1, to=10, add=TRUE, col="palegreen", lwd=2)
legend("bottomright", lwd=2, lty=1, col=c('salmon', 'palegreen'), legend=c('F','G'))

# Simulate geometric rv from uniform rv
set.seed(109)
system.time({
  U <- runif(10000);
  X <- floor(1 + (log(1-U))/log(q))})

# Rejection method
library(lattice)
n.display <- 10
p.i <- function(i) 6/(pi^2 * i^2)
q.i <- function(i) 1/(i*(i+1))
c.val <- 12/pi^2
p.val <- p.i(1:n.display)
c.q <- c.val*q.i(1:n.display)
dat <- data.frame(ht = c(p.val, c.q), 
                  grp=rep(c("p","c.q"), each=10), x=rep(1:10, 2))
barchart(ht ~ x, groups=grp, data=dat, horizontal=FALSE, 
         auto.key=list(space='right', cex=0.8), box.ratio=3, 
         main="Target and dominating distributions", ylab="")

# Geometric interpretation of the alias method
plot(0.5,0.5,xaxt='n', yaxt='n',  type='n', xlim=c(0,1), ylim=c(0,1), xlab='U',
     ylab='V')
polygon(c(0,0,1/3,1/3,0), c(0,5/8,5/8,0,0), col='palevioletred')
polygon(c(0,0,1/3,1/3,0), c(5/8,1,1,5/8,5/8), col='indianred')
polygon(c(1/3,1/3,2/3,2/3,1/3), c(0,14/32,14/32,0,0), col='lightblue')
polygon(c(1/3,1/3,2/3,2/3,1/3), c(14/32,1,1,14/32,14/32), col='royalblue1')
polygon(c(2/3,2/3,1,1,2/3), c(0,22/32,22/32,0,0), col='lawngreen')
polygon(c(2/3,2/3,1,1,2/3), c(22/32,1,1,22/32,22/32), col='seagreen3')
text(0.16, 0.15, labels=expression(Q[0]^(1)==5/8))
text(0.16, 0.90, labels=expression(Q[2]^(1)==3/8))
text(0.50, 0.15, labels=expression(Q[1]^(2)==14/32))
text(0.50, 0.90, labels=expression(Q[3]^(2)==18/32))
text(0.85, 0.15, labels=expression(Q[0]^(3)==22/32))
text(0.85, 0.90, labels=expression(Q[1]^(3)==10/32))
title('Alias Method for Example 10')
axis(side=2, at=c(0, 1))
axis(side=1, at=c(0, 0.333, 0.666, 1))

# Alias Method
vec.len <- c(100, 250, 500)
#n.samples <- 500
#n.samples <- c(1e+02, 1e+03, 1e+04, 1e+05, 1e+06)
n.samples <- 50000 # activate this in final version!
time.seq <- rep(0, length(vec.len))
time.alias <- rep(0, length(vec.len))
set.seed(89)

aliasSetup <- function(prob.vec) { # Setting up and generating from alias table
  # Assumes all values have non-zero probability
  n <- K <- length(prob.vec)
  P <- prob.vec
  Q.table <- matrix(0, nrow=K-1, ncol=3)
  
  for(m in 1:(K-2)) {
    id <- which(P > 0 & P < 1/(n-1))
    i <- id[which.min(P[id])] - 1
    j <- which.max(P) - 1
    Q.i <- P[i+1]*(n-1)
    Q <- rep(0, K)
    Q[i+1] <- Q.i
    Q[j+1] <- 1 - Q.i
    Q.table[m,] <- c(i, j, Q.i)
    P[i+1] <- 0
    P[-(i+1)] <- ((P - (1/(n-1))*Q)*(n-1)/(n-2))[-(i+1)]
    n <- n-1
  }
  id <- which(P > 0 & P < 1/(n-1))
  i <- id[which.min(P[id])] - 1
  j <- which.max(P) - 1
  Q.table[K-1,] <- c(i,j, P[i+1])
  colnames(Q.table) <- c("i", "j", "Q.stage.i")
  Q.table
}

aliasGenerate <- function(size, alias.table) { # Getting output from alias table
  n <- nrow(alias.table) + 1
  U <- runif(size)
  V <- runif(size)
  row.num <- floor((n-1)*U) + 1
  out <- ifelse(V <= alias.table[row.num, 3], alias.table[row.num, "i"],
                alias.table[row.num, "j"])
  out
}

invertDiscreteCdfDirect <- function(U, prob.vec) { # To invert the pmf directly
# Given one uniform draw, this returns one draw from the discrete distribution.
  p.sum <- prob.vec[1]
  for(ii in 1:(length(prob.vec)-1)) {
    if(U <= p.sum) 
      return(ii) else
        p.sum <- p.sum + prob.vec[ii+1]
  }
  return(length(prob.vec))
}

invertDiscreteCdfDirect <- Vectorize(invertDiscreteCdfDirect,vectorize.args="U")

# Sequential inversion vs alias method
for(ii in length(vec.len)) {
  prob.vec <- runif(vec.len[ii])
  prob.vec <- prob.vec/sum(prob.vec)
  time.seq[ii] <- system.time(out <- invertDiscreteCdfDirect(runif(n.samples),
                                                             prob.vec))[3]
  time.alias[ii] <- system.time({
    alias.table.1 <- aliasSetup(prob.vec);
    out <- aliasGenerate(n.samples, alias.table.1)})[3]
}
time.seq
time.alias

##########################################################
# Chapter 5: Continuous rv                               #
##########################################################

# Numerical Inversion to generate from N(0,1)
f <- function(X, U) pnorm(X) - U
invert_f <- function(U) {
  if(U <= 0.5) {
    out <- uniroot(f, c(-7, 0), U=U)
  } else {
    out <- uniroot(f, c(0, 7), U=U)
  }
  out$root
}
set.seed(11)
U <- runif(1000)
X <- sapply(U, invert_f)
opar <- par(mfrow=c(1,2))
hist(X, col='moccasin')
qqnorm(X, col='yellowgreen')
qqline(X)

# Plotting a Beta(2, 1.3) graph
curve(dbeta(x, 2, 1.3), 0, 1, main="The Beta(2, 1.3) distribution", ylab='f',
      ylim=c(0,2), lwd=2)
polygon(c(0,0,1,1), c(0,1.5,1.5,0), border='thistle3', lty=2, lwd=2)
set.seed(1)
u1 <- runif(40, 0, 1)
u2 <- runif(40, 0, 1.5)
id <- which(u2 <= dbeta(u1, 2, 1.3))
points(u1[-id], u2[-id], pch=5, col='orangered')
points(u1[id], u2[id], pch=5, col='darkgreen')
legend('topleft', lty=c(1,2), col=c('black', 'thistle3'), legend=c('Beta(2, 1.3)',
                                                                   'Larger Box'))
legend('topright', pch=5, col=c('darkgreen', 'orangered'), legend=c('Accepted', 'Rejected'))

# Plotting a Beta(2, 4) graph
curve(dbeta(x, 2, 4), 0, 1, main="The Beta(2, 4) distribution", ylab='f,cg',
      ylim=c(0,4), lwd=3, col="grey81", xlab='x')
abline(h=2.5, lwd=1.8, lty=2, col="greenyellow")
abline(h=4, lwd=1.8, lty=3, col="turquoise2")
legend('topleft', lty=c(1,2,3), col=c('grey81', 'greenyellow', "turquoise2"), 
       legend=c('Beta(2, 4)', '2.5g', '4g'))

# Rejection method to generate from Beta(2, 4)
nsim <- 10000
set.seed(24)
U <- runif(nsim); V <- runif(nsim)
keep.index <- ifelse(U <= (256/27)*V*(1-V)^3, TRUE, FALSE)
# keep.index <- (U <= (256/27)*V*(1-V)^3)
X <- V[keep.index]
# number of iterations per success:
nsim/sum(keep.index)
hist(X, freq=FALSE, main='Rejection Method Samples', 
     border='royalblue3', xlim=c(0,1), col='darkorchid3')
curve(dbeta(x, 2, 4), 0, 1, lwd=3, col="grey81", add=TRUE)

# Plotting a Gamma(1.5, 1)
curve(dgamma(x, 1.5, 1), 0, 4, main="The Gamma(1.5, 1) distribution", ylab='f,g',
      ylim=c(0,0.6), lwd=2, col="grey87", xlab='x')
curve(dexp(x, rate=2/3), 0, 4, lwd=2, lty=2, col="lightpink3", add=TRUE)
legend('topleft', lty=c(1,2), col=c('grey87', 'lightpink3'), 
       legend=c('Gamma(1.5, 1)', 'Exp(2/3)'))

# Rejection method to generate from Gamma(1.5, 1)
nsim <- 10000
set.seed(1512)
U <- runif(nsim); V <- runif(nsim)
Y <- -1.5*log(V)
keep.index <- U <= sqrt(2*exp(1)*Y/3)*exp(-Y/3)
X <- Y[keep.index]
# number of iterations per success:
nsim/sum(keep.index)
hist(X, freq=FALSE, main='Rejection Method Samples', 
     border='plum2', xlim=c(0,10), ylim=c(0,0.6),
     col='dodgerblue')
curve(dgamma(x, 1.5, 1), 0, 12, lwd=3, col="thistle4", 
      add=TRUE)

# Rejection method to generate from Normal(0, 1)
nsim <- 10000
set.seed(10)
U1 <- runif(nsim); U2 <- runif(nsim); U3 <- runif(nsim)
Y <- -log(U1)
keep.index <- U2 <= exp(-0.5*(Y-1)^2)
X <- Y[keep.index]
X <- ifelse(U3 <= 0.5, -X, X)
# number of iterations per success:
nsim/sum(keep.index)
hist(X, freq=FALSE, main='Rejection Method Samples', 
     border='lemonchiffon1', xlim=c(-3,3), col='goldenrod4')
curve(dnorm(x, 0, 1), -3, 3, lwd=3, col="grey76", 
      add=TRUE)

# Plotting a Gamma(2, 1) and truncated Gamma(2, 1)
opar <- par(mfrow=c(1,2))
set.seed(112)
X <- rgamma(5000, shape=2, rate=1) 
hist(X, col="royalblue2", main="Histogram of Gamma(2,1)", xlim=c(0,10),
     freq=FALSE, xlab="Y")
hist(X[X >= 5], col="royalblue2", main="Histogram of Conditional Gamma(2,1)",
     xlim=c(0,10), freq=FALSE, xlab="X")
par(opar)

# Rejection method to generate from truncated Gamma(2, 1)
nsim <- 10000
set.seed(10)
U1 <- runif(nsim); U2 <- runif(nsim)
Y <- 5 - 2*log(U1)
keep.index <- U2 <= 0.2*Y*exp(-Y/2)*exp(5/2)
X <- Y[keep.index]
# number of iterations per success:
nsim/sum(keep.index)
hist(X, freq=FALSE, main='Rejection Method Samples', 
     border='slateblue3', col='grey76')
curve(dgamma(x, 2, 1)/(6*exp(-5)), 5, 14, lwd=3, 
      col="lavenderblush2", add=TRUE)

# Generating from a polynomial density
nsim <- 10000
set.seed(35)
U0 <- runif(nsim)
genX <- function(u) {
  ifelse(u <= 0.25, (runif(1))^(1/4), (runif(1))^(1/6))
}
X <- sapply(U0, genX)
hist(X, freq=FALSE, main='Mixture of Polynomials', 
     border='linen', col='grey80', xlim=c(0,1))
curve(x^3 + 4.5*x^5, 0, 1, lwd=3, 
      col="blue", add=TRUE)

# Box-Muller transformation
boxMuller <- function(U1, U2) {
  R <- sqrt(-2*log(U1))
  X <- R * cos(2*pi*U2)
  Y <- R * sin(2*pi*U2)
  c(X,Y)
}
boxMuller <- Vectorize(boxMuller, vectorize.args=c("U1", "U2"))
set.seed(113)
out <- boxMuller(runif(1000), runif(1000))
round(out[,1:6], 3)

