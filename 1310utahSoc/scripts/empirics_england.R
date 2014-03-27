windows(record=T)
### data series for diss -- mtoespline (1300-2008), gdpspline(1086-2009), popspline (1086-1895)



###	spline mtoe data with na.stineterp

mtoespline = na.stinterp(mtoetz)	

mtoesplineln = log(mtoespline)

###	plotit

xyplot(window(mtoespline,start=c(1300),end=c(1873)))

xyplot(window(mtoesplineln,start=c(1300),end=c(1873)))

xyplot(window(mtoesplineln,start=c(1300)))

### spline gdp with na.stineterp

gdpspline = na.stinterp(ukgdptz)

gdpsplineln = log(gdpspline)

###	plotit

xyplot(window(gdpspline,start=c(1300),end=c(1873)))

xyplot(window(gdpsplineln,start=c(1300),end=c(1873)))

xyplot(window(gdpsplineln,start=c(1300)))

### spline pop with na.stineterp

popspline = na.stinterp(ukpoptz)

popsplineln = log(popspline)

###	plotit

xyplot(window(popspline,start=c(1300),end=c(1873)))

xyplot(window(popsplineln,start=c(1300),end=c(1873)))

xyplot(window(popsplineln,start=c(1300)))

###################################################
### Preliminaries with new Warde data--from
###	1560-2001
###################################################

petajoule=ts(read.csv("warde_energy_1560_2001.csv"),start=1560)

peta = zoo(petajoule)
gdp  = gdpspline
pop  = popspline

### combine the level series with 1300 start

england=cbind(mtoespline,gdpspline,popspline)
england1300 = window(england,start=1300,end=1895)				###	other series start in 1086, no pop after 1895
englandln=cbind(mtoesplineln,gdpsplineln,popsplineln)
england1300ln = window(englandln,start=1300,end=1895)

###################################################
### Preliminaries with new Warde data--from
###	1560-2001
###################################################

england = cbind(peta,gdp,pop)
england1560 = window(england, start=1560,end=1895)
england1560ln = log(england1560)

### vars package	install.packages("vars", repos="http://cran.stat.ucla.edu/")


library(vars)
library(zoo)

###	display the data

###################################################
### Preliminaries
###################################################
library("vars")

summary(england1560)
plot(england1560, nc = 2, xlab = "")
summary(england1560ln)
plot(england1560ln)

###################################################
### Structural tests try efp first
###################################################
repl=england1560ln[,"energy"]


## compute OLS-based CUSUM process and plot
## with standard and alternative boundaries
ocus<- efp(repl ~ 1, type = "OLS-CUSUM")
plot(ocus)
plot(ocus, alpha = 0.01, alt.boundary = TRUE)
## calculate corresponding test statistic
sctest(ocus)

###	test with RE

library(forecast)		###	to use auto.arima. note that this should be done after struct tests, so modify this section

re <- efp(fitted(auto.arima(repl))~1, type = "RE")
plot(re, alpha = 0.01, alt.boundary=T)

## dating
bp <- breakpoints(fitted(auto.arima(repl))~1, h = 0.1)
summary(bp)
lines(bp, breaks = length(bp$breakpoints))
lines(bp, breaks = 1)
lines(bp, breaks = 2)

###################################################
### think about fitting more complex models as in 
###	breakpoints eg with SARIMA
###################################################


## F statistics indicate one breakpoint
fs <- Fstats(repl ~ 1)
plot(fs)
breakpoints(fs)
lines(breakpoints(fs))

## or
bp <- breakpoints(repl ~ 1)
summary(bp)

## the BIC 
plot(bp)
breakpoints(bp)

## fit null hypothesis model and model with n breakpoint
fm0 <- lm(repl ~ 1)
fm1 <- lm(repl ~ breakfactor(bp, breaks = length(bp$breakpoints)))
plot(repl)
lines(ts(fitted(fm0), start = 1560), col = 3)
lines(ts(fitted(fm1), start = 1560), col = 4)
lines(bp)

## confidence interval
ci <- confint(bp,level=.95)
ci
lines(ci)

###	base on stru tests of mtoe, use exo starting in 1780 for varselect etc.

exo = rep(0,336)
exo = ts(exo, start = 1560)
exo=zoo(exo)
window(exo, start=1800)<- 1
exo = as.matrix(exo)
colnames(exo)=c("ex1800")



###	need to decide on trend, constant, both. use auto.arima for this with exo




###################################################
### ADF - Tests
###################################################

###################################################
###	mtoe
###################################################
adf1 <- summary(ur.df(england1300[, "mtoespline"], type = "trend", lags = 4, selectlags="BIC"))
adf1	### accept no trend, next test for drift
adf1 <- summary(ur.df(england1300[, "mtoespline"], type = "drift", lags = 4, selectlags="BIC"))
adf1	###	accept no drift so pure random walk, so test for I(1) next
adf2 <-summary(ur.df(diff(england1300[, "mtoespline"],differences=1), type = "none", lags = 4, selectlags="BIC"))
adf2	###	accept pure unit root, nex ttest for I(2)
adf2 <-summary(ur.df(diff(england1300[, "mtoespline"],differences=2), type = "none", lags = 4, selectlags="BIC"))
adf2	###	reject unit root. So, I(2) no trend, no drift, 4 lags
auto.arima(england1300[,"mtoespline"]	###	test of above assumptions
###	shows arima(4,2,2)	###	so only problem is in trend
###	next, do structural tests with breakpoints, first extend to latest date.
###	then do pp and kpss
adf3 <-summary(ur.df(england1300[, "gdpspline"], type = "trend", lags = 4, selectlags="BIC"))
adf4 <-summary(ur.df(diff(england1300[, "gdpspline"],differences=2), type = "drift", lags = 4, selectlags="BIC"))
adf5 <-summary(ur.df(england1300[, "popspline"], type = "trend", lags = 4, selectlags="BIC"))
adf6 <-summary(ur.df(diff(england1300[, "popspline"],differences=2), type = "drift", lags = 4, selectlags="BIC"))



###################################################
### Lag-order selection
###################################################

VARselect(england1560ln, lag.max = 8, type = "none",exogen=exo)

###################################################
### VAR(2)
###################################################
p5ct <- VAR(england1560ln, p = 2, type = "both",exogen = exo)
p5ct

###################################################
### Diagnostic Tests 2
###################################################
## Serial
ser21 <- serial.test(p2ct, lags.pt = 16, type = "PT.asymptotic")$serial
ser22 <- serial.test(p2ct, lags.pt = 16, type = "PT.adjusted")$serial
ser21
ser22
## JB
norm2 <-normality.test(p2ct)$jb.mul$JB
norm2
## ARCH
arch2 <- arch.test(p2ct, lags.multi = 5)
##	plots
plot(arch2, names = "e")
plot(stability(p2ct), nc = 2)

###################################################
### irf
###################################################

plot(irf(p5ct, impulse = "energy", response = c("energy", "gdp", "pop"), boot =T,ci=.95))

###################################################
### fevd
###################################################

fevd(p2ct, n.ahead = 10)
plot(fevd(p2ct, n.ahead = 30))

###################################################
### Granger-causality
###################################################

causality(p2ct, cause = "e")
causality(p2ct, cause = "prod")

###################################################
### Blanchard-Quah decomposition
###################################################

BQ(p2ct)

###################################################
### restrict
###################################################

p2ctres <- restrict(p2ct, method = "ser")
p2ctres

p2ctres$restrictions

plot(irf(p2ctres, impulse = "e", response = c("prod", "rw", "U"), boot =T,ci=.95))

plot(fevd(p2ctres, n.ahead = 10))

causality(p2ctres, cause = "e")
causality(p2ctres, cause = "prod")


###	nov 3 class 

windows(record=T)

library(vars)
data(Canada)

###################################################
### VECM
###################################################
vecm.p3 <- summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 3, spec = "transitory"))
vecm.p2 <- summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 2, spec = "transitory"))


###################################################
### VECM r = 1
###################################################
vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace", ecdet = "trend", K = 3, spec = "transitory") 
vecm.r1 <- cajorls(vecm, r = 1)
##
## Calculation of t-values for alpha and beta
##
alpha <- coef(vecm.r1$rlm)[1, ]
names(alpha) <- c("rw", "prod", "e", "U")
alpha
beta <- vecm.r1$beta
beta
resids <- resid(vecm.r1$rlm)
N <- nrow(resids)
sigma <- crossprod(resids) / N
## t-stats for alpha (calculated by hand)
alpha.se <- sqrt(solve(crossprod(cbind(vecm@ZK %*% beta, vecm@Z1)))[1, 1] * diag(sigma))
names(alpha.se) <-  c("rw", "prod", "e", "U")
alpha.t <- alpha / alpha.se
alpha.t
## Differ slightly from coef(summary(vecm.r1$rlm))
## due to degrees of freedom adjustment 
coef(summary(vecm.r1$rlm))
## t-stats for beta
beta.se <- sqrt(diag(kronecker(solve(crossprod(vecm@RK[, -1])),
                               solve(t(alpha) %*% solve(sigma) %*% alpha))))
beta.t <- c(NA, beta[-1] / beta.se)
names(beta.t) <- rownames(vecm.r1$beta)
beta.t


###################################################
### SVEC
###################################################
vecm <- ca.jo(Canada[, c("prod", "e", "U", "rw")], type = "trace", 
              ecdet = "trend", K = 3, spec = "transitory")
SR <- matrix(NA, nrow = 4, ncol = 4)
SR[4, 2] <- 0
LR <- matrix(NA, nrow = 4, ncol = 4)
LR[1, 2:4] <- 0
LR[2:4, 4] <- 0
svec <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, 
             boot = TRUE, runs = 100)
summary(svec)


###################################################
### SR-table
###################################################
SR <- round(svec$SR, 2)
SRt <- round(svec$SR / svec$SRse, 2)


###################################################
### LR-table
###################################################
LR <- round(svec$LR, 2)
LRt <- round(svec$LR / svec$LRse, 2)


###################################################
### Over-identification
###################################################
LR[3, 3] <- 0
svec.oi <- update(svec, LR = LR, lrtest = TRUE, boot = FALSE)
svec.oi$LRover


###################################################
### SVEC - IRF
###################################################
svec.irf <- irf(svec, response = "U", n.ahead = 48, boot = TRUE)
plot(svec.irf)


###################################################
### SVEC - FEVD
###################################################
fevd.U <- fevd(svec, n.ahead = 48)$U








###	extensive BVAR example - I am still working through this to get it running so don't attempt yet

library(MSBVAR)
data(IsraelPalestineConflict)
# Find the mode of an msbvar model
# Initial guess is based on random draw, so set seed.
set.seed(123)
xm <- msbvar(y=IsraelPalestineConflict, p=1, h=2,
lambda0=0.8, lambda1=0.15,
lambda3=2, lambda4=1, lambda5=0, mu5=0,
mu6=0, qm=12,
alpha.prior=matrix(c(5,2,2,10), 2, 2))


# Plot out the initial mode
plot(ts(xm$fp))
print(xm$Q)


# Now sample the posterior
N1 <- 100
N2 <- 500


# First, so this with random permutation sampling
x1 <- gibbs.msbvar(xm, N1=N1, N2=N2, permute=TRUE)


# Since the sample was permuted, we need to cluster the posterior
# to see what identifies the h! posterior modes
Q.clus <- kmeans(x1$Q.sample, centers=2)


# Look at the modes
print(Q.clus$centers)

# We need to translate these into identification restrictions on the
# intercepts or the variances.  Here's how we can extract these from the
# posterior:
m <- ncol(IsraelPalestineConflict)
h <- x1$h
p <- 1
intercept.indices <- seq(m*p + 1, by = m+1, length=m*h)


# Extract the intercept and variance coefficients from the posterior
# sample
intercepts <- x1$Beta.sample[,intercept.indices]
intercepts <- rbind(intercepts[,1:2], intercepts[,3:4])
colnames(intercepts) <- colnames(IsraelPalestineConflict)
# Extract out the variance elements


tmp <- (rep(c(1,m:2),h))
variance.indices <- tmp
for(i in 2:(m*h)) variance.indices[i] <- variance.indices[i-1] + tmp[i]
Sigma <- x1$Sigma.sample[,variance.indices]
Sigma <- rbind(Sigma[,1:2], Sigma[,3:4])
colnames(Sigma) <- colnames(IsraelPalestineConflict)


# Make a vector of the indicators for the colors
indicator <- rep(Q.clus$cluster, h)


# Here's how to plot those based on the posterior clustering above.
pairs(intercepts, pch=".", col=indicator)
pairs(Sigma, pch=".", col=indicator)

# Now sample, clustering on the intercepts of the first equation. To see
# what the index is for this, look at the output of the mode:
print(xm$hreg$Bk)
x2 <- gibbs.msbvar(xm, N1=N1, N2=N2, permute=FALSE, Beta.idx=c(3,1))


# Plot the regime probabilities
plot.SS(x2)


# Nicer plot with some labeling
plot(ts(mean.SS(x2), start=c(1979,15), freq=52))


# Look at the clustering of the intercepts for the identified model
intercepts2 <- x2$Beta.sample[,intercept.indices]

# Identified posterior modes
summary(intercepts2)

# So the first regime is high conflict (negative values) and the second
#  regime is low conflict (closer to positive values):
pairs(rbind(intercepts2[,1:2], intercepts2[,3:4]),
col=c(rep(1,N2), rep(2, N2)))
## End(Not run)






