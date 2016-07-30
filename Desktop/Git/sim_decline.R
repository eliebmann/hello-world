library(MASS)
library(MASS)

mu <- c(1, 0.75, -1, -2)
Sigma <- matrix(c(
  1.0, .75, .75, .75, 
  .75, 1.0, .75, .75, 
  .75, .75, 1.0, .75,
  .75, .75, .75, 1.0), 
  nrow=4, ncol=4) 

fun.shit <- function(mu, sigma, n, disturb){
  require(reshape2)
  require(ggplot2)
  rawvars <- mvrnorm(n, mu=mu, Sigma=Sigma)
  rawvars <- as.data.frame(rawvars)
  rawvars$id <- factor(seq(1:n))
  raw.m <- melt(rawvars, id = "id")
  raw.m$time <- rep(1:length(mu), each = n)
  if(disturb == TRUE) {
    rawvars[, 3] <- punif(rawvars[, 3])
  }
 # print(cor(rawvars[, 1:3]))
  p <- ggplot(raw.m, aes(x = time, y = value, group = id)) + geom_line()
  print(p)
  
  z <- ggplot(raw.m, aes(x = value)) + geom_density(aes(group = variable, colour = variable, fill = variable), alpha = .3)
  #z + aes(group = variable, colour = variable))
  print(z)
  #p + stat_smooth(method = 'auto')
}

fun.shit(mu, sigma, 500, disturb = FALSE)  

mu <- c(0, -0.05, -1, -2)
Sigma <- matrix(c(
  1.0, .20, .20, .20, 
  .20, 1.0, .40, .40, 
  .20, .40, 1.0, .40,
  .20, .40, .40, 1.0), 
  nrow=4, ncol=4) 

  library(MASS)
rawvars <- as.data.frame(mvrnorm(1000, mu=mu, Sigma=Sigma))


library(Hmisc)
rcorr(as.matrix(rawvars))

names(rawvars)
  
library(lavaan)

meas1 <-'
F1 =~ V1 + V2 + V3 + V4
'

fit.meas1 <- cfa(meas1, data = rawvars, std.lv = TRUE, meanstructure = TRUE)
summary(fit.meas1, fit.measures = TRUE)

lamb <- parameterEstimates(fit.meas1)
lamb <- lamb[1:4, 4]
int <- parameterEstimates(fit.meas1)
int <- int[10:13, 4]
var <- parameterEstimates(fit.meas1)
var <- var[5:8, 4]

vals <- predict(fit.meas1)

df <- data.frame(lamb = lamb, int = int)
df$disc <- lamb/sqrt(1-(lamb)^2) 
df$diff <- -1*(int/sqrt(1-(lamb)^2)) 
df$info <- (lamb)^2/(var)^2

df1 <- data.frame(vals = vals, disc1 = rep(df[1,1], 1000), disc2 = rep(df[2,1], 1000), disc3 = rep(df[3,1], 1000), disc4 = rep(df[4,1], 1000), diff1  = rep(df[1,2], 1000), diff2  = rep(df[2,2], 1000), diff3  = rep(df[3,2], 1000), diff4  = rep(df[4,2], 1000)) 

df1$prob1 <- exp(1.7*df1[1,1]*(vals-df1[1,2]))/(1 + exp(1.7*df1[1,1]*(vals-df1[1,2])))
df1$prob1 <- as.numeric(df1$prob1)
df1$prob2 <- exp(1.7*df1[2,1]*(vals-df1[2,2]))/(1 + exp(1.7*df1[2,1]*(vals-df1[2,2])))
df1$prob2 <- as.numeric(df1$prob2)
df1$prob3 <- exp(1.7*df1[3,1]*(vals-df1[3,2]))/(1 + exp(1.7*df1[3,1]*(vals-df1[3,2])))
df1$prob3 <- as.numeric(df1$prob3)
df1$prob4 <- exp(1.7*df1[4,1]*(vals-df1[4,2]))/(1 + exp(1.7*df1[4,1]*(vals-df1[4,2])))
df1$prob4 <- as.numeric(df1$prob4)

plot(prob1 ~ vals, data = df1, col = 1)
for (i in 11:13){
  points(df1[, i] ~ vals, data = df1, col = i)
}
abline(h = 0.5, lty = 3)
abline(h = df[1,3], col = 1)
abline(h = df[2,3], col = 11)
abline(h = df[3,3], col = 12)
abline(h = df[4,3], col = 13)

head(df1)