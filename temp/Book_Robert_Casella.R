### Robert, Casella (2010) "Introducing Monte Carlo Methods with R", Springer
### http://www.springer.com/statistics/computational+statistics/book/978-1-4419-1575-7

require(ggplot2)

### 3.2 Classical Monte Carlo integration

# Example 3.3.

h=function(x){(cos(50*x)+sin(20*x))^2}

par(mar=c(2,2,2,1),mfrow=c(2,1))

curve(h,xlab="Function",ylab="",lwd=2)

integrate(h,0,1)
# 0.965201 with absolute error < 1.9e-10

x=h(runif(10^4))
head(x)

estint=cumsum(x)/(1:10^4)
head(estint)

esterr=sqrt(cumsum((x-estint)^2))/(1:10^4)
head(esterr)

plot(estint, xlab="Mean and error range", type="l", lwd=2,
     ylim=mean(x)+20*c(-esterr[10^4],esterr[10^4]),ylab="")
lines(estint+2*esterr,col="gold",lwd=2)
lines(estint-2*esterr,col="gold",lwd=2)



### 4.2 Monitoring Variation

x=matrix(h(runif(200*10^4)),ncol=200)
x[1:6, 1:10]

estint=apply(x, 2, cumsum)/(1:10^4)
estint[1:6, 1:10]

# esterr=sqrt(apply(x, 2, function(y){cumsum(y^2) - (1:length(y)) * (cumsum(y) / (1:length(y))) ^ 2}) / (1:10^4))
# esterr[1:6, 1:10]
esterr=sqrt(cumsum((x[,1] - estint[,1])^2))/(1:10^4)
head(esterr)


y=apply(estint, 1, quantile, c(.025,.975))
y[, 1:10]
head(t(y))

plot(estint[,1], ty="l", col=0, ylim=c(.8,1.2))
polygon(c(1:10^4, 10^4:1), c(y[1,], rev(y[2,])), col="wheat")


boot=matrix(sample(x[,2], 200*10^4, rep=T), nrow=10^4, ncol=200)

bootit=apply(boot,2,cumsum)/(1:10^4)
bootit[1:6, 1:10]

bootup=apply(bootit,1,quantile,.975)
bootdo=apply(bootit,1,quantile,.025)
head(bootup)
head(bootdo)

d <- data.frame(i = 1:10^4, x = estint[,1], err = esterr, t(y), bootdo, bootup)
head(d)

ggplot(d, aes(x = i)) +
  geom_point(aes(y = x), size = 1) +
  geom_point(aes(y = x + 2 * err), size = 1) +
  geom_point(aes(y = x - 2 * err), size = 1) +
  geom_point(aes(y = X2.5.), size = 1, colour = "red") +
  geom_point(aes(y = X97.5.), size = 1, colour = "red") +
  geom_point(aes(y = bootdo), size = 1, colour = "green") +
  geom_point(aes(y = bootup), size = 1, colour = "green") +
  scale_y_continuous(limits = estint[10^4,1] + 20 * esterr[10^4] * c(-1, 1)) +
  theme_bw()
                       
  