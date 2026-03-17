
set.seed(123)

# Set model specifications:
alpha1 <- 0 # if we had autoregressive y_1
alpha2 <- 0 # if we had autoregressive y_2
Alpha <- diag(c(alpha1,alpha2))
sigma1 <- 4
sigma2 <- 4
D <- matrix(c(sigma1,0,0,sigma2),2,2)
beta1 <- 1
beta2 <- 1
Gamma <- matrix(c(beta1,beta2),2,1)
phi <- .8

# Simulate model:
T <- 100
Y <- NULL;X <- NULL
Alpha.Y_1 <- NULL
y <- c(0,0);x <- 0
for(i in 1:T){
  Alpha.Y_1 <- rbind(Alpha.Y_1,c(Alpha %*% y))
  y <- Alpha %*% y + Gamma * x + D %*% rnorm(2)
  x <- phi * x + rnorm(1)
  Y <- rbind(Y,t(y));X <- rbind(X,x)
}

# Define matrices needed in the Kalman_filter procedures:
nu_t <- matrix(0,T,1)
H <- phi
G <- Gamma
mu_t <- Alpha.Y_1
N <- 1
M <- D
Sigma_0 <- 1/(1-phi^2) # unconditional variance of w
rho_0 <- 0 # unconditional mean of w
filter.res   <- Kalman_filter(Y,nu_t,H,N,mu_t,G,M,Sigma_0,rho_0)
smoother.res <- Kalman_smoother(Y,nu_t,H,N,mu_t,G,M,Sigma_0,rho_0)

par(mfrow=c(3,1))
par(plt=c(.05,.97,.2,.8))

par(mfrow=c(3,1))
plot(Y[,1],type="l",lwd=2,xlab="",ylab="",las=1,
     main=expression(paste("(a) First observed variable (",y[1*","*t],")",sep="")))
grid()
plot(Y[,2],type="l",lwd=2,xlab="",ylab="",las=1,
     main=expression(paste("(b) Second observed variable (",y[2*","*t],")",sep="")))
grid()
plot(X,type="l",lwd=2,xlab="",ylab="",las=1,
     main=expression(paste("(c) Latent variable (",w[t],")",sep="")))
grid()



par(mfrow=c(3,1))
par(plt=c(.05,.97,.2,.8))

plot(Y[,1],type="l",lwd=2,xlab="",ylab="",las=1,
     main=expression(paste("(a) Observations of ",
                           y[1*","*t]," and ",y[2*","*t],sep="")),
     ylim=c(-10,10))
grid()
lines(Y[,2],type="l",lwd=2,lty=3)

legend("topleft",
       c(expression(paste(y[1*","*t],sep="")),
         expression(paste(y[2*","*t],sep=""))),
       lwd=c(2), # line width
       lty=c(1,3),
       bg = "white")

# lower_bound <- filter.res$r - 2*sqrt(filter.res$Sigma_tt)
# upper_bound <- filter.res$r + 2*sqrt(filter.res$Sigma_tt)
lower_bound <- smoother.res$r - 2*sqrt(smoother.res$S_smooth)
upper_bound <- smoother.res$r + 2*sqrt(smoother.res$S_smooth)

plot(X,type="l",lwd=2,xlab="",ylab="",col="white",las=1,
     ylim=c(min(lower_bound),1.6*max(upper_bound)),
     main=expression(paste("(b) Filtered and smoothed estimates of ",w[t],sep="")))
grid()

polygon(c(1:T,T:1),c(lower_bound,rev(upper_bound)),col="#88888844",border = NaN)

lines(X,lwd=2,col="dark grey",lty=1)
lines(filter.res$r,col="black",lwd=2,lty=2)
lines(smoother.res$r,col="black",lwd=2,lty=3)


legend("topleft", 
       c(expression(w[t]),
         expression(paste("Filtered values of ",w[t],sep="")),
         expression(paste("Smoothed values of ",w[t],sep=""))),
       lwd=c(2), # line width
       lty=c(1,2,3),
       col=c("dark grey","black","black"), # gives the legend lines the correct color and width
       seg.len = 3,
       bg = "white"
)
legend("topright",
       c(expression(paste("95% confidence interval of ",w[t]," (smoothed estimates)",sep=""))),
       lwd=c(NaN), # line width
       lty=c(NaN),
       pch=15,
       pt.cex=2,
       col=c(col="#88888844"),
       bg = "white"
)

plot(sqrt(filter.res$Sigma_tt),type="l",lty=2,
     lwd=2,xlab="",ylab="",col="black",
     ylim=c(min(sqrt(smoother.res$S_smooth)),1.1*max(sqrt(filter.res$Sigma_tt))),las=1,
     main=expression(paste("(c) Variance of filtered and smoothed estimates of ",w[t],sep="")))
grid()
lines(sqrt(smoother.res$S_smooth),lwd=2,col="black",lty=3)

legend("topright", 
       c(expression(paste("Filtered standard errors (",sqrt(Var(w[t]*"|"*y[t],y[t-1],...)),")",sep="")),
         expression(paste("Smoothed standard errors (",sqrt(Var(w[t]*"|"*y[T],y[T-1],...)),")",sep=""))
       ),
       lwd=c(2), # line width
       lty=c(2,3),
       col=c("black","black"),
       seg.len = 3,
       bg = "white"
)

Y.modif <- Y
Y.modif[30:50,1] <- NaN
Y.modif[40:70,2] <- NaN

# Call of Kalman filter and smoother:
filter.res   <- Kalman_filter(Y.modif,nu_t,H,N,mu_t,
                              G,M,Sigma_0,rho_0)
smoother.res <- Kalman_smoother(Y.modif,nu_t,H,N,mu_t,
                                G,M,Sigma_0,rho_0)


par(mfrow=c(3,1))
par(plt=c(.05,.97,.13,.8))

plot(Y.modif[,1],type="l",lwd=2,xlab="",ylab="",las=1,
     main=expression(paste("(a) Observations of ",
                           y[1*","*t]," and ",y[2*","*t],sep="")),
     ylim=c(-10,10))

grid()
lines(Y.modif[,2],type="l",lwd=2,lty=3)

legend("topleft",
       c(expression(paste(y[1*","*t],sep="")),
         expression(paste(y[2*","*t],sep=""))),
       lwd=c(2), # line width
       lty=c(1,3),
       bg = "white")

# lower_bound <- filter.res$r - 2*sqrt(filter.res$Sigma_tt)
# upper_bound <- filter.res$r + 2*sqrt(filter.res$Sigma_tt)
lower_bound <- smoother.res$r - 2*sqrt(smoother.res$S_smooth)
upper_bound <- smoother.res$r + 2*sqrt(smoother.res$S_smooth)

plot(X,type="l",lwd=2,xlab="",ylab="",col="white",las=1,
     ylim=c(min(lower_bound),1.6*max(upper_bound)),
     main=expression(paste("(b) Filtered and smoothed estimates of ",w[t],sep="")))
grid()

polygon(c(1:T,T:1),c(lower_bound,rev(upper_bound)),col="#88888844",border = NaN)

lines(X,lwd=2,col="dark grey",lty=1)
lines(filter.res$r,col="black",lwd=2,lty=2)
lines(smoother.res$r,col="black",lwd=2,lty=3)


legend("topleft", 
       c(expression(w[t]),
         expression(paste("Filtered values of ",w[t],sep="")),
         expression(paste("Smoothed values of ",w[t],sep=""))),
       lwd=c(2), # line width
       lty=c(1,2,3),
       col=c("dark grey","black","black"), # gives the legend lines the correct color and width
       seg.len = 3,
       bg = "white"
)
legend("topright",
       c(expression(paste("95% confidence interval of ",w[t]," (smoothed estimates)",sep=""))),
       lwd=c(NaN), # line width
       lty=c(NaN),
       pch=15,
       pt.cex=2,
       col=c(col="#88888844"),
       bg = "white"
)

plot(sqrt(filter.res$Sigma_tt),type="l",lty=2,
     lwd=2,xlab="",ylab="",col="black",
     ylim=c(min(sqrt(smoother.res$S_smooth)),1.1*max(sqrt(filter.res$Sigma_tt))),las=1,
     main=expression(paste("(c) Variance of filtered and smoothed estimates of ",w[t],sep="")))
grid()
lines(sqrt(smoother.res$S_smooth),lwd=2,col="black",lty=3)

legend("topright", 
       c(expression(paste("Filtered standard errors (",sqrt(Var(w[t]*"|"*y[t],y[t-1],...)),")",sep="")),
         expression(paste("Smoothed standard errors (",sqrt(Var(w[t]*"|"*y[T],y[T-1],...)),")",sep=""))
       ),
       lwd=c(2), # line width
       lty=c(2,3),
       col=c("black","black"),
       seg.len = 3,
       bg = "white"
)

