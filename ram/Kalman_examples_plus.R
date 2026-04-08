
#set.seed(123)

library(optimx)

# Set model specifications:
alpha1 <- 0 # if we had autoregressive y_1
alpha2 <- 0 # if we had autoregressive y_2
Alpha <- diag(c(alpha1,alpha2))
sigma1 <- 1.5
sigma2 <- 1.5
D <- matrix(c(sigma1,0,0,sigma2),2,2)
beta1 <- 1
beta2 <- 2
Gamma <- matrix(c(beta1,beta2),2,1)
phi <- .8

# Simulate model:
T <- 300
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

loglik <- function(theta,Y){
  
  model <- param2model(theta)
  
  T <- dim(Y)[1]
  
  D <- matrix(c(model$sigma1,0,0,model$sigma2),2,2)
  Gamma <- matrix(c(model$beta1,model$beta2),2,1)
  
  # Define matrices needed in the Kalman_filter procedures:
  nu_t <- matrix(0,T,1)
  H <- model$phi
  G <- Gamma
  mu_t <- Alpha.Y_1
  N <- 1
  M <- D
  Sigma_0 <- 1/(1-phi^2) # unconditional variance of w
  rho_0 <- 0 # unconditional mean of w
  
  filter.res   <- Kalman_filter(Y,nu_t,H,N,mu_t,
                                G,M,Sigma_0,rho_0)
  
  logL <- sum(filter.res$loglik.vector)
  
  return(- logL)
}


param2model <- function(theta){
  
  sigma1 <- exp(theta[1])
  sigma2 <- exp(theta[2])
  beta1  <- theta[3]
  beta2  <- theta[4]
  phi    <- exp(theta[5])/(1+exp(theta[5]))
  
  model <- list(sigma1 = sigma1,
                sigma2 = sigma2,
                beta1 = beta1,
                beta2 = beta2,
                phi = phi)
  
  return(model)
}

model2param <- function(model){
  
  theta <- matrix(NaN,5,1)
  
  theta[1] <- log(model$sigma1)
  theta[2] <- log(model$sigma2)
  theta[3] <- model$beta1
  theta[4] <- model$beta2
  theta[5] <- log(model$phi/(1-model$phi))
  
  return(c(theta))
}


true_model <- list(sigma1 = sigma1,
                   sigma2 = sigma2,
                   beta1 = beta1,
                   beta2 = beta2,
                   phi = phi)

model0 <- list(sigma1 = .4,
              sigma2 = .3,
              beta1 = 2,
              beta2 = 1,
              phi = .3)

true_theta <- model2param(true_model)

theta0 <- model2param(model0)

loglik(theta0,Y.modif)
loglik(true_theta,Y.modif)

theta_est <- theta0

nb_loop <- 3

for(i in 1:nb_loop){
  print(paste("--- Loop ",i," ---",sep=""))
  for(method in c("Nelder-Mead","nlminb")){
    res.optim <- optimx(par=theta_est,
                        fn=loglik,
                        Y=Y.modif,
                        method=method,
                        control=list(trace=TRUE,
                                     maxit=ifelse(method=="Nelder-Mead",4000,20),
                                     kkt = FALSE))
    print(paste("--- Log-lik (",method,"): ",round(res.optim$value,2)," ---",sep=""))
    theta_est  <- c(as.matrix(res.optim)[1:length(theta_est)])
  }
}

model <- param2model(theta_est)


