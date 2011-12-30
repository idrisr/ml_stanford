require(ggplot2)

sigmoid <- function(X){
    #Takes a m by 1 matrix and returns its sigmoid between 0 and 1
    X <- 1 / (1+exp(-X))
    return(X)
}

# Plot Data Original Data
plot_data <- function(){
    # figure out how to add labels -- see the R work code I've done for examples
    # on tweaking options
    g <- ggplot(data) 
    g <- g + geom_point(aes(x=V1, y=V2, colour=V3))
    return(g)
}

# Create all 6th degree features
feature_map <- function(X1, X2){
    out <- matrix(1, length(X1))
    degree <- 6
    for(i in 1:degree){
        for(j in 0:i){
            col <- as.matrix(X1^(i-j) * (X2^j))
            out <- cbind(out, col)
        }
    }
    return(out)
}

# Cost Function
compute_cost <- function(theta, lambda){
    m <- length(Y)
    J <- 0
    grad <- as.matrix(0, dim(theta)[1])
    theta_len <- length(theta)
    J <- 1/m * sum(-t(Y)   %*% log(    sigmoid(X %*% theta)) - 
                    t(1-Y) %*% log(1 - sigmoid(X %*% theta))) 

    J <- J + lambda/(2*m) * sum(theta[2:theta_len])
    return(J)
}

# Gradient
gradient <- function(theta, lambda){
    m <- length(theta)
    grad <- 1/m * t(X) %*% (sigmoid(X %*% theta) - Y)
    grad[2:m] <- grad[2:m] + lambda/m * theta[2:m]
    return(grad)
}

# how well does model match training data
pred_quality <- function(opt){
    pred <- sigmoid(X %*% opt$par) >= 0.5
    pred.qual <- mean(Y == pred)
    return(pred.qual)
}

lambda <- 0.5
data <- read.csv('ex2data2.txt', header=FALSE)
Y <- as.matrix(data$V3)
X1 <- as.matrix(data[,1])
X2 <- as.matrix(data[,2])

# Create all 6th degree polynomials
X <- feature_map(X1, X2)

data$V3 <- factor(data$V3)
g <- plot_data()
theta <- matrix(1, dim(X)[2])
J <- compute_cost(theta, lambda)
print(J)
iter <- 4000
opt.grad <- optim(theta, compute_cost, gradient, lambda, method=c('CG'),
             control=list(maxit=iter))
opt.nograd <- optim(theta, compute_cost, NULL, lambda, method=c('CG'))
J.grad <- compute_cost(opt.grad$par, lambda)
J.nograd <- compute_cost(opt.nograd$par, lambda)
print(J.grad)
print(J.nograd)

print(opt.grad$value)
print(opt.nograd$value)

pred_quality(opt.grad)
pred_quality(opt.nograd)

