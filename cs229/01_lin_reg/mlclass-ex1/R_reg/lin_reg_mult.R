require(ggplot2)

compute_cost <-function(X, Y, theta){
    m = dim(X)[1]
    J = 1/(2*m) * sum((X%*%theta - Y)^2)
    return(J)
}

read_data <-function(file){
    df <- read.csv(file, header=FALSE)
    return(df)
}

add_ones <- function(X){
    X <- as.matrix(X)
    X <- cbind(rep(1, dim(X)[1]), X)
}
gradient_descent <- function(X, Y, theta, alpha, iterations){
    m = dim(Y)[1]
    for(i in 1:iterations){
        h_theta_y <- (X %*% theta) - Y
        dJ_dTheta <- t(X) %*% h_theta_y
        theta <- theta - (alpha/m * dJ_dTheta)
        #[[TODO: add graph of iterations vs. J]]
        #J <- compute_cost(X, Y, theta)
        #print(J)
    }
    return(theta)
}

unscale <- function(X, mu, sig){
    sig <- sapply(sig, function(x) rep(x, dim(X)[1]))
    mu  <- sapply(mu, function(x) rep(x, dim(X)[1]))
    X[,-1] <- (X[,-1] * sig) + mu
    return(X)
}


data_file = '../ex1data2.txt'
data <- read_data(data_file)
X <- data[,-length(data)]
mu <- mean(X)
sig <- sd(X)
X <- scale(X)
X <- as.matrix(add_ones(X))
#colnames(X) <- c('X0', 'X1', 'X2')
colnames(X) <- NULL
Y <- matrix(data[,dim(data)[2]])
theta = matrix(rep(0, dim(X)[2]), nrow=dim(X)[2], ncol=1)
iterations = 1000
alpha = 0.01

# compute and display initial cost
J = compute_cost(X, Y, theta)
print(J)
theta <- gradient_descent(X, Y, theta, alpha, iterations)
print(theta)
X <- unscale(X, mu, sig)
