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

plot_data <- function(X, Y, theta){
    # Add chart labels
    df <- data.frame(X, Y)
    df <- cbind(df, pred=(X %*% theta))
    p <- ggplot(df, aes(X2, pred)) + geom_line(colour='blue') + 
        geom_point(aes(X2, Y))
    return(p)
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

plot_contour <- function(X, Y){
    #% Grid over which we will calculate J
    x <- seq(-10, 10, length.out=100);
    y <- seq(-1, 4, length.out=100);
    #x <- 1:4
    #y <- 4:7
    xs <- sapply(x, function (x) rep(x, length(y)))
    xs <- as.vector(xs)
    ys <- rep(y, length(x))
    xy <- cbind(xs, ys)

    Js <- rep(0, dim(xy)[1])
    for(i in 1:dim(xy)[1]){
        theta = as.vector(xy[i,])
        Js[i] = compute_cost(X, Y, theta)
    }
    df <- data.frame(xy, Js)
    g <- ggplot(df, aes(xs, ys, z=log(Js))) + 
        geom_contour(aes(colour=..level..), bins = 15)
    return(g)
}

unscale <- function(X, mu, sig){
    sig <- sapply(sig, function(x) rep(x, dim(X)[1]))
    mu  <- sapply(mu, function(x) rep(x, dim(X)[1]))
    X[,-1] <- (X[,-1] * sig) + mu
    return(X)
}


data_file = '../ex1data1.txt'
data <- read_data(data_file)
X <- data[,-length(data)]
mu <- mean(X)
sig <- sd(X)
print(mu)
print(sig)

# why does this problem work without scaling? And why did I have such a hard
# time making this work with normalization?
#X <- scale(X)
X <- as.matrix(add_ones(X))
colnames(X) <- NULL
Y <- matrix(data[,dim(data)[2]])
theta = matrix(rep(0, dim(X)[2]), nrow=dim(X)[2], ncol=1)
iterations = 1500 
alpha = 0.01

# compute and display initial cost
J = compute_cost(X, Y, theta)
#print(J)
theta <- gradient_descent(X, Y, theta, alpha, iterations)
print(theta)

#unscale theta values
#Actually, no need to do this. Why is that exactly? hmm
#theta[2] <- (theta[2] * sig) + mu
print(theta)
#X <- unscale(X, mu, sig)
g <- plot_contour(X, Y) + opts(title = "Gradient Descent Contour")
ggsave(filename = 'grad_contour_plot.jpeg', plot = g)
p <- plot_data(X, Y, theta)
ggsave(filename = 'prediction.jpeg', plot = p)
