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
    J = list()
    for(i in 1:iterations){
        h_theta_y <- (X %*% theta) - Y
        dJ_dTheta <- t(X) %*% h_theta_y
        theta <- theta - (alpha/m * dJ_dTheta)
        #[[TODO: add graph of iterations vs. J]]
        J[i] <- compute_cost(X, Y, theta)
    }
    r = list(theta=theta, J=J)
    return(r)
}

unscale <- function(X, mu, sig){
    sig <- sapply(sig, function(x) rep(x, dim(X)[1]))
    mu  <- sapply(mu, function(x) rep(x, dim(X)[1]))
    X[,-1] <- (X[,-1] * sig) + mu
    return(X)
}

plot_cost <- function(Js){
    Js$iter <- 1:dim(Js)[1]
    g <- ggplot(Js, aes(iter, Cost)) + geom_line()
    g <- g + opts(title = 'Cost vs Iteration')
    return(g)
}

normal_sol <- function(X, Y){
    # solve linear regression through normal equation
    # Taking the inverse is very expensive, which is why we don't always
    # use this approach. And for degenerate matrices which have no inverse
    theta = solve(t(X) %*% X) %*% t(X) %*% Y
    return(theta)
}


data_file = '../ex1data2.txt'
data <- read_data(data_file)
X <- data[,-length(data)]

#Save data to unscale features
mu <- mean(X)
sig <- sd(X)
X <- scale(X)

# convert dataframes into matrices
X <- as.matrix(add_ones(X))
colnames(X) <- NULL
Y <- matrix(data[,dim(data)[2]])
theta = matrix(rep(0, dim(X)[2]), nrow=dim(X)[2], ncol=1)
iterations = 1000
alpha = 0.01

# compute and display initial cost
J = compute_cost(X, Y, theta)
print(J)

#contains theta and costs
r <- gradient_descent(X, Y, theta, alpha, iterations)
theta <- r["theta"]
Js <- as.data.frame(unlist(r["J"]))
rownames(Js) <- NULL
names(Js) <- 'Cost'
g <- plot_cost(Js)
ggsave(filename = 'Cost_v_iteration.jpeg', plot=g)
theta_norm <- normal_sol(X, Y)
print(theta_norm)
