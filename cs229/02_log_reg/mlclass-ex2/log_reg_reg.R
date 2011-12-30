require(ggplot2)

sigmoid <- function(X){
    #Takes a m by 1 matrix and returns its sigmoid between 0 and 1
    X <- 1 / (1+exp(-X))
    return(X)
}

# Objective 1: Plot Data
plot_data <- function(){
    # figure out how to add labels -- see the R work code I've done for examples
    # on tweaking options
    g <- ggplot(data) 
    g <- g + geom_point(aes(x=V1, y=V2, colour=V3))
    return(g)
}

# Objective 2: Create 28 more 6th degree features
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

#Objective 3: create cost function
compute_cost <- function(thetax, lambda){
    m <- length(Y)

    J <- 0
    grad <- as.matrix(0, dim(thetax)[1])

    thetax_len <- length(thetax)

    J <- 1/m * sum(-t(Y)   %*% log(    sigmoid(X %*% thetax)) - 
                    t(1-Y) %*% log(1 - sigmoid(X %*% thetax))) 

    J <- J + lambda/(2*m) * sum(thetax[2:thetax_len])
    return(J)
}

gradient <- function(theta, lambda){
    m <- length(theta)
    grad <- 1/m * t(X) %*% (sigmoid(X %*% theta) - Y)
    grad[2:m] <- grad[2:m] + lambda/m * theta[2:m]
    return(grad)
}

decision_boundary <- function(theta){
    #u = linspace(-1, 1.5, 50);
    #v = linspace(-1, 1.5, 50);
    x <- seq(-1, 1.5, length = 50)
    y <- seq(-1, 1.5, length = 50)
    xs <- sapply(x, function(x) rep(x, length(y)))
    xs <- as.vector(xs)
    ys <- rep(y, length(x))
    xy <- cbind(xs, ys)

    #z = zeros(length(u), length(v));
    z <- matrix(0, dim(xy)[1])
    for(i in 1:dim(xy)[1]){
        z[i] <- feature_map(xy[i,1], xy[i,2]) %*% theta
    }
    df <- data.frame(xy, z)
    g <- ggplot(df, aes(xs, ys, z=z)) + 
        geom_contour(aes(colour=..level..), bins = 1)
    return(g)

    #% Evaluate z = theta*x over the grid
    #for i = 1:length(u)
        #for j = 1:length(v)
            #z(i,j) = mapFeature(u(i), v(j))*theta;
        #end
    #end

    #z = z'; % important to transpose z before calling contour

    #% Plot z = 0
    #% Notice you need to specify the range [0, 0]
    #contour(u, v, z, [0, 0], 'LineWidth', 2)

}

pred_quality <- function(opt){
    pred <- sigmoid(X %*% opt$par) >= 0.5
    pred.qual <- mean(Y == pred)
    return(pred.qual)
}

plot_decision_boundary <- function(data, title){
    g <- ggplot(data, aes(V1, V2, pred, V3))
    g <- g + geom_point(aes(x=V1, y=V2, colour=V3))
    g <- g + geom_line(aes(x=V1, y=pred))
    g <- g + opts(title = title)
    return(g)
}
# Objective 4: Plot decision boundary

lambda <- 0
data <- read.csv('ex2data2.txt', header=FALSE)
Y <- as.matrix(data$V3)
X1 <- as.matrix(data[,1])
X2 <- as.matrix(data[,2])
X <- feature_map(X1, X2)

# don't pass in data since we don't have to.
data$V3 <- factor(data$V3)
g <- plot_data()
theta <- matrix(1, dim(X)[2])
J <- compute_cost(theta, lambda)
print(J)
iter <- 4000
opt.grad <- optim(theta, compute_cost, gradient, lambda, method=c('CG'),
             control=list(maxit=iter))
opt.nograd <- optim(theta, compute_cost, NULL, lambda, method=c('CG'))

theta.grad <- opt.grad$par
theta.nograd <- opt.nograd$par
#decision_boundary(theta.grad)
decision_boundary(theta.nograd)


#data.grad <- data
#data.nograd <- data
#data.grad$pred <- sapply(data$V1, function(x) decision_boundary(x, theta.grad))
#data.nograd$pred <- sapply(data$V1, function(x) decision_boundary(x, theta.nograd))
#p1 <- plot_decision_boundary(data.grad, "With Gradient")
#p2 <- plot_decision_boundary(data.nograd, "Without Gradient")
