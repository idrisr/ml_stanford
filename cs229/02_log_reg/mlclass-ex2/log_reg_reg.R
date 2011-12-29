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
feature_map <- function(){
    X1 <- as.matrix(data[,1])
    X2 <- as.matrix(data[,2])
    out <- matrix(1, dim(X1))
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
    #e% Initialize some useful values
    #m = length(y); % number of training examples
    m <- length(Y)

    #% You need to return the following variables correctly 
    #J = 0;
    J <- 0
    #grad = zeros(size(thetax));
    grad <- as.matrix(0, dim(thetax)[1])

    #% ====================== YOUR CODE HERE ======================
    #% Instructions: Compute the cost of a particular choice of thetax.
    #%               You should set J to the cost.
    #%               Compute the partial derivatives and set grad to the partial
    #%               derivatives of the cost w.r.t. each parameter in thetax

    #thetax_len = size(thetax)(1);
    thetax_len <- length(thetax)

    #J = 1/m * sum(-y' * log(sigmoid(X*thetax)) - (1-y)'*log(1-sigmoid(X*thetax)));
    J <- 1/m * sum(-t(Y)   %*% log(    sigmoid(X %*% thetax)) - 
                    t(1-Y) %*% log(1 - sigmoid(X %*% thetax))) 

    #J += lambda/(2*m) * sum(thetax(2:thetax_len,:).^2);
    J <- J + lambda/(2*m) * sum(thetax[2:thetax_len])
    return(J)
    #grad = 1/m * X'*(sigmoid(X*thetax) - y);
    #grad(2:thetax_len, :) = grad(2:thetax_len, :) + lambda/m .* thetax(2:thetax_len,:);

    #% =============================================================

    #end
}

gradient <- function(theta){
    m <- length(theta)
    grad <- 1/m * t(X) %*% (sigmoid(X %*% theta) - Y)
    grad[2:m] <- grad[2:m] + lambda/m * theta[2:m]
    return(grad)

}

lambda <- 1
data <- read.csv('ex2data2.txt', header=FALSE)
Y <- as.matrix(data$V3)
X <- feature_map()

# don't pass in data since we don't have to.
data$V3 <- factor(data$V3)
g <- plot_data()
theta <- matrix(1, dim(X)[2])
J <- compute_cost(theta, lambda)
print(J)
opt <- optim(theta, compute_cost, gradient, lambda, method=c('CG'))
gradient(theta)
J <- compute_cost(opt$par, lambda)
print(J)
