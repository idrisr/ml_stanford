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
compute_cost <- function(lambda, theta){
    #% Initialize some useful values
    #m = length(y); % number of training examples
    m <- length(Y)

    #% You need to return the following variables correctly 
    #J = 0;
    J <- 0
    #grad = zeros(size(theta));
    grad <- matrix(0, dim(theta))

    #% ====================== YOUR CODE HERE ======================
    #% Instructions: Compute the cost of a particular choice of theta.
    #%               You should set J to the cost.
    #%               Compute the partial derivatives and set grad to the partial
    #%               derivatives of the cost w.r.t. each parameter in theta

    #theta_len = size(theta)(1);
    theta_len <- dim(theta.init)[1]

    #J = 1/m * sum(-y' * log(sigmoid(X*theta)) - (1-y)'*log(1-sigmoid(X*theta)));
    J <- 1/m * sum(-t(Y)   %*% log(    sigmoid(X %*% theta)) - 
                    t(1-Y) %*% log(1 - sigmoid(X %*% theta))) 

    #J += lambda/(2*m) * sum(theta(2:theta_len,:).^2);
    J <- J + lambda/(2*m) * sum(theta[2:theta_len,])
    #grad = 1/m * X'*(sigmoid(X*theta) - y);
    #grad(2:theta_len, :) = grad(2:theta_len, :) + lambda/m .* theta(2:theta_len,:);

    #% =============================================================

    #end
}
data <- read.csv('ex2data2.txt', header=FALSE)
Y <- as.matrix(data$V3)
X <- feature_map()

# don't pass in data since we don't have to.
data$V3 <- factor(data$V3)
g <- plot_data()
theta.init <- matrix(0, dim(X)[2])
compute_cost(1, theta.init)

