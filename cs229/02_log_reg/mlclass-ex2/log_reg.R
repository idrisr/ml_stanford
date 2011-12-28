require(ggplot2)
setwd('~/code/ml/cs229/02_log_reg/mlclass-ex2')

plot_data <- function(data){
    g <- ggplot(data, aes(Exam1, Exam2, colour=Admitted)) + geom_point()
    g <- g + opts(title = "Admittance based on 2 Exams")
}

sigmoid <- function(X){
    #Takes a m by 1 matrix and returns its sigmoid between 0 and 1
    X <- 1 / (1+exp(-X))
    return(X)
}

add_ones <- function(X){
    # grabbed from lin_reg1.R. 
    # Obviously this is bad style. Need to figure out how to properly call 
    # functions from other files in R
    X <- as.matrix(X)
    X <- cbind(rep(1, dim(X)[1]), X)
}

gradient_descent <- function(theta, alpha, iterations){
    m <- dim(X)[1]
    J = matrix(0, iterations)
    for(i in 1:iterations){
        # Wait - this isn't the right cost function
        # Let's graph it before and after and see what happens
        #grad <- 1/m * (t(X) %*% (sigmoid(X%*%theta) - Y))
        grad <- gradient(theta)
        theta <- theta - (alpha/m * grad)
        J[i] = cost_function(theta)
    }
    r = list(theta=theta, J=J)
    return(r)
}

plot_cost <- function(Js, alpha){
    # Another function copied from lin_reg_mult.R
    # Find a better way!
    iter = dim(Js)[1]
    title = paste('Alpha = ', alpha, "\n", 'Iterations = ', iter, sep='')
    Js$iter <- 1:dim(Js)[1]
    g <- ggplot(Js, aes(iter, Cost)) + geom_line()
    g <- g + opts(title = title)
    return(g)
}

cost_function <- function(theta){
    m = dim(X)[1]
    J = 1/m * sum(-t(Y) %*% log(sigmoid(X%*%theta)) - 
                 t(1-Y) %*% log((1-sigmoid(X%*%theta))))
    return(J)
}

gradient <- function(theta){
    #compute the gradient for one value of theta
    m <- dim(X)[1]
    grad <- 1/m * (t(X) %*% (sigmoid(X%*%theta) - Y))
    return(grad)
}

calc_accuracy <- function(theta){
    predict <- sigmoid(X %*% theta) > 0.5
    return(predict)
}

decision_boundary <- function(theta){
    plot_x <- c(min(X[,2])-2,  max(X[,2])+2)
    plot_y <- (-1./theta[3]) * (theta[2] * plot_x + theta[1])
}

data <- read.csv('ex2data1.txt', header=FALSE)
X <- as.matrix(data[, -length(data)])
Y <- as.matrix(data[, length(data)])
X <- add_ones(X)
names(data) <- c('Exam1', 'Exam2', 'Admitted')
data <- transform(data, Admitted = factor(Admitted))
g.init <- plot_data(data)
#ggsave(filename='initial_plot.jpeg', plot=g.init)
theta_init <- matrix(0, dim(X)[2])
parms <- list(X=X, Y=Y, theta=theta_init)

# Why doesn't gradient cause any difference?
theta.opt <- optim(theta_init, cost_function, gradient)$par

iterations <- 1000
alpha <- .1
r <- gradient_descent(theta_init, alpha, iterations)
theta.gd <- as.matrix(r[["theta"]])
rownames(theta.gd) <- NULL
Js <- as.data.frame(r["J"])
rownames(Js) <- NULL
names(Js) <- 'Cost'
g <- plot_cost(Js, alpha)
#filename = paste('Alpha_', alpha, "_", 'Iterations_', iterations, '.jpeg', sep='')
#ggsave(filename = filename, plot = g)

print("Accuracy of Gradient Descent")
print(mean(Y==calc_accuracy(theta.gd)))

print("Accuracy of Optimized Method")
print(mean(Y==calc_accuracy(theta.opt)))
