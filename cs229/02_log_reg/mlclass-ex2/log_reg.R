require(ggplot2)

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

cost_function <- function(X, Y,theta){
    m = dim(X)[1]
    J = 1/m * sum(-solve(Y) * log(sigmoid(X%*%theta)) - 
                 solve(1-Y) * log(1-sigmoid(X%*%theta)))
}

gradient <- function(X, Y, initial_theta, alpha, iterations){
    J = list()
    for(i in 1:iterations){
        grad <- 1/m * (t(X) %*% (sigmoid(X%*%theta) - y))
        theta <- theta - (alpha/m * grad)
        J[i] = cost_function(X, Y, theta)
    }
}

data <- read.csv('ex2data1.txt', header=FALSE)
X <- data[, -length(data)]
Y <- data[, length(data)]
names(data) <- c('Exam1', 'Exam2', 'Admitted')
data <- transform(data, Admitted = factor(Admitted))
g <- plot_data(data)
#ggsave(filename='initial_plot.jpeg', plot=g)


