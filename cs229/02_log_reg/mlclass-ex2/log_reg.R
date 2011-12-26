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
    J = 1/m * sum(-t(Y) %*% log(sigmoid(X%*%theta)) - 
                 t(1-Y) %*% log((1-sigmoid(X%*%theta))))
    return(J)
}

gradient <- function(X, Y, theta, alpha, iterations){
    m <- dim(X)[1]
    J = matrix(0, iterations)
    for(i in 1:iterations){
        # Wait - this isn't the right cost function
        # Let's graph it before and after and see what happens
        grad <- 1/m * (t(X) %*% (sigmoid(X%*%theta) - Y))
        theta <- theta - (alpha/m * grad)
        J[i] = cost_function(X, Y, theta)
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

data <- read.csv('ex2data1.txt', header=FALSE)
X <- as.matrix(data[, -length(data)])
Y <- as.matrix(data[, length(data)])
X <- add_ones(X)
names(data) <- c('Exam1', 'Exam2', 'Admitted')
data <- transform(data, Admitted = factor(Admitted))
g <- plot_data(data)
#ggsave(filename='initial_plot.jpeg', plot=g)
theta_init <- matrix(0, dim(X)[2])
J_init <- cost_function(X, Y, theta_init)
print(J_init)
iterations = 150000
alpha = .1
r <- gradient(X, Y, theta_init, alpha, iterations)
theta <- r["theta"]
Js <- as.data.frame(r["J"])
rownames(Js) <- NULL
names(Js) <- 'Cost'
g <- plot_cost(Js, alpha)
filename = paste('Alpha_', alpha, "_", 'Iterations_', iterations, '.jpeg', sep='')
ggsave(filename = filename, plot = g)
