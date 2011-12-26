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

data <- read.csv('ex2data1.txt', header=FALSE)
X <- data[, -length(data)]
Y <- data[, length(data)]
names(data) <- c('Exam1', 'Exam2', 'Admitted')
data <- transform(data, Admitted = factor(Admitted))
g <- plot_data(data)
#ggsave(filename='initial_plot.jpeg', plot=g)
