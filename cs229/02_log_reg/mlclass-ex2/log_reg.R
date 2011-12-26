require(ggplot2)
data <- read.csv('ex2data1.txt', header=FALSE)

plot_data <- function(data){
    ggplot(data,        
}

X <- data[, -length(data)]
Y <- data[, length(data)]
names(data) <- c('Exam 1', 'Exam 2', 'Admitted')

