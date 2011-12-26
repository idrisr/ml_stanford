require(ggplot2)
data <- read.csv('ex2data1.txt', header=FALSE)

plot_data <- function(data){
    g <- ggplot(data, aes(Exam1, Exam2, colour=Admitted)) + geom_point()
    g <- g + opts(title = "Admittance based on 2 Exams")
}

X <- data[, -length(data)]
Y <- data[, length(data)]
names(data) <- c('Exam1', 'Exam2', 'Admitted')
data <- transform(data, Admitted = factor(Admitted))
g <- plot_data(data)
