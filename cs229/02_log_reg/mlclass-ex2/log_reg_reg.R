require(ggplot2)

plot_data <- function(){
    # figure out how to add labels -- see the R work code I've done for examples
    # on tweaking options
    g <- ggplot(data, aes(V1, V2, V3))
    g <- g + geom_point(aes(x=V1, y=V2, colour=V3))
    g <- g + options(legend=FALSE)
    return(g)
}


# Objective 1: Plot Data
data <- read.csv('ex2data2.txt', header=FALSE)
data$V3 <- factor(data$V3)
plot_data()
