require(ggplot2)

# Objective 1: Plot Data
plot_data <- function(){
    # figure out how to add labels -- see the R work code I've done for examples
    # on tweaking options
    g <- ggplot(data, aes(V1, V2, V3))
    g <- g + geom_point(aes(x=V1, y=V2, colour=V3))
    return(g)
}

# Objective 2: Create 28 more 6th degree features
feature_map <- function(){
    # Octave code
    #degree = 6;
    #out = ones(size(X1(:,1)));
    #for i = 1:degree
        #for j = 0:i
            #out(:, end+1) = (X1.^(i-j)).*(X2.^j);
        #end
    #end

    #end
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


data <- read.csv('ex2data2.txt', header=FALSE)
data$V3 <- factor(data$V3)

# don't pass in data since we don't have to.
#g <- plot_data()
X <- feature_map()
