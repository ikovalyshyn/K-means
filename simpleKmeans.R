# simple clustering using k-means

library(ggplot2)
library(gridExtra)
library(kernlab) # for kernel K-means

# ==================== Experiment 1 ============
# generate data1
set.seed(1)
x <- matrix(rnorm(200), nrow = 100, ncol = 2)
xmean <- c(0, 10)
which <- factor(sample(1:2, 100, replace = TRUE))
x[,2] <- x[,2] + xmean[which]

# plot data
x <- as.data.frame(x)
p <- ggplot(data = x, aes(x[,1], x[,2]))
p <- p + geom_point(aes(color = which))

# k-means
km <- kmeans(x, 2)


# plot result of clustering
png('Exp1.png')
(p12 <- p + geom_point(aes(color = factor(km$cluster)), shape = 0, size = 5) + 
        ggtitle("Experiment 1 (K-means)"))
dev.off()

# Accuracy
mean(which == km$cluster) # 1


# ==================== Experiment 2 ===========
# generate data2
set.seed(2)
x2 <- matrix(rnorm(200), nrow = 100, ncol = 2)
xmean2 <- c(0, 3)
which2 <- factor(sample(1:2, 100, replace = TRUE))
x2[,2] <- x2[,2] + xmean2[which2]

# plot data
x2 <- as.data.frame(x2)
p2 <- ggplot(data = x2, aes(x2[,1], x2[,2]))
p2 <- p2 + geom_point(aes(color = which2))

# k-means
km2 <- kmeans(x2, 2)


# plot result of clustering
p2 + geom_point(aes(color = factor(km2$cluster)), shape = 0, size = 5)

png('Exp2.png')
(p22 <- p2 + geom_point(aes(color = factor(km2$cluster)), shape = 0, size = 5)+
        ggtitle("Experiment 2 (K-means)"))
dev.off()

# Accuracy
mean(which2 == km2$cluster) # 0.94

png('Exp21.png')
grid.arrange(p12, p22)
dev.off()
# ==================== Experiment 3 ============
# generate data2
set.seed(3)
x3 <- matrix(rnorm(100), nrow = 50, ncol = 2)
#x = r*cos(t)
#y = r*sin(t)
t <- runif(50, min = 0, max = 2*pi)
x.c <- 5*cos(t) + 10 # r = 5
y.c <- 5*sin(t) + 10

d3.1 <- c(x3[,1] + 10, x.c)
d3.2 <- c(x3[,2] + 10, y.c)
df3 <- data.frame(x = d3.1, y = d3.2, cl = 1)
df3$cl[51:100] <- 2
df3$cl <- factor(df3$cl)

# plot data
p3 <- ggplot(data = df3, aes(x, y))
p3 <- p3 + geom_point(aes(color = cl))


# k-means
km3 <- kmeans(df3[,c(1,2)], 2)


# plot result of clustering
png('Exp3.png')
(p33 <- p3 + geom_point(aes(color = factor(km3$cluster)), shape = 0, size = 5)+
        ggtitle("Experiment 3 (K-means)"))
dev.off()


# Accuracy
mean(df3$cl == km3$cluster) # 0.32

#================== Experiment 3.1 (Kernel k-means) =================
df3.matr <- as.matrix(df3[,c(1,2)])
km33 <- kkmeans(df3.matr, 2)


# plot result of clustering
png('Exp31.png')
(p34 <- p3 + geom_point(aes(color = factor(km33@.Data)), shape = 0, size = 5)+
        ggtitle("Experiment 3.1 (Kernel k-means)"))
dev.off()

# Accuracy
mean(df3$cl == km33@.Data) # 1