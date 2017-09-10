library(MASS)
require(plyr)
require(reshape2)
require(ggplot2)
# Calculate the covariance matrix using the squared exponential covariance
# function

# We will choose for simplicity the squared exponential covariance function
# First step is to calculate the Euclidean distance. 
euclidean_distance <- function(x, y){
   return(sqrt(sum((x -y) ^ 2)))
}

# confirm that this is the case using a 3-4-5 triangle
x <- c(0,0.1,2,3)
y <- c(0,0.1,2,3)
euclidean_distance(x,y) # should return 5

squared_exponential_covariance <- function(x,y, l=1, tau=1){
  return((tau ^ 2) * exp( -euclidean_distance(x,y) / (2 * l ^ 2)))
}

radial_basis_kernel <- function(x,y){

    sigma <- matrix(rep(0, length(x) * length(y)), nrow= length(x), ncol=length(y))
    for (i in 1:nrow(sigma)){
      for (j in 1:ncol(sigma)){
      sigma[i,j] = squared_exponential_covariance(x[i], y[j])
    }
  }
    return(sigma)
}
 



radial_basis_kernel(x,y)

x.star <- seq(0,1,len=2)


num_samples <- 3

results <- matrix(rep(0, num_samples * length(x.star)), ncol = num_samples)

for (i in 1:num_samples){
  results[,i] <- mvrnorm(1, rep(0, length(x.star)),radial_basis_kernel(x.star, x.star))
}

results

results <- cbind(x=x.star, as.data.frame(results))
results <- melt(results, id="x")

fig2a <- ggplot(results,aes(x=x,y=value)) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=-2, ymax=2, fill="grey80") +
  geom_line(aes(group=variable)) +
  theme_bw() +
  scale_y_continuous(lim=c(-2.5,2.5), name="output, f(x)") +
  xlab("input, x")
fig2a



# Plot the shape of this function as the distance between x and y grows bigger and bigger
# Notice that this function is not differentiable at x=0,y=0!
y <- seq(-100, 100, by=.1)
results <- lapply(y, sq_exp_cov, x=x)
plot(y, results)




x.star <- seq(-5,5,len=50)

# Calculate the covariance matrix
sigma <- radial_basis_kernel(x.star,x.star)

# Generate a number of functions from the process
n.samples <- 3
values <- matrix(rep(0,length(x.star)*n.samples), ncol=n.samples)
for (i in 1:n.samples) {
  # Each column represents a sample from a multivariate normal distribution
  # with zero mean and covariance sigma
  values[,i] <- mvrnorm(1, rep(0, length(x.star)), sigma)
}
values <- cbind(x=x.star,as.data.frame(values))
values <- melt(values,id="x")

# Plot the result
fig2a <- ggplot(values,aes(x=x,y=value)) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=-2, ymax=2, fill="grey80") +
  geom_line(aes(group=variable)) +
  theme_bw() +
  scale_y_continuous(lim=c(-2.5,2.5), name="output, f(x)") +
  xlab("input, x")

f <- data.frame(x=c(-4,-3,-1,0,2),
                y=c(-2,0,1,2,-1))

x <- f$x
y <- f$y
k.xx <- radial_basis_kernel(x,x)
k.xxs <- radial_basis_kernel(x,x.star)
k.xsx <- radial_basis_kernel(x.star,x)
k.xsxs <- radial_basis_kernel(x.star,x.star)

ident<- matrix(rep(1,nrow(k.xx) * ncol(k.xx)), ncol = ncol(k.xx))

solve(k.xx) %*% k.xx
k.xx %*% solve(k.xx)

dim(k.xsx)
?mvrnorm()

f.star.bar <- k.xsx%*%solve(k.xx)%*%f$y
cov.f.star <- k.xsxs - k.xsx%*%solve(k.xx)%*%k.xxs

n.samples <- 1
values <- matrix(rep(0,length(x.star)*n.samples), ncol=n.samples)
for (i in 1:n.samples) {
  values[,i] <- mvrnorm(1, f.star.bar, cov.f.star)
}

values <- cbind(x=x.star,as.data.frame(values))
values <- melt(values,id="x")

fig2b <- ggplot(values,aes(x=x,y=value)) +
  geom_line(aes(group=variable), colour="grey80") +
  geom_line(data=NULL,aes(x=as.vector(x.star),y=as.vector(f.star.bar)),colour="red", size=1) + 
  theme_bw() +
  scale_y_continuous(lim=c(-3,3), name="output, f(x)") +
  xlab("input, x")

fig2a
fig2b



def conditional(x_new, x, y, params):
kxsx = exponential_cov(x_new, x, params)
kxx = exponential_cov(x, x, params)
kxsxs = exponential_cov(x_new, x_new, params)

mu = np.linalg.inv(kxx).dot(kxsx.T).T.dot(y)
sigma = kxsxs - kxsx.dot(np.linalg.inv(kxx).dot(kxsx.T))
return(mu.squeeze(), sigma.squeeze())

