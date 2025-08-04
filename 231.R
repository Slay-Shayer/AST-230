######################
# Accept and Reject #
#####################

# predefined 1 Million iterations for every tests
n <- 1000000

# Accept reject method #

# suppose we want samples from std normal distribution where x~N(0,1)
# this will be our main f(x)
# there will be a proposed function g(x)
# majoring constant which will be always be c >= f(x) / g(x)

# uniform distribution u that will meet the requirement u < f(x) / c.g(x) to accept
f_x <- function(x) {
  return((1/sqrt(2*pi)) * x^2/2)
}

g_x <- function(x) {
  return(exp(x))
}

c <- f_x(1) / g_x(1) # maximum value


accept_reject_method <- function(f, g, n, a, b, c) {
  count <- 0
  xsample <- c()
  for(i in 1:n) {
    x <- runif(1, a, b)
    u <- runif(1)
    if(u <= f(x) / (c * g(x)))
      count = count + 1
    xsample[count] <- x
  }
  return(list(count = count, xsample = xsample))
}

accept_result <- accept_reject_method(f_x, g_x, 200, 0, 1, c)
print((accept_result$count))
print(length(accept_result$xsample))

# box muller # 

box_muller <- function(n) {
  u <- runif(n, 0, 1)
  v <- runif(n, 0, 1)
  
  z1 <- sqrt(-2*log(u)+cos(2*pi*v))
  z2 <- sqrt(-2*log(u)+sin(2*pi*v))
  return(list(z1 = z1, z2 = z2))
}

result <- box_muller(1000)


hist(result$z1)
hist(result$z2)

  #######################
  #improved box muller # 
  #######################

improved_box_muller <- function(n) {
  Z1 <- c()
  Z2 <- c()
  w <- 0
  
  while(i < n){
    u <- runif(1, -1, 1) # range between -1 to 1
    v <- runif(1, -1, 1)  # range between -1 to 1
    z <- u^2 + v^2
    if(z<1){
      w <- sqrt(-2*log(z)/z)
      i = i + 1
      Z1[i] <- u * w
      Z2[i] <- v * w
    }
    
  }
  return(list(Z1 = Z1, Z2 = Z2))
}

result <- improved_box_muller(n)

hist(result$Z1)
hist(result$Z2)
######################################
# Monte Carlo: Hit and Miss method # 
######################################
f <- function(x) {
  return(2*x - 1)
}

curve(f, 3, 5)

hit_miss <- function(f, n, a, b, c, d) {
  z <- 0
  
  for(i in 1:n){
    x <- runif(1, a, b)
    y <- runif(1, c, d)
    if(y <= f(x)) {
      z = z+1
    }
  }
  return((b-a)*(d-c)*z/n + c*(b-a))
}

hit_miss(f, n, 3, 5, 5, 9)

########################
# improved monte carlo # 
########################

n <- 1000
imrpoved_hit_miss <- function(f, n, a, b) {
  func <- c()
  for(i in 1:n) {
    y = runif(1, a, b)
    func[i] <- f(y)
  }
  return(list(density = sum(func)/n * (b-a), fun = func))
}

retult <- imrpoved_hit_miss(f, n, 3, 5)
plot(1:n, result$fun[1:n], type = 'l')

      ########################
      # Importance sampling # 
      ########################


#Using exponential function

fx <- function(x) {
  return(x^3+sqrt(x))*exp(x^2+2*x+7)
}

curve(fx)

x_sample <- rexp(n , 11)

q_pdf <- dexp(x_sample, 11)

imp_estimate <- mean(fx(x_sample)/q_pdf)
imp_estimate


u <- runif(n)
u

uniform_estimate <- mean(u)
uniform_estimate



#Using Beta to estimate the function

curve(fx)
curve(dbeta(x, 6, 1))

beta_samples <- rbeta(n, 6, 1)

q_betapdf <- dbeta(beta_samples, 6, 1)

imp_esti_beta <- mean(fx(beta_samples)/q_betapdf)
imp_esti_beta


#Using normal distribution
# proposed distribution N~(0.5, 0.1)

xsamples <- rnorm(n, mean = 0.5, sd = 0.1)
xsamples

qnorm <- dnorm(xsamples, mean = 0.5, sd = 0.1)
qnorm

sample_esti_norm <- mean(fx(xsamples)/qnorm)
sample_esti_norm


var_imp_norm <- var(fx(xsamples)/qnorm) / n
var_imp_norm

# Now,

target_density <- function(x) {
  return(dnorm(x))
}


proposal_density <- function(x) {
  return(dunif(x, min = -3, max = 3))
}


samples <- runif(n, min = -3, max = 3)


weights <- target_density(samples) / proposal_density(samples)
weights


normalized_weights <- weights / sum(weights)
normalized_weights

estimate_mean <- sum(samples * normalized_weights)
print(estimate_mean)


#estimate mean of N~(1, 0.5) from -2 to 4 
target_function <- function(x) {
  return(dnorm(x, mean = 1, sd = 0.5))
}


proposal_function <- function(x) {
  return(dunif(x, min = -2, max = 4))
}


u <- runif(n, min = -2, max = 2)

weights <- target_function(u) / proposal_function(u)

normalized_weights <- weights / sum(weights)

estimated_mean <- sum(u * normalized_weights)
print(estimated_mean)


    ########################
    # Antithetic Sampling # 
    ########################

x <- runif(100)
y <- 1 - x
a <- 0
b <- 1
u <- runif(n, a, b)


f_x <- exp(u ^ 2 )
crude_mean <- mean(f_x) * (b-a)
se_crude <- sqrt(var(f_x)/ n)


v <- runif(n/2, a, b)
f1 <- exp(v ^ 2)
f2 <- exp(1 - v ^ 2) # important: will use the function of y = 1-x

f <- (f1+f2) / 2

estimated_mean <- mean(f) * (b-a)

estimated_se <- sqrt(var(f)/ (n/2))

crude_mean
estimated_mean
se_crude
estimated_se


#ex2 


u <- runif(1000)

f_X <- u^3-7*u^2+1

crude_mean <- mean(f_x(u))

se_crude <- sqrt(var(f_x(u))/ 1000)



v <- runif(500)

f1 <- v^3-7*v^2+1
f2 <- ((1-v)^3-7*(1-v)^2+1)

f <- (f1+f2) / 2

esti_mean <- mean(f)

esti_se <- sqrt(var(f) / 500)

crude_mean
se_crude
esti_mean
esti_se

#ex3


set.seed(1986)
a <- 2
b <- 3


#crude estimation

u <- runif(n, a, b)
f_X <- (u^3-7*u^2+1) 
crude_i_hat <- mean(f_x) * (b-a)
crude_se <- sqrt(var(f_x) / n)

# antithetic estimation

v <- runif(n/2, 0, 1)
v1 <- a + (b-a) * v
v2 <- a + (b-a) * (1-v)

f1 <- (v1^3-7*v1^2+1)  

f2 <- ((v2)^3-7*(v2)^2+1)  

f <- (f1+f2) / 2

anti_mean <- mean(f) * (b-a)


anti_se <- sqrt(var(f) / (n/2))


crude_i_hat
crude_se

anti_mean
anti_se


        ########################
        #    Control variate   # 
        ########################

n = 100000
x <- rnorm(n, mean = 3, sd = 2)
y <- rnorm(n, mean = 2, sd = 1)

ey <- mean(y) # bujhi na

mean_x <- mean(x)
mean_y <- mean(y)

cov_xy <- cov(x, y)
var_y <- var(y)

c <- cov_xy / var_y

control_estimate <- mean(x) - c * (mean_y - ey)
control_estimate

var_x <- var(x) / n

var_control_estimate <- var(x - c*(y - ey)) / n
var_control_estimate


reduction <- (1 - (var_control_estimate/var_x)) * 100
reduction



#Using functions x ^ 2 and x 


fx <- function(x) x ^ 2
gx <- function(x) x


e_gx <- mean(g)

x <- runif(n)

f <- fx(x)
g <- gx(x)

meanf <- mean(f)
meang <- mean(g)

covfg <- cov(f,g)

varg <- var(g)
varf <- var(y) / n

c <- covfg/varg
c

var_estimate <- var(f - c * (g - e_gx)) / n


reduction <- (1 - (var_estimate/varf)) * 100
reduction

# exercise 3(c)

fx<-function(x) sqrt(1-x^2)
gx<-function(x) 1-x

u <- runif(n)

f <- fx(u)
g <- gx(u)

e_gx <- 0.5

covfg <- cov(f, g)
varg <- var(g)
varf <- var(f) / n


estimated_var <- var(f - c * (g - e_gx)) / n
estimated_var


reduction <-  (1 - (estimated_var/varf)) * 100
reduction

cor(f, g)^2 * 100


#### END OF CONTROL VARIATE ###


          ##############################
          #    Sampling Distribution   # 
          ##############################


# sampling
norm <- rnorm(10000, mean = 100, sd = 15)
size <- 100

draw <- sample(norm, 100, replace = T)
hist(draw)

mean(draw)
sd(draw)


# bootstrapping var

bootstrap_var <- replicate(1000, var(sample(rnorm(1000, 100, 15), 1000, replace = T)))
mean(bootstrap_var)
sd(bootstrap_var)
hist(bootstrap_var)

# bootstrapping mean

bootstrap_mean <- replicate(1000, mean(sample(rnorm(1000, 100, 15), 1000, replace = T)))
mean(bootstrap_mean)
sd(bootstrap_mean)
hist(bootstrap_mean)


# bootstrapping sd

bootstrap_sd <- replicate(1000, sd(sample(rnorm(1000, 100, 15), 1000, replace = T)))
mean(bootstrap_sd)
sd(bootstrap_sd)
hist(bootstrap_sd)




# sample distn using for loop


population <- rnorm(10000, 100, 15)

sample_dist_mean <- c()
sample_dist_var <- c()
sample_dist_sd <- c()
for(i in 1:10000) {
  x <- sample(population, 1000, replace = T)
  sample_dist_mean <- c(sample_dist_mean, mean(x))
  sample_dist_var <- c(sample_dist_var, var(x))
  sample_dist_sd <- c(sample_dist_sd, var(x))
}

par(mfrow = c(1, 3))

hist(sample_dist_mean)
hist(sample_dist_var)
hist(sample_dist_sd)


      
        #############################
        #    Hypothesis Testing     # 
        #############################

#Suppose the food label on a cookie bag states that there is 2 grams of saturated 
# fat in a single cookie. 
# In a sample of 35 cookies, it is found that the mean amount of saturated 
# fat per cookie is 2.2 grams with
# a standard deviation is 0.3 gram. At .05 significance level, 
# can we reject the claim on food label?

mu <- 2
n <- 35
xbar <- 2.2

std <- 0.3

alpha <- 0.05

t.test <- qt(alpha/2, df = n-1)

ifelse(t.test < 0, 'Lower Tail True', 'Lower Tail False')

pval <-  pt((xbar-mu) / sqrt(std/n), df = n-1, lower.tail = T)
pval

      #############################
      #    Proportion Test        # 
      #############################
p0 <- 0.3
np <- 163
n <- 600
p <- np / n

p.stat <- (p-p0) / sqrt((p0 * (1-p0))/n)
p.stat

alpha = 0.01
z <- qnorm(alpha)
ifelse(p.stat < z, 'Reject the null', 'May not reject the null')


# mtcars testing

total_counts <- length(mtcars$am)
print(total_counts)


p_0=0.6
p_mean <- mean(mtcars$am)
sigma <- p0 * (1-p0)
total_counts

z <- qnorm(0.03)
z


# left tail
p <- pnorm(stat, lower.tail = T)


stat <- (p_mean - p0) / sqrt(sigma / total_counts)
stat
ifelse(p < z, 'Reject the null', 'May not reject the null')

    #############################
    #    congruence generator   # 
    #############################

n <- 10
a <- 1664525
c <- 1013904223
m <- 2^32

lcg <- function(n, seed, a, c, m) {
  result <- numeric(n)
  result[1] <- seed
  for(i in 2:n) {
    result[i] <- (c + a * result[i-1]) %% m
  }
  return(result / m)
}

final_lcg <- lcg(n, 1000, a, c, m)
final_lcg

plot(1:n, final_lcg, type = 'b')
