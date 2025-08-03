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

c <- f_x(1) / g_x(1)


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
  
  z1 <- sqrt(-2*log(u1)+cos(2*pi*v))
  z2 <- sqrt(-2*log(u1)+sin(2*pi*v))
  return(c(z1, z2))
}

box_muller(1000)


hist(z1)
hist(z2)

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


imrpoved_hit_miss <- function(f, n, a, b) {
  func <- c()
  for(i in 1:n) {
    y = runif(1, a, b)
    func[i] <- f(y)
  }
  return(sum(func)/n * (b-a))
}

imrpoved_hit_miss(f, n, 3, 5)

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



