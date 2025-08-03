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

#improved box muller # 

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
  return(list(Z1 = Z1, Z2, Z2))
}

result <- improved_box_muller(1000000)

hist(result$Z1)
hist(result$Z2)

# Monte Carlo Hit and Miss method

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
  return((b-a)*(d-c)*z/n)
}

hit_miss(f, 1000000, 3, 5, 5, 9)


# improved monte carlo

imrpoved_hit_miss <- function(f, n, a, b) {
  func <- c()
  for(i in 1:n) {
    y = runif(1, a, b)
    func[i] <- f(y)
  }
  return(sum(func)/n * (b-a))
}

imrpoved_hit_miss(f, 1000000, 3, 5)
