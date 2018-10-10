L <- function(theta, y, x){
  p <- 1/(1+exp(-x%*%t(t(theta)))) # Logistik sannolikhet
  
  res <- c()
  for (pi in p){
    it <- dbinom(1, 1, pi) # Bernoulli fördelning
    res <- append(res, it)
  }

  return(prod(res))
}

l <- function(theta, y, x){
  p <- 1/(1+exp(-x%*%t(t(theta))))
  
  res <- c()
  for (pi in p){
    it <- dbinom(1, 1, pi)
    res <- append(res, it)
  }

  return(sum(log(res)))
}

S <- function(theta, y, x){
  p <- 1/(1+exp(-x%*%t(t(theta))))
  res <- t(x) %*% (y-p)
  return(res)
}

I <- function(theta, y, x){
  p <- 1/(1+exp(-x%*%t(t(theta))))
  v <- p*(1 - p)
  D <- diag(as.vector(v))
  res <- t(x) %*% D %*% x
  return(res)
}

NR <- function(theta0, niter, y, x){
  count <- 0
  
  ST <- S(theta0, y, x)
  IT <- I(theta0, y, x)

  ntheta <- theta0
  while (count <= niter){
    ntheta <- ntheta + solve(IT) %*% ST
    ST <- S(ntheta, y, x)
    IT <- I(ntheta, y, x)
    count <- count + 1
  }
  return(ntheta)
}