y = c(1,6,16,23,27,39,31,30,43,51,63,70,88,97,91,104,110,113,149,159)
period = c(1:20)
logPeriod = log(period)

plot(logPeriod, y)


### --- CODE --- ###

# function to calculate diagonal matrix W
W_matrix = function(X, b){
  
  # initialize diagonal of matrix
  diag_vector = c()
  
  # calculate w_ii and add to diagonal vector
  for( i in 1:nrow(X) ){
    w_ii = exp( b[1]*X[i,1] + b[2]*X[i,2]  )
    
    diag_vector = c(diag_vector, w_ii)
  }
  
  # create diagonal matrix with diag()
  return(diag(diag_vector))
}

# function to calculate element z_i
z_vector = function(X, y, b){
  
  # initialize empty z vector
  z = c()
  
  # calculate z_i
  for( i in 1: length(y) ){
    z_i = (b[1]*X[i,1] + b[2]*X[i,2]) + ( y[i] / exp(b[1]*X[i,1] + b[2]*X[i,2]) ) - 1
    
    z = c( z, z_i )
  }
  
  return(z)
}

# function to calculate matrix Xtranpose*W*X
# tranpose of a matrix A: t(A)
# matrix multiplication operator: %*%, e.g, A%*%B is A multiply with B
Xtranpose_WX = function(X, W){
  return( t(X) %*% W %*% X)
}

# function to calculate matrix Xtranspose*W*z
Xtranpose_Wz = function(X, W, z){
  return( t(X) %*% W %*% z )
}

# deviance of model
CalculateDeviance = function(X, y, b){
  
  D = 0
  for( i in 1:length(y) ){
    y_hat = b[1]*X[i,1] + b[2]*X[i,2]
    
    cat(y[i], " --- ", y_hat)
    cat("\n")
    
    D = D + ( y[i] * log(y[i] / y_hat) )
  }
  
  return( 2*D )
}

# matrix X
X = matrix( c( rep(1, 20), log( c(1:20) ) ) , 20, 2 )

# vector y
y = c(1,6,16,23,27,39,31,30,43,51,63,70,88,97,91,104,110,113,149,159)

# ERROR ACCEPTABLE
ACCEPTABLE_ERROR = c(10**-6, 10**-6)

# initiate b(0) = (1,1)
previous_b = c(1, 1)
current_b  = c(0, 0)

iterated_step = 0

#A = Xtranpose_WX(X, W)
#B = Xtranpose_Wz(X, W, z)
#C = W_matrix(X, previous_b)

# iterated weight least squared, finding vector b
while(TRUE){
  
  iterated_step =+ 1
  
  # W matrix
  W = W_matrix(X, previous_b)
  
  # z vector
  z = z_vector(X, y, previous_b)
  
  # calculate current_b, using solve(A) to calculate inverse of a matrix A
  Xt_WX = Xtranpose_WX(X, W)
  Xt_Wz = Xtranpose_Wz(X, W, z)
  
  current_b = solve(Xt_WX) %*% Xt_Wz
  
  residual = abs(current_b - previous_b)
  print(residual)
  
  if( all(residual < ACCEPTABLE_ERROR) ){
    break
  }
  else{
    previous_b = current_b
  }
  
}

# deviance of current model
D = CalculateDeviance(X, y, current_b)

# chi squared test of deviance to verify goodness-of-fit
pchisq(D, nrow(X) - length(current_b), lower.tail = FALSE )

print(current_b)
print(iterated_step)
print(D)

# verify with glm in R
res.p = glm(y ~ X, family = poisson(link = "log"))
summary(res.p)




