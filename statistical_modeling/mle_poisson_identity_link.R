
Xtranpose_WX = function(X, b){
  
  e11 = 0
  e12 = 0
  e21 = 0
  e22 = 0
  
  for( i in 1:nrow(X) ){
    e11 = e11 + ( 1 / (b[1]*X[i,1] + b[2]*X[i,2]) )
    
    e12 = e12 + ( X[i,2] / (b[1]*X[i,1] + b[2]*X[i,2]) )
    
    e21 = e21 + ( X[i,2] / (b[1]*X[i,1] + b[2]*X[i,2]) )
    
    e22 = e22 + ( (X[i,2]**2) / (b[1]*X[i,1] + b[2]*X[i,2]) )
  }
  
  return( matrix(c(e11, e12, e21, e22), 2, 2) )
}

Xtranpose_Wz = function(X, y, b){
  e11 = 0
  e21 = 0
  
  for( i in 1:nrow(y) ){
    e11 = e11 + ( y[i,1] / ((b[1]*X[i,1] + b[2]*X[i,2])) )
    
    e21 = e21 + ( (X[i,2] * y[i,1]) / ((b[1]*X[i,1] + b[2]*X[i,2])) )
  }
  
  return( matrix(c(e11, e21), 2, 1) )
}

# deviance of model
CalculateDeviance = function(X, y, b){
  
  D = 0
  for( i in 1:nrow(y) ){
    y_hat = b[1]*X[i,1] + b[2]*X[i,2]
    
    D = D + (y[i,1] * log(y[i,1] / y_hat) - (y[i,1] - y_hat))
  }
  
  return( 2*D )
}

X = matrix(c(1,1,1,1,1,1,1,1,1, -1,-1,0,0,0,0,1,1,1),9,2)
y = matrix(c(2,3,6,7,8,9,10,12,15),9,1)

# ERROR ACCEPTABLE
ACCEPTABLE_ERROR = c(10**-6, 10**-6)

# initiate b(0) = (7,5)
previous_b = c(7, 5)
current_b  = c(0, 0)

iterated_step = 0

#A = Xtranpose_WX(X, previous_b)
#B = Xtranpose_Wz(X, y, previous_b)

# iterated weight least squared, finding vector b
while(TRUE){
  
  iterated_step =+ 1
  
  # solve(A) : get inverse matrix of matrix A
  current_b = solve(Xtranpose_WX(X, previous_b)) %*% Xtranpose_Wz(X, y, previous_b)
  
  residual = abs(current_b - previous_b)
  
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
res.p = glm(y ~ X, family = poisson(link = "identity"))
summary(res.p)




