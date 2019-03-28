
### --- CODE --- ###

# function to calculate pi_i
pi_i = function(X, b, i){
  tu_so  = exp( b[1]*X[i,1] + b[2]*X[i,2] )
  mau_so = 1 + exp( b[1]*X[i,1] + b[2]*X[i,2] )
  
  return( tu_so / mau_so )
}

# function to calculate diagonal matrix W
W_matrix = function(X, b, n){
  
  # initialize diagonal of matrix
  diag_vector = c()
  
  # calculate w_ii and add to diagonal vector
  for( i in 1:nrow(X) ){
    w_ii = n[i] * pi_i(X, b, i) * (1 - pi_i(X,b,i))
    
    diag_vector = c(diag_vector, w_ii)
  }
  
  # create diagonal matrix with diag()
  return(diag(diag_vector))
}

# function to calculate log(pi_i / (1 - pi_i))
eta_i = function(X, b, i){
  return ( log( pi_i(X, b, i) / (1 - pi_i(X, b, i)) ) )
}

# function to calculate element z_i
z_vector = function(X, y, b, n){
  
  # initialize empty z vector
  z = c()
  
  # calculate z_i
  for( i in 1: length(y) ){
    z_i = eta_i(X, b, i) + ( y[i] - n[i]*pi_i(X, b, i) ) / ( n[i]*pi_i(X, b, i)*(1 - pi_i(X, b, i)) )
    #z_i = log(pi_i(X,b,i) / (1 - pi_i(X,b,i)) ) + ( y[i] - n[i]*pi_i(X, b, i) ) / ( n[i]*pi_i(X, b, i)*(1 - pi_i(X, b, i)) )
    
    print(z_i)
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
CalculateDeviance = function(X, y, b, n){
  
  D = 0
  for( i in 1:length(y) ){
    y_hat= n[i] * pi_i(X, b, i)
    
    first_component  = y[i] * log(y[i] / y_hat)
    second_component = (n[i] - y[i]) * log( (n[i]-y[i]) / (n[i]-y_hat ) )
    
    D = D + ( first_component + second_component )
  }
  
  return( 2*D )
}

# matrix X
X = matrix( c( rep(1, 8), c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839) ) , 8, 2 )
X
# vector y
y = c(6,13,18,28,52,53,61,60)
n_y=n-y
beetle.mat=cbind(y,n_y)


# vector n
n = c(59,60,62,56,63,59,62,60)

# ERROR ACCEPTABLE
ACCEPTABLE_ERROR = c(10**-6, 10**-6)

# initiate b(0) = (1,1)
previous_b = c(1, 1)
current_b  = c(0, 0)

iterated_step = 0

z = z_vector(X, y, previous_b, n)
z

W = W_matrix(X, previous_b, n)
W
A = Xtranpose_WX(X, W)
A
B = Xtranpose_Wz(X, W, z)
B

C = W_matrix(X, previous_b,n)
C

current_b = solve(A) %*% B
current_b - previous_b

# iterated weight least squared, finding vector b
while(TRUE){
  
  iterated_step =+ 1
  
  # W matrix
  W = W_matrix(X, previous_b, n)
  
  # z vector
  z = z_vector(X, y, previous_b, n)
  
  # calculate current_b, using solve(A) to calculate inverse of a matrix A
  Xt_WX = Xtranpose_WX(X, W)
  Xt_Wz = Xtranpose_Wz(X, W, z)
  
  current_b = solve(Xt_WX) %*% Xt_Wz
  
  residual = abs(current_b - previous_b)
  print(residual)
  
  if( all(residual < ACCEPTABLE_ERROR)) {
    break
  }
  else{
    previous_b = current_b
  }
  
}

# deviance of current model
D = CalculateDeviance(X, y, current_b, n)

# chi squared test of deviance to verify goodness-of-fit
qchisq(0.05, df = nrow(X) - length(current_b))

# thay D < chi-square voi bac tu do n-p, ket luan mo hinh phu hop

print(current_b)
print(iterated_step)
print(D)

# verify with glm in R
res.p = glm(beetle.mat ~ X, family = binomial(link = "logit"))
summary(res.p)

qchisq(0.95, 6)



