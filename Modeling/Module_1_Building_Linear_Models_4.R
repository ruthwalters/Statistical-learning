# Building Linear Regression Models with User-Specified Loss Function


# ------------------------------------------
# ------------------------------------------
# Experiment 1: Define a function in R
# ------------------------------------------
# ------------------------------------------

# Suppose that I would like to define a function, f(x)=x^2

# Define a function
my.fun = function(x){
  output = (x-2)^2 + (x+3)^2
  return(output)
}

# run the function
my.fun(4)
my.fun(-6)


# ------------------------------------------
# ------------------------------------------
# Experiment 2: Numerically minimize/maximize a function
# ------------------------------------------
# ------------------------------------------

# Suppose that I would like to minimize the function, f(x)=x^2
output = optim(par=200, my.fun, method = "BFGS")
# par: initial values for the parameters to be optimized over.

# extract the optimal value:
output$par


# Suppose that I would like to minimize the function, f(x)=(x-4)^2 + 2
my.fun.2 = function(x){
  output = (x-4)^2 + 2
  return(output) 
}

output = optim(par=8, my.fun.2)
# par: initial values for the parameters to be optimized over.

# extract the optimal value:
output$par


# ------------------------------------------
# ------------------------------------------
# Experiment 3: Building a linear regression model with user-defined loss function
# ------------------------------------------
# ------------------------------------------


wd = "/Users/Xiao/Desktop/2023 Spring/code"
setwd(wd)
data = read.csv("Advertising.csv", 
	header=TRUE, stringsAsFactors = FALSE, sep=",", row.names=1)
head(data)
data = Advertising[,c("TV","sales")] # only one feature "TV" is used here


# let's build the model using the quadratic loss function: r^2

my.loss = function(beta, input.data){
  y.hat = beta[1] + beta[2]*input.data[,1] # y_hat = b0 + b1X_i
  residual = y.hat - input.data[,2] # y_hat - y
  loss = sum( residual^2 )
  return(loss)
}

model.1 = optim(par=c(0,0), my.loss, input.data=data)
beta.1 = model.1$par

# plot
plot(data)
abline(coef = beta.1, col="red")


# Now, let's use a different loss function: |r|

my.loss.2 = function(beta, input.data){
  y.hat = beta[1] + beta[2]*input.data[,1]
  residual = y.hat - input.data[,2]
  loss = sum( abs(residual) ) #sum(|r_i|)
  return(loss)
}

model.2 = optim(par=c(0,0), my.loss.2, input.data=data)
beta.2 = model.2$par

# plot
plot(data)
abline(coef = beta.2, col="blue")
abline(coef = beta.1, col="red")



# Now, let's use a different loss function: 
# If r > 0, then, r^2. If r < 0, then, |r|

my.loss.3 = function(beta, input.data){
  y.hat = beta[1] + beta[2]*input.data[,1]
  residual = y.hat - input.data[,2]
  loss.part1 = 0
  if (sum(residual >= 0)>0){
    print('no')
    case1 = which(residual >= 0)
    loss.part1 = sum( residual$TV[case1]^2 )
  }
  loss.part2 = 0
  if (sum(residual < 0)>0){
    case2 = which(residual < 0)
    loss.part2 = sum ( abs(residual$TV[case2]) )
  }
  loss = loss.part1 + loss.part2
  return(loss)
}

model.3 = optim(par=c(0,0), my.loss.3, input.data=data)
beta.3 = model.3$par

# plot
plot(data)
abline(coef = beta.3, col="green")
abline(coef = beta.2, col="blue")
abline(coef = beta.1, col="red")








