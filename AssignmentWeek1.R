#load data in project
#data <- load("/Users/dsalimian/Documents/School/Master/Multivariate Statistics/Week1/supermarket1996.RData")

# a: program ridge regression as an R function

ridgeRegression <- function(X, y, lambda, standardize = TRUE, intercept = TRUE){
  # NOTE: maybe first test for heteroscedasticity and use dependent on the results either the normal or hetero ridge approach
  
  # if we want to standardize our input, then do so here
  if(standardize){
    # standardize the columns to z-score
    X_mean <- apply(X, 2, mean) #note this is a vector containing all column means of matrix X
    X_stdev <- apply(X, 2, sd) # note this is a vector containing all column standard deviations of matrix X
    X_stnd <- scale(X,X_mean,X_stdev) # this contains now the z-scores for each column  
  }
  
  # if we do not want any intercept..
  if(!intercept){
    # make sure that our (in)dependent variable sums to 0. Hence remove the mean from each observation
    y_mean <- mean(y)
    y_stnd <- y-y_mean
    
    # only de-mean X when it isn't standardized yet
    if(!standardize){
      X_mean <- apply(X, 2, mean) #note this is a vector containing all column means of matrix X
      X_stnd <- scale(X,X_mean, FALSE)
    }
  }else{
    y_stnd <- y # when we do not de-mean, and thus leave the intercept, we need to rename our dependent var for the rest of the code to work
  }
  
  if(!standardize && intercept){
    X_stnd <- as.matrix(X) # when we do not standardize, and leave the intercept, we need to rename our input var for the rest of the code to work
  }

  X_stnd <- rbind(X_stnd, diag(dim(X_stnd)[2])*sqrt(lambda))
                  
}
    
  
  # # since lambda is given, we can proceed and calculate the beta_ridged now
  # b_ridged <- solve(t(X_stnd) %*% X_stnd + lambda * diag(dim(X_stnd)[2])) %*% t(X_stnd) %*% y
  # 
  # # using the esitmates paramters, we can calculate our y_hat values
  # y_hat <- X_stnd %*% b_ridged
  # 
  # # now calculate the standard deviation of the estimated parameters b_ridged
  # estimation_error <- y_stnd-y_hat
  # error_variance <- (t(estimation_error)%*%estimation_error)/(dim(X_stnd)[1]-dim(X_stnd)[2])
  # error_variance <- as.numeric(error_variance)
  # 
  # b_ridged_var <- error_variance * solve(t(X_stnd) %*% X_stnd + lambda * diag(dim(X_stnd)[2])) %*% t(X_stnd) %*%
  #   X_stnd %*% solve(t(X_stnd) %*% X_stnd + lambda * diag(dim(X_stnd)[2]))
  # 
  # # calculate the degrees of freedom
  # hat_ridge <- X_stnd %*% solve(t(X_stnd) %*% X_stnd + lambda * diag(dim(X_stnd)[2])) %*% t(X_stnd)
  # df_ridge <- sum(diag(hat_ridge)) # this is taking the trace of the hat_ridge variable
  # 
  # # calculate the AIC value
  # AIC <- dim(X_stnd)[2] * log(t(estimation_error)%*%estimation_error) + 2 * df_ridge
  # AIC <- as.numeric(AIC)
  # 
  # # calculate the AIC value
  # BIC <- dim(X_stnd)[2] * log(t(estimation_error)%*%estimation_error) + 2 * df_ridge * log(dim(X_stnd)[2])
  # BIC <- as.numeric(BIC)
  # 
  # # calculate the correlation coefficient of y and y_hat
  # R = cov(y,y_hat)/(sd(y) * sd(y_hat))
  # R = as.numeric(R)
  # R_sq = R^2
  # 
  # result = list(b_ridge=b_ridged, b_ridged_var=b_ridged_var, estimation_error_sum=sum(estimation_error), AIC=AIC, BIC=BIC, R_sq=R_sq, df_ridge=df_ridge)
  # return(result)
  
# }

X <- supermarket1996[,6:50]
y <- supermarket1996$GROCCOUP_sum
lambda = 1
model1 = ridgeRegression(X,y,lambda, standardize = FALSE, intercept = TRUE) # answer to a) 1
model2 = ridgeRegression(X,y,lambda, standardize = FALSE, intercept = FALSE) # answer to a) 2
model3 = ridgeRegression(X,y,lambda, standardize = TRUE, intercept = FALSE) # answer to c)

print('b_ridged is as follows:')
print(model1$b_ridged)
print('variance of b_ridged is as follows:')
print(model1$b_ridged_var)
paste('the sum of the estimation errors is', model1$estimation_error_sum)
paste('the AIC value is', model1$AIC)
paste('the BIC value is', model1$BIC)
paste('R^2 value is ', model1$R_sq)
paste('the effective degrees of freedom is ', model1$df_ridge)

# now get the results using a package that is available for R; glmnet

# install.packages('glmnet') # helps installing the package if neccesary
library('glmnet')
ridge_glmnet = glmnet(as.matrix(X),y, standardize=TRUE, intercept=FALSE )
ridge_glmnet

# install.packages('genridge')
library('genridge')
model_ridge = ridge(y, X)
