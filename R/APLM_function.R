ci <- function(x, y, who, beta, alpha=0.05){

  n <- dim(x)[1]
  lw <- length(who)
  model <- lm(formula=y ~ ., data=x)

  sse <- sum((model$residuals)^2)
  df <- model$df.residual
  mse <- sse/df

  xx <- c(NULL); sxx <- c(NULL)
  cil <- c(NULL); ciu <- c(NULL)

  for(i in 1:lw){

    if(who[i]==0){

      xx[i] <- mean(x[,1])
      sxx[i] <- sum((x[,1]-xx[1])^2)

      cil[i] <- model$coefficients[1] - qnorm(1-alpha/2^lw) * sqrt(mse*(1/n+xx[1]^2/sxx[1]))
      ciu[i] <- model$coefficients[1] + qnorm(1-alpha/2^lw) * sqrt(mse*(1/n+xx[1]^2/sxx[1]))
    }else{

      xx[i] <- mean(x[,who[i]])
      sxx[i] <- sum((x[,who[i]]-xx[i])^2)

      cil[i] <- model$coefficients[who[i]+1] - qnorm(1-alpha/2^lw) * sqrt(mse/sxx[i])
      ciu[i] <- model$coefficients[who[i]+1] + qnorm(1-alpha/2^lw) * sqrt(mse/sxx[i])
    }
  }
  ciall <- data.frame(cil, ciu)
  rownames(ciall) <- paste0("Beta", who)
  return(ciall)
}

yget <- function(data, beta, e=rnorm(dim(data)[1]), way=1){
  data <- as.matrix(data)
  if(way==1){
    k <- length(beta)-1
    mat <- cbind(rep(1,dim(data)[1]), data[,1:k])
  }else if(way==2){
    k <- dim(data)[2]-length(beta)+1
    beta <- c(beta, rep(0, k))
    mat <- cbind(rep(1,dim(data)[1]), data)
  }
  y <- mat%*%beta+e
  return(y)
}

glt <- function(model1, model2, alpha=0.05){
  l1 <- length(model1$coefficients)
  l2 <- length(model1$coefficients)
  sse1 <- sum((model1$residuals)^2)
  sse2 <- sum((model2$residuals)^2)
  df1 <- model1$df.residual
  df2 <- model2$df.residual
  ans <- ((sse1-sse2)/(l1-l2))/(sse2/df2) > qf(1-alpha,2,df2)
  if(ans){
    print("model1 is better")
  }else{
    print("model2 is better")
  }
}
