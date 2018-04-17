generate_data = function(n, p) {
  covariates = as.matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n)
  responses = rnorm(n, mean = 0, sd = 1)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff) {
  retained = covariates[, summary(lm(responses ~ covariates))$coefficients[,4] <= cutoff]
  return(summary(lm(responses ~ retained))$coefficients[,4])
}