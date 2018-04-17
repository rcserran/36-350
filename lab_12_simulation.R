generate_data = function(n, p) {
  covariates = as.matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n)
  responses = rnorm(n, mean = 0, sd = 1)
  return(list(covariates = covariates, responses = responses))
}