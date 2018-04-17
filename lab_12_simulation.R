generate_data = function(n, p) {
  covariates = as.matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n)
  responses = rnorm(n, mean = 0, sd = 1)
  return(list(covariates = covariates, responses = responses))
}

model_select = function(covariates, responses, cutoff) {
  retained = covariates[, summary(lm(responses ~ covariates))$coefficients[,4] <= cutoff]
  return(summary(lm(responses ~ retained))$coefficients[,4])
}

run_simulation = function(n_trials, n, p, cutoff) {
  p.values = vector(mode = "numeric", length = n_trials)
  for(trial in 1:n_trials) {
    trial.data = generate_data(n,p)
    p.values[trial] = model_select(trial.data$covariates, trial.data$responses, cutoff)
  }
  return(write.csv(p.values), file = "p_values.csv")
}

make_plot = function(datapath) {
  hist(read.csv(datapath))
}