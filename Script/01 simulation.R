# Simulation function
sim_function <- function(lm_obj, nsim = 1000, scenario) {
  beta_hat <- coef(lm_obj)
  V_hat <- vcov(lm_obj)
  library(MASS)
  S <- mvrnorm(nsim, beta_hat, V_hat)
  if (is.null(nrow(scenario))) {
    scenario <- matrix(scenario, nrow = 1)
  }
  if (ncol(scenario) != length(lm_obj$coefficients)) {
    stop("The scenario has the wrong number of variables.")
  }
  EV <- S %*% t(scenario)
  return(EV)
}

# Hypothesis 1
tenure_range <- seq(min(data$tenure, na.rm = T), max(data$tenure, na.rm = T), length.out = 250)
authorities_range <- seq(min(data$authorities, na.rm = T), max(data$authorities, na.rm = T), length.out = 250)

high_authorities <- quantile(data$authorities, 0.75, na.rm = TRUE)


## Scenario 1, 3, 5
scenario1 <-
  cbind(
    1,                                        # intercept
    1,                                        # female
    1,                                        # minority
    median(data$tenure, na.rm = T),           # tenure
    median(data$authorities, na.rm = T),      # authorities
    median(data$elite, na.rm = T),            # elite
    1*1                                       # interaction term
  )

set.seed(4547)
sim_res1 <- sim_function(model4, nsim = 1000, scenario1)

## Scenario 2
scenario2 <-
  cbind(
    1,                                        # intercept
    0,                                        # female
    0,                                        # minority
    median(data$tenure, na.rm = T),           # tenure
    median(data$authorities, na.rm = T),      # authorities
    median(data$elite, na.rm = T),            # elite
    0*0                                       # interaction term
  )

set.seed(4547)
sim_res2 <- sim_function(model4, nsim = 1000, scenario2)

## Scenario 4
scenario4 <-
  cbind(
    1,                                        # intercept
    1,                                        # female
    0,                                        # minority
    median(data$tenure, na.rm = T),           # tenure
    median(data$authorities, na.rm = T),      # authorities
    median(data$elite, na.rm = T),            # elite
    1*0                                       # interaction term
  )

set.seed(4547)
sim_res4 <- sim_function(model4, nsim = 1000, scenario4)

## Scenario 6
scenario6 <-
  cbind(
    1,                                        # intercept
    0,                                        # female
    1,                                        # minority
    median(data$tenure, na.rm = T),           # tenure
    median(data$authorities, na.rm = T),      # authorities
    median(data$elite, na.rm = T),            # elite
    0*0                                       # interaction term
  )

set.seed(4547)
sim_res6 <- sim_function(model4, nsim = 1000, scenario6)

#Xbeta values in the response function 
exp_1 <- exp(sim_res1)
exp_2 <- exp(sim_res2)
exp_4 <- exp(sim_res4)
exp_6 <- exp(sim_res6)

df_scenarios <- cbind(exp_1, exp_2, exp_4, exp_6)

sc1.2 <- df_scenarios[, 2] - df_scenarios[, 1]
sc1.4 <- df_scenarios[, 3] - df_scenarios[, 1]
sc1.6 <- df_scenarios[, 4] - df_scenarios[, 1]

median1.2 <- median(sc1.2) # Median for fd
ci_fd1.2 <-  quantile(sc1.2, probs = c(0.025, 0.975)) # 95% CIs for first difference

median1.4 <- median(sc1.4) 
ci_fd1.4 <-  quantile(sc1.4, probs = c(0.025, 0.975))

median1.6 <- median(sc1.6) 
ci_fd1.6 <-  quantile(sc1.6, probs = c(0.025, 0.975))

pl.line <- data.frame(
 line = c(ci_fd1.2, ci_fd1.4, ci_fd1.6),
 val = c(2,2,1,1,0,0),
 cat = c(1,1,2,2,3,3)
)

pl.dot <- data.frame(
  line = c(median1.2, median1.4, median1.6),
  val = c(2,1,0),
  cat = c(1,2,3)
)
