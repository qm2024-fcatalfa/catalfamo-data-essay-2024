load("data/data_essay24_data.Rdata")

pack <- c("tidyverse", "stargazer", "MASS", "optimx", "patchwork", "haven", "knitr")

lapply(pack, library, character.only=T)
  
# Rename column in the dataset
name <- c("citation", "authorities", "female", "minority",
          "elite", "tenure", "constit", "termmeritsper",
          "year")

colnames(data) <- name

# Summary statitics and plots ----
summary(data)

## Plot the number of citations ----
ggplot(data, aes(citation)) +
  geom_bar(na.rm = T, fill = "#55952B") +
  theme_bw() +
  labs(
    title = "Number of citations",
    x = "Total number of citations after five years",
    y = ""
  ) +
  theme(legend.position = "none",
        plot.title = element_text(
          hjust = 0.5, 
          size = 20,
          face = "bold"
        ),
        plot.subtitle = element_text(
          hjust = 0.5, 
          size = 10,
          face = "italic"
        ))

## Plot gender and race by number of citations ----
# Generate a new variable based on race and gender
data$var <- ifelse(data$female == 1 & data$minority == 1, "Female-Minority",
                   ifelse(data$female == 1 & data$minority == 0, "Female-White",
                   ifelse(data$female == 0 & data$minority == 1, "Male-Minority",
                   ifelse(data$female == 0 & data$minority == 0, "Male-White", NA))))

ggplot(data = subset(data, !is.na(var)), aes(var, na.rm = TRUE)) +
  geom_bar(na.rm = T, fill = c("#AAD500", "#80C000", "#64B200", "#55952B")) +
  theme_bw() +
  labs(
    title = "Number of citations by race and gender",
    subtitle = "",
    caption = "",
    x = "Total number of citations after five years",
    y = ""
  ) +
  theme(legend.position = "none",
        plot.title = element_text(
          hjust = 0.5, 
          size = 20,
          face = "bold"
        ),
        plot.subtitle = element_text(
          hjust = 0.5, 
          size = 10,
          face = "italic"
        ))

# Models ----
## Poisson model ----
model1 <- glm(citation ~ female * minority +             # Poisson model
              authorities + tenure + elite, 
              family = "poisson", data = data)
summary(model1)


## Negative binomial model ----
model2 <- glm.nb(citation ~ female * minority +          # Negative binomial base model
                 authorities + tenure + elite,            
                 data = data,
                 control = glm.control(maxit = 100))
summary(model2)

### Overdispersion test ----
L1 <- logLik(model1)         # Log-likelihood - Poisson base model                                
L2 <- logLik(model2)         # Log-likelihood - Negative binomial base model

LRT <- -2 * L1 + 2 * L2 

LRT > qchisq(0.95, df = 1)   # Since the outcome is true, the best model is the Negative binomial 

## Stargazer ----
# Create a stargazer for the two models
stargazer(
  model2,
  out = "table_lab.tex",
  title = "Regression Results",
  intercept.bottom = F,
  covariate.labels = c(
    "Constant",
    "Female",
    "Ethnic minority",
    "Existing precedent",
    "Years in Court",
    "Elite school",
    "Female*Ethnic minority",
    ""
  ),
  dep.var.labels = c("Number of citation"),
  column.labels = c("(1)", "(2)"),
  model.numbers = F,
  header = F
)

# Simulating expected values ----
## Simulation function ----
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

### Hypothesis 1  - minorities female and white male ----
# Scenario 1, 3, 5 - minorities female
scenario1 <-
  cbind(
    1,                                        # intercept
    1,                                        # female
    1,                                        # minority
    mean(data$tenure, na.rm = T),             # tenure
    median(data$authorities, na.rm = T),      # authorities
    median(data$elite, na.rm = T),            # elite
    1*1                                       # interaction term
  )

set.seed(4547)
sim_res1 <- sim_function(model2, nsim = 1000, scenario1)

# Scenario 2 - white male
scenario2 <-
  cbind(
    1,                                        # intercept
    0,                                        # female
    0,                                        # minority
    mean(data$tenure, na.rm = T),             # tenure
    median(data$authorities, na.rm = T),      # authorities
    median(data$elite, na.rm = T),            # elite
    0*0                                       # interaction term
  )

set.seed(4547)
sim_res2 <- sim_function(model2, nsim = 1000, scenario2)

### Hypothesis 2  - minorities female and white female ----
# Scenario 4  - white female
scenario4 <-
  cbind(
    1,                                        # intercept
    1,                                        # female
    0,                                        # minority
    mean(data$tenure, na.rm = T),             # tenure
    median(data$authorities, na.rm = T),      # authorities
    median(data$elite, na.rm = T),            # elite
    1*0                                       # interaction term
  )

set.seed(4547)
sim_res4 <- sim_function(model2, nsim = 1000, scenario4)

### Hypothesis 3 - minorities female and minorities male ----
# Scenario 6 - minorities male
scenario6 <-
  cbind(
    1,                                        # intercept
    0,                                        # female
    1,                                        # minority
    mean(data$tenure, na.rm = T),             # tenure
    median(data$authorities, na.rm = T),      # authorities
    median(data$elite, na.rm = T),            # elite
    0*0                                       # interaction term
  )

set.seed(4547)
sim_res6 <- sim_function(model2, nsim = 1000, scenario6)

exp_1 <- exp(sim_res1)                                # Exponential inverse link function
exp_2 <- exp(sim_res2)
exp_4 <- exp(sim_res4)
exp_6 <- exp(sim_res6)

df_scenarios <- cbind(exp_1, exp_2, exp_4, exp_6)     # Data frame for all the simulation

sc1.2 <- df_scenarios[, 2] - df_scenarios[, 1]        # First difference - white male and minorities female
sc1.4 <- df_scenarios[, 3] - df_scenarios[, 1]        # First difference - white female and minorities female
sc1.6 <- df_scenarios[, 4] - df_scenarios[, 1]        # First difference - minorities male and minorities female

median1.2 <- median(sc1.2)                            # Median for first difference
ci_fd1.2 <-  quantile(sc1.2, probs = c(0.025, 0.975)) # 95% CIs for first difference

median1.4 <- median(sc1.4)                            
ci_fd1.4 <-  quantile(sc1.4, probs = c(0.025, 0.975)) 

median1.6 <- median(sc1.6)                            
ci_fd1.6 <-  quantile(sc1.6, probs = c(0.025, 0.975))

## Plot the first differences ----
pl.line <- data.frame(
  line = c(ci_fd1.2, ci_fd1.4, ci_fd1.6),
  val = c(2,2,1,1,0,0),
  cat = c("a","a","b","b","c","c")
)

pl.dot <- data.frame(
  line = c(median1.2, median1.4, median1.6),
  val = c(2,1,0),
  cat = c("a","b","c")
)

lab <- c("Minorities male
         Minorities female",
         "White female
         Minorities female",
         "White male
         Minorities female")

ggplot(pl.line, aes(x = line, y = val, color = cat)) +
  geom_line(linewidth = 0.5) +
  theme_bw() +
  geom_point(pl.dot, map = aes(x = line, y = val)) +
  geom_point(pl.line, map = aes(x = line, y = val)) +
  geom_vline(xintercept = 0, colour = "#FF3300", linetype = "dashed", linewidth = 0.75) +
  scale_color_manual(values = c("#AAD500", "#80C000", "#64B200")) +
  labs(
    title = "First differences",
    subtitle = "White male, white female and non-white male 
    minus non-white female",
    x = "First differences and 95% CI",
    y = ""
  ) +
  scale_y_continuous(breaks = c(0,1,2), limits = c(-0.5,2.5), labels = lab) +
  theme(legend.position = "none",
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        plot.title = element_text(
          hjust = 0.5, 
          size = 20,
          face = "bold"
        ),
        plot.subtitle = element_text(
          hjust = 0.5, 
          size = 11,
          face = "italic"
        ))


# Robustness ----
modelr <- glm.nb(citation ~ female * minority +          # Negative binomial with control variables
                 authorities + tenure + elite,
                 data = model2$model[!rstandard(model2) > 2, ],   # Remove all the outliers
                 control = glm.control(maxit = 100)
  )
summary(modelr)

## Simulating the new first differences using the previous scenario ----
### Hypothesis 1  - minorities female and white male ----
# Scenario 1, 3, 5 - minorities female
set.seed(4547)
sim_res1.r <- sim_function(modelr, nsim = 1000, scenario1)

# Scenario 2 - white male
set.seed(4547)
sim_res2.r <- sim_function(modelr, nsim = 1000, scenario2)

### Hypothesis 2  - minorities female and white female ----
# Scenario 4  - white female
set.seed(4547)
sim_res4.r <- sim_function(modelr, nsim = 1000, scenario4)

### Hypothesis 3 - minorities female and minorities male ----
# Scenario 6 - minorities male
set.seed(4547)
sim_res6.r <- sim_function(modelr, nsim = 1000, scenario6)

exp_1.r <- exp(sim_res1.r)
exp_2.r <- exp(sim_res2.r)
exp_4.r <- exp(sim_res4.r)
exp_6.r <- exp(sim_res6.r)

df_scenarios.r <- cbind(exp_1.r, exp_2.r, exp_4.r, exp_6.r)

sc1.2.r <- df_scenarios.r[, 2] - df_scenarios.r[, 1]
sc1.4.r <- df_scenarios.r[, 3] - df_scenarios.r[, 1]
sc1.6.r <- df_scenarios.r[, 4] - df_scenarios.r[, 1]

median1.2.r <- median(sc1.2.r)
ci_fd1.2.r <-  quantile(sc1.2.r, probs = c(0.025, 0.975))

median1.4.r <- median(sc1.4.r) 
ci_fd1.4.r <-  quantile(sc1.4.r, probs = c(0.025, 0.975))

median1.6.r <- median(sc1.6.r) 
ci_fd1.6.r <-  quantile(sc1.6.r, probs = c(0.025, 0.975))

## Plot the first differences ----
pl.line.r <- data.frame(
  line = c(ci_fd1.2.r, ci_fd1.4.r, ci_fd1.6.r),
  val = c(2,2,1,1,0,0),
  cat = c("a","a","b","b","c","c")
)

pl.dot.r <- data.frame(
  line = c(median1.2.r, median1.4.r, median1.6.r),
  val = c(2,1,0),
  cat = c("a","b","c")
)

lab <- c("Minorities male
         Minorities female",
         "White female
         Minorities female",
         "White male
         Minorities female")

ggplot(pl.line.r, aes(x = line, y = val, color = cat)) +
  geom_line(linewidth = 0.5) +
  theme_bw() +
  geom_point(pl.dot.r, map = aes(x = line, y = val)) +
  geom_point(pl.line.r, map = aes(x = line, y = val)) +
  geom_vline(xintercept = 0, colour = "#FF3300", linetype = "dashed", linewidth = 0.75) +
  scale_color_manual(values = c("#AAD500", "#80C000", "#64B200")) +
  labs(
    title = "First differences - Robustness check",
    subtitle = "White male, white female and non-white male 
    minus non-white female",
    x = "First differences and 95% CI",
    y = ""
  ) +
  scale_y_continuous(breaks = c(0,1,2), limits = c(-0.5,2.5), labels = lab) +
  theme(legend.position = "none",
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        plot.title = element_text(
          hjust = 0.5, 
          size = 20,
          face = "bold"
        ),
        plot.subtitle = element_text(
          hjust = 0.5, 
          size = 11,
          face = "italic"
        ))