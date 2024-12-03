load("data/data_essay24_data.Rdata")

pack <- c("tidyverse", "stargazer", "MASS", "optimx", "patchwork")

lapply(pack, library, character.only=TRUE)

name <- c("citation", "authorities", "female", "minority",
          "elite", "tenure", "constit", "termmeritsper",
          "year")

colnames(data) <- name

data$female <- as.factor(data$female)
data$minority <- as.factor(data$minority)

# idea
data$var <- ifelse(data$female == 1 & data$minority == 1, "f.m",
            ifelse(data$female == 1 & data$minority == 0, "f.w",
            ifelse(data$female == 0 & data$minority == 1, "m.m",
            ifelse(data$female == 0 & data$minority == 0, "m.w", NA))))

table(data$var)
colSums(is.na(data))

colnames(data)

val <- c(0:40)

con.df <- as.data.frame(
  val
)

con <- data |>
  count(citation, sort = TRUE) |>
  drop_na()

k <- con$citation
freq <- con$n
N <- sum(freq)

# Likelihood function
poisson_l <- function(lambda) {
  likel <- 1
  for (i in 1:length(k)) {
    likel <- likel * (lambda^k[i] * exp(-lambda) / factorial(k[i]))^freq[i]
  }
  return(likel)
}

# Log Likelihood function
poisson_ll <- function(lambda) {
  log_likel <- 0
  for (i in 1:length(k)) {
    log_likel <- log_likel + freq[i] * (k[i] * log(lambda) - lambda - log(factorial(k[i])))
  }
  return(log_likel)
}

lambda_vals <- seq(0, 5, by = 0.01)  # Range of lambda
likelihood_vals <- sapply(lambda_vals, poisson_l)
log_likelihood_vals <- sapply(lambda_vals, poisson_ll)

df <- data.frame(
  lambda_vals,
  likelihood_vals,
  log_likelihood_vals
)

# Plot the likelihood function
lik <- ggplot(df, aes(lambda_vals, likelihood_vals)) +
  geom_line(color = "#207040") + theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Poisson Likelihood Function")  +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))


# Plot the log-likelihood function
llik <- ggplot(df, aes(lambda_vals, log_likelihood_vals)) +
  geom_line(color = "#FB3A6C") + theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Poisson Log Likelihood Function") +
  theme(axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=1))

lik + llik


poisson_l <- function(lambda, Y, X) {
  expected <- dpois(X, lambda) * sum(Y)
  ll <- sum(Y * log(expected))
  return(-ll)  # Negative log-likelihood for optimization
}

# Initial value for lambda
start_val <- 0.001

# Optimize log-likelihood with constraints
res <- optim(
  par = start_val,
  fn = poisson_l,
  method = "Brent",
  lower = 0.001,
  upper = 2,  
  Y = freq,
  X = k
)

# Extract MLE for lambda
lambda_hat <- res$par
lambda_hat

# Compute expected frequencies from the Poisson distribution
expected_freq <- dpois(k, lambda_hat) * sum(freq)

# Combine the real (observed) and expected values into a data frame
comparison <- data.frame(
  k = k,
  Observed = freq,
  Expected = round(expected_freq, 2)
)

df.f <- data.frame(
  cat = "Real",
  fr = con$citation,
  val = freq
)

con$citation.2 <- con$citation + 0.5

df.e <- data.frame(
  cat = "Expected",
  fr = con$citation.2,
  val = round(expected_freq, 3)
)

df2 <- rbind(df.f, df.e)

ggplot(df2, aes(fr, val, fill = cat)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#000058", "#639500")) +
  theme_bw() +
  geom_text(aes(label = val),
            check_overlap = TRUE,
            size = 2,
            vjust = -1) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  labs(
    title = "Compare real and expected values",
    x = "",
    y = "Values"
  )
