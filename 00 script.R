load("data/data_essay24_data.Rdata")

library("tidyverse", "stargazer", "MASS", "optimx")

name <- c("citation", "authorities", "female", "minority",
          "elite", "tenure", "constit", "termmeritsper",
          "year", "var")

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

model_interaction <- glm.nb(citation ~ female * minority + 
                           constit + tenure, data = data,
                           control = glm.control(maxit = 100))
summary(model_interaction)

poisson_model <- glm(citation ~ female + minority + 
                       constit + tenure, 
                     family = poisson(link = "log"), data = data)
summary(poisson_model)

overdispersion_test <- sum(residuals(poisson_model, type = "pearson")^2) / df.residual(poisson_model)

library(margins)
marginal_effects <- margins(poisson_model)
summary(marginal_effects)
