# Poisson ----
model1 <- glm(citation ~ female * minority, 
              family = "poisson", data = data)
summary(model1)

model2 <- glm(citation ~ + female * minority +
                constit + tenure + authorities + elite, 
              family = "poisson", data = data)
summary(model2)

# Negative binomial ----
model3 <- glm.nb(citation ~ female * minority, 
                 data = data,
                 control = glm.control(maxit = 100))
summary(model3)

model4 <- glm.nb(citation ~ female * minority + 
                 tenure + authorities + elite, 
                 data = data,
                 control = glm.control(maxit = 100))
summary(model4)

list <- list(model3, model4)

write_rds(list, "data/model.rds")

# Stargazer ----
stargazer(
  list,
  out = "table_lab.tex",
  title = "Regression Results",
  intercept.bottom = F,
  covariate.labels = c(
    "Constant",
    "Female",
    "Ethnic minority",
    "Years in Court",
    "Existing precedent",
    "Elite school",
    "Female*Ethnic minority",
    ""
  ),
  dep.var.labels = c("Number of citation"),
  model.numbers = F,
  header = F
)

# Over dispersion test ----

L1 <- logLik(model2) 
L2 <- logLik(model4) 

LRT <- -2 * L1 + 2 * L2 

LRT > qchisq(0.95, df = 1)

# If the result is TRUE we can not use the Poisson model and 
# we have to use the Negative Binomial model

# Include in text but not table or other stuff
# That's the proof of why we need the Negative Binomial model
