model1 <- glm(citation ~ female * minority, 
              family = "poisson", data = data)
summary(model1)

model2 <- glm(citation ~ + female * minority +
                constit + tenure + authorities + elite, 
              family = "poisson", data = data)
summary(model2)

model3 <- glm.nb(citation ~ female * minority, 
                 data = data,
                 control = glm.control(maxit = 100))
summary(model3)

model4 <- glm.nb(citation ~ female * minority + 
                 tenure + authorities + elite, 
                 data = data,
                 control = glm.control(maxit = 100))
summary(model4)

# Overdispersion test

L1 <- logLik(model2) 
L2 <- logLik(model4) 

LRT <- -2 * L1 + 2 * L2 

LRT > qchisq(0.95, df = 1)

# If the result is TRUE we can not use the Poisson model and 
# we have to use the Negative Binomial model

# Include in text but not table or other stuff
# That's the proof of why we need the Negative Binomial model
