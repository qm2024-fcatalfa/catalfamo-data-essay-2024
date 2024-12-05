rob <-
  glm.nb(
    citation ~ female * minority + 
      tenure + authorities + elite,
    data = model4$model[!rstandard(model4) > 2, ],
    control = glm.control(maxit = 100),
  )

summary(rob)

