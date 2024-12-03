library("tidyvere")

df <- subset(data, var != "NA")

ggplot(data, aes(citation, fill = var)) +
  geom_bar() +
  theme_bw() +
  facet_wrap(~ var)

ggplot(data, aes(var)) +
  geom_bar() +
  theme_bw()

summary(data)

table(data$var, data$citation)

