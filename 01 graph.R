library("tidyvere")

df <- subset(data, var != "NA")

ggplot(df, aes(citation, fill = var)) +
  geom_bar() +
  theme_bw() +
  facet_wrap(~ var)

ggplot(df, aes(var)) +
  geom_bar() +
  theme_bw()

summary(data)

ggplot(data, aes(citation, authorities, fill = var)) +
  geom_smooth(method = "glm")

table(data$var, data$citation)

