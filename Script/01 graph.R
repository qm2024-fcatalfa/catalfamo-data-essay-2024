library("tidyvere")

df <- subset(data, var != "NA")

# Introduction
ggplot(data, aes(citation)) +
  geom_bar() +
  theme_bw()

ggplot(df, aes(citation, fill = var)) +
  geom_bar() +
  theme_bw() +
  facet_wrap(~ var)


# Simulation
ggplot(pl.line, aes(x = line, y = val, group = cat)) +
  geom_line() +
  theme_bw() +
  geom_point(pl.dot, map = aes(x = line, y = val, group = cat)) +
  geom_point(pl.line, map = aes(x = line, y = val, group = cat)) +
  geom_vline(xintercept = 0, colour = "red", linetype = "dotted")