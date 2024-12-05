library("tidyverse")

df <- subset(data, var != "NA")

ggplot(df, aes(citation)) +
  geom_bar() +
  theme_bw() +
  facet_wrap(~ var)

# Introduction
cit <- ggplot(data, aes(citation)) +
  geom_bar(na.rm = T, fill = "#55952B") +
  theme_bw() +
  labs(
    title = "Number of citations",
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

write_rds(cit, "Plot/cit_plot.rds")

g.e.plot <- ggplot(df, aes(var)) +
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

write_rds(g.e.plot, "Plot/ge_plot.rds")

# Simulation
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

fd <- ggplot(pl.line, aes(x = line, y = val, color = cat)) +
  geom_line(size = 0.5) +
  theme_bw() +
  geom_point(pl.dot, map = aes(x = line, y = val, group = cat, shape = cat)) +
  geom_point(pl.line, map = aes(x = line, y = val, group = cat, shape = cat)) +
  geom_vline(xintercept = 0, colour = "#FF3300", linetype = "dashed", size = 0.75) +
  scale_color_manual(values = c("#AAD500", "#80C000", "#64B200")) +
  labs(
    title = "First differences for the three models",
    subtitle = "White male, white female and non-white male minus non-white female",
    caption = "",
    x = "First differences and 95% CI",
    y = ""
  ) +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        plot.title = element_text(
          hjust = 0.5, 
          size = 20,
          face = "bold"
        ),
        plot.subtitle = element_text(
          hjust = 0.5, 
          size = 10,
          face = "italic"
        )) +
  scale_y_continuous(breaks = c(0,1,2), limits = c(-0.5,2.5))

write_rds(fd, "Plot/fd_plot.rds")

ggplot(data, aes(citation, constit)) +
  geom_smooth()

ggplot(df, aes(var)) +
  geom_bar() +
  facet_wrap(~elite)
