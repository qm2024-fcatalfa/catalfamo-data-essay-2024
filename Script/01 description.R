library("tidyverse")

f3 <- as.data.frame(as.array(summary(df2$prior_os_new == df2$prior_os)))
colnames(df3) <- c("Mode","logical")
df3 <- df3[-c(1), ]
kable(df3, row.names = FALSE)


f1 <- as.data.frame(as.array(summary(data$authorities)))

f2 <- as.data.frame(as.array(summary(data$elite)))

f3 <- as.data.frame(as.array(summary(data$tenure)))

dd <- data.frame(
  var = f1$Var1,
  f1 = f1$Freq,
  f2 = f2$Freq,
  f3 = f3$Freq
)

name <- c("Description", "Quality of decision", "Elite school", "Years in court")

colnames(dd) <- name

min <- as.vector(unlist(dd[1,2:4]))
med <- as.vector(unlist(round(dd[3,2:4], digits = 0)))
max <- as.vector(unlist(round(dd[6,2:4], digits = 0)))

dd2 <- data.frame(
  Var = c("Quality of decision", "Elite school", "Years in court (log)"),
  Min = min,
  Median = med,
  Max = max
)

name <- c("Variable", "Minimum", "Median", "Maximum")

colnames(dd2) <- name

desc <- data.frame(dd2)

write_rds(desc, "data/desc.rds")
