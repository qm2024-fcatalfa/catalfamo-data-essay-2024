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

desc <- data.frame(dd[c(1,3,6), ])

write_rds(desc, "data/desc.rds")
