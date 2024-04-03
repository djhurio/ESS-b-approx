library(data.table)
library(ggplot2)

B <- 2
RR <- 0.481
RI <- 0.008

est_b <- function(B, RR, RI, m = 1e3) {

  n <- B
  p <- RR * (1 - RI)

  x <- rbinom(n = m, size = n, prob = p)

  return(data.frame(
    mean_the = n * p,
    mean_emp = mean(x),
    mean_trunc_the = n * p / (1 - (1 - p) ^ n),
    mean_trunc_emp = mean(x[x > 0])
  ))

}

est_b(B = 2, RR = RR, RI = RI, m = 1e6)


dat <- data.table(
  B = 1:15,
  RR = RR,
  RI = RI
)

dat[, n := B]
dat[, p := RR * (1 - RI)]

dat[, b_orig := n * p]
dat[, b_trunc := n * p / (1 - (1 - p) ^ n)]

dat_melt <- melt.data.table(
  data = dat,
  id.vars = "B",
  measure.vars = c("b_orig", "b_trunc"),
  value.name = "b"
)

ggplot(data = dat_melt,
       mapping = aes(x = B, y = b, colour = variable, group = variable)) +
  geom_hline(yintercept = 1, colour = "red", linetype  = "dotted") +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = dat$B) +
  scale_y_continuous(breaks = dat$B) +
  scale_color_brewer(palette = "Set1") +
  theme_bw()
