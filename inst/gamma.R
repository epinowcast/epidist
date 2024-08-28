library(brms)

set.seed(123)
n <- 100

difficulty <- runif(n, 0, 10)
method <- factor(sample(c("A", "B", "C"), n, replace = TRUE))

time <- rgamma(
  n,
  shape = 2,
  scale = 5 / (1 + 0.5 * difficulty - 0.3 * as.numeric(method))
)

data <- data.frame(time, difficulty, method)

model <- brms::brm(
  time ~ difficulty + method,
  data = data,
  family = Gamma(link = "log")
)

summary(model)

stancode <- brms::stancode(
  time ~ difficulty + method,
  data = data,
  family = Gamma(link = "log")
)

stancode
