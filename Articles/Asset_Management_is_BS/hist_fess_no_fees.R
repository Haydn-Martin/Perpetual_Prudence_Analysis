
x <- seq(-50, 50, 0.1)
plot(x, dnorm(x, mean = 5, sd = 10), col = "red", type = "l")
abline(v=5, col="blue")
curve(dnorm(x, mean = 3, sd = 10), col = "green", add = T)