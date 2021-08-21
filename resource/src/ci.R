library(dplyr)
library(ggplot2)
mu <- 31.5
sample_mean_1 <- 31.3
sample_mean_2 <- 31.7
sample_mean_3 <- 32.5
std <- 0.8
data <- data.frame(x = seq(27.5, 35.5, length.out = 100)) %>%
  mutate(
    y = dnorm(x, mean = mu, sd=std),
    e = dnorm(x, mean = sample_mu, sd=std)
  )

ggplot(data) +
  geom_line(aes(x=x, y=y)) +
  geom_vline(xintercept = mu, color="sky blue") +
  ## poluation mean
  annotate(geom="text", x=(mu - 0.7155), y= -0.01, label=paste0("[ ", (mu - 0.7155)),
           color="sky blue") + 
  annotate(geom="text", x=((mu + 0.7155) + (mu - 0.7155))/2, y= -0.01, label=paste0("--", mu, "--"),
           color="sky blue") + 
  annotate(geom="text", x=(mu + 0.7155), y= -0.01, label=paste0((mu + 0.7155), " ]"),
           color="sky blue") + 
  ## sample mean 1
  annotate(geom="text", x=(sample_mean_1 - 0.7155), y= -0.04, label=paste0("[ ", (sample_mean_1 - 0.7155)),
           color="red") + 
  annotate(geom="text", x=((sample_mean_1 + 0.7155) + (sample_mean_1 - 0.7155))/2, y= -0.04, label=paste0("-- ", sample_mean_1, " --"),
           color="red") + 
  annotate(geom="text", x=(sample_mean_1 + 0.7155), y= -0.04, label=paste0((sample_mean_1 + 0.7155), " ]"),
           color="red") + 
  ## sample mean 2
  annotate(geom="text", x=(sample_mean_2 - 0.7155), y= -0.08, label=paste0("[ ", (sample_mean_2 - 0.7155)),
           color="chocolate1") + 
  annotate(geom="text", x=((sample_mean_2 + 0.7155) + (sample_mean_2 - 0.7155))/2, y= -0.08, label=paste0("-- ", sample_mean_2, " --"),
           color="chocolate1") + 
  annotate(geom="text", x=(sample_mean_2 + 0.7155), y= -0.08, label=paste0((sample_mean_2 + 0.7155), " ]"),
           color="chocolate1") + 
  ## sample mean 3
  annotate(geom="text", x=(sample_mean_3 - 0.7155), y= -0.12, label=paste0("[ ", (sample_mean_3 - 0.7155)),
           color="darkgreen") + 
  annotate(geom="text", x=((sample_mean_3 + 0.7155) + (sample_mean_3 - 0.7155))/2, y= -0.12, label=paste0("-- ", sample_mean_3, " --"),
           color="darkgreen") + 
  annotate(geom="text", x=(sample_mean_3 + 0.7155), y= -0.12, label=paste0((sample_mean_3 + 0.7155), " ]"),
           color="darkgreen") + 
  theme_bw() +
  labs(y = 'Density f(x)',
       title = 'The normal distribution N(31.5, 0.8)',
       subtitle = paste0('Confidence Interval (95.44%) of population mean (blue):', mu, ' and sample mean (red): ', sample_mu))

# geom_line(aes(x=x, y=y)) +
#   geom_line(aes(x=x, y=e),linetype="dashed") +
#   geom_vline(xintercept = mu, color="sky blue") +
#   geom_vline(xintercept = input$sample_mean, color = "red") +
#   geom_ribbon(data = subset(ci_data(), x > (31.5 - two_sigma) & x < (31.5 + two_sigma)),
#               aes(x=x, ymin=0, ymax=y),
#               fill = "sky blue", alpha = .3) +
#   geom_ribbon(data = subset(ci_data(), x > (input$sample_mean - two_sigma) & x < (input$sample_mean + two_sigma)),
#               aes(x=x, ymin=0, ymax=y),
#               fill = "red", alpha = .3) +fill = "red", alpha = .3) +
