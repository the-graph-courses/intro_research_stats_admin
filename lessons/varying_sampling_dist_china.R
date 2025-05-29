## Sampling Distributions demo – China Diabetes data  ➜ varying sample size and number
## ---------------------------------------------------------------------

pacman::p_load(
  ggplot2,
  dplyr,
  purrr,
  patchwork,
  glue,
  here,
  fs
)

diabetes_china_chen <- read_csv(here::here("data/diabetes_china_chen.csv"))

set.seed(2048)                      # reproducible sampling

pop          <- diabetes_china_chen$sbp_mm_hg        # Blood pressure measurements
sample_size  <- 100
max_samples  <- 100

# 1. Draw all 100 samples *once* and keep their means
sample_means_full <- replicate(
  max_samples,
  mean(sample(pop, size = sample_size, replace = FALSE))
)

sample_means_df <- tibble(mean = sample_means_full)
mean_value <- mean(sample_means_full, na.rm = T)

## 4) Plot 1 – “population” density (a flat line) + vertical red lines for each mean
#p_left <- ggplot() +
#  # vertical red lines at the locations of the 100 sample means
#  geom_linerange(data = sample_means_df,
#                 aes(x = mean, ymin = 0, ymax = 2),
#                 colour = "red", alpha = 0.4, size = 1) +
#  scale_x_continuous(expand = c(0, 0)) +
#  scale_y_continuous(expand = c(0, 0)) +
#  labs(x = NULL, y = NULL) +
#  theme_minimal()
#p_left


## 5) Plot 2 – histogram of the sample means
p_right <- ggplot(sample_means_df, aes(mean)) +
  geom_histogram(bins = 0.5*length(sample_means_full), fill = "lightgrey",
                 mapping = aes(y = after_stat(density))) +
  geom_vline(aes(xintercept = mean_value), color = "#D8A106", 
             linetype = "dashed", linewidth = 1.2) +
  scale_x_continuous(limits = c(110, 130), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()+ 
  geom_density(color = "cornflowerblue", alpha = 0.5, 
               size = 1.3, adjust = 2)
p_right
