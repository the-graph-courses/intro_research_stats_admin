# ------------------------------------------------------------
# Central‑Limit‑Theorem demo – uniform data  ➜ sample means
# ------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(patchwork)   # easiest way to put the two plots side‑by‑side
set.seed(123)        # reproducible results

## 1) Generate 1000 points from a Uniform(0,1) distribution
pop <- runif(1000, min = 0, max = 1)

## 2 & 3) Take 100 simple‑random samples of size 25 and keep their means
n_samples   <- 100
sample_size <- 25

sample_means <- replicate(
  n_samples,
  mean(sample(pop, size = sample_size, replace = FALSE))
)

means_df <- tibble(mean = sample_means)

## 4) Plot 1 – “population” density (a flat line) + vertical red lines for each mean
p_left <- ggplot() +
  # horizontal black segment representing the constant pdf of Uniform(0,1)
  geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1),
               size = 2, colour = "black") +
  # vertical red lines at the locations of the 100 sample means
  geom_linerange(data = means_df,
                 aes(x = mean, ymin = 0, ymax = 2),
                 colour = "red", alpha = 0.4, size = 1) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()

## 5) Plot 2 – histogram of the sample means
p_right <- ggplot(means_df, aes(mean)) +
  geom_histogram(bins = 20, fill = "grey50", colour = "black") +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = NULL, y = NULL) +
  theme_minimal()+ 
  geom_density(color = "cornflowerblue", alpha = 0.5, 
               size = 1.3,
               adjust = 1.5)

## Display the two plots side by side
p_left + p_right
