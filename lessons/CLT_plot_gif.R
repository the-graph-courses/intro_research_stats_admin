## Central‑Limit‑Theorem demo – uniform data  ➜ iterated sample‑mean plots
## ---------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(purrr)
library(patchwork)
library(glue)
library(here)
library(fs)

set.seed(123)                      # reproducible sampling
pop          <- runif(1000)        # Uniform(0,1) population
sample_size  <- 25
max_samples  <- 100

# 1. Draw all 100 samples *once* and keep their means
sample_means_full <- replicate(
  max_samples,
  mean(sample(pop, size = sample_size, replace = FALSE))
)

# 2. Pure helper that builds the two‑panel plot for any n ≤ 100
make_clt_plot <- function(n, sample_means) {
  means_df <- tibble(mean = sample_means[seq_len(n)])
  
  p_left <- ggplot() +
    geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1),
                 size = 2, colour = "black") +
    geom_linerange(data = means_df,
                   aes(x = mean, ymin = 0, ymax = 2),
                   colour = "red", alpha = 0.4, size = 1) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) +
    theme_minimal(base_size = 11) +
    labs(x = NULL, y = NULL)
  
  p_right <- ggplot(means_df, aes(mean)) +
    geom_histogram(aes(y = ..density..),
      bins = 20, fill = "lightgrey", colour = "white") +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_minimal(base_size = 11) +
    labs(x = NULL, y = NULL) + 
    geom_density(color = "cornflowerblue", alpha = 0.5, size = 1.3,
                 adjust = 1.5)
  
  p_left + p_right
}

# 3. Sequence of sample counts: 1, 10, 20, …, 100
sample_counts <- c(1, seq(10, max_samples, by = 10))

# Ensure output folder exists once
dir_create(here("images"))

# 4. Build each plot (map) and write it out (walk)
plots <- map(sample_counts, make_clt_plot, sample_means_full)

walk2(
  plots,
  sample_counts,
  ~ ggsave(
    filename = here("images", glue("clt_uniform_{.y}_samples.png")),
    plot     = .x,
    width    = 8, height = 4, dpi = 300
  )
)


# -----------------------------------------
# Create an animated GIF of the saved plots
# -----------------------------------------

library(magick)   # install.packages("magick")  if needed
library(stringr)

gif_path  <- here("images", "clt_uniform_evolution.gif")

img_files <- dir_ls(
  here("images"),
  regexp = "clt_uniform_[0-9]+_samples\\.png$"
) %>% 
  # extract the numeric piece (1, 10, 20 … 100) and order by it
  (\(x) x[order(as.integer(str_extract(path_file(x), "\\d+(?=_samples)")))] )()

# Read → animate → write
image_read(img_files) |>
  image_animate(fps = 0.5, dispose = "previous") |>
  image_write(gif_path)

message("GIF saved to: ", gif_path)
