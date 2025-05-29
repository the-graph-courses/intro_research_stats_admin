# ── libraries ───────────────────────────────────────────────────────────────────
library(ggplot2)
library(dplyr)

# ── fabricate some systolic BP data ─────────────────────────────────────────────
set.seed(123)                  # for reproducibility
n_all <- 1000                  # total number of observations
df <- tibble(
  id  = 1:n_all,
  SBP = rnorm(n_all, mean = 150, sd = 15)
)

# ── randomly select the 20 points to highlight ─────────────────────────────────
highlight_ids <- sample(df$id, 20)
df <- df %>%
  mutate(highlight = id %in% highlight_ids)

# ── compute the mean of the 20 highlighted points ──────────────────────────────
mean20 <- df %>%
  filter(highlight) %>%
  summarise(m = mean(SBP)) %>%
  pull(m)

# ── plot ────────────────────────────────────────────────────────────────────────
ggplot(df, aes(x = 0, y = SBP)) +                          # x = 0 keeps points in one column
  ## background “population” of points
  geom_jitter(width = 0.4, colour = "grey70", alpha = 0.35, size = 1.2) +
  
  ## 20 highlighted points
  geom_jitter(
    data  = filter(df, highlight),
    width = 0.2, colour = "navy", size = 3
  ) +
  
  ## dashed cross-bar at the sample mean
  geom_hline(
    yintercept = mean20,
    linetype   = "dashed",
    linewidth  = 1,
    colour     = "#D8A106"      # golden-orange like in the image
  ) +
  
  ## annotate the numeric value
  annotate(
    "text",
    x        = 0.35,            # nudged right of the dot swarm
    y        = mean20,
    label    = sprintf("Sample mean:\n%.1f", mean20),
    hjust    = 0.5,
    colour   = "#D8A106",
    fontface = "bold"
  ) +
  
  ## cosmetics to mimic the look
  scale_y_continuous(name = "SBP (mmHg)", limits = c(100, 200), expand = c(0.02, 0)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

