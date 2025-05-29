# ── libraries ───────────────────────────────────────────────────────────────────
library(ggplot2)
library(dplyr)

# ── load some systolic BP data ─────────────────────────────────────────────
                # for reproducibility

diabetes_china_chen <- read_csv(here::here("data/diabetes_china_chen.csv"))


# ── randomly select the 50 points to highlight ─────────────────────────────────
set.seed(123)  
#set.seed(234)  
#set.seed(345)  
highlight_ids <- sample(diabetes_china_chen$id, 50)
diabetes_china_chen <- diabetes_china_chen %>%
  mutate(highlight = id %in% highlight_ids)

# ── compute the mean of the 20 highlighted points ──────────────────────────────
mean50 <- diabetes_china_chen %>%
  filter(highlight) %>%
  summarise(m = mean(sbp_mm_hg)) %>%
  pull(m)

# --- compute the true mean -------------------------

tm <- mean(diabetes_china_chen$sbp_mm_hg, na.rm = T)

# ── plot ────────────────────────────────────────────────────────────────────────
ggplot(diabetes_china_chen, aes(x = 0, y = sbp_mm_hg)) +                          # x = 0 keeps points in one column
  ## background “population” of points
  geom_jitter(width = 0.4, colour = "grey70", alpha = 0.35, size = 1.2) +
  
  ## 20 highlighted points
  geom_jitter(
    data  = filter(diabetes_china_chen, highlight),
    width = 0.2, colour = "navy", size = 3
  ) +
  
  ## dashed cross-bar at the sample mean
  geom_hline(
    yintercept = mean50,
    linetype   = "dashed",
    linewidth  = 1,
    colour     = "#D8A106"      # golden-orange like in the image
  ) +
  
  ## annotate the numeric value
  annotate(
    "text",
    x        = 0.35,            # nudged right of the dot swarm
    y        = mean50,
    label    = sprintf("Sample mean:\n%.1f", mean50),
    hjust    = 0.5,
    colour   = "#D8A106",
    fontface = "bold"
  ) +
  
  ## cosmetics to mimic the look
  scale_y_continuous(name = "SBP (mmHg)", limits = c(50, 250), expand = c(0.02, 0)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

# ── plot with true mean ───────────────────────────────────────────────────────────────
ggplot(diabetes_china_chen, aes(x = 0, y = sbp_mm_hg)) +                          # x = 0 keeps points in one column
  ## background “population” of points
  geom_jitter(width = 0.4, colour = "grey70", alpha = 0.35, size = 1.2) +
  
  
  ## straight cross-bar at the TRUE mean
  geom_hline(
    yintercept = tm,
    #linetype   = "dashed",
    linewidth  = 1,
    colour     = "#C21807"      # golden-orange like in the image
  ) +
  
  ## annotate the numeric value
  annotate(
    "text",
    x        = 0.35,            # nudged right of the dot swarm
    y        = tm,
    label    = sprintf("True mean:\n%.1f", tm),
    hjust    = 0.5,
    colour   = "#C21807",
    fontface = "bold"
  ) +
  
  ## cosmetics to mimic the look
  scale_y_continuous(name = "SBP (mmHg)", limits = c(50, 250), expand = c(0.02, 0)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

