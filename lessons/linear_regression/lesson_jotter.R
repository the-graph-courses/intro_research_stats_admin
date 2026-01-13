# Load required libraries
# Load packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, openintro)
# Load the births14 dataset
data(births14)

# Filter for preemie babies only
preemie_data <- births14 %>%
    filter(premie == "premie")

# Plot 1: All preemie babies
p1 <- ggplot(preemie_data, aes(x = weeks, y = weight)) +
    geom_point() +
    labs(
        title = "All Preemie Babies: Birthweight vs Gestational Weeks",
        x = "Gestational Weeks",
        y = "Birthweight (pounds)"
    ) +
    theme_minimal()

print(p1)

# Plot 2: Top 6 data points (first 6 rows)
preemie_top6 <- preemie_data %>%
    slice_head(n = 6)

p2 <- ggplot(preemie_top6, aes(x = weeks, y = weight)) +
    geom_point() +
    labs(
        title = "Top 6 Preemie Babies (First 6 Rows): Birthweight vs Gestational Weeks",
        x = "Gestational Weeks",
        y = "Birthweight (pounds)"
    ) +
    theme_minimal()

print(p2)

# Plot 3: Top 10 data points (first 10 rows)
preemie_top10 <- preemie_data %>%
    slice_head(n = 10)

p3 <- ggplot(preemie_top10, aes(x = weeks, y = weight)) +
    geom_point() +
    labs(
        title = "Top 10 Preemie Babies (First 10 Rows): Birthweight vs Gestational Weeks",
        x = "Gestational Weeks",
        y = "Birthweight (pounds)"
    ) +
    theme_minimal()

print(p3)

# Plot 4: Bottom 8 data points (last 8 rows)
preemie_bottom8 <- preemie_data %>%
    slice_tail(n = 8)

p4 <- ggplot(preemie_bottom8, aes(x = weeks, y = weight)) +
    geom_point() +
    labs(
        title = "Bottom 8 Preemie Babies (Last 8 Rows): Birthweight vs Gestational Weeks",
        x = "Gestational Weeks",
        y = "Birthweight (pounds)"
    ) +
    theme_minimal()

print(p4)

# Plot 5: Bottom 10 data points (last 10 rows)
preemie_bottom10 <- preemie_data %>%
    slice_tail(n = 10)

p5 <- ggplot(preemie_bottom10, aes(x = weeks, y = weight)) +
    geom_point() +
    labs(
        title = "Bottom 10 Preemie Babies (Last 10 Rows): Birthweight vs Gestational Weeks",
        x = "Gestational Weeks",
        y = "Birthweight (pounds)"
    ) +
    theme_minimal()

print(p5)
