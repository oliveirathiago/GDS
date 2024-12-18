library(ggplot2)
library(patchwork)
library(dplyr)

dataplot_all <- readRDS("data/dataplot.RDS")

# Reversed order for y-axis
reversed_levels <- c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6') %>% rev

# Create a function to generate consistent plots
create_plot <- function(data, title) {
  ggplot(data, aes(y = Coefficient, x = scen, colour = mor, group = mor)) +
    geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper, alpha = significance), 
                  width = 0.25, position = position_dodge(width = 1), lwd = 0.5) +
    geom_point(aes(shape = significance, alpha = significance, group = mor), 
               position = position_dodge(width = 1)) +
    scale_alpha_manual(values = c(1), guide = "none") +  # Remove alpha legend
    scale_shape_manual(
      values = c(8), 
      labels = c("95% confidence interval\ndoes not cross zero"),
      guide = guide_legend(title = NULL)  # Remove shape legend title
    ) +
    geom_hline(yintercept = 0, size = 0.35, linetype = "dashed", color = 'darkgray') +
    coord_flip() +
    scale_x_discrete(limits = reversed_levels, breaks = reversed_levels) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = title, x = NULL, y = NULL, colour = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 2, colour = "#3C3C3C", size = 12),
      axis.text.y = element_text(colour = "#3C3C3C", size = 10),
      axis.text.x = element_text(colour = "#3C3C3C", size = 8),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "#3C3C3C", size = 0.2),
      legend.title = element_text(colour = "#3C3C3C", size = 10),
      legend.key = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = "#3C3C3C", size = 8),
      legend.text = element_text(colour = "#3C3C3C", size = 8),
      plot.margin = margin(0.5, 0, 0.5, 0, "cm"),
      aspect.ratio = 1
    )
}

create_plot_sig <- function(data, title) {
  ggplot(data, aes(y = Coefficient, x = scen, colour = mor, group = mor)) +
    geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper, alpha = significance), 
                  width = 0.25, position = position_dodge(width = 1), lwd = 0.5) +
    geom_point(aes(shape = significance, alpha = significance, group = mor), 
               position = position_dodge(width = 1)) +
    scale_alpha_manual(values = c(.3, 1), guide = "none") +  # Remove alpha legend
    scale_shape_manual(
      values = c(NA, 8), 
      labels = c("", "95% confidence interval\ndoes not cross zero"),
      guide = guide_legend(title = NULL)  # Remove shape legend title
    ) +
    geom_hline(yintercept = 0, size = 0.35, linetype = "dashed", color = 'darkgray') +
    coord_flip() +
    scale_x_discrete(limits = reversed_levels, breaks = reversed_levels) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = title, x = NULL, y = NULL, colour = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 2, colour = "#3C3C3C", size = 12),
      axis.text.y = element_text(colour = "#3C3C3C", size = 10),
      axis.text.x = element_text(colour = "#3C3C3C", size = 8),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "#3C3C3C", size = 0.2),
      legend.title = element_text(colour = "#3C3C3C", size = 10),
      legend.key = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = "#3C3C3C", size = 8),
      legend.text = element_text(colour = "#3C3C3C", size = 8),
      plot.margin = margin(0.5, 0, 0.5, 0, "cm"),
      aspect.ratio = 1
    )
}

##################################################


# Split data by mod
dataplot_arrest_crime <- dataplot_all %>% filter(var == "crime", mod == "arrest")
dataplot_morality_crime <- dataplot_all %>% filter(var == "crime", mod == "morality")
dataplot_purchase_crime <- dataplot_all %>% filter(var == "crime", mod == "purchase")

# Generate individual plots
plot_arrest_crime <- create_plot(dataplot_arrest_crime, "Been arrested?")
plot_morality_crime <- create_plot(dataplot_morality_crime, "Moral judgements")
plot_purchase_crime <- create_plot(dataplot_purchase_crime, "Purchased drugs?")

# Combine with patchwork, adding the overall title
combined_plot_crime <- (plot_arrest_crime + plot_morality_crime + plot_purchase_crime) +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(
    title = "Relationship between outcome-specific expectations and support for each scenario",
    subtitle = "Outcome-specific expectation: 'Approach would reduce crime and public disorder'",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )

# Print the combined plot
print(combined_plot_crime)

pdf("plots/1_plot_reducecrime.pdf", width = 10, height = 12, paper = 'a4r')
print(combined_plot_crime)
dev.off()


##############################################################

# Split data by mod
dataplot_arrest_encour <- dataplot_all %>% filter(var == "encour", mod == "arrest")
dataplot_morality_encour <- dataplot_all %>% filter(var == "encour", mod == "morality")
dataplot_purchase_encour <- dataplot_all %>% filter(var == "encour", mod == "purchase")


# Generate individual plots
plot_arrest <- create_plot_sig(dataplot_arrest_encour, "Been arrested?")
plot_morality <- create_plot_sig(dataplot_morality_encour, "Moral judgements")
plot_purchase <- create_plot_sig(dataplot_purchase_encour, "Purchased drugs?")

# Combine with patchwork, adding the overall title
combined_plot_encour <- (plot_arrest + plot_morality + plot_purchase) +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(
    title = "Relationship between outcome-specific expectations and support for each scenario",
    subtitle = "Outcome-specific expectation: 'Approach would encourage drug use'",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )

# Print the combined plot
print(combined_plot_encour)

pdf("plots/2_plot_encour.pdf", width = 10, height = 12, paper = 'a4r')
print(combined_plot_encour)
dev.off()


##############################################################

# Split data by mod
dataplot_arrest_free <- dataplot_all %>% filter(var == "free", mod == "arrest")
dataplot_morality_free <- dataplot_all %>% filter(var == "free", mod == "morality")
dataplot_purchase_free <- dataplot_all %>% filter(var == "free", mod == "purchase")


# Generate individual plots
plot_arrest_free <- create_plot(dataplot_arrest_free, "Been arrested?")
plot_morality_free <- create_plot_sig(dataplot_morality_free, "Moral judgements")
plot_purchase_free <- create_plot(dataplot_purchase_free, "Purchased drugs?")

# Combine with patchwork, adding the overall title
combined_plot_free <- (plot_arrest_free + plot_morality_free + plot_purchase_free) +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(
    title = "Relationship between outcome-specific expectations and support for each scenario",
    subtitle = "Outcome-specific expectation: 'Approach would restrict people's freedom'",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )

# Print the combined plot
print(combined_plot_free)

pdf("plots/3_plot_free.pdf", width = 10, height = 12, paper = 'a4r')
print(combined_plot_free)
dev.off()


##############################################################

# Split data by mod
dataplot_arrest_health <- dataplot_all %>% filter(var == "health", mod == "arrest")
dataplot_morality_health <- dataplot_all %>% filter(var == "health", mod == "morality")
dataplot_purchase_health <- dataplot_all %>% filter(var == "health", mod == "purchase")


# Generate individual plots
plot_arrest_health <- create_plot(dataplot_arrest_health, "Been arrested?")
plot_morality_health <- create_plot(dataplot_morality_health, "Moral judgements")
plot_purchase_health <- create_plot(dataplot_purchase_health, "Purchased drugs?")

# Combine with patchwork, adding the overall title
combined_plot_health <- (plot_arrest_health + plot_morality_health + plot_purchase_health) +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(
    title = "Relationship between outcome-specific expectations and support for each scenario",
    subtitle = "Outcome-specific expectation: 'Approach would improve overall health and wellbeing'",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )

# Print the combined plot
print(combined_plot_health)

pdf("plots/4_plot_health.pdf", width = 10, height = 12, paper = 'a4r')
print(combined_plot_health)
dev.off()


##############################################################

# Split data by mod
dataplot_arrest_vulner <- dataplot_all %>% filter(var == "vulner", mod == "arrest")
dataplot_morality_vulner <- dataplot_all %>% filter(var == "vulner", mod == "morality")
dataplot_purchase_vulner <- dataplot_all %>% filter(var == "vulner", mod == "purchase")


# Generate individual plots
plot_arrest_vulner <- create_plot(dataplot_arrest_vulner, "Been arrested?")
plot_morality_vulner <- create_plot(dataplot_morality_vulner, "Moral judgements")
plot_purchase_vulner <- create_plot(dataplot_purchase_vulner, "Purchased drugs?")

# Combine with patchwork, adding the overall title
combined_plot_vulner <- (plot_arrest_vulner + plot_morality_vulner + plot_purchase_vulner) +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(
    title = "Relationship between outcome-specific expectations and support for each scenario",
    subtitle = "Outcome-specific expectation: 'Approach would hurt the most vulerable people'",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )

# Print the combined plot
print(combined_plot_vulner)

pdf("plots/5_plot_vulner.pdf", width = 10, height = 12, paper = 'a4r')
print(combined_plot_vulner)
dev.off()


##############################################################

# Split data by mod
dataplot_arrest_econ <- dataplot_all %>% filter(var == "econ", mod == "arrest")
dataplot_morality_econ <- dataplot_all %>% filter(var == "econ", mod == "morality")
dataplot_purchase_econ <- dataplot_all %>% filter(var == "econ", mod == "purchase")


# Generate individual plots
plot_arrest_econ <- create_plot(dataplot_arrest_econ, "Been arrested?")
plot_morality_econ <- create_plot(dataplot_morality_econ, "Moral judgements")
plot_purchase_econ <- create_plot(dataplot_purchase_econ, "Purchased drugs?")

# Combine with patchwork, adding the overall title
combined_plot_econ <- (plot_arrest_econ + plot_morality_econ + plot_purchase_econ) +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(
    title = "Relationship between outcome-specific expectations and support for each scenario",
    subtitle = "Outcome-specific expectation: 'Approach would boost the economy'",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )

# Print the combined plot
print(combined_plot_econ)

pdf("plots/6_plot_econ.pdf", width = 10, height = 12, paper = 'a4r')
print(combined_plot_econ)
dev.off()



##############################################################

# Split data by mod
dataplot_arrest_trust <- dataplot_all %>% filter(var == "trust", mod == "arrest")
dataplot_morality_trust <- dataplot_all %>% filter(var == "trust", mod == "morality")
dataplot_purchase_trust <- dataplot_all %>% filter(var == "trust", mod == "purchase")


# Generate individual plots
plot_arrest_trust <- create_plot(dataplot_arrest_trust, "Been arrested?")
plot_morality_trust <- create_plot(dataplot_morality_trust, "Moral judgements")
plot_purchase_trust <- create_plot(dataplot_purchase_trust, "Purchased drugs?")

# Combine with patchwork, adding the overall title
combined_plot_trust <- (plot_arrest_trust + plot_morality_trust + plot_purchase_trust) +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(
    title = "Relationship between outcome-specific expectations and support for each scenario",
    subtitle = "Outcome-specific expectation: 'Approach would improve trust in government'",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )

# Print the combined plot
print(combined_plot_trust)

pdf("plots/7_plot_trust.pdf", width = 10, height = 12, paper = 'a4r')
print(combined_plot_trust)
dev.off()



