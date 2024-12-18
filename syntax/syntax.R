library(tidyverse)
library(haven)
library(ltm)
library(MASS)
library(broom)
library(mfx)
library(texreg)

data <- read_spss('data/gds2022_policy_45094.sav')
data_raw <- data

alpha_scen1 <- 
  data %>%
  dplyr::select(scen1crime:scen1econ, scen1trust) %>%
  cronbach.alpha(na.rm = T)

alpha_scen2 <- 
  data %>%
  dplyr::select(scen2crime:scen2econ, scen2trust) %>%
  cronbach.alpha(na.rm = T)

alpha_scen3 <- 
  data %>%
  dplyr::select(scen3crime:scen3econ, scen3trust) %>%
  cronbach.alpha(na.rm = T)

alpha_scen4 <- 
  data %>%
  dplyr::select(scen4crime:scen4econ, scen4trust) %>%
  cronbach.alpha(na.rm = T)

alpha_scen5 <- 
  data %>%
  dplyr::select(scen5crime:scen5econ, scen5trust) %>%
  cronbach.alpha(na.rm = T)

alpha_scen6 <- 
  data %>%
  dplyr::select(scen6crime:scen6econ, scen6trust) %>%
  cronbach.alpha(na.rm = T)


## let's try them separately

data <- 
  data_raw %>%
  #mutate(#scen1_support = case_when(scen1supp == 4 | scen1supp == 5 ~ T, T ~ F),
         #scen2_support = case_when(scen2supp == 4 | scen2supp == 5 ~ T, T ~ F),
         #scen3_support = case_when(scen3supp == 4 | scen3supp == 5 ~ T, T ~ F),
         #scen4_support = case_when(scen4supp == 4 | scen4supp == 5 ~ T, T ~ F),
         #scen5_support = case_when(scen5supp == 4 | scen5supp == 5 ~ T, T ~ F),
         #scen6_support = case_when(scen6supp == 4 | scen6supp == 5 ~ T, T ~ F),
         #scen1_immoral = case_when(scen1immoral == "Agree" | scen1immoral == "Strongly agree" ~ T, T ~ F),
         #scen2_immoral = case_when(scen2immoral == "Agree" | scen2immoral == "Strongly agree" ~ T, T ~ F),
         #scen3_immoral = case_when(scen3immoral == "Agree" | scen3immoral == "Strongly agree" ~ T, T ~ F),
         #scen4_immoral = case_when(scen4immoral == "Agree" | scen4immoral == "Strongly agree" ~ T, T ~ F),
         #scen5_immoral = case_when(scen5immoral == "Agree" | scen5immoral == "Strongly agree" ~ T, T ~ F),
         #scen6_immoral = case_when(scen6immoral == "Agree" | scen6immoral == "Strongly agree" ~ T, T ~ F)) %>%
  mutate(across(c(scen1supp, scen2supp, scen3supp, scen4supp, scen5supp, scen6supp), ~as_factor(.))) %>%
  mutate(across(c(scen1crime:scen1trust, scen2crime:scen2trust, scen3crime:scen3trust, scen4crime:scen4trust, scen5crime:scen5trust, scen6crime:scen6trust), 
                ~ case_when(. == 4 | . == 5 ~ T, T ~ F))) %>%
  mutate(male = gender == 1,
         white = ethnicity == 1,
         age = case_when(age > 85 ~ NA_real_, TRUE ~ age)) %>%
  mutate(age = as.numeric(age)) %>%
  filter(ncountry == 10 | ncountry == 13 | ncountry == 14 | ncountry == 21 | ncountry == 25 | ncountry == 38 | ncountry == 47 | ncountry == 58 | ncountry == 72 | ncountry == 73 | ncountry == 80
         | ncountry == 98 | ncountry == 104 | ncountry == 106 | ncountry == 140 | ncountry == 155 | ncountry == 173 | ncountry == 174 | ncountry == 201 | ncountry == 206 | ncountry == 208
         | ncountry == 228 | ncountry == 227) %>%
  mutate(country = as_factor(scountry))

table(data$scen1supp) %>% prop.table
table(data$scen2supp) %>% prop.table
table(data$scen3supp) %>% prop.table
table(data$scen4supp) %>% prop.table
table(data$scen5supp) %>% prop.table
table(data$scen6supp) %>% prop.table

###################
## ordinal logit models

## aproach would reduce crime
m_scen1_crime <- polr(scen1supp ~ scen1crime + age + male + country, data = data, method = 'logistic')
m_scen2_crime <- polr(scen2supp ~ scen2crime + age + male + country, data = data, method = 'logistic')
m_scen3_crime <- polr(scen3supp ~ scen3crime + age + male + country, data = data, method = 'logistic')
m_scen4_crime <- polr(scen4supp ~ scen4crime + age + male + country, data = data, method = 'logistic')
m_scen5_crime <- polr(scen5supp ~ scen5crime + age + male + country, data = data, method = 'logistic')
m_scen6_crime <- polr(scen6supp ~ scen6crime + age + male + country, data = data, method = 'logistic')

## approach would encourage drug use
m_scen1_encour <- polr(scen1supp ~ scen1encour + age + male + country, data = data, method = 'logistic')
m_scen2_encour <- polr(scen2supp ~ scen2encour + age + male + country, data = data, method = 'logistic')
m_scen3_encour <- polr(scen3supp ~ scen3encour + age + male + country, data = data, method = 'logistic')
m_scen4_encour <- polr(scen4supp ~ scen4encour + age + male + country, data = data, method = 'logistic')
m_scen5_encour <- polr(scen5supp ~ scen5encour + age + male + country, data = data, method = 'logistic')
m_scen6_encour <- polr(scen6supp ~ scen6encour + age + male + country, data = data, method = 'logistic')

## approach would restrict freedom
m_scen1_free <- polr(scen1supp ~ scen1free + age + male + country, data = data, method = 'logistic')
m_scen2_free <- polr(scen2supp ~ scen2free + age + male + country, data = data, method = 'logistic')
m_scen3_free <- polr(scen3supp ~ scen3free + age + male + country, data = data, method = 'logistic')
m_scen4_free <- polr(scen4supp ~ scen4free + age + male + country, data = data, method = 'logistic')
m_scen5_free <- polr(scen5supp ~ scen5free + age + male + country, data = data, method = 'logistic')
m_scen6_free <- polr(scen6supp ~ scen6free + age + male + country, data = data, method = 'logistic')

## approach would improve health and wellbeing
m_scen1_health <- polr(scen1supp ~ scen1health + age + male + country, data = data, method = 'logistic')
m_scen2_health <- polr(scen2supp ~ scen2health + age + male + country, data = data, method = 'logistic')
m_scen3_health <- polr(scen3supp ~ scen3health + age + male + country, data = data, method = 'logistic')
m_scen4_health <- polr(scen4supp ~ scen4health + age + male + country, data = data, method = 'logistic')
m_scen5_health <- polr(scen5supp ~ scen5health + age + male + country, data = data, method = 'logistic')
m_scen6_health <- polr(scen6supp ~ scen6health + age + male + country, data = data, method = 'logistic')

## approach would hurt the most vulnerable
m_scen1_vulner <- polr(scen1supp ~ scen1vulner + age + male + country, data = data, method = 'logistic')
m_scen2_vulner <- polr(scen2supp ~ scen2vulner + age + male + country, data = data, method = 'logistic')
m_scen3_vulner <- polr(scen3supp ~ scen3vulner + age + male + country, data = data, method = 'logistic')
m_scen4_vulner <- polr(scen4supp ~ scen4vulner + age + male + country, data = data, method = 'logistic')
m_scen5_vulner <- polr(scen5supp ~ scen5vulner + age + male + country, data = data, method = 'logistic')
m_scen6_vulner <- polr(scen6supp ~ scen6vulner + age + male + country, data = data, method = 'logistic')

## would boost the economy
m_scen1_econ <- polr(scen1supp ~ scen1econ + age + male + country, data = data, method = 'logistic')
m_scen2_econ <- polr(scen2supp ~ scen2econ + age + male + country, data = data, method = 'logistic')
m_scen3_econ <- polr(scen3supp ~ scen3econ + age + male + country, data = data, method = 'logistic')
m_scen4_econ <- polr(scen4supp ~ scen4econ + age + male + country, data = data, method = 'logistic')
m_scen5_econ <- polr(scen5supp ~ scen5econ + age + male + country, data = data, method = 'logistic')
m_scen6_econ <- polr(scen6supp ~ scen6econ + age + male + country, data = data, method = 'logistic')

## would improve trust in govt
m_scen1_trust <- polr(scen1supp ~ scen1trust + age + male + country, data = data, method = 'logistic')
m_scen2_trust <- polr(scen2supp ~ scen2trust + age + male + country, data = data, method = 'logistic')
m_scen3_trust <- polr(scen3supp ~ scen3trust + age + male + country, data = data, method = 'logistic')
m_scen4_trust <- polr(scen4supp ~ scen4trust + age + male + country, data = data, method = 'logistic')
m_scen5_trust <- polr(scen5supp ~ scen5trust + age + male + country, data = data, method = 'logistic')
m_scen6_trust <- polr(scen6supp ~ scen6trust + age + male + country, data = data, method = 'logistic')

models <- list(m_scen1_crime, m_scen2_crime, m_scen3_crime, m_scen4_crime, m_scen5_crime, m_scen6_crime,
               m_scen1_encour, m_scen2_encour, m_scen3_encour, m_scen4_encour, m_scen5_encour, m_scen6_encour,
               m_scen1_free, m_scen2_free, m_scen3_free, m_scen4_free, m_scen5_free, m_scen6_free,
               m_scen1_health, m_scen2_health, m_scen3_health, m_scen4_health, m_scen5_health, m_scen6_health,
               m_scen1_vulner, m_scen2_vulner, m_scen3_vulner, m_scen4_vulner, m_scen5_vulner, m_scen6_vulner,
               m_scen1_econ, m_scen2_econ, m_scen3_econ, m_scen4_econ, m_scen5_econ, m_scen6_econ,
               m_scen1_trust, m_scen2_trust, m_scen3_trust, m_scen4_trust, m_scen5_trust, m_scen6_trust)

# Function to extract required details from a model
extract_coefficients <- function(model) {
  coef_summary <- summary(model)$coefficients
  tibble(
    Coefficient = coef_summary[1, 1],
    Std_Error = coef_summary[1, 2],
    t_value = coef_summary[1, 3]
  )
}

# Apply the function to each model in the list and bind rows
coefficients_data <- lapply(models, extract_coefficients) %>%
  bind_rows()

# Add `scen` and `crime` columns
dataplot_all <- coefficients_data %>%
  mutate(scen = rep(c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6'), 7),
         var = rep(c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust'), each = 6),
         ci.lower = Coefficient - 1.96 * Std_Error,
         ci.upper = Coefficient + 1.96 * Std_Error) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))


plot_scenario <-  
  ggplot(dataplot_all, aes(y = Coefficient, x = var)) +
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper, alpha = significance), width = .15, position = position_dodge(width = .75), lwd = .5, show.legend = T) + 
  #geom_point(aes(shape = significance, alpha = significance), 
  #           size = ifelse(dataplot_all$significance == "significant", 3, 0), 
  #           position = position_dodge(width = .75)) +
  scale_alpha_manual(values = c(1)) +
  #scale_shape_manual(values = c(8), labels = c("95% confidence interval\ndoes not cross zero")) +
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  facet_wrap(~ scen) +
  coord_flip() + 
  ggtitle("Effects on support for each policy scenario") + 
  ylab("") + xlab("This approach would...") + 
  guides(colour = guide_legend(title = ""),
         alpha = "none",
         #shape = guide_legend(title = "", 
         #                      override.aes = list(linetype = c(1),
         #                                         shape = c(8),
         #                                         size = c(.25)))
         ) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 10),
        legend.key = element_blank(),
        strip.background = element_blank(),
        #legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 12),
        strip.text = element_text(colour = "#3C3C3C", size = 9),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 0, colour = "#3C3C3C", margin = unit(c(0,0,0,0), "mm"), size = 8),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 1.5) + 
  scale_x_discrete(limits = c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust') %>% rev,
                   breaks = c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust') %>% rev,
                   labels = c('Reduce crime and\npublic disorder', 
                              'Encourage drug\nuse',
                              "Restrict people's\nfreedom",
                              "Improve overall\nhealth and wellbeing",
                              "Hurt the most\nvulnerable people",
                              "Boost the economy",
                              "Improve trust\nin government") %>% rev
                     )

plot_items <-  
  ggplot(dataplot_all
         , aes(y = Coefficient, x = scen)) +
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper, alpha = significance), width = .15, position = position_dodge(width = .75), lwd = .5, show.legend = T) + 
  #geom_point(aes(shape = significance, alpha = significance), 
  #           size = ifelse(dataplot_all$significance == "significant", 3, 0), 
  #           position = position_dodge(width = .75)) +
  scale_alpha_manual(values = c(1)) +
  #scale_shape_manual(values = c(8), labels = c("95% confidence interval\ndoes not cross zero")) +
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  facet_wrap(~ var, labeller = labeller(var = c(crime = 'Reduce crime and\npublic disorder', 
                                                encour = 'Encourage drug\nuse',
                                                free = "Restrict people's\nfreedom",
                                                health = "Improve overall\nhealth and wellbeing",
                                                vulner = "Hurt the most\nvulnerable people",
                                                econ = "Boost the economy",
                                                trust = "Improve trust\nin government"))) +
  coord_flip() + 
  ggtitle("Effects on support for each policy scenario") + 
  ylab("") + xlab("") + 
  guides(colour = guide_legend(title = ""),
         alpha = "none",
         #shape = guide_legend(title = "", 
         #                      override.aes = list(linetype = c(1),
         #                                         shape = c(8),
         #                                         size = c(.25)))
  ) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 10),
        legend.key = element_blank(),
        strip.background = element_blank(),
        #legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 12),
        strip.text = element_text(colour = "#3C3C3C", size = 8),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 0, colour = "#3C3C3C", margin = unit(c(0,0,0,0), "mm"), size = 8),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 1) +
  scale_x_discrete(limits = c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6') %>% rev,
                   breaks = c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6') %>% rev)


pdf("plots/support_by_scenario.pdf", width = 10, height = 10, paper = 'a4r')
plot_scenario
dev.off()

pdf("plots/support_by_surveyitems.pdf", width = 10, height = 10, paper = 'a4r')
plot_items
dev.off()

###############################################################

data <-
  data_raw %>%
  mutate(scen1_immoral = case_when(scen1immoral == 4 | scen1immoral == 5 ~ "Immoral",
                                   scen1immoral == 3 ~ "Neutral",
                                   scen1immoral == 1 | scen1immoral == 2 ~ "Moral"),
         scen2_immoral = case_when(scen2immoral == 4 | scen2immoral == 5 ~ "Immoral",
                                   scen2immoral == 3 ~ "Neutral",
                                   scen2immoral == 1 | scen2immoral == 2 ~ "Moral"),
         scen3_immoral = case_when(scen3immoral == 4 | scen3immoral == 5 ~ "Immoral",
                                   scen3immoral == 3 ~ "Neutral",
                                   scen3immoral == 1 | scen3immoral == 2 ~ "Moral"),
         scen4_immoral = case_when(scen4immoral == 4 | scen4immoral == 5 ~ "Immoral",
                                   scen4immoral == 3 ~ "Neutral",
                                   scen4immoral == 1 | scen4immoral == 2 ~ "Moral"),
         scen5_immoral = case_when(scen5immoral == 4 | scen5immoral == 5 ~ "Immoral",
                                   scen5immoral == 3 ~ "Neutral",
                                   scen5immoral == 1 | scen5immoral == 2 ~ "Moral"),
         scen6_immoral = case_when(scen6immoral == 4 | scen6immoral == 5 ~ "Immoral",
                                   scen6immoral == 3 ~ "Neutral",
                                   scen6immoral == 1 | scen6immoral == 2 ~ "Moral")) %>%
  mutate(across(c(scen1supp, scen2supp, scen3supp, scen4supp, scen5supp, scen6supp), ~as_factor(.))) %>%
  mutate(across(c(scen1crime:scen1trust, scen2crime:scen2trust, scen3crime:scen3trust, scen4crime:scen4trust, scen5crime:scen5trust, scen6crime:scen6trust), 
                ~ case_when(. == 4 | . == 5 ~ T, T ~ F))) %>%
  mutate(male = gender == 1,
         white = ethnicity == 1,
         age = case_when(age > 85 ~ NA_real_, TRUE ~ age)) %>%
  mutate(age = as.numeric(age)) %>%
  filter(ncountry == 10 | ncountry == 13 | ncountry == 14 | ncountry == 21 | ncountry == 25 | ncountry == 38 | ncountry == 47 | ncountry == 58 | ncountry == 72 | ncountry == 73 | ncountry == 80
         | ncountry == 98 | ncountry == 104 | ncountry == 106 | ncountry == 140 | ncountry == 155 | ncountry == 173 | ncountry == 174 | ncountry == 201 | ncountry == 206 | ncountry == 208
         | ncountry == 228 | ncountry == 227) %>%
  mutate(country = as_factor(scountry))

# change ref groups
data_immoral <- data %>% mutate(across(c(scen1_immoral:scen6_immoral), ~factor(., levels = c('Immoral', 'Neutral', 'Moral'))))
data_neutral <- data %>% mutate(across(c(scen1_immoral:scen6_immoral), ~factor(., levels = c('Neutral', 'Immoral', 'Moral'))))
data_moral <- data %>% mutate(across(c(scen1_immoral:scen6_immoral), ~factor(., levels = c('Moral', 'Immoral', 'Neutral'))))

# ref: immoral
## aproach would reduce crime
m_scen1_immoral_crime <- polr(scen1supp ~ scen1crime * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_crime <- polr(scen2supp ~ scen2crime * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_crime <- polr(scen3supp ~ scen3crime * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_crime <- polr(scen4supp ~ scen4crime * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_crime <- polr(scen5supp ~ scen5crime * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_crime <- polr(scen6supp ~ scen6crime * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

## approach would encourage drug use
m_scen1_immoral_encour <- polr(scen1supp ~ scen1encour * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_encour <- polr(scen2supp ~ scen2encour * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_encour <- polr(scen3supp ~ scen3encour * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_encour <- polr(scen4supp ~ scen4encour * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_encour <- polr(scen5supp ~ scen5encour * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_encour <- polr(scen6supp ~ scen6encour * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

## approach would restrict freedom
m_scen1_immoral_free <- polr(scen1supp ~ scen1free * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_free <- polr(scen2supp ~ scen2free * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_free <- polr(scen3supp ~ scen3free * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_free <- polr(scen4supp ~ scen4free * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_free <- polr(scen5supp ~ scen5free * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_free <- polr(scen6supp ~ scen6free * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

## approach would improve health and wellbeing
m_scen1_immoral_health <- polr(scen1supp ~ scen1health * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_health <- polr(scen2supp ~ scen2health * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_health <- polr(scen3supp ~ scen3health * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_health <- polr(scen4supp ~ scen4health * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_health <- polr(scen5supp ~ scen5health * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_health <- polr(scen6supp ~ scen6health * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

## approach would hurt the most vulnerable
m_scen1_immoral_vulner <- polr(scen1supp ~ scen1vulner * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_vulner <- polr(scen2supp ~ scen2vulner * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_vulner <- polr(scen3supp ~ scen3vulner * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_vulner <- polr(scen4supp ~ scen4vulner * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_vulner <- polr(scen5supp ~ scen5vulner * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_vulner <- polr(scen6supp ~ scen6vulner * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

## would boost the economy
m_scen1_immoral_econ <- polr(scen1supp ~ scen1econ * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_econ <- polr(scen2supp ~ scen2econ * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_econ <- polr(scen3supp ~ scen3econ * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_econ <- polr(scen4supp ~ scen4econ * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_econ <- polr(scen5supp ~ scen5econ * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_econ <- polr(scen6supp ~ scen6econ * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

## would improve trust in govt
m_scen1_immoral_trust <- polr(scen1supp ~ scen1trust * scen1_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen2_immoral_trust <- polr(scen2supp ~ scen2trust * scen2_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen3_immoral_trust <- polr(scen3supp ~ scen3trust * scen3_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen4_immoral_trust <- polr(scen4supp ~ scen4trust * scen4_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen5_immoral_trust <- polr(scen5supp ~ scen5trust * scen5_immoral + age + male + country, data =data_immoral, method = 'logistic')
m_scen6_immoral_trust <- polr(scen6supp ~ scen6trust * scen6_immoral + age + male + country, data =data_immoral, method = 'logistic')

models_immoral <- list(m_scen1_immoral_crime, m_scen2_immoral_crime, m_scen3_immoral_crime, m_scen4_immoral_crime, m_scen5_immoral_crime, m_scen6_immoral_crime,
               m_scen1_immoral_encour, m_scen2_immoral_encour, m_scen3_immoral_encour, m_scen4_immoral_encour, m_scen5_immoral_encour, m_scen6_immoral_encour,
               m_scen1_immoral_free, m_scen2_immoral_free, m_scen3_immoral_free, m_scen4_immoral_free, m_scen5_immoral_free, m_scen6_immoral_free,
               m_scen1_immoral_health, m_scen2_immoral_health, m_scen3_immoral_health, m_scen4_immoral_health, m_scen5_immoral_health, m_scen6_immoral_health,
               m_scen1_immoral_vulner, m_scen2_immoral_vulner, m_scen3_immoral_vulner, m_scen4_immoral_vulner, m_scen5_immoral_vulner, m_scen6_immoral_vulner,
               m_scen1_immoral_econ, m_scen2_immoral_econ, m_scen3_immoral_econ, m_scen4_immoral_econ, m_scen5_immoral_econ, m_scen6_immoral_econ,
               m_scen1_immoral_trust, m_scen2_immoral_trust, m_scen3_immoral_trust, m_scen4_immoral_trust, m_scen5_immoral_trust, m_scen6_immoral_trust)

# Apply the function to each model in the list and bind rows
coefficients_data_immoral <- lapply(models_immoral, extract_coefficients) %>%
  bind_rows() %>%
  mutate(mor = "Immoral",
         scen = rep(c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6'), 7),
         var = rep(c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust'), each = 6),
         ci.lower = Coefficient - 1.96 * Std_Error,
         ci.upper = Coefficient + 1.96 * Std_Error) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))

m_scen1_neutral_crime <- polr(scen1supp ~ scen1crime * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_crime <- polr(scen2supp ~ scen2crime * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_crime <- polr(scen3supp ~ scen3crime * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_crime <- polr(scen4supp ~ scen4crime * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_crime <- polr(scen5supp ~ scen5crime * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_crime <- polr(scen6supp ~ scen6crime * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

## approach would encourage drug use
m_scen1_neutral_encour <- polr(scen1supp ~ scen1encour * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_encour <- polr(scen2supp ~ scen2encour * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_encour <- polr(scen3supp ~ scen3encour * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_encour <- polr(scen4supp ~ scen4encour * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_encour <- polr(scen5supp ~ scen5encour * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_encour <- polr(scen6supp ~ scen6encour * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

## approach would restrict freedom
m_scen1_neutral_free <- polr(scen1supp ~ scen1free * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_free <- polr(scen2supp ~ scen2free * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_free <- polr(scen3supp ~ scen3free * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_free <- polr(scen4supp ~ scen4free * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_free <- polr(scen5supp ~ scen5free * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_free <- polr(scen6supp ~ scen6free * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

## approach would improve health and wellbeing
m_scen1_neutral_health <- polr(scen1supp ~ scen1health * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_health <- polr(scen2supp ~ scen2health * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_health <- polr(scen3supp ~ scen3health * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_health <- polr(scen4supp ~ scen4health * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_health <- polr(scen5supp ~ scen5health * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_health <- polr(scen6supp ~ scen6health * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

## approach would hurt the most vulnerable
m_scen1_neutral_vulner <- polr(scen1supp ~ scen1vulner * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_vulner <- polr(scen2supp ~ scen2vulner * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_vulner <- polr(scen3supp ~ scen3vulner * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_vulner <- polr(scen4supp ~ scen4vulner * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_vulner <- polr(scen5supp ~ scen5vulner * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_vulner <- polr(scen6supp ~ scen6vulner * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

## would boost the economy
m_scen1_neutral_econ <- polr(scen1supp ~ scen1econ * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_econ <- polr(scen2supp ~ scen2econ * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_econ <- polr(scen3supp ~ scen3econ * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_econ <- polr(scen4supp ~ scen4econ * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_econ <- polr(scen5supp ~ scen5econ * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_econ <- polr(scen6supp ~ scen6econ * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

## would improve trust in govt
m_scen1_neutral_trust <- polr(scen1supp ~ scen1trust * scen1_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen2_neutral_trust <- polr(scen2supp ~ scen2trust * scen2_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen3_neutral_trust <- polr(scen3supp ~ scen3trust * scen3_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen4_neutral_trust <- polr(scen4supp ~ scen4trust * scen4_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen5_neutral_trust <- polr(scen5supp ~ scen5trust * scen5_immoral + age + male + country, data =data_neutral, method = 'logistic')
m_scen6_neutral_trust <- polr(scen6supp ~ scen6trust * scen6_immoral + age + male + country, data =data_neutral, method = 'logistic')

models_neutral <- list(m_scen1_neutral_crime, m_scen2_neutral_crime, m_scen3_neutral_crime, m_scen4_neutral_crime, m_scen5_neutral_crime, m_scen6_neutral_crime,
                       m_scen1_neutral_encour, m_scen2_neutral_encour, m_scen3_neutral_encour, m_scen4_neutral_encour, m_scen5_neutral_encour, m_scen6_neutral_encour,
                       m_scen1_neutral_free, m_scen2_neutral_free, m_scen3_neutral_free, m_scen4_neutral_free, m_scen5_neutral_free, m_scen6_neutral_free,
                       m_scen1_neutral_health, m_scen2_neutral_health, m_scen3_neutral_health, m_scen4_neutral_health, m_scen5_neutral_health, m_scen6_neutral_health,
                       m_scen1_neutral_vulner, m_scen2_neutral_vulner, m_scen3_neutral_vulner, m_scen4_neutral_vulner, m_scen5_neutral_vulner, m_scen6_neutral_vulner,
                       m_scen1_neutral_econ, m_scen2_neutral_econ, m_scen3_neutral_econ, m_scen4_neutral_econ, m_scen5_neutral_econ, m_scen6_neutral_econ,
                       m_scen1_neutral_trust, m_scen2_neutral_trust, m_scen3_neutral_trust, m_scen4_neutral_trust, m_scen5_neutral_trust, m_scen6_neutral_trust)

# Apply the function to each model in the list and bind rows
coefficients_data_neutral <- lapply(models_neutral, extract_coefficients) %>%
  bind_rows() %>%
  mutate(mor = "Neutral",
         scen = rep(c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6'), 7),
         var = rep(c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust'), each = 6),
         ci.lower = Coefficient - 1.96 * Std_Error,
         ci.upper = Coefficient + 1.96 * Std_Error) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))


#

m_scen1_moral_crime <- polr(scen1supp ~ scen1crime * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_crime <- polr(scen2supp ~ scen2crime * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_crime <- polr(scen3supp ~ scen3crime * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_crime <- polr(scen4supp ~ scen4crime * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_crime <- polr(scen5supp ~ scen5crime * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_crime <- polr(scen6supp ~ scen6crime * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

## approach would encourage drug use
m_scen1_moral_encour <- polr(scen1supp ~ scen1encour * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_encour <- polr(scen2supp ~ scen2encour * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_encour <- polr(scen3supp ~ scen3encour * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_encour <- polr(scen4supp ~ scen4encour * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_encour <- polr(scen5supp ~ scen5encour * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_encour <- polr(scen6supp ~ scen6encour * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

## approach would restrict freedom
m_scen1_moral_free <- polr(scen1supp ~ scen1free * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_free <- polr(scen2supp ~ scen2free * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_free <- polr(scen3supp ~ scen3free * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_free <- polr(scen4supp ~ scen4free * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_free <- polr(scen5supp ~ scen5free * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_free <- polr(scen6supp ~ scen6free * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

## approach would improve health and wellbeing
m_scen1_moral_health <- polr(scen1supp ~ scen1health * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_health <- polr(scen2supp ~ scen2health * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_health <- polr(scen3supp ~ scen3health * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_health <- polr(scen4supp ~ scen4health * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_health <- polr(scen5supp ~ scen5health * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_health <- polr(scen6supp ~ scen6health * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

## approach would hurt the most vulnerable
m_scen1_moral_vulner <- polr(scen1supp ~ scen1vulner * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_vulner <- polr(scen2supp ~ scen2vulner * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_vulner <- polr(scen3supp ~ scen3vulner * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_vulner <- polr(scen4supp ~ scen4vulner * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_vulner <- polr(scen5supp ~ scen5vulner * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_vulner <- polr(scen6supp ~ scen6vulner * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

## would boost the economy
m_scen1_moral_econ <- polr(scen1supp ~ scen1econ * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_econ <- polr(scen2supp ~ scen2econ * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_econ <- polr(scen3supp ~ scen3econ * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_econ <- polr(scen4supp ~ scen4econ * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_econ <- polr(scen5supp ~ scen5econ * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_econ <- polr(scen6supp ~ scen6econ * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

## would improve trust in govt
m_scen1_moral_trust <- polr(scen1supp ~ scen1trust * scen1_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen2_moral_trust <- polr(scen2supp ~ scen2trust * scen2_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen3_moral_trust <- polr(scen3supp ~ scen3trust * scen3_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen4_moral_trust <- polr(scen4supp ~ scen4trust * scen4_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen5_moral_trust <- polr(scen5supp ~ scen5trust * scen5_immoral + age + male + country, data =data_moral, method = 'logistic')
m_scen6_moral_trust <- polr(scen6supp ~ scen6trust * scen6_immoral + age + male + country, data =data_moral, method = 'logistic')

models_moral <- list(m_scen1_moral_crime, m_scen2_moral_crime, m_scen3_moral_crime, m_scen4_moral_crime, m_scen5_moral_crime, m_scen6_moral_crime,
                       m_scen1_moral_encour, m_scen2_moral_encour, m_scen3_moral_encour, m_scen4_moral_encour, m_scen5_moral_encour, m_scen6_moral_encour,
                       m_scen1_moral_free, m_scen2_moral_free, m_scen3_moral_free, m_scen4_moral_free, m_scen5_moral_free, m_scen6_moral_free,
                       m_scen1_moral_health, m_scen2_moral_health, m_scen3_moral_health, m_scen4_moral_health, m_scen5_moral_health, m_scen6_moral_health,
                       m_scen1_moral_vulner, m_scen2_moral_vulner, m_scen3_moral_vulner, m_scen4_moral_vulner, m_scen5_moral_vulner, m_scen6_moral_vulner,
                       m_scen1_moral_econ, m_scen2_moral_econ, m_scen3_moral_econ, m_scen4_moral_econ, m_scen5_moral_econ, m_scen6_moral_econ,
                       m_scen1_moral_trust, m_scen2_moral_trust, m_scen3_moral_trust, m_scen4_moral_trust, m_scen5_moral_trust, m_scen6_moral_trust)

# Apply the function to each model in the list and bind rows
coefficients_data_moral <- lapply(models_moral, extract_coefficients) %>%
  bind_rows() %>%
  mutate(mor = "Moral",
         scen = rep(c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6'), 7),
         var = rep(c('crime', 'encour', 'free', 'health', 'vulner', 'econ', 'trust'), each = 6),
         ci.lower = Coefficient - 1.96 * Std_Error,
         ci.upper = Coefficient + 1.96 * Std_Error) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))

dataplot_immoral <-
  coefficients_data_immoral %>%
  bind_rows(coefficients_data_neutral) %>%
  bind_rows(coefficients_data_moral)

plot_items_immoral <-  
  ggplot(dataplot_immoral
         , aes(y = Coefficient, x = scen, colour = mor, group = mor)) +
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper, alpha = significance), width = .25, position = position_dodge(width = 1), lwd = .5, show.legend = T) + 
  geom_point(aes(shape = significance, alpha = significance, group = mor), 
             size = ifelse(dataplot_immoral$significance == "significant", 1, 0), 
             position = position_dodge(width = 1)) +
  scale_alpha_manual(values = c(.3, 1)) +
  scale_shape_manual(values = c(NA,8), labels = c("", "95% confidence interval\ndoes not cross zero")) +
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  facet_wrap(~ var, labeller = labeller(var = c(crime = 'Reduce crime and\npublic disorder', 
                                                encour = 'Encourage drug\nuse',
                                                free = "Restrict people's\nfreedom",
                                                health = "Improve overall\nhealth and wellbeing",
                                                vulner = "Hurt the most\nvulnerable people",
                                                econ = "Boost the economy",
                                                trust = "Improve trust\nin government"))) +
  coord_flip() + 
  ggtitle("Effects on support for each policy scenario") + 
  ylab("") + xlab("") + 
  guides(colour = guide_legend(title = ""),
         alpha = "none",
         shape = guide_legend(title = "", 
                               override.aes = list(linetype = c(0,1),
                                                   shape = c(NA,8),
                                                   size = c(0,1)))
  ) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 10),
        legend.key = element_blank(),
        strip.background = element_blank(),
        #legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 12),
        strip.text = element_text(colour = "#3C3C3C", size = 8),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 0, colour = "#3C3C3C", margin = unit(c(0,0,0,0), "mm"), size = 8),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 1) +
  scale_x_discrete(limits = c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6') %>% rev,
                   breaks = c('Scenario 1', 'Scenario 2', 'Scenario 3', 'Scenario 4', 'Scenario 5', 'Scenario 6') %>% rev) + 
  scale_color_brewer(palette = "Dark2")

pdf("plots/support_by_surveyitems_IMMORAL.pdf", width = 10, height = 12, paper = 'a4r')
plot_items_immoral
dev.off()


###############################################################
## effects on "would reduce crime"

m_crime_scen1 <- polr(scen1crime ~ scen1_support, data = data, method = 'logistic')
m_crime_scen2 <- polr(scen2crime ~ scen2_support, data = data, method = 'logistic')
m_crime_scen3 <- polr(scen3crime ~ scen3_support, data = data, method = 'logistic')
m_crime_scen4 <- polr(scen4crime ~ scen4_support, data = data, method = 'logistic')
m_crime_scen5 <- polr(scen5crime ~ scen5_support, data = data, method = 'logistic')
m_crime_scen6 <- polr(scen6crime ~ scen6_support, data = data, method = 'logistic')

m_crime_scen1X <- polr(scen1crime ~ scen1_support * scen1_immoral, data = data, method = 'logistic')
m_crime_scen2X <- polr(scen2crime ~ scen2_support * scen2_immoral, data = data, method = 'logistic')
m_crime_scen3X <- polr(scen3crime ~ scen3_support * scen3_immoral, data = data, method = 'logistic')
m_crime_scen4X <- polr(scen4crime ~ scen4_support * scen4_immoral, data = data, method = 'logistic')
m_crime_scen5X <- polr(scen5crime ~ scen5_support * scen5_immoral, data = data, method = 'logistic')
m_crime_scen6X <- polr(scen6crime ~ scen6_support * scen6_immoral, data = data, method = 'logistic')

m_crime_scen1X_imm <- polr(scen1crime ~ scen1_support * scen1_immoral, data = data %>% mutate(scen1_immoral = factor(scen1_immoral, levels = c('TRUE', 'FALSE'))), method = 'logistic')
m_crime_scen2X_imm <- polr(scen1crime ~ scen2_support * scen1_immoral, data = data %>% mutate(scen2_immoral = factor(scen2_immoral, levels = c('TRUE', 'FALSE'))), method = 'logistic')
m_crime_scen3X_imm <- polr(scen1crime ~ scen3_support * scen1_immoral, data = data %>% mutate(scen3_immoral = factor(scen3_immoral, levels = c('TRUE', 'FALSE'))), method = 'logistic')
m_crime_scen4X_imm <- polr(scen1crime ~ scen4_support * scen1_immoral, data = data %>% mutate(scen4_immoral = factor(scen4_immoral, levels = c('TRUE', 'FALSE'))), method = 'logistic')
m_crime_scen5X_imm <- polr(scen1crime ~ scen5_support * scen1_immoral, data = data %>% mutate(scen5_immoral = factor(scen5_immoral, levels = c('TRUE', 'FALSE'))), method = 'logistic')
m_crime_scen6X_imm <- polr(scen1crime ~ scen6_support * scen1_immoral, data = data %>% mutate(scen6_immoral = factor(scen6_immoral, levels = c('TRUE', 'FALSE'))), method = 'logistic')

screenreg(list(m_crime_scen1, m_crime_scen2, m_crime_scen3, m_crime_scen4, m_crime_scen5, m_crime_scen6))
screenreg(list(m_crime_scen1X, m_crime_scen2X, m_crime_scen3X, m_crime_scen4X, m_crime_scen5X, m_crime_scen6X))

dataplot_crime <-
  summary(m_crime_scen1)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  filter(rowname == "scen1_supportTRUE") %>%
  mutate(var = 'Crime reduction', scen = 'Scenario 1',
         group = 'All') %>%
  bind_rows(
    summary(m_crime_scen1X)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen1_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 1',
             group = "Moral")
  ) %>%
  bind_rows(
    summary(m_crime_scen1X_imm)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen1_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 1',
             group = "Immoral")
  ) %>%
  bind_rows(
    summary(m_crime_scen2)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen2_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 2',
             group = 'All')
  ) %>%
  bind_rows(
    summary(m_crime_scen2X)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen2_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 2',
             group = "Moral")
  ) %>%
  bind_rows(
    summary(m_crime_scen2X_imm)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen2_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 2',
             group = "Immoral")
  ) %>%
  bind_rows(
    summary(m_crime_scen3)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen3_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 3',
             group = 'All')
  ) %>%
  bind_rows(
    summary(m_crime_scen3X)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen3_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 3',
             group = "Moral")
  ) %>%
  bind_rows(
    summary(m_crime_scen3X_imm)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen3_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 3',
             group = "Immoral")
  ) %>%
  bind_rows(
    summary(m_crime_scen4)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen4_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 4',
             group = 'All')
  ) %>%
  bind_rows(
    summary(m_crime_scen4X)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen4_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 4',
             group = "Moral")
  ) %>%
  bind_rows(
    summary(m_crime_scen4X_imm)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen4_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 4',
             group = "Immoral")
  ) %>%
  bind_rows(
    summary(m_crime_scen5)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen5_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 5',
             group = 'All')
  ) %>%
  bind_rows(
    summary(m_crime_scen5X)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen5_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 5',
             group = "Moral")
  ) %>%
  bind_rows(
    summary(m_crime_scen5X_imm)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen5_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 5',
             group = "Immoral")
  ) %>%
  bind_rows(
    summary(m_crime_scen6)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen6_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 6',
             group = 'All')
  ) %>%
  bind_rows(
    summary(m_crime_scen6X)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen6_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 6',
             group = "Moral")
  ) %>%
  bind_rows(
    summary(m_crime_scen6X_imm)$coefficients %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      filter(rowname == "scen6_supportTRUE") %>%
      mutate(var = 'Crime reduction', scen = 'Scenario 6',
             group = "Immoral")
  ) %>%
  mutate(ci.lower = Value - 1.96 * `Std. Error`,
         ci.upper = Value + 1.96 * `Std. Error`,
         scen = factor(scen, levels = c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5", "Scenario 6")),
         group = factor(group, levels = c("Immoral", "All", "Moral"))) %>%
  mutate(significance = case_when(ci.lower > 0 & ci.upper > 0 ~ 'significant',
                                  ci.lower < 0 & ci.upper < 0 ~ 'significant',
                                  TRUE ~ 'non significant'))

crime_plot <-  
  ggplot(dataplot_crime, aes(y = Value, x = var, colour = group, group = group)) +
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper, alpha = significance), width = .15, position = position_dodge(width = .75), lwd = .5, show.legend = T) + 
  geom_point(aes(shape = significance, alpha = significance, group = group), 
             size = ifelse(dataplot_crime$significance == "significant", 3, 0), 
             position = position_dodge(width = .75)) +
  scale_alpha_manual(values = c(1)) +
  scale_shape_manual(values = c(8), labels = c("95% confidence interval\ndoes not cross zero")) +
  geom_hline(yintercept = 0, size = .35, linetype = "dashed", color = 'darkgray') + 
  facet_wrap(~ scen) +
  coord_flip() + 
  ggtitle("Effects on beliefs about crime reduction") + 
  ylab("") + xlab("") + 
  guides(colour = guide_legend(title = ""),
         alpha = "none",
         shape = guide_legend(title = "", 
                              override.aes = list(linetype = c(1),
                                                  shape = c(8),
                                                  size = c(1)))) +
  theme(plot.title = element_text(hjust = .5, vjust = 2, colour = "#3C3C3C", size = 12)) +
  theme(axis.text.y = element_text(colour = "#3C3C3C", size = 10),
        axis.text.x = element_text(colour = "#3C3C3C", size = 8)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "#3C3C3C", size = .2),
        legend.title = element_text(colour = "#3C3C3C", size = 10),
        legend.key = element_blank(),
        strip.background = element_blank(),
        #legend.text = element_text(colour = "#3C3C3C", size = 10),
        strip.text.x = element_text(size = 12),
        strip.text = element_text(colour = "#3C3C3C", size = 10),
        panel.spacing.x=unit(1, "lines"),
        panel.spacing.y=unit(1, "lines"),
        plot.caption = element_text(hjust = 0, colour = "#3C3C3C", margin = unit(c(0,0,0,0), "mm"), size = 8),
        plot.margin = margin(.5, 0, .5, 0, "cm")) + 
  theme(aspect.ratio = 2.5) + 
  scale_color_brewer(palette = "Dark2",
                     breaks = c("Moral", "All", "Immoral"))

crime_plot

pdf("plots/crimereduction.pdf", width = 10, height = 10, paper = 'a4r')
crime_plot
dev.off()