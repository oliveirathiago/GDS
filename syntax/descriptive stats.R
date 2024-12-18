library(tidyverse)
library(flextable)
library(officer)
library(haven)

data_raw <- read_spss('data/gds2022_policy_45094.sav')

#### SCENARIO 1 ####

data_scen1 <- data_raw %>%
  dplyr::select(scen1supp, scen1crime, scen1encour, scen1econ, scen1free, scen1health, scen1vulner, scen1trust, scen1immoral) %>%
  drop_na() %>%
  mutate(across(everything(), ~ as.numeric(as_factor(.))))

# Define custom labels for variables
custom_labels_scen1 <- c(
  scen1supp = "I support this approach",
  scen1crime = "This approach would reduce crime and public disorder in our communities",
  scen1encour = "Trust in This approach would encourage drug use",
  scen1econ = "This approach would boost the economy",
  scen1free = "This approach would restrict people's freedom to act as they wish",
  scen1health = "This approach would improve the overall health and wellbeing of people in our communities",
  scen1vulner = "This approach would hurt the most vulenrable people in our society",
  scen1trust = "This approach would improve trust in government",
  scen1immoral = "This approach is immoral"
)


# Calculate counts and percentages for each variable
stats_table_scen1 <- data_scen1 %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>%
  mutate(Response = factor(Response, levels = 1:5)) %>%
  group_by(Variable, Response) %>%
  summarise(
    Count = n(),  # Calculate count for each response
    .groups = "drop"
  ) %>%
  group_by(Variable) %>%
  mutate(
    Percentage = round((Count / sum(Count)) * 100, 0)  # Calculate percentage within each variable
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(Count, Percentage), names_to = "Measure", values_to = "Value") %>%
  pivot_wider(names_from = Response, values_from = Value, values_fill = 0)

# Reformat table so each variable has 2 rows (Count and Percentage)
final_table_scen1 <- stats_table_scen1 %>%
  mutate(Variable = recode(Variable, !!!custom_labels_scen1)) %>%
  arrange(Variable, Measure) %>%
  dplyr::select(Variable, Measure, `1`, `2`, `3`, `4`, `5`) %>%
  mutate(Measure = ifelse(Measure == "Count", "Count", "Percentage (%)"))

ft_scen1 <- flextable(final_table_scen1) %>%
  set_header_labels(
    `1` = "Strongly Disagree",
    `2` = "Disagree",
    `3` = "Neutral",
    `4` = "Agree",
    `5` = "Strongly Agree"
  ) %>%
  merge_v(j = "Variable") %>%  # Merge rows in the Variable column
  align(j = "Variable", align = "center", part = "all") %>%  # Center-align Variable column
  add_header_lines(values = "Descriptive Statistics for Scenario 1: Death penalty") %>%
  set_table_properties(layout = "autofit")

# Create a Word document and add the flextable
doc_scen1 <- read_docx()
doc_scen1 <- doc_scen1 %>%
  body_add_flextable(ft_scen1)

# Save the document
print(doc_scen1, target = "tables - descriptive statistics/scenario1_descriptive_stats_table.docx")

#######################################

#### SCENARIO 2 ####

data_scen2 <- data_raw %>%
  dplyr::select(scen2supp, scen2crime, scen2encour, scen2econ, scen2free, scen2health, scen2vulner, scen2trust, scen2immoral) %>%
  drop_na() %>%
  mutate(across(everything(), ~ as.numeric(as_factor(.))))

# Define custom labels for variables
custom_labels_scen2 <- c(
  scen2supp = "I support this approach",
  scen2crime = "This approach would reduce crime and public disorder in our communities",
  scen2encour = "Trust in This approach would encourage drug use",
  scen2econ = "This approach would boost the economy",
  scen2free = "This approach would restrict people's freedom to act as they wish",
  scen2health = "This approach would improve the overall health and wellbeing of people in our communities",
  scen2vulner = "This approach would hurt the most vulenrable people in our society",
  scen2trust = "This approach would improve trust in government",
  scen2immoral = "This approach is immoral"
)


# Calculate counts and percentages for each variable
stats_table_scen2 <- data_scen2 %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>%
  mutate(Response = factor(Response, levels = 1:5)) %>%
  group_by(Variable, Response) %>%
  summarise(
    Count = n(),  # Calculate count for each response
    .groups = "drop"
  ) %>%
  group_by(Variable) %>%
  mutate(
    Percentage = round((Count / sum(Count)) * 100, 0)  # Calculate percentage within each variable
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(Count, Percentage), names_to = "Measure", values_to = "Value") %>%
  pivot_wider(names_from = Response, values_from = Value, values_fill = 0)

# Reformat table so each variable has 2 rows (Count and Percentage)
final_table_scen2 <- stats_table_scen2 %>%
  mutate(Variable = recode(Variable, !!!custom_labels_scen2)) %>%
  arrange(Variable, Measure) %>%
  dplyr::select(Variable, Measure, `1`, `2`, `3`, `4`, `5`) %>%
  mutate(Measure = ifelse(Measure == "Count", "Count", "Percentage (%)"))

ft_scen2 <- flextable(final_table_scen2) %>%
  set_header_labels(
    `1` = "Strongly Disagree",
    `2` = "Disagree",
    `3` = "Neutral",
    `4` = "Agree",
    `5` = "Strongly Agree"
  ) %>%
  merge_v(j = "Variable") %>%  # Merge rows in the Variable column
  align(j = "Variable", align = "center", part = "all") %>%  # Center-align Variable column
  add_header_lines(values = "Descriptive Statistics for Scenario 2: Prohibition (without death penalty)") %>%
  set_table_properties(layout = "autofit")

# Create a Word document and add the flextable
doc_scen2 <- read_docx()
doc_scen2 <- doc_scen2 %>%
  body_add_flextable(ft_scen2)

# Save the document
print(doc_scen2, target = "tables - descriptive statistics/scenario2_descriptive_stats_table.docx")

#######################################

#### SCENARIO 3 ####

data_scen3 <- data_raw %>%
  dplyr::select(scen3supp, scen3crime, scen3encour, scen3econ, scen3free, scen3health, scen3vulner, scen3trust, scen3immoral) %>%
  drop_na() %>%
  mutate(across(everything(), ~ as.numeric(as_factor(.))))

# Define custom labels for variables
custom_labels_scen3 <- c(
  scen3supp = "I support this approach",
  scen3crime = "This approach would reduce crime and public disorder in our communities",
  scen3encour = "Trust in This approach would encourage drug use",
  scen3econ = "This approach would boost the economy",
  scen3free = "This approach would restrict people's freedom to act as they wish",
  scen3health = "This approach would improve the overall health and wellbeing of people in our communities",
  scen3vulner = "This approach would hurt the most vulenrable people in our society",
  scen3trust = "This approach would improve trust in government",
  scen3immoral = "This approach is immoral"
)


# Calculate counts and percentages for each variable
stats_table_scen3 <- data_scen3 %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>%
  mutate(Response = factor(Response, levels = 1:5)) %>%
  group_by(Variable, Response) %>%
  summarise(
    Count = n(),  # Calculate count for each response
    .groups = "drop"
  ) %>%
  group_by(Variable) %>%
  mutate(
    Percentage = round((Count / sum(Count)) * 100, 0)  # Calculate percentage within each variable
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(Count, Percentage), names_to = "Measure", values_to = "Value") %>%
  pivot_wider(names_from = Response, values_from = Value, values_fill = 0)

# Reformat table so each variable has 2 rows (Count and Percentage)
final_table_scen3 <- stats_table_scen3 %>%
  mutate(Variable = recode(Variable, !!!custom_labels_scen3)) %>%
  arrange(Variable, Measure) %>%
  dplyr::select(Variable, Measure, `1`, `2`, `3`, `4`, `5`) %>%
  mutate(Measure = ifelse(Measure == "Count", "Count", "Percentage (%)"))

ft_scen3 <- flextable(final_table_scen3) %>%
  set_header_labels(
    `1` = "Strongly Disagree",
    `2` = "Disagree",
    `3` = "Neutral",
    `4` = "Agree",
    `5` = "Strongly Agree"
  ) %>%
  merge_v(j = "Variable") %>%  # Merge rows in the Variable column
  align(j = "Variable", align = "center", part = "all") %>%  # Center-align Variable column
  add_header_lines(values = "Descriptive Statistics for Scenario 3: Decriminalisation (with diversion)") %>%
  set_table_properties(layout = "autofit")

# Create a Word document and add the flextable
doc_scen3 <- read_docx()
doc_scen3 <- doc_scen3 %>%
  body_add_flextable(ft_scen3)

# Save the document
print(doc_scen3, target = "tables - descriptive statistics/scenario3_descriptive_stats_table.docx")

#######################################

#### SCENARIO 4 ####

data_scen4 <- data_raw %>%
  dplyr::select(scen4supp, scen4crime, scen4encour, scen4econ, scen4free, scen4health, scen4vulner, scen4trust, scen4immoral) %>%
  drop_na() %>%
  mutate(across(everything(), ~ as.numeric(as_factor(.))))

# Define custom labels for variables
custom_labels_scen4 <- c(
  scen4supp = "I support this approach",
  scen4crime = "This approach would reduce crime and public disorder in our communities",
  scen4encour = "Trust in This approach would encourage drug use",
  scen4econ = "This approach would boost the economy",
  scen4free = "This approach would restrict people's freedom to act as they wish",
  scen4health = "This approach would improve the overall health and wellbeing of people in our communities",
  scen4vulner = "This approach would hurt the most vulenrable people in our society",
  scen4trust = "This approach would improve trust in government",
  scen4immoral = "This approach is immoral"
)


# Calculate counts and percentages for each variable
stats_table_scen4 <- data_scen4 %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>%
  mutate(Response = factor(Response, levels = 1:5)) %>%
  group_by(Variable, Response) %>%
  summarise(
    Count = n(),  # Calculate count for each response
    .groups = "drop"
  ) %>%
  group_by(Variable) %>%
  mutate(
    Percentage = round((Count / sum(Count)) * 100, 0)  # Calculate percentage within each variable
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(Count, Percentage), names_to = "Measure", values_to = "Value") %>%
  pivot_wider(names_from = Response, values_from = Value, values_fill = 0)

# Reformat table so each variable has 2 rows (Count and Percentage)
final_table_scen4 <- stats_table_scen4 %>%
  mutate(Variable = recode(Variable, !!!custom_labels_scen4)) %>%
  arrange(Variable, Measure) %>%
  dplyr::select(Variable, Measure, `1`, `2`, `3`, `4`, `5`) %>%
  mutate(Measure = ifelse(Measure == "Count", "Count", "Percentage (%)"))

ft_scen4 <- flextable(final_table_scen4) %>%
  set_header_labels(
    `1` = "Strongly Disagree",
    `2` = "Disagree",
    `3` = "Neutral",
    `4` = "Agree",
    `5` = "Strongly Agree"
  ) %>%
  merge_v(j = "Variable") %>%  # Merge rows in the Variable column
  align(j = "Variable", align = "center", part = "all") %>%  # Center-align Variable column
  add_header_lines(values = "Descriptive Statistics for Scenario 4: Decriminalisation (without diversion)") %>%
  set_table_properties(layout = "autofit")

# Create a Word document and add the flextable
doc_scen4 <- read_docx()
doc_scen4 <- doc_scen4 %>%
  body_add_flextable(ft_scen4)

# Save the document
print(doc_scen4, target = "tables - descriptive statistics/scenario4_descriptive_stats_table.docx")

#######################################

#### SCENARIO 5 ####

data_scen5 <- data_raw %>%
  dplyr::select(scen5supp, scen5crime, scen5encour, scen5econ, scen5free, scen5health, scen5vulner, scen5trust, scen5immoral) %>%
  drop_na() %>%
  mutate(across(everything(), ~ as.numeric(as_factor(.))))

# Define custom labels for variables
custom_labels_scen5 <- c(
  scen5supp = "I support this approach",
  scen5crime = "This approach would reduce crime and public disorder in our communities",
  scen5encour = "Trust in This approach would encourage drug use",
  scen5econ = "This approach would boost the economy",
  scen5free = "This approach would restrict people's freedom to act as they wish",
  scen5health = "This approach would improve the overall health and wellbeing of people in our communities",
  scen5vulner = "This approach would hurt the most vulenrable people in our society",
  scen5trust = "This approach would improve trust in government",
  scen5immoral = "This approach is immoral"
)


# Calculate counts and percentages for each variable
stats_table_scen5 <- data_scen5 %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>%
  mutate(Response = factor(Response, levels = 1:5)) %>%
  group_by(Variable, Response) %>%
  summarise(
    Count = n(),  # Calculate count for each response
    .groups = "drop"
  ) %>%
  group_by(Variable) %>%
  mutate(
    Percentage = round((Count / sum(Count)) * 100, 0)  # Calculate percentage within each variable
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(Count, Percentage), names_to = "Measure", values_to = "Value") %>%
  pivot_wider(names_from = Response, values_from = Value, values_fill = 0)

# Reformat table so each variable has 2 rows (Count and Percentage)
final_table_scen5 <- stats_table_scen5 %>%
  mutate(Variable = recode(Variable, !!!custom_labels_scen5)) %>%
  arrange(Variable, Measure) %>%
  dplyr::select(Variable, Measure, `1`, `2`, `3`, `4`, `5`) %>%
  mutate(Measure = ifelse(Measure == "Count", "Count", "Percentage (%)"))

ft_scen5 <- flextable(final_table_scen5) %>%
  set_header_labels(
    `1` = "Strongly Disagree",
    `2` = "Disagree",
    `3` = "Neutral",
    `4` = "Agree",
    `5` = "Strongly Agree"
  ) %>%
  merge_v(j = "Variable") %>%  # Merge rows in the Variable column
  align(j = "Variable", align = "center", part = "all") %>%  # Center-align Variable column
  add_header_lines(values = "Descriptive Statistics for Scenario 5: Legalisation (with government regulation)") %>%
  set_table_properties(layout = "autofit")

# Create a Word document and add the flextable
doc_scen5 <- read_docx()
doc_scen5 <- doc_scen5 %>%
  body_add_flextable(ft_scen5)

# Save the document
print(doc_scen5, target = "tables - descriptive statistics/scenario5_descriptive_stats_table.docx")

#######################################

#### SCENARIO 6 ####

data_scen6 <- data_raw %>%
  dplyr::select(scen6supp, scen6crime, scen6encour, scen6econ, scen6free, scen6health, scen6vulner, scen6trust, scen6immoral) %>%
  drop_na() %>%
  mutate(across(everything(), ~ as.numeric(as_factor(.))))

# Define custom labels for variables
custom_labels_scen6 <- c(
  scen6supp = "I support this approach",
  scen6crime = "This approach would reduce crime and public disorder in our communities",
  scen6encour = "Trust in This approach would encourage drug use",
  scen6econ = "This approach would boost the economy",
  scen6free = "This approach would restrict people's freedom to act as they wish",
  scen6health = "This approach would improve the overall health and wellbeing of people in our communities",
  scen6vulner = "This approach would hurt the most vulenrable people in our society",
  scen6trust = "This approach would improve trust in government",
  scen6immoral = "This approach is immoral"
)


# Calculate counts and percentages for each variable
stats_table_scen6 <- data_scen6 %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") %>%
  mutate(Response = factor(Response, levels = 1:5)) %>%
  group_by(Variable, Response) %>%
  summarise(
    Count = n(),  # Calculate count for each response
    .groups = "drop"
  ) %>%
  group_by(Variable) %>%
  mutate(
    Percentage = round((Count / sum(Count)) * 100, 0)  # Calculate percentage within each variable
  ) %>%
  ungroup() %>%
  pivot_longer(cols = c(Count, Percentage), names_to = "Measure", values_to = "Value") %>%
  pivot_wider(names_from = Response, values_from = Value, values_fill = 0)

# Reformat table so each variable has 2 rows (Count and Percentage)
final_table_scen6 <- stats_table_scen6 %>%
  mutate(Variable = recode(Variable, !!!custom_labels_scen6)) %>%
  arrange(Variable, Measure) %>%
  dplyr::select(Variable, Measure, `1`, `2`, `3`, `4`, `5`) %>%
  mutate(Measure = ifelse(Measure == "Count", "Count", "Percentage (%)"))

ft_scen6 <- flextable(final_table_scen6) %>%
  set_header_labels(
    `1` = "Strongly Disagree",
    `2` = "Disagree",
    `3` = "Neutral",
    `4` = "Agree",
    `5` = "Strongly Agree"
  ) %>%
  merge_v(j = "Variable") %>%  # Merge rows in the Variable column
  align(j = "Variable", align = "center", part = "all") %>%  # Center-align Variable column
  add_header_lines(values = "Descriptive Statistics for Scenario 6: Legalisation (without government regulation)") %>%
  set_table_properties(layout = "autofit")

# Create a Word document and add the flextable
doc_scen6 <- read_docx()
doc_scen6 <- doc_scen6 %>%
  body_add_flextable(ft_scen6)

# Save the document
print(doc_scen6, target = "tables - descriptive statistics/scenario6_descriptive_stats_table.docx")

#######################################