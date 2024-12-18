library(tidyverse)
library(haven)
library(MASS)
library(broom)
library(sjPlot)
library(officer)
library(broom)
library(flextable)

data <- 
  data_raw %>%
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
  mutate(country = as_factor(scountry)) %>%
  mutate(arrest = case_when(
    polactionsarrusecan == 1 | polactionsarrusecan == 2~ T,
    polactionsarrsupcan == 1 | polactionsarrsupcan == 2 ~ T,
    polactionsarruseoth == 1 | polactionsarruseoth == 2 ~ T,
    polactionsarrsupoth == 1 | polactionsarrsupoth == 2 ~ T,
    polactionsconvuse == 1 | polactionsconvuse == 2 ~ T,
    polactionsconvsup == 1 | polactionsconvsup == 2 ~ T,
    TRUE ~ FALSE
  ))

models_arrest <- list(m_scen1_arrest_crime, m_scen2_arrest_crime, m_scen3_arrest_crime, m_scen4_arrest_crime, m_scen5_arrest_crime, m_scen6_arrest_crime,
                      m_scen1_arrest_encour, m_scen2_arrest_encour, m_scen3_arrest_encour, m_scen4_arrest_encour, m_scen5_arrest_encour, m_scen6_arrest_encour,
                      m_scen1_arrest_free, m_scen2_arrest_free, m_scen3_arrest_free, m_scen4_arrest_free, m_scen5_arrest_free, m_scen6_arrest_free,
                      m_scen1_arrest_health, m_scen2_arrest_health, m_scen3_arrest_health, m_scen4_arrest_health, m_scen5_arrest_health, m_scen6_arrest_health,
                      m_scen1_arrest_vulner, m_scen2_arrest_vulner, m_scen3_arrest_vulner, m_scen4_arrest_vulner, m_scen5_arrest_vulner, m_scen6_arrest_vulner,
                      m_scen1_arrest_econ, m_scen2_arrest_econ, m_scen3_arrest_econ, m_scen4_arrest_econ, m_scen5_arrest_econ, m_scen6_arrest_econ,
                      m_scen1_arrest_trust, m_scen2_arrest_trust, m_scen3_arrest_trust, m_scen4_arrest_trust, m_scen5_arrest_trust, m_scen6_arrest_trust)

# Assign names to models_neutral based on their variable names
names(models_arrest) <- c(
  "m_scen1_arrest_crime", "m_scen2_arrest_crime", "m_scen3_arrest_crime", "m_scen4_arrest_crime", 
  "m_scen5_arrest_crime", "m_scen6_arrest_crime",
  "m_scen1_arrest_encour", "m_scen2_arrest_encour", "m_scen3_arrest_encour", "m_scen4_arrest_encour", 
  "m_scen5_arrest_encour", "m_scen6_arrest_encour",
  "m_scen1_arrest_free", "m_scen2_arrest_free", "m_scen3_arrest_free", "m_scen4_arrest_free", 
  "m_scen5_arrest_free", "m_scen6_arrest_free",
  "m_scen1_arrest_health", "m_scen2_arrest_health", "m_scen3_arrest_health", "m_scen4_arrest_health", 
  "m_scen5_arrest_health", "m_scen6_arrest_health",
  "m_scen1_arrest_vulner", "m_scen2_arrest_vulner", "m_scen3_arrest_vulner", "m_scen4_arrest_vulner", 
  "m_scen5_arrest_vulner", "m_scen6_arrest_vulner",
  "m_scen1_arrest_econ", "m_scen2_arrest_econ", "m_scen3_arrest_econ", "m_scen4_arrest_econ", 
  "m_scen5_arrest_econ", "m_scen6_arrest_econ",
  "m_scen1_arrest_trust", "m_scen2_arrest_trust", "m_scen3_arrest_trust", "m_scen4_arrest_trust", 
  "m_scen5_arrest_trust", "m_scen6_arrest_trust"
)

clean_model_results <- function(model) {
  # Tidy the model
  results <- broom::tidy(model)
  
  # Filter only 'coefficient' type
  results <- results[results$coef.type == 'coefficient', ]
  
  # Remove column 'coef.type
  results <- results[, 1:4]
  
  # Regular expressions to omit control variables
  omit_pattern <- paste0("^(", paste(c("age", "male", "country"), collapse = "|"), ")(TRUE|.*)?$")
  
  # Filter out unwanted terms
  results <- results[!grepl(omit_pattern, results$term), ]
  
  return(results)
}

models_arrest_scen1 <- list(crime = m_scen1_arrest_crime, econ = m_scen1_arrest_econ, encour = m_scen1_arrest_encour, free = m_scen1_arrest_free, 
                              health = m_scen1_arrest_health, trust = m_scen1_arrest_trust, vulner = m_scen1_arrest_vulner) %>%
  lapply(clean_model_results) %>%
  imap_dfr(~mutate(.x, model = .y)) %>%
  mutate(scenario = 'scenario 1') %>%
  mutate(significance = case_when(
    statistic > 1.96 | statistic < -1.96 ~ "statistically significant",
    TRUE ~ "not statistically significant"
  ))
models_arrest_scen2 <- list(crime = m_scen2_arrest_crime, econ = m_scen2_arrest_econ, encour = m_scen2_arrest_encour, free = m_scen2_arrest_free, 
                              health = m_scen2_arrest_health, trust = m_scen2_arrest_trust, vulner = m_scen2_arrest_vulner) %>%
  lapply(clean_model_results) %>%
  imap_dfr(~mutate(.x, model = .y)) %>%
  mutate(scenario = 'scenario 2') %>%
  mutate(significance = case_when(
    statistic > 1.96 | statistic < -1.96 ~ "statistically significant",
    TRUE ~ "not statistically significant"
  ))
models_arrest_scen3 <- list(crime = m_scen3_arrest_crime, econ = m_scen3_arrest_econ, encour = m_scen3_arrest_encour, free = m_scen3_arrest_free, 
                              health = m_scen3_arrest_health, trust = m_scen3_arrest_trust, vulner = m_scen3_arrest_vulner) %>%
  lapply(clean_model_results) %>%
  imap_dfr(~mutate(.x, model = .y)) %>%
  mutate(scenario = 'scenario 3') %>%
  mutate(significance = case_when(
    statistic > 1.96 | statistic < -1.96 ~ "statistically significant",
    TRUE ~ "not statistically significant"
  ))
models_arrest_scen4 <- list(crime = m_scen4_arrest_crime, econ = m_scen4_arrest_econ, encour = m_scen4_arrest_encour, free = m_scen4_arrest_free, 
                              health = m_scen4_arrest_health, trust = m_scen4_arrest_trust, vulner = m_scen4_arrest_vulner) %>%
  lapply(clean_model_results) %>%
  imap_dfr(~mutate(.x, model = .y)) %>%
  mutate(scenario = 'scenario 4') %>%
  mutate(significance = case_when(
    statistic > 1.96 | statistic < -1.96 ~ "statistically significant",
    TRUE ~ "not statistically significant"
  ))
models_arrest_scen5 <- list(crime = m_scen5_arrest_crime, econ = m_scen5_arrest_econ, encour = m_scen5_arrest_encour, free = m_scen5_arrest_free, 
                              health = m_scen5_arrest_health, trust = m_scen5_arrest_trust, vulner = m_scen5_arrest_vulner) %>%
  lapply(clean_model_results) %>%
  imap_dfr(~mutate(.x, model = .y)) %>%
  mutate(scenario = 'scenario 5') %>%
  mutate(significance = case_when(
    statistic > 1.96 | statistic < -1.96 ~ "statistically significant",
    TRUE ~ "not statistically significant"
  ))
models_arrest_scen6 <- list(crime = m_scen6_arrest_crime, econ = m_scen6_arrest_econ, encour = m_scen6_arrest_encour, free = m_scen6_arrest_free, 
                              health = m_scen6_arrest_health, trust = m_scen6_arrest_trust, vulner = m_scen6_arrest_vulner) %>%
  lapply(clean_model_results) %>%
  imap_dfr(~mutate(.x, model = .y)) %>%
  mutate(scenario = 'scenario 6') %>%
  mutate(significance = case_when(
    statistic > 1.96 | statistic < -1.96 ~ "statistically significant",
    TRUE ~ "not statistically significant"
  ))

all_models <-
  models_arrest_scen1 %>%
  bind_rows(models_arrest_scen2) %>%
  bind_rows(models_arrest_scen3) %>%
  bind_rows(models_arrest_scen4) %>%
  bind_rows(models_arrest_scen5) %>%
  bind_rows(models_arrest_scen6) %>%
  mutate(scenario = case_when(
    scenario == "scenario 1" ~ "Scenario 1: Death Penalty",
    scenario == "scenario 2" ~ "Scenario 2: Prohibition (without death penalty)",
    scenario == "scenario 3" ~ "Scenario 3: Decriminalisation (with diversion)",
    scenario == "scenario 4" ~ "Scenario 4: Decriminalisation (without diversion)",
    scenario == "scenario 5" ~ "Scenario 5: Legalisation (with government regulation)",
    scenario == "scenario 6" ~ "Scenario 6: Legalisation (without government regulation)",
  )) %>%
  mutate(unique = paste(scenario, "-", model))

ft_list <- list()

term_labels <- c(
  "scen1crimeTRUE" = "Approach would reduce crime",
  "scen2crimeTRUE" = "Approach would reduce crime",
  "scen3crimeTRUE" = "Approach would reduce crime",
  "scen4crimeTRUE" = "Approach would reduce crime",
  "scen5crimeTRUE" = "Approach would reduce crime",
  "scen6crimeTRUE" = "Approach would reduce crime",
  "scen1econTRUE" = "Approach would boost the economy",
  "scen2econTRUE" = "Approach would boost the economy",
  "scen3econTRUE" = "Approach would boost the economy",
  "scen4econTRUE" = "Approach would boost the economy",
  "scen5econTRUE" = "Approach would boost the economy",
  "scen6econTRUE" = "Approach would boost the economy",
  "scen1freeTRUE" = "Approach would restrict freedom",
  "scen2freeTRUE" = "Approach would restrict freedom",
  "scen3freeTRUE" = "Approach would restrict freedom",
  "scen4freeTRUE" = "Approach would restrict freedom",
  "scen5freeTRUE" = "Approach would restrict freedom",
  "scen6freeTRUE" = "Approach would restrict freedom",
  "scen1encourTRUE" = "Approach would encourage drug use",
  "scen2encourTRUE" = "Approach would encourage drug use",
  "scen3encourTRUE" = "Approach would encourage drug use",
  "scen4encourTRUE" = "Approach would encourage drug use",
  "scen5encourTRUE" = "Approach would encourage drug use",
  "scen6encourTRUE" = "Approach would encourage drug use",
  "scen1healthTRUE" = "Approach would improve health and wellbeing",
  "scen2healthTRUE" = "Approach would improve health and wellbeing",
  "scen3healthTRUE" = "Approach would improve health and wellbeing",
  "scen4healthTRUE" = "Approach would improve health and wellbeing",
  "scen5healthTRUE" = "Approach would improve health and wellbeing",
  "scen6healthTRUE" = "Approach would improve health and wellbeing",
  "scen1vulnerTRUE" = "Approach would hurt the most vulnerable",
  "scen2vulnerTRUE" = "Approach would hurt the most vulnerable",
  "scen3vulnerTRUE" = "Approach would hurt the most vulnerable",
  "scen4vulnerTRUE" = "Approach would hurt the most vulnerable",
  "scen5vulnerTRUE" = "Approach would hurt the most vulnerable",
  "scen6vulnerTRUE" = "Approach would hurt the most vulnerable",
  "scen1trustTRUE" = "Approach would improve trust in government",
  "scen2trustTRUE" = "Approach would improve trust in government",
  "scen3trustTRUE" = "Approach would improve trust in government",
  "scen4trustTRUE" = "Approach would improve trust in government",
  "scen5trustTRUE" = "Approach would improve trust in government",
  "scen6trustTRUE" = "Approach would improve trust in government",
  "arrestTRUE" = "Has been arrested"
)


# Loop through each unique model
for (model_name in unique(all_models$unique)) {
  
  # Filter data for the current model
  model_data <- all_models %>% filter(unique == model_name)
  
  # save title
  #my_title <- model_data$scenario
  
  # Remove 'model' and 'scenario' columns
  model_data <- model_data %>% dplyr::select(-model, -scenario, -unique)
  
  # Round numeric columns to two decimal points
  model_data <- model_data %>%
    mutate(across(c(estimate, std.error, statistic), ~ round(.x, 2)))
  
  # Customize the labels in the 'term' column
  model_data <- model_data %>%
    mutate(term = recode(term, !!!term_labels))
  
  # Create a flextable for the current model
  ft <- model_data %>%
    flextable() %>%
    set_header_labels(term = "Variable",  # Custom label for 'Term'
                      estimate = "Estimate", 
                      std.error = "Standard Error", 
                      statistic = "Statistic", 
                      significance = "Significance") %>%
    # Add a centered title row at the top
    add_header_lines(values = c(paste("I support this approach:", sub(" -.*", "", model_name)))) %>%
    set_table_properties(layout = "autofit") %>%
    #add_header_row(values = c(paste("I support this approach:", sub(" -.*", "", model_name)), rep("", ncol(model_data)-1)), top = TRUE) %>%
    #align(align = "center", part = "header") %>%  # Center the header row
    theme_vanilla()
  
  # Store the table in the list
  ft_list[[model_name]] <- ft
}


# Create a function to export tables to Word
export_to_word <- function(ft_list, scenario_name) {
  # Create a new Word document
  doc <- read_docx()
  
  # Correctly pull models in the current scenario
  models_in_scenario <- unique(all_models %>% filter(scenario == scenario_name) %>% pull(unique))
  
  # Loop through the models in the current scenario and add tables
  for (model_name in models_in_scenario) {
    # Ensure the flextable exists for the current model
    ft <- ft_list[[model_name]]
    
    # Check if the model exists in ft_list
    if (is.null(ft)) {
      warning(paste("Model", model_name, "is missing from ft_list"))
      next  # Skip to the next model if it's missing
    }
    
    # If it's already a flextable, directly add it to the document
    if (inherits(ft, "flextable")) {
      # Add the flextable to the Word document
      doc <- doc %>%
        body_add_flextable(ft) %>%  # Add the flextable directly
        body_add_par(" ", style = "Normal")  # Add space between tables (optional)
    } else {
      warning(paste("Model", model_name, "is not a valid flextable"))
    }
  }
  
  # Export the Word document to a file
  output_file <- paste0("tables - models Arrest/model_arrest_results_", scenario_name, ".docx")
  print(doc, target = output_file)
}

# Export tables for each scenario
for (scenario_name in unique(all_models$scenario)) {
  export_to_word(ft_list, scenario_name)
}