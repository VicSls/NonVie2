
library("readxl")
library("dplyr")
library("ggplot2")
library("MASS")
library("mgcv")
library("caret")
library("gridExtra")
library("parallel")
library("tidyr")

#Prepare the data
data <- read_excel("ulb_data.xlsx")
data_copy = data

data_train <- data %>% filter(!is.na(claim_nb_tpl_md))
data_test <- data %>% filter(is.na(claim_nb_tpl_md))

#Distributions of the data
data_train %>%
  dplyr::select(where(is.numeric),-id,-geo_postcode_lng,-geo_postcode_lat,-driv_y_add_flg) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  facet_wrap(~ name, scales = "free") +
  labs(title = "Histograms of Numerical Variables") +
  theme_minimal()

#Gam model
#Prepare the data
data_train$veh_make <- as.factor(data_train$veh_make)
data_train$veh_type <- as.factor(data_train$veh_type)
data_train$veh_use <- as.factor(data_train$veh_use)

vars <- c("claim_nb_tpl_md", "veh_age", "veh_power", "veh_value", "driv_m_age", 
          "cont_seniority", "veh_seats", "veh_weight", "veh_make", "veh_type", 
          "veh_use", "admi_risk_exposure")

for (var in vars) {
  cat(var, ": length =", length(data_train[[var]]), 
      ", NA count =", sum(is.na(data_train[[var]])), 
      ", Unique values =", length(unique(data_train[[var]])), "\n")
}

test_variable <- function(var_name) {
  tryCatch({
    # Simple model with just one variable
    test_model <- gam(claim_nb_tpl_md ~ s(get(var_name)) + offset(log(admi_risk_exposure)),
                      family = poisson, data = data_train)
    return(paste(var_name, ": OK"))
  }, error = function(e) {
    return(paste(var_name, ": ERROR -", e$message))
  })
}

results <- sapply(vars, test_variable)
print(results)

# GAM-Model

gam_model <- gam(claim_nb_tpl_md ~ s(veh_age)  + s(veh_power) + s(veh_value) + s(driv_m_age) + s(cont_seniority) +  veh_seats + s(veh_weight) + veh_make + veh_type + veh_use, family = poisson(link = "log"), data = data_train, offset = log(admi_risk_exposure))

data_train$gam_model = gam_model$fitted.values 

summary(gam_model)
