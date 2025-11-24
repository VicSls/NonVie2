library("readxl")
library("dplyr")
library("ggplot2")
library("MASS")
library("mgcv")
library("caret")
library("gridExtra")
library("parallel")
library("tidyr")
library("plyr")
library("plotrix")

#Prepare the data
data<-ulb_data
data_clean <- data %>% filter(!is.na(claim_nb_tpl_md))
data_clean <- data_clean %>% filter(!is.na(claim_nb_tpl_bi))

#exposition au risque sup à 1, est-ce possible que expo peut être supérieur 
data_clean <- data_clean %>%
  mutate(admi_risk_exposure = pmin(admi_risk_exposure, 1))

# Nous faisons cela pour conserver l'information (supposant que NA signifie 0 année sans sinistre)
data_clean <- data_clean %>% 
  mutate(veh_years_claim_free = ifelse(is.na(veh_years_claim_free), 0, veh_years_claim_free))%>% 
  mutate(driv_y_age = ifelse(is.na(driv_y_age), 0, driv_y_age))

#categoriel to numeric
data_clean <- data_clean %>% 
  mutate(
    claim_nb_tpl_md = as.numeric(claim_nb_tpl_md), 
    claim_nb_tpl_bi = as.numeric(claim_nb_tpl_bi)  
  )

###############################################################################

summary(data_clean)
#Distributions of the data
data_clean %>%
  dplyr::select(where(is.numeric),-id,-geo_postcode_lng,-geo_postcode_lat,-driv_y_add_flg) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  facet_wrap(~ name, scales = "free") +
  labs(title = "Histograms of Numerical Variables") +
  theme_minimal()

#Intro: tests sur les v.a.

#Nbr total d'exposition aux risques en année
format(sum(data_clean$admi_risk_exposure))

#Nbr de police par mois d'exposition
table(cut(data_clean$admi_risk_exposure, breaks = seq(from = 0, to = 1,by = 1/12), labels = 1:12))

#% du portefeuille par mois d'exposition
round(prop.table(table(cut(data_clean$admi_risk_exposure, breaks = seq(from = 0, to = 1,by = 1/12), labels = 1:12))), 4)

#graph du nbr d'exposure 
Exposure.summary = cut(data_clean$admi_risk_exposure, breaks = seq(from = 0, to = 1,by = 1/12))
levels(Exposure.summary) = 1:12
ggplot()+geom_bar(aes(x=Exposure.summary)) + xlab("Number of months") + ggtitle("Exposure in months")


#Nbr claim
ggplot(data_clean, aes(x=Tot_claim))+geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)+ylim(c(0,210000))+
  ylab("")+ xlab("Number of Claims")+  ggtitle("Proportion of policies by number of claims")


#We can compute the average claim frequency in this portfolio, taking into account the different exposures.
#resultat rejoint les 5% du prof
sum(data_clean$Tot_claim) / sum(data_clean$admi_risk_exposure)

#veh_power
#nbr total de véhicule par puissance
table(data_clean$veh_power)

#Exposition des véhicules par puissance 

Power.summary = ddply(data_clean, .(veh_power), summarize, totalExposure = sum(admi_risk_exposure), Number.Observations= length(admi_risk_exposure))

ggplot(Power.summary, aes(x=veh_power, y=totalExposure, fill=veh_power)) + 
  geom_bar(stat="identity")+
  ylab("Exposure in years")+
  geom_text(stat='identity', aes(label=round(totalExp, 0), color=veh_power), vjust=-0.5)+
  guides(fill=FALSE, color=FALSE)

#Nbr de claim en proportion de l'exposition au risque
Power.summary = ddply(data_clean, .(veh_power), summarize, totalExposure = sum(admi_risk_exposure), Number.Observations = length(admi_risk_exposure), Number.Claims = sum(Tot_claim), Obs.Claim.Frequency = sum(Tot_claim)/sum(admi_risk_exposure))
ggplot(Power.summary, aes(x=veh_power, y=Obs.Claim.Frequency, fill=veh_power)) + 
  geom_bar(stat="identity")+
  ylab("Nbr de claim en proportion de l'exposition au risque")+
  geom_text(stat='identity', aes(label=round(totalExposure, 0), color=veh_power), vjust=-0.5)+
  guides(fill=FALSE, color=FALSE)

#Same+ ligne rouge qui correspond à la moyenne du portefeuille
portfolio.cf = sum(data_clean$Tot_claim)/ sum(data_clean$admi_risk_exposure)
ggplot(Power.summary) + geom_bar(stat="identity", aes(x=veh_power, y=Obs.Claim.Frequency, fill=veh_power)) + 
  geom_line(aes(x = as.numeric(veh_power),y=portfolio.cf), color="red") + guides(fill=FALSE)

#veh_age
#Nbr tot, mais pas relater à l'exposion
ggplot(data_clean, aes(x=veh_age)) + geom_bar()  + xlab("Age of the Car")
#Par rapport à l'exposition, on voit que les plots se ressemblent
CarAge.summary = ddply(data_clean, .(veh_age), summarize, totalExposure = sum(admi_risk_exposure), Number.Observations = length(admi_risk_exposure))
ggplot(CarAge.summary, aes(x=veh_age, y=totalExposure)) + geom_bar(stat='identity') + ylab("Exposure in years")

ggplot(data_clean[data_clean$veh_age==0,], aes(x="Exposure", y=admi_risk_exposure)) + geom_boxplot() +ggtitle("Exposure of new cars")

CarAge.summary = ddply(data_clean, .(veh_age), summarize, totalExposure = sum(admi_risk_exposure), Number.Observations = length(admi_risk_exposure), Number.Claims = sum(Tot_claim), Obs.Claim.Freq = sum(Tot_claim)/sum(admi_risk_exposure))
ggplot(CarAge.summary) + geom_bar(stat="identity", aes(x=veh_age, y=Obs.Claim.Freq, fill=veh_age)) + 
  geom_line(aes(x = as.numeric(veh_age),y=portfolio.cf), color="red") + guides(fill=FALSE)

#driv age
DriverAge.summary = ddply(data_clean, .(driv_m_age), summarize, totalExposure = sum(admi_risk_exposure), Number.Observations = length(admi_risk_exposure), Number.Claims = sum(Tot_claim), Obs.Claim.Freq = sum(Tot_claim)/sum(admi_risk_exposure))

#We can show the Exposures by Age of the Driver
ggplot(DriverAge.summary, aes(x=driv_m_age, y=totalExposure)) + geom_bar(stat='identity', width=0.8) + ylab("Exposure in years")+xlab("Age of the Driver")

#Observed claim frequency
ggplot(DriverAge.summary) + geom_bar(stat="identity", aes(x=driv_m_age, y=Obs.Claim.Freq, fill=driv_m_age)) + 
  geom_line(aes(x = as.numeric(driv_m_age),y=portfolio.cf), color="red") + guides(fill=FALSE)


#Brand
Brand.summary = ddply(data_clean, .(veh_make), summarize, totalExposure = sum(admi_risk_exposure), Number.Observations = length(admi_risk_exposure), Number.Claims = sum(Tot_claim), Obs.Claim.Freq = sum(Tot_claim)/sum(admi_risk_exposure))
ggplot(Brand.summary, aes(x=reorder(veh_make,totalExposure), y=totalExposure, fill=veh_make)) +
  geom_bar(stat='identity') +
  coord_flip()+guides(fill=FALSE)+xlab("")+ylab("Exposure in years")

#Observed claim frequency
ggplot(Brand.summary, aes(x=reorder(veh_make,Obs.Claim.Freq), y=Obs.Claim.Freq, fill=veh_make)) +
  geom_bar(stat='identity') +
  coord_flip()+guides(fill=FALSE)+ ggtitle("Observed Claim Frequencies by Brand of the car")+xlab("")+ylab("Observed Claim Frequency")
#ressemble à une approximation de la puissance véhicule ???


#Gas
Gas.summary = ddply(data_clean, .(veh_fuel), summarize, totalExposure = sum(admi_risk_exposure), Number.Observations = length(admi_risk_exposure), Number.Claims = sum(Tot_claim), Obs.Claim.Freq = sum(Tot_claim)/sum(admi_risk_exposure))
ggplot(Gas.summary, aes(x=veh_fuel, y=totalExposure, fill=veh_fuel)) + geom_bar(stat="identity") + guides(fill=FALSE)
#Usually diesel has a bigger impact portfolio, here hybrid and electricity are still big
ggplot(Gas.summary, aes(x=veh_fuel, y=Obs.Claim.Freq, fill=veh_fuel)) + geom_bar(stat="identity") + guides(fill=FALSE)

#Postcode or geo_munty_fr or coordonnée ?
sapply(
  data_clean[, c("geo_postcode_2digits", "geo_munty_fr")],
  function(x) length(unique(x))
)
n_unique_coords <- nrow(
  distinct(data_clean, geo_postcode_lat, geo_postcode_lng)
)
n_unique_coords
#Donc geo_postcode_2digits est enough
Postcode.summary = ddply(data_clean, .(geo_postcode_2digits), summarize, totalExposure = sum(admi_risk_exposure), Number.Observations = length(admi_risk_exposure), Number.Claims = sum(Tot_claim), Obs.Claim.Freq = sum(Tot_claim)/sum(admi_risk_exposure))
twoord.plot(1:40,Postcode.summary$totalExposure,1:40,Postcode.summary$Obs.Claim.Freq,xlab="Region",
            rylim=c(0,0.1),type=c("bar","p"), xticklab = Postcode.summary$geo_postcode_2digits, ylab = "Exposure", rylab = "Observed Claim Frequency")

#2nd selection dataset de ce qu'on a besoin, j'enlève l'année mais pourrait être utile à l'avenir
Cdata <- data_clean %>% 
  dplyr::select(
    #-geo_munty_fr, 
    #-geo_province_fr, 
    #-geo_region_fr, 
    #-geo_postcode_lat,
    #-geo_postcode_lng, 
    -id,
    -purpose,
    #-admi_risk_year
  ) 
#on enlève purpose car 1 seul classe, on a perdu les autres classes 
#car ils étaient NA

##########################################################################
#GAM claim_nb_tpl_md

set.seed(123) # For reproducibility
Cdata$offset_link <- log(Cdata$admi_risk_exposure)
train_index <- createDataPartition(Cdata$claim_nb_tpl_md,times=1, p = 0.8, list = FALSE)
train_set <- Cdata[train_index, ]
val_set <- Cdata[-train_index, ]

#au lieu de mettre si il y a un deuxième driver, je mets juste l'âge du deuxième driver
y<- c("claim_nb_tpl_md")
vars_all= c("veh_years_claim_free", "veh_age", 
            "veh_power", "veh_value",
            "veh_weight" , "cont_seniority"
            ,"driv_m_age","driv_y_age",
            "veh_fuel","veh_make", 
            "veh_seats", "veh_type", 
            "veh_use", "geo_postcode_2digits",
            "geo_munty_fr","geo_province_fr",
            "geo_region_fr", "geo_postcode_lat",
            "geo_postcode_lng " , "driv_y_add_flg")

for (var in vars_all) {
  cat(var, ": length =", length(Cdata[[var]]), 
      ", NA count =", sum(is.na(Cdata[[var]])), 
      ", Unique values =", length(unique(Cdata[[var]])), "\n")
}


#On va tester claim_nb_tpl_md //  admi_risk_exposure est dans l'offset
form <- function(var_name, data) {
  v <- data[[var_name]]
  #Binaire to facteur
  if (is.numeric(v) && length(unique(v)) <= 2) {
    as.formula(
      paste0("claim_nb_tpl_md ~ factor(", var_name, ") + offset(offset_link)")
    )
  } 
  #continue to spline
  else if (is.numeric(v)) {
    as.formula(
      paste0("claim_nb_tpl_md ~ s(", var_name, ") + offset(offset_link)")
    )
  } else {
    # facteur
    as.formula(
      paste0("claim_nb_tpl_md ~ ", var_name, " + offset(offset_link)")
    )
  }
}

transfo<- function(var_name, data) {
  v <- data[[var_name]]
  
  if (is.numeric(v) && length(unique(v)) <= 2) {
    # binaire to facteur
    return(paste0("factor(", var_name, ")"))
  } else if (is.numeric(v)) {
    # continue to spline
    return(paste0("s(", var_name, ")"))
  } else {
    # facteur → tel quel
    return(var_name)
  }
}

#intercept mean frequency, meanfrequency is consitent with expected
#family=quasipoisson() ou nb()
fit0<-gam(claim_nb_tpl_md~1, data=train_set, poisson(link = "log"), offset=offset_link,  method = "REML")
mean_frequency <- exp(coef(fit0))
mean_frequency


#Firt: gam will all var


transfo_var_all <- sapply(vars_all, transfo, data = train_set)
fit_all <- gam(as.formula(paste("claim_nb_tpl_md ~", 
                                paste(transfo_var_all, collapse = " + ")))
               ,family = poisson(link = "log"), data = train_set, 
               offset = offset, method = "REML")
summary (fit_all)

#figure margin ?
plot(fit_all, pages = 1, residuals = TRUE, all.terms = TRUE)


#Second : only variable signicicant better than intercept

pvalue_solo_test <- function(var, data = train_set) {
  fit0 <- gam(claim_nb_tpl_md ~ 1 + offset(offset_link), 
              family = poisson(link = "log"), 
              data = data, method = "REML")  
  f<-form(var, data)
  v<-data[[var]]
  res <- tryCatch({
    fit <- gam(f, family = poisson(link = "log"), data = data,  method = "REML")

    lrt_test <- anova(fit0, fit, test = "Chisq")
    p_val <- lrt_test$`Pr(>Chi)`[2]
    
    data.frame(
      variable   = var,
      p_value    = p_val,
      significant = p_val < 0.05
    )
  })
  return(res)
}
#Compare les varaibles meilleur que l'intercept à alpha 0,05, puis donne 
#les variables si significatives qu'ils serviront de base pour les algo suivants
#afin de limoter la force
results <- do.call(rbind, lapply(vars_all, pvalue_solo_test))
results

vars_s<- subset(results, significant)$variable
vars_s
#"veh_years_claim_free" "veh_age"  "veh_value" "veh_weight"           
#"cont_seniority"       "driv_m_age" "driv_y_age" "veh_fuel"             
#"veh_seats""veh_type"  "veh_use"             
#"geo_postcode_2digits""geo_munty_fr"  "geo_province_fr"      
#"geo_region_fr"  "geo_postcode_lat"     "geo_postcode_lng "   

transfo_s<- sapply(vars_s, transfo, data = train_set)

signi_model <- gam(as.formula(paste("claim_nb_tpl_md ~", 
                                   paste(transfo_s, collapse = " + ")))
                  ,family = poisson(link = "log"), data = train_set, 
                  offset = offset_link,  method = "REML")



# Third : Nested model to check the best one



cv_poisson<- function(form, data, folds) {
  K <- length(folds)
  dev <- numeric(K)
  
  for (k in seq_along(folds)) {
    test_idx <- folds[[k]]
    train_idx <- setdiff(seq_len(nrow(data)), test_idx)
    
    train_k <- data[train_idx, ]
    test_k  <- data[test_idx, ]
    
    fit_k <- gam(
      as.formula(form),
      family   = poisson(link = "log"),
      data     = train_k,
      offset   = train_k$offset_link,
      method   = "REML"
    )
    
    mu_hat <- predict(fit_k, newdata = test_k, type = "response")
    y      <- test_k$claim_nb_tpl_md
    
    dev[k] <- 2 * sum(
      ifelse(y == 0, 0, y * log(y / mu_hat)) - (y - mu_hat)
    )
  }
  
  mean(dev)
}

compare_models <- function(formu_base, v, data,folds,cv_base ) {

   formu_new <- paste0(formu_base, "+", transfo(v, data))
  cv_new <- cv_poisson(form = formu_new,  data = data, folds = folds )
  
    gain_rel <- (cv_base - cv_new) / cv_base
  
  list(
    variable = v,  
    formu_new = formu_new,
        gain_rel= gain_rel,
    cv_new=cv_new
      )
}

idx_top3 <- order(results$p_value)[1:3]
base_var <- subset(results[idx_top3, ])$variable
base_var
#"cont_seniority" "driv_m_age" "geo_munty_fr"  

set.seed(123)
folds <- createFolds(train_set$claim_nb_tpl_md, k = 5, list = TRUE)

transfo_base_nest<- sapply(base_var, transfo, data = train_set)
formu_base_nest<-paste0("claim_nb_tpl_md ~",  paste(transfo_base, collapse = " + "))
cv_base_nest <- cv_poisson(formu_base_nest, data = train_set,  folds)
cv_base_nest

nest_var <- base_var
remain_vars <- vars_s[!vars_s %in% c("cont_seniority", "driv_m_age","geo_munty_fr")]


for (var in sample(remain_vars)) {
    result_nest <- compare_models(formu_base_nest,var, train_set_folds,cv_base_nest)
    cat("Var : ", var, "\n",
      "  gain_rel  = ", round(result_nest$gain_rel, 6), "\n"
    )
    #Gain de deviance sup à 1%
    if (result_nest$gain_rel > 0.001) {
    nest_var <- c(nest_var, var) 
    formu_base_nest<- result_nest$formu_new
    cv_base_nest <- result_nest$cv_new  
    cat(" >>> Variable retenue : ", var, "\n")
      } 
}

nest_var
nested_model <- gam(formu_base_nest,family = poisson(link = "log"), data = train_set, 
                     offset = offset_link,  method = "REML")



# Fourth : CV with bivariate interraction 



interactions_to_test <- c(
  "ti(cont_seniority, driv_m_age)",
  "ti(cont_seniority, veh_age)",
  
  "ti(driv_m_age, veh_age)",

  "s(cont_seniority, by = geo_munty_fr)",
  "s(veh_age , by = geo_munty_fr)",
  "s(driv_m_age, by = geo_munty_fr)",
  
  "s(cont_seniority, by = veh_use)",
  "s(veh_age , by = veh_use)",
  "s(driv_m_age , by = veh_use)"
)


test_interac_cv <- function(interac, form, data, folds,cv_base) {
  
  formu <- paste0(form, "+", interac)
  
  cv_new <- cv_poisson(form = formu,  data = data, folds = folds )
 
  gain_rel <- (cv_base - cv_new) / cv_base
  
   list(
    interaction = interac,
    formu=formu,
    cv_new= cv_new,
    gain_rel = gain_rel
  )
}

#Initia
base_model  
base_formula <- paste0(deparse(formula(base_model)), collapse = " ")

cv_base <- cv_poisson(base_formula, data = train_set,  folds)
cv_base

cv_results <- do.call(rbind,lapply(interactions_to_test,test_interac_cv,
 base_formula,data= train_set,folds = folds )
)

interac_s<-c()

for (interac in sample(interactions_to_test)) {
  result_interac <- test_interac_cv(interac, base_formula,train_set, folds,cv_base )
  cat("Interaction : ", interac, "\n",
      "  Gain de deviance= ", result_interac$gain_rel, "\n"
  )
  #Only gains de deviance sup à 1%
  if (result_interac$gain_rel > 0.001) {
    interac_s <- c(interac_s, interac) 
    base_formula <- result_interac$formu  
    cat(" >>> Intéraction retenue : ", interac, "\n")
  } 
}

base_formula <- paste0(deparse(formula(base_model)), collapse = " ")
form_interac <- paste0(base_formula, "+", interac_s)
interac_model <- gam(form,family = poisson(link = "log"), data = train_set, 
                  offset = offset_link,  method = "REML")



# Fifth: CV with others Binomial negativ or quasipoisson 



check_overdispersion <- function(model) {
  #pearson_residuals = (y_i - μ_i) / √(V(μ_i))
  residual_df <- df.residual(model)
  #pearson_chisq = Σ r_i² = Σ [(y_i - μ_i)² / μ_i]
  #pearson_chisq ~ χ²(n - p)
  pearson_chisq <- sum(residuals(model, type = "pearson")^2)
  #dispersion = pearson_chisq / (n - p)
  dispersion <- pearson_chisq / residual_df
  p_value <- pchisq(pearson_chisq, residual_df, lower.tail = FALSE)
  #H₀: dispersion = 1 (Poisson approprié)
  #H₁: dispersion ≠ 1 (sur/sous-dispersion)
  #p_value = P(χ²(n-p) > pearson_chisq)
  return(list(dispersion = dispersion, p_value = p_value))
}

check_overdispersion(fit_all)
check_overdispersion(nested_model)
check_overdispersion(signi_model)
check_overdispersion(interac_model)
#Forte surdispersion

cv_family <- function(form, data, folds, family) {
  K <- length(folds)
  dev <- numeric(K)
  
  for (k in seq_along(folds)) {
    test_idx <- folds[[k]]
    train_idx <- setdiff(seq_len(nrow(data)), test_idx)
    
    train_k <- data[train_idx, ]
    test_k  <- data[test_idx, ]
    
    fit_k <- gam(
      as.formula(form),
      family   = family,
      data     = train_k,
      offset   = train_k$offset_link,
      method   = "REML"
    )
    
    mu_hat <- predict(fit_k, newdata = test_k, type = "response")
    y  <- test_k$claim_nb_tpl_md
    
    # Deviance classique
    if (family$family == "poisson") {
      dev[k] <- 2 * sum(ifelse(y == 0, 0, y * log(y / mu_hat)) - (y - mu_hat))
    } else if (family$family == "quasipoisson") {
      # Quasi Poisson  déviance
      dev[k] <- 2 * sum(ifelse(y == 0, 0, y * log(y / mu_hat)) - (y - mu_hat))
    } else if (family$family == "nb") {
      # Déviance binomiale négative
      theta <- fit_k$family$getTheta(TRUE)  # paramètre de dispersion
      dev[k] <- 2 * sum(lgamma(y + theta) - lgamma(theta) - lgamma(y + 1) +
      theta * log(theta) + y * log(mu_hat) -(theta + y) * log(theta + mu_hat)
      )
    }
  }
  
  mean(dev)
}

# Fonction pour tester toutes les familles sur un modèle donné
test_families_cv <- function(model_formula, model_name, data, folds) {
  families <- list(
    poisson = poisson(link = "log"),
    quasipoisson = quasipoisson(link = "log"),
    negbin = nb(link = "log")
  )
  
  results <- list()
  
  for (fam in names(families)) {
    cv_score <- cv_family(form = model_formula,data = data,
      folds = folds,  family = families[[fam]]
    )
    
    results[[fam]] <- data.frame(
      model = model_name,
      family = fam,
      cv_deviance = cv_score,
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, results)
}

# All va modele
formula_all <- paste0("claim_nb_tpl_md ~", paste(transfo_var_all, collapse = " + "))
results_all <- test_families_cv(formula_all, "all_variables", train_set, folds)

# Significative sup intercept
formula_signi <- paste("claim_nb_tpl_md ~", paste(transfo_s, collapse = " + "))
results_signi <- test_families_cv(formula_signi, "significant_vars", train_set, folds)

# Nested modele
results_nest <- test_families_cv(formu_base_nest, "base_vars", train_set, folds)

# Interac modele
results_inter <- test_families_cv(form_interac, "with_interactions", train_set, folds)

# Combiner tous les résultats
all_results <- rbind(results_all, results_signi, results_nest, results_inter)

# Afficher les résultats triés par meilleure déviance
all_results <- all_results[order(all_results$cv_deviance), ]
print(all_results)

# Visualisation comparative
library(ggplot2)
ggplot(all_results, aes(x = model, y = cv_deviance, fill = family)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparaison des familles par validation croisée",
       x = "Modèle", y = "Déviance CV") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

AIC(m_pois)
AIC(m_nb)
pred_pois <- predict(m_pois, newdata = val_set, type="response")
RMSE_pois <- sqrt(mean((val_set$claim_nb_tpl_md - pred)^2))
pred_nb <- predict(m_nb, newdata = val_set, type="response")
RMSE_nb <- sqrt(mean((val_set$claim_nb_tpl_md - pred)^2))


# Terminal: Validation on the validation set & compute loss_function


validation_predictions <- predict(gam_model, newdata = val_set, type = "response")
validation_predictions

total_loss_validation <- 0
for (i in length(validation_predictions)) {
  loss <- 2*(val_set$Tot_claim[i]*log(val_set$Tot_claim[i]/validation_predictions[i])-(Val_set$Tot_claim[i]-validation_predictions[i]))
  total_loss_validation = total_loss_validation + loss
}
total_loss_validation = total_loss_validation/length(validation_set$claim_nb_tpl_md)
total_loss_validation


##################################################################################################################################
#Random Forests
install.packages("randomForest")
install.packages("rfCountData")
install.packages("ipred")
install.packages("doParallel")
library(rpart)
library(rpart.plot)
library(randomForest)
library(rfCountData)
library(ipred)
library(doParallel)
library(foreach)
library(iterators)

if (!require(devtools)) install.packages("devtools")
require(devtools)
install_github("fpechon/rfCountData")

#Compute loss function
loss_function <- function(test_set, prediction, var1) {
  total_loss <- 0
  for (i in length(prediction)) {
    loss <- 2*(test_set$var1[i]*log(test_set$var1[i]/prediction[i])-(test_set$var1[i]-prediction[i]))
    total_loss <- total_loss + loss
  }
  total_loss <- total_loss/length(test_set$var1)
}

#do a regression tree

tree <- rpart(Tot_claim ~ veh_age+veh_power+veh_value+veh_power+veh_weight+veh_use+veh_years_claim_free+cont_seniority+driv_m_age+veh_type+veh_use+
                offset(log(admi_risk_exposure)),data=train_set,
              method = "poisson",control = rpart.control(cp=0, minbucket = 5000))
prp(tree)
printcp(tree)
plotcp(tree, minline = TRUE, upper = "split", ylim = c(0.975, 1.01))

tree.minCV <- prune(tree, cp=0.00013522)
print(tree.minCV)
prp(tree.minCV)

#RF

set.seed(87)
folds = createFolds(train_set$Tot_claim, k = 5, list = TRUE)
grid.param = expand.grid(fold = 1:5,
                         mtry. = seq(from = 21, to = 1, by = -1),
                         nodesize. = c(500,1000))
cl <- makeCluster(12) # Number of nodes for parallel computing
registerDoParallel(cl)
#clusterCall(cl, function() library(rfCountData)) #Export package to nodes
set.seed(64)
res = foreach(i =1:nrow(grid.param)) %dopar% {
  X=folds[[grid.param[i,]$fold]] #Current fold (-> test set)
  rfPoisson(x = train_set[-X,!names(train_set) %in% c("Tot_claim", "admi_risk_exposure", "claim_nb_tpl_md","claim_nb_tpl_bi","id","driv_y_add:flg")],
            offset = log(train_set[-X,]$admi_risk_exposure),
            y = train_set[-X,]$Tot_claim,
            xtest = train_set[X,!names(train_set) %in% c("Tot_claim", "admi_risk_exposure", "claim_nb_tpl_md","claim_nb_tpl_bi","id","driv_y_add:flg")],
            offsettest = log(train_set[X,]$admi_risk_exposure),
            ytest = train_set[X,]$Tot_claim,
            ntree = 2000,
            mtry = grid.param[i,]$mtry., # Current mtry
            nodesize = grid.param[i,]$nodesize., # Current nodesize
            keep.forest = TRUE)
}
