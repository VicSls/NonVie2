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
library("doParallel")

#Prepare the data
data<-ulb_data
game_data =data$purpose

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



#First: gam will all var



transfo_var_all <- sapply(vars_all, transfo, data = train_set)
all_model <- gam(as.formula(paste("claim_nb_tpl_md ~", 
                                paste(transfo_var_all, collapse = " + ")))
               ,family = poisson(link = "log"), data = train_set, 
               offset = offset, method = "REML")
#figure ?



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



# Third : Nested model to check the best one



cv_poisson<- function(form, data, folds,n_cores = detectCores() - 1) {
  K <- length(folds)
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  dev <- foreach(k = seq_along(folds), .combine = c, .packages = "mgcv") %dopar% {
    test_idx <- folds[[k]]
    train_idx <- setdiff(seq_len(nrow(data)), test_idx)
    
    train_k <- data[train_idx, ]
    test_k  <- data[test_idx, ]
    
    fit_k <- mgcv::gam(
      as.formula(form),
      family   = poisson(link = "log"),
      data     = train_k,
      offset   = train_k$offset_link,
      method   = "REML"
    )
    
    û_i <- predict(fit_k, newdata = data[test_idx, ], type = "response")
    y_i <-data$claim_nb_tpl_md[test_idx]
#D = 2 × [log-vraisemblance(modèle saturé) - log-vraisemblance(votre modèle)]

    2 * sum(
      ifelse(y_i == 0, 0, y_i * log(y_i / û_i)) - (y_i - û_i)
   #Si y_i = 0 : deviance_i = 2 × [0 - (0 - μ_i)] = 2 × μ_i
    #Si y_i > 0 : deviance_i = 2 × [y_i × log(y_i / μ_i) - (y_i - μ_i)]
       )
  }
  
  stopCluster(cl)
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
formu_base_nest<-paste0("claim_nb_tpl_md ~",  paste(transfo_base_nest, collapse = " + "))
cv_base_nest <- cv_poisson(formu_base_nest, data = train_set,  folds)
cv_base_nest

nest_var <- base_var
#choisir une base forte au lieu d'une barrière forte à estimer
remain_vars <- vars_s[!vars_s %in% c("cont_seniority", "driv_m_age","geo_munty_fr")]
 
set.seed(123)
for (var in sample(remain_vars)) {
    result_nest <- compare_models(formu_base_nest,var,train_set, folds,cv_base_nest)
    cat("Var : ", var, "\n",
      "  gain_rel  = ", round(result_nest$gain_rel, 6), "\n"
    )
    if (result_nest$gain_rel > 0) {
    nest_var <- c(nest_var, var) 
    formu_base_nest<- result_nest$formu_new
    cv_base_nest <- result_nest$cv_new  
    cat("IN ","\n",
        "--------------------------------------------------------------","\n")
    }else{
        cat("OUT","\n",
            "--------------------------------------------------------------","\n")
      } 
}
#Attention aux limitations :
#Forward stepwise peut mener à des optimums locaux
#Pas de garantie d'obtenir le meilleur sous-ensemble de variables
#Risque d'inclure des variables marginalement utiles

nest_var
#nest_var<-c("cont_seniority", "driv_m_age","geo_munty_fr","veh_fuel","geo_region_fr", "veh_type", "veh_use")
transfo_nest<- sapply(nest_var, transfo, data = train_set)
form_nest_pois<-paste0("claim_nb_tpl_md ~",  paste(transfo_nest, collapse = " + "))
nested_pois <- gam(as.formula(form_nest_pois),family = poisson(link = "log"), 
                    data = train_set, offset = offset_link,  method = "REML")
#"claim_nb_tpl_md ~s(cont_seniority) + s(driv_m_age) + geo_munty_fr 
#+ veh_fuel + geo_region_fr + veh_type + veh_use"


# Fourth : CV with bivariate interraction 



interactions_to_test <- c(
  #Interaction complex non linéaire
  
  "ti(cont_seniority, driv_m_age)",
  
  #Interaction simple linéiare, sinon bug
  
  "cont_seniority:geo_munty_fr",
  "driv_m_age:geo_munty_fr",

  "cont_seniority:geo_region_fr ",
  "driv_m_age:geo_region_fr ",

  "cont_seniority:veh_fuel",
  "driv_m_age:veh_fuel",

  "cont_seniority:veh_type",
  "driv_m_age:veh_type",
  
  "cont_seniority:veh_use",
  "driv_m_age:veh_use",

    "geo_munty_fr:geo_region_fr ",
  "geo_munty_fr:veh_fuel",
  "geo_munty_fr:veh_use",
  "geo_munty_fr:veh_type",
  
  "veh_use:veh_fuel",
  "veh_use:veh_type",
  "veh_use:geo_region_fr",
  
  "veh_type:geo_region_fr",
  "veh_type:veh_fuel",
  
  "geo_region_fr:veh_fuel"
  
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
base_formula <- form_nest_pois
cv_base <-cv_base_nest
#4485.29
tot_ite<-as.numeric(length(interactions_to_test))
interac_s<-c()

set.seed(123)
for (interac in sample(interactions_to_test)) {
  tot_ite<-tot_ite-1
  cat("Interaction : ", interac, "\n",
      "Nombre de test restant",tot_ite,"\n")
  result_interac <- test_interac_cv(interac, base_formula,train_set, folds,cv_base )
    cat("  Gain de deviance= ", result_interac$gain_rel, "\n" )
  if (result_interac$gain_rel > 0) {
    interac_s <- c(interac_s, interac) 
    base_formula <- result_interac$formu  
    cat(" IN", "\n",
        "--------------------------------------------------------------","\n")
  } else{
    cat("OUT","\n",
        "--------------------------------------------------------------","\n")
  }
}

interac_s<-paste0(interac_s, collapse = " + ")
#interac_s<-"veh_type:geo_region_fr + cont_seniority:geo_region_fr  + geo_region_fr:veh_fuel + driv_m_age:veh_type + veh_use:geo_region_fr + geo_munty_fr:geo_region_fr  + veh_use:veh_type"

form_interac_pois <- paste0(form_nest_pois, "+", interac_s)
#form_interac_pois<-"claim_nb_tpl_md ~s(cont_seniority) + s(driv_m_age) + geo_munty_fr + veh_fuel + geo_region_fr + veh_type + veh_use+veh_type:geo_region_fr + cont_seniority:geo_region_fr  + geo_region_fr:veh_fuel + driv_m_age:veh_type + veh_use:geo_region_fr + geo_munty_fr:geo_region_fr  + veh_use:veh_type"
#méthode REML: restricted !
interac_pois <- gam(as.formula(form_interac_pois),family = poisson(link = "log"), data = train_set, 
                  offset = offset_link,  method = "REML")


# Fifth: CV with others Binomial negativ or quasipoisson
#which law would suit better the data ? 




check_overdispersion <- function(model) {
  #pearson_residuals = (y_i - μ_i) / √(V(μ_i))
  residual_df <- df.residual(model)
  
  #pearson_chisq = Σ r_i² = Σ [(y_i - μ_i)² / μ_i] //pearson_chisq ~ χ²(n - p)
  pearson_chisq <- sum(residuals(model, type = "pearson")^2)
  
  #dispersion = pearson_chisq / (n - p)
  dispersion <- pearson_chisq / residual_df
  
  p_value <- pchisq(pearson_chisq, residual_df, lower.tail = FALSE)
  #H0: dispersion = 1 (Poisson approprié)
  #H1: dispersion ≠ 1 (sur/sous-dispersion)
  #p_value = P(χ²(n-p) > pearson_chisq)
  return(list(dispersion = dispersion, p_value = p_value))
}

check_overdispersion(nested_pois)
check_overdispersion(interac_pois)
#Forte surdispersion


#Pas restester nested and interac test avec negbin et quasipoisson acr prend trop de temps

nested_nb <- gam(as.formula(form_nest_pois),family = nb(link = "log"), 
                     data = train_set, offset = offset_link,method = "REML")
theta_nested<-nested_nb$family$getTheta(TRUE)
#Theta->10991.57, suggère pas de surdispersion 

interac_nb <- gam(as.formula(form_interac_pois),family = nb(link = "log"), 
                      data = train_set, offset = offset_link, method = "REML")
theta_interac <- interac_nb$family$getTheta(TRUE)

# Modèles quasi-Poisson
nested_quasi <- gam(as.formula(form_nest_pois),family = quasipoisson(link = "log"), 
                           data = train_set,offset = offset_link,method = "REML")

interac_quasi <- gam(as.formula(form_interac_pois),family = quasipoisson(link = "log"), 
                            data = train_set, offset = offset_link,   method = "REML")

phi_nested <- summary(nested_quasi)$dispersion
phi_interac <- summary(interac_quasi)$dispersion
install.packages("AER")
library("AER")
dispersiontest(nested_pois)
dispersiontest(nested_pois)

# Terminal: Validation on the validation set & compute loss_function



evaluate_model <- function(model, val_set, model_name) {
  
  # Prédictions val set
  predic_val  <- predict(model, newdata = val_set, type = "response")
  y_val  <- val_set$claim_nb_tpl_md
  
  # % d'amélioration par rapport intercept
  dev_train <- model$deviance
  null_deviance <- model$null.deviance
  deviance_explained <- (null_deviance - dev_train) / null_deviance
  
  # Les résidus de DEVIANCE sont plus utiles pour :
  # - Diagnostic de l'ajustement du modèle
  # - Vérification des hypothèses de distribution
  # - Ils suivent approximativement une distribution normale si le modèle est correct
  deviance_residuals <- residuals(model, type = "deviance")
  
  # Les résidus de PEARSON sont plus utiles pour :
  # - Détection de la surdispersion
  # - Tests d'adéquation (chi²)
  # - Points influents
  pearson_residuals <- residuals(model, type = "pearson")
  
    # Déviance sur validation : généralisation du modèle
  #  Si ≫ déviance entraînement : overfitting
  #  Si ∼ déviance entraînement : bon équilibre
    #bonne déviance même avec binomial négative ? Loss fonction?
   if (model$family$family == "Negative Binomial") {
      term1 <- ifelse(y_val == 0, 0, y_val * log(y_val / predic_val))
    term2 <- (y_val + theta) * log((y_val + theta) / (predic_val + theta))
    dev_val<- 2 * sum(term1 - term2)
    
  } else {
    # Déviance Poisson & quasipoisson
    dev_val<-2 * sum(ifelse(y_val == 0, 0, y_val * log(y_val / predic_val)) - (y_val - predic_val))
  } 
  
  #mesure d'erreur
  mse <- mean((y_val - predic_val)^2)
  #mesure de modèle
  aic_value<-AIC(model)

  # Intervalle de confiance sur l'échelle linéaire
  pred_IC <- predict(model, newdata = val_set,
                          type = "link", se.fit = TRUE)
  # Transformation vers l'échelle de réponse
    lower_IC <- exp(pred_IC$fit - 1.96 * pred_IC$se.fit)
  upper_IC <- exp(pred_IC$fit + 1.96 * pred_IC$se.fit)
  #Idéalement ~95% si le modèle est bien calibré
  coverage <- mean(y_val >= lower_IC & y_val <= upper_IC) * 100
  
  # H0: Le modèle est bien adapté aux données
  # p-value > 0.05 : pas de preuve contre H0 → modèle adéquat
  # p-value < 0.05 : modèle potentiellement mal adapté
  chi2_stat <- sum(pearson_residuals^2)
  chi2_pvalue <- pchisq(chi2_stat, df.residual(model), lower.tail = FALSE)
  
  return(list(
    model_name = model_name,
    dev_train = dev_train,
    dev_explained = deviance_explained,
    dev_val = dev_val,
    aic = aic_value,
    mse = mse,
    coverage_ci = coverage,
    chi2_pvalue = chi2_pvalue,
    deviance_resid_summary = summary(deviance_residuals),
    pearson_resid_summary = summary(pearson_residuals)
  ))
}

# Évaluation de tous les modèles
models_to_evaluate <- list()

models_to_evaluate[["nested_pois"]] <- nested_pois
models_to_evaluate[["interac_pois"]] <- interac_pois

models_to_evaluate[["nested_nb"]] <- nested_nb
models_to_evaluate[["interac_nb"]] <- interac_nb

models_to_evaluate[["nested_quasi"]] <- nested_quasi
models_to_evaluate[["interac_quasi"]] <- interac_quasi

# Évaluation complète sur le jeu de validation
results_validation <- list()

for (model_name in names(models_to_evaluate)) {
  cat("Évaluation du modèle:", model_name, "\n")
  results_validation[[model_name]] <- evaluate_model(
    models_to_evaluate[[model_name]], 
    val_set, 
    model_name
  )
}
r<-as.data.frame(do.call(rbind, results_validation))

library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyr)

# Créer un dataframe propre à partir des résultats
create_results_df <- function(results_validation) {
  # Extraire les métriques principales
  main_metrics <- do.call(rbind, lapply(results_validation, function(x) {
    data.frame(
      model = x$model_name,
      dev_explained = x$dev_explained * 100,  # Convertir en pourcentage
      dev_val = x$dev_val,
      coverage = x$coverage_ci,
      chi2_pvalue = x$chi2_pvalue,
      stringsAsFactors = FALSE
    )
  }))
  
  # Extraire les résidus de Pearson (summary -> valeurs)
  deviance_resid <- do.call(rbind, lapply(names(results_validation), function(model_name) {
    x <- results_validation[[model_name]]
    if(!is.null(x$deviance_resid_summary)) {
      resid_values <- as.numeric(unlist(strsplit(
        gsub("[^0-9.,-]+", "", x$deviance_resid_summary), 
        split = ",")))
      data.frame(
        model = model_name,
        resid_type = "Deviance",
        min = resid_values[1],
        q1 = resid_values[2],
        median = resid_values[3],
        q3 = resid_values[4],
        max = resid_values[5]
      )
    } else {
      NULL
    }
  }))
  
  list(main_metrics = main_metrics, deviance_resid = deviance_resid)
}

# Préparer les données
results_data <- create_results_df(results_validation)
main_df <- results_data$main_metrics
deviance_df <- results_data$deviance_resid

# Palette de couleurs cohérente
model_colors <- c(
  "nested_pois" = "#1f77b4",
  "interac_pois" = "#ff7f0e", 
  "nested_nb" = "#2ca02c",
  "interac_nb" = "#d62728",
  "nested_quasi" = "#9467bd",
  "interac_quasi" = "#8c564b"
)
plot_dev_explained <- ggplot(main_df, 
                             aes(x = reorder(model, dev_explained), 
                                 y = dev_explained,
                                 fill = model)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.3f%%", dev_explained)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = model_colors) +
  labs(
    title = "Déviance expliquée par le modèle",
    subtitle = "Pourcentage de déviance nulle expliquée\n(plus élevé = mieux)",
    x = "Modèle",
    y = "Déviance expliquée (%)",
    fill = "Modèle"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, max(main_df$dev_explained) * 1.15)

print(plot_dev_explained)

plot_dev_val <- ggplot(main_df, 
                       aes(x = reorder(model, dev_val), 
                           y = dev_val,
                           fill = model)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = format(round(dev_val, 1), big.mark = ",")),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = model_colors) +
  labs(
    title = "Déviance Poisson sur l'ensemble de validation",
    subtitle = "Mesure de l'erreur de prédiction\n(plus bas = mieux)",
    x = "Modèle",
    y = "Déviance",
    fill = "Modèle"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, max(main_df$dev_val) * 1.1)

print(plot_dev_val)

plot_coverage <- ggplot(main_df, 
                        aes(x = reorder(model, coverage), 
                            y = coverage,
                            fill = model)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", coverage)),
            vjust = -0.5, size = 4, fontface = "bold") +
  geom_hline(yintercept = 95, 
             linetype = "dashed", 
             color = "red", 
             size = 1.2,
             alpha = 0.7) +
  annotate("text", 
           x = 6, 
           y = 100, 
           label = "Valeur cible: 95%",
           color = "red",
           fontface = "bold",
           size = 4) +
  scale_fill_manual(values = model_colors) +
  labs(
    title = "Coverage des intervalles de confiance à 95%",
    subtitle = "Pourcentage des observations dans l'IC de prédiction\n(Idéalement ~95%)",
    x = "Modèle",
    y = "Coverage (%)",
    fill = "Modèle"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "none"
  ) +
  ylim(0, 105)

print(plot_coverage)
#Manque bcp de chose poour graphh up 

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



##################################################################################""
# NN

library(neuralnet)
library(caret)


# Pour un réseau de neurones, nous devons:
# 1. Gérer les variables continues (standardisation)
# 2. Gérer les variables catégorielles (encodage)


data_nn <- function(data, vars,target = "claim_nb_tpl_md") {
  
  df <- data[, c(target,"offset_link", vars,"admi_risk_exposure")]
  continuous_vars <- c()
  categorical_vars <- c()
  # Standardiser toutes les variables continues
  # Standardisation des variables continues (Z-score normalization)
  # FORMULE: (x - mean(x)) / sd(x)
  # POURQUOI? Pour que toutes les variables aient une échelle similaire
  # Cela aide la convergence lors de l'entraînement
  for(var in c(vars)) {
    if(is.numeric(df[[var]]) && length(unique(df[[var]])) > 10) {
      continuous_vars <- c(continuous_vars, var)
            df[[var]] <- as.numeric(scale(df[[var]]))
    }else{
      categorical_vars <- c(categorical_vars, var)
            df[[var]] <- as.factor(df[[var]])
    }  }

    # Convertir les facteurs en variables dummy
  #Crée des dummies pour les variables catégorielles (automatiquement)
  
  formula <- as.formula(paste("~", paste(categorical_vars, collapse = " + ")))
  mm <- model.matrix(formula, data = df)
  mm <- mm[, -1, drop = FALSE]

  cont_data <- as.matrix(df[, continuous_vars, drop = FALSE])
  final_data <- cbind(cont_data, mm)
  
  final_df <- as.data.frame(final_data)
  final_df$target <- df[[target]]
  final_df$exposure    <- pmax(df[["admi_risk_exposure"]], 1e-10)
  final_df$freq        <- final_df$target / final_df$exposure
  final_df$logexposure<-df[["offset_link"]]

  return(final_df)
  }

# Sélectionner un sous-ensemble de variables importantes
#Ajouter celle de RF // gbm ?
important_vars <- c("cont_seniority", "driv_m_age", "geo_munty_fr", 
                    "veh_fuel", "geo_region_fr", "veh_type", "veh_use")

Cdata_nn<-data_nn(Cdata,important_vars)
train_nn <- Cdata_nn[train_index, ]
val_nn <- Cdata_nn[-train_index, ]

#Test
target_train_original <- train_set$claim_nb_tpl_md 
target_val_original <- val_set$claim_nb_tpl_md

target_train_nn <- train_nn[, "target"]
target_val_nn <- val_nn[, "target"]

diff_train <- target_train_original - target_train_nn
diff_val <- target_val_original - target_val_nn
sum(diff_train)
sum(diff_val)
for (var in names(train_nn)) {
  cat(var, ": length =", length(train_nn[[var]]), 
      ", NA count =", sum(is.na(train_nn[[var]])), 
      ", Unique values =", length(unique(train_nn[[var]])),"\n")
}
for (var in names(train_nn)) {
  vals <- train_nn[[var]]
  
  cat(
    var, ":",
    "length =", length(vals),
    ", NA count =", sum(is.na(vals)),
    ", Infinite count =", sum(is.infinite(vals)),
    ", NaN count =", sum(is.nan(vals)),
    ", Non-finite total =", sum(!is.finite(vals)),
    ", Unique values =", length(unique(vals)),
    "\n"
  )
}



cv_nn_poisson <- function(data, hidden_architectures, folds,
                          n_cores = parallel::detectCores() - 1) {
  
  # Copie avec noms safe
  data_safe <- data
  names(data_safe) <- make.names(names(data_safe), unique = TRUE)
  
  predictor_names <- setdiff(
    names(data_safe),
    c("target", "logexposure", "freq", "exposure")
  )
  missing_global <- setdiff(predictor_names, names(data_safe))
  if (length(missing_global)) stop("Colonnes manquantes globalement: ", paste(missing_global, collapse=", "))
  
  
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  
  results_list <- list()
  
  for (arch_idx in seq_along(hidden_architectures)) {
    hidden    <- hidden_architectures[[arch_idx]]
    arch_name <- paste("Arch", arch_idx, ":", paste(hidden, collapse = "-"))
    cat("\nTesting architecture:", hidden, "\n")
    
    dev <- foreach::foreach(
      k = seq_along(folds),
      .combine  = c,
      .packages = c("neuralnet"),
      .export   = c("predictor_names"),   
      .noexport = character(0)
    ) %dopar% {
      set.seed(123)
      test_idx  <- folds[[k]]
      train_idx <- setdiff(seq_len(nrow(data_safe)), test_idx)
      
      train_k <- data_safe[train_idx, , drop = FALSE]
      test_k  <- data_safe[test_idx,  , drop = FALSE]
      
      tryCatch({
        formula_nn <- reformulate(
          termlabels = predictor_names,
          response   = "freq"
        )
        
        nn_model <-try(neuralnet::neuralnet(
          formula       = formula_nn,
          data          = train_k,
          hidden        = hidden,
          linear.output = TRUE,
          err.fct       = "sse",
          act.fct       = "logistic",
          algorithm     = "rprop+",
          rep           = 1,
          stepmax       = 2e7,
          lifesign      = "none",
          threshold     = 0.05 
        ), silent = TRUE)
        if (inherits(nn_model, "try-error") || is.null(nn_model$weights)) return(NA_real_)
        
        preds <- try(neuralnet::compute(
          nn_model, test_k[, predictor_names, drop = FALSE]
        ), silent = TRUE)
        if (inherits(preds, "try-error") || is.null(preds$net.result)) return(NA_real_)
        
        freq_hat <- as.numeric(preds$net.result)
        
        mu_hat <- freq_hat * test_k$exposure
        mu_hat <- pmax(mu_hat, 1e-10)
        y_i    <- test_k$target
        
        deviance <- 2 * sum(
          ifelse(y_i == 0, 0, y_i * log(y_i / mu_hat)) - (y_i - mu_hat)
        )
        
        deviance
        
        
      }, error = function(e) {
        message(sprintf("[PAR ERROR] %s fold %d: %s", arch_name, k, conditionMessage(e)))
        cat("[PAR ERROR] Arch", arch_name, "fold", k, ":", conditionMessage(e), "\n")
        return(NA_real_)
      })
    }
    
    results_list[[arch_name]] <- list(
      architecture  = hidden,
      mean_deviance = mean(dev, na.rm = TRUE),
      sd_deviance   = stats::sd(dev,  na.rm = TRUE),
      failed_folds  = sum(is.na(dev)),
      all_deviances = dev
    )
    
    cat("  Mean deviance:", mean(dev, na.rm = TRUE),
        " | failed folds:", sum(is.na(dev)), "/", length(dev), "\n")  }
  
  parallel::stopCluster(cl)
  
  results_df <- data.frame(
    architecture  = sapply(results_list, function(x) paste(x$architecture, collapse = "-")),
    mean_deviance = sapply(results_list, function(x) x$mean_deviance),
    sd_deviance   = sapply(results_list, function(x) x$sd_deviance),
    row.names     = names(results_list),
    stringsAsFactors = FALSE
  )
  
  return(list(results = results_df, details = results_list))
}

# Test with just one fold and simple architecture
test_folds <- list(1:1000)  # Use only first 1000 observations
test_arch <- list(c(3))


# If that works, test parallel version with same data
cv_test <- cv_nn_poisson(train_nn[1:2000, ], c(1), folds, n_cores = 2)
architectures <- list(
  c(1),           c(2),         
  c(3),        
  c(4)      # 2 couches: 15 puis 8 neurones
)
cv_results <- cv_nn_poisson( train_nn, architectures,  folds )
best_idx <- which.min(cv_results$results$mean_deviance)
best_arch <- architectures[[best_idx]]
# Fonction pour entraîner le modèle final
train_final_nn_model <- function(train_data, val_data, hidden_architecture, 
                                 learningrate = 0.01, epochs = 1000) {
  
  predictor_names <- setdiff(names(train_data), c("target", "exposure", "freq"))
  formula_nn <- as.formula(paste("freq ~", paste(predictor_names, collapse = " + ")))
  
  # Entraîner le modèle final
  final_model <- neuralnet(
    formula = formula_nn,
    data = train_data,
    hidden = hidden_architecture,
    linear.output = TRUE,
    err.fct = "sse",
    act.fct = "logistic",
    algorithm = "rprop+",  # Rprop+ pour la stabilité
    learningrate = learningrate,
    rep = 3,  # Plusieurs répétitions
    stepmax = epochs,
    lifesign = "full"
  )
  
  # Prédictions sur validation
  val_predictions <- compute(final_model, val_data[, predictor_names, drop = FALSE])
  freq_pred_val <- as.numeric(val_predictions$net.result)
  freq_pred_val[freq_pred_val < 0] <- 1e-10
  
  # Calculer la déviance sur validation
  mu_pred_val <- freq_pred_val * val_data$exposure
  y_obs_val <- val_data$target
  
  deviance_val <- 2 * sum(
    ifelse(y_obs_val == 0, 0, y_obs_val * log(y_obs_val / mu_pred_val)) - 
      (y_obs_val - mu_pred_val)
  )
  
  cat("Déviance sur ensemble de validation:", deviance_val, "\n")
  
  return(list(
    model = final_model,
    validation_deviance = deviance_val,
    predictions = freq_pred_val
  ))
}

# EXEMPLE D'UTILISATION



# 2. Définir les architectures à tester
# Une couche cachée
one_layer_archs <- list(
  c(3),   # 3 neurones
  c(5),   # 5 neurones
  c(7),   # 7 neurones
  c(10),  # 10 neurones
  c(15),  # 15 neurones
  c(20)   # 20 neurones
)

# Deux couches cachées
two_layer_archs <- list(
  c(5, 3),    # 5 puis 3 neurones
  c(7, 4),    # 7 puis 4 neurones
  c(10, 5),   # 10 puis 5 neurones
  c(15, 8),   # 15 puis 8 neurones
  c(20, 10),  # 20 puis 10 neurones
  c(7, 7)     # 7 puis 7 neurones
)

# Combiner toutes les architectures
all_architectures <- c(one_layer_archs, two_layer_archs)

# 3. Exécuter la validation croisée
set.seed(123)
best_results <- find_best_nn_architecture(
  data = Cdata_nn[train_index, ],
  hidden_architectures = all_architectures,
  n_folds = 5,
  n_cores = detectCores() - 1
)

# 4. Entraîner le modèle final avec la meilleure architecture
final_model_result <- train_final_nn_model(
  train_data = Cdata_nn[train_index, ],
  val_data = Cdata_nn[-train_index, ],
  hidden_architecture = best_results$best_architecture,
  learningrate = 0.01,
  epochs = 1000
)

# 5. Comparer avec un modèle de référence (Poisson GLM)
compare_with_poisson <- function(nn_result, data_nn, train_idx, val_idx) {
  # Préparer les données pour GLM
  predictor_names <- setdiff(names(data_nn), c("target", "exposure", "freq"))
  
  # Poisson GLM
  poisson_formula <- as.formula(
    paste("target ~", paste(predictor_names, collapse = " + "), 
          "+ offset(log(exposure))")
  )
  
  poisson_model <- glm(poisson_formula, 
                       data = data_nn[train_idx, ],
                       family = poisson(link = "log"))
  
  # Prédictions Poisson
  poisson_pred <- predict(poisson_model, 
                          newdata = data_nn[val_idx, ],
                          type = "response")
  
  # Déviance Poisson
  y_obs <- data_nn$target[val_idx]
  poisson_deviance <- 2 * sum(
    ifelse(y_obs == 0, 0, y_obs * log(y_obs / poisson_pred)) - 
      (y_obs - poisson_pred)
  )
  
  cat("\n=== COMPARAISON ===\n")
  cat("Réseau de neurones - Déviance:", nn_result$validation_deviance, "\n")
  cat("Poisson GLM - Déviance:", poisson_deviance, "\n")
  cat("Différence:", nn_result$validation_deviance - poisson_deviance, "\n")
  
  return(list(
    poisson_model = poisson_model,
    poisson_deviance = poisson_deviance,
    comparison = data.frame(
      model = c("Neural Network", "Poisson GLM"),
      deviance = c(nn_result$validation_deviance, poisson_deviance)
    )
  ))
}

# Exécuter la comparaison
comparison <- compare_with_poisson(
  final_model_result,
  Cdata_nn,
  train_index,
  setdiff(1:nrow(Cdata_nn), train_index)
)


cv_nn_poisson_seq <- function(data, hidden_architectures, folds) {
  data_safe <- data
  names(data_safe) <- make.names(names(data_safe), unique = TRUE)
  
  predictor_names <- setdiff(
    names(data_safe),
    c("target", "logexposure", "freq", "exposure")
  )
  
  results_list <- list()
  
  for (arch_idx in seq_along(hidden_architectures)) {
    hidden    <- hidden_architectures[[arch_idx]]
    arch_name <- paste("Arch", arch_idx, ":", paste(hidden, collapse = "-"))
    cat("\n[SEQ] Testing architecture:", arch_name, "\n")
    
    dev <- c()
    
    for (k in seq_along(folds)) {
      cat("[SEQ]  Fold", k, "\n")
      
      test_idx  <- folds[[k]]
      train_idx <- setdiff(seq_len(nrow(data_safe)), test_idx)
      
      train_k <- data_safe[train_idx, ]
      test_k  <- data_safe[test_idx, ]
      
      formula_nn <- reformulate(
        termlabels = predictor_names,
        response   = "freq"
      )
      
      cat("[SEQ]   Fitting neural net...\n")
      nn_model <- neuralnet::neuralnet(
        formula       = formula_nn,
        data          = train_k,
        hidden        = hidden,
        linear.output = TRUE,
        err.fct       = "sse",
        act.fct       = "logistic",
        algorithm     = "rprop+",
        rep           = 3,
        stepmax       = 1e5,
        lifesign      = "none"
      )
      
      cat("[SEQ]   Predicting...\n")
      preds <- neuralnet::compute(
        nn_model,
        test_k[, predictor_names, drop = FALSE]
      )
      freq_hat <- as.numeric(preds$net.result)
      
      mu_hat <- freq_hat * test_k$exposure
      mu_hat <- pmax(mu_hat, 1e-10)
      y_i    <- test_k$target
      
      deviance <- 2 * sum(
        ifelse(y_i == 0, 0, y_i * log(y_i / mu_hat)) - (y_i - mu_hat)
      )
      
      dev <- c(dev, deviance)
    }
    
    results_list[[arch_name]] <- list(
      architecture  = hidden,
      mean_deviance = mean(dev),
      sd_deviance   = stats::sd(dev),
      all_deviances = dev
    )
    
    cat("[SEQ]  Mean deviance:", mean(dev), "\n")
  }
  
  results_df <- data.frame(
    architecture  = sapply(results_list, function(x) paste(x$architecture, collapse = "-")),
    mean_deviance = sapply(results_list, function(x) x$mean_deviance),
    sd_deviance   = sapply(results_list, function(x) x$sd_deviance),
    row.names     = names(results_list),
    stringsAsFactors = FALSE
  )
  
  list(results = results_df, details = results_list)
}
cv_seq <- cv_nn_poisson_seq(train_nn, list(c(5)), folds)

