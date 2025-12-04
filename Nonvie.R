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
#"cont_seniority" "driv_m_age"     "geo_munty_fr"   "veh_fuel"      
#"geo_region_fr"  "veh_type"       "veh_use"  
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
#"veh_type:geo_region_fr + cont_seniority:geo_region_fr  + 
#geo_region_fr:veh_fuel + driv_m_age:veh_type + veh_use:geo_region_fr 
#+ geo_munty_fr:geo_region_fr  + veh_use:veh_type"

form_interac_pois <- paste0(form_nest_pois, "+", interac_s)
#méthode REML: restricted 
interac_pois <- gam(as.formula(form_interac_pois),family = poisson(link = "log"), data = train_set, 
                  offset = offset_link,  method = "REML")

#s(cont_seniority) + s(driv_m_age) + geo_postcode_2digits + 
#veh_fuel + veh_type + veh_use + veh_type:geo_region_fr + 
#  cont_seniority:geo_region_fr + geo_region_fr:veh_fuel + driv_m_age:veh_type + 
 # veh_use:geo_region_fr + geo_munty_fr:geo_region_fr + veh_use:veh_type

# Fifth: CV with others Binomial negativ or quasipoisson
#which law would suit better the data ? 
#the least CV deviance will be used,to do a nested model
#and interactions nested model



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



cv_family <- function(form, data, folds, family, n_cores = detectCores() - 1) {
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
      family   = family,
      data     = train_k,
      offset   = train_k$offset_link,
      method   = "REML"
    )
    
    û_i <- predict(fit_k, newdata = test_k, type = "response")
    y_i <- test_k$claim_nb_tpl_md
    
    if (family$family == "Negative Binomial"|| family$family == "negbin") {
      # Récupération de dispersion  theta estimé sur l'échantillon d'entraînement
      #proviens du gam dans le max de vraisemblance
      theta <- fit_k$family$getTheta(TRUE) 
      
      term1 <- ifelse(y_i == 0, 0, y_i * log(y_i / û_i))
      term2 <- (y_i + theta) * log((y_i + theta) / (û_i + theta))
      2 * sum(term1 - term2)
      
    } else {
      # Déviance Poisson &quasipoisson
      2 * sum(ifelse(y_i == 0, 0, y_i * log(y_i / û_i)) - (y_i - û_i))
    } 
  
  }
  stopCluster(cl)
  mean(dev)
}

# Fonction pour tester toutes les familles sur un modèle donné
test_families_cv <- function(model_formula, model_name, data, folds) {
  families <- list(
    poisson = poisson(link = "log"),
    #la dispersion estimer avec les résidus de pearson, et la quasivraisemblance
    quasipoisson = quasipoisson(link = "log"),
    negbin = nb(link = "log")
  )
  
  results <- list()
  
  for (fam in names(families)) {
    cv_score <- cv_family(form = model_formula,data = data,
      folds = folds,  family = families[[fam]]
    )
    cat("Famille : ", fam, "\n",
        "  Déviance ", cv_score, "\n"    )
    
    results[[fam]] <- data.frame(
      model = model_name,
      family = fam,
      cv_deviance = cv_score,
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, results)
}


# Nested modele
results_nest_pois <- test_families_cv(form_nest_pois, "base_vars", train_set, folds)
#"claim_nb_tpl_md ~s(cont_seniority) + s(driv_m_age) + geo_postcode_2digits 
#+ veh_fuel + veh_type + veh_use"
#model       family cv_deviance
#poisson      base_vars      poisson    7482.517
#quasipoisson base_vars quasipoisson    7481.319
#negbin       base_vars       negbin    7482.518

# Interac modele
results_inter_pois <- test_families_cv(form_interac_pois, "with_interactions", train_set, folds)
#Famille :  poisson 
#Déviance  7482.672 
#Famille :  quasipoisson 
#Déviance  7483.096 
#Famille :  negbin 
#Déviance  7486.032 

# Combiner tous les résultats
all_results <- rbind(results_nest_pois, results_inter_pois)


# Visualisation comparative
library(ggplot2)
ggplot(all_results, aes(x = model, y = cv_deviance, fill = family)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparaison des familles par validation croisée",
       x = "Modèle", y = "Déviance CV") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



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
    theta <- model$family$getTheta(TRUE)
      term1 <- ifelse(y_val == 0, 0, y_val * log(y_val / predic_val))
    term2 <- (y_val + theta) * log((y_val + theta) / (predic_val + theta))
    dev_val<- 2 * sum(term1 - term2)
    
  } else {
    # Déviance Poisson & quasipoisson
    2 * sum(ifelse(y_val == 0, 0, y_val * log(y_val / predic_val)) - (y_val - predic_val))
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

models_to_evaluate[["nested_poisson"]] <- nested_model
models_to_evaluate[["interac_poisson"]] <- interac_model

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

# Création d'un tableau comparatif
comparison_table <- do.call(rbind, lapply(results_validation, function(x) {
  data.frame(
    Modèle = x$model_name,
    Déviance = x$deviance,
    Déviance_Expliquée = round(x$deviance_explained * 100, 2),
    Déviance_Validation = x$deviance_validation,
    AIC = round(x$aic, 1),
    Dispersion = round(x$dispersion, 3),
    P_value_Dispersion = ifelse(x$dispersion_pvalue < 0.001, "<0.001", 
                                round(x$dispersion_pvalue, 3)),
    MSE = round(x$mse, 4),
    MAE = round(x$mae, 4),
    Couverture_IC = round(x$coverage_ci, 1),
    P_value_Chi2 = ifelse(x$chi2_pvalue < 0.001, "<0.001", 
                          round(x$chi2_pvalue, 3)),
    stringsAsFactors = FALSE
  )
}))

comparison_table <- comparison_table[order(comparison_table$Déviance), ]

# Affichage des résultats
print(comparison_table)



par(mfrow = c(1, 2))
plot(fitted(model), residus_deviance, main = "Résidus Deviance")
abline(h = 0, col = "red")
plot(fitted(model), residus_pearson, main = "Résidus Pearson")
abline(h = 0, col = "red")

# Visualisation des résidus
par(mfrow = c(2, 2))
for (i in 1:min(4, length(models_to_evaluate))) {
  model_name <- names(models_to_evaluate)[i]
  model <- models_to_evaluate[[model_name]]
  
  # Résidus de déviance
  plot(fitted(model), residuals(model, type = "deviance"),
       main = paste("Résidus déviance -", model_name),
       xlab = "Valeurs prédites", ylab = "Résidus")
  abline(h = 0, col = "red")
  
  # QQ plot des résidus
  qqnorm(residuals(model, type = "deviance"), 
         main = paste("QQ Plot -", model_name))
  qqline(residuals(model, type = "deviance"), col = "red")
}


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
