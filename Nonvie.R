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
            "geo_postcode_lng " ,"admi_risk_year" )

for (var in vars_all) {
  cat(var, ": length =", length(Cdata[[var]]), 
      ", NA count =", sum(is.na(Cdata[[var]])), 
      ", Unique values =", length(unique(Cdata[[var]])), "\n")
}

vars <- c( "veh_years_claim_free", "veh_age", 
           "veh_power", "veh_value",
           "veh_weight" , "cont_seniority"
           ,"driv_m_age","driv_y_age",
           "veh_fuel","veh_make", 
           "veh_seats", "veh_type", 
           "veh_use", "geo_postcode_2digits"
)

for (var in vars) {
  cat(var, ": length =", length(Cdata[[var]]), 
      ", NA count =", sum(is.na(Cdata[[var]])), 
      ", Unique values =", length(unique(Cdata[[var]])), "\n")
}


#On va tester claim_nb_tpl_md //  admi_risk_exposure est dans l'offset
#Je ne prends pas en compte de l'âge de l'autre conducteur, mais déjà si y en a 1


form <- function(var_name, data) {
  v <- data[[var_name]]
  #Binaire to facteur
  if (is.numeric(v) && length(unique(v)) <= 2) {
    as.formula(
      paste("claim_nb_tpl_md ~ factor(", var_name, ") + offset(offset_link)
)")
    )
  } 
  #continue to spline
  else if (is.numeric(v)) {
    as.formula(
      paste("claim_nb_tpl_md ~ s(", var_name, ") + offset(offset_link
)")
    )
  } else {
    # facteur
    as.formula(
      paste("claim_nb_tpl_md ~", var_name, "+ offset(offset_link)")
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

transfo_var <- sapply(vars_all, transfo, data = train_set)
offset<-log(train_set$admi_risk_exposure)

#intercept mean frequency, meanfrequency is consitent with expected
#family=quasipoisson() ou nb()
fit0<-gam(claim_nb_tpl_md~1, data=train_set, poisson(link = "log"), offset=offset_link,  method = "REML")
mean_frequency <- exp(coef(fit0))
mean_frequency


#Firt everything gam
x<-as.formula(paste("claim_nb_tpl_md ~", paste(transfo_var, collapse = " + ")))

fit_all <- gam(as.formula(paste("claim_nb_tpl_md ~", 
                                paste(transfo_var, collapse = " + ")))
               ,family = poisson(link = "log"), data = train_set, 
               offset = offset, method = "REML")
summary (fit_all)


#veh_make + veh_use + veh_years_claim_free + veh_age
# veh_value + cont_seniority + driv_m_age
 
#figure margin ?
plot(fit_all, pages = 1, residuals = TRUE, all.terms = TRUE)


#Second test variable + model on significativ one by one 


pvalue_solo_test <- function(var_name, data = train_set) {
  form<-form(var_name, data)
  v<-data[[var_name]]
  res <- tryCatch({
    fit <- gam(form, family = poisson(link = "log"), data = data,  method = "REML")
    s   <- summary(fit)
    
    # p-value principale
    if (is.numeric(v) && length(unique(v)) > 2) {
      # cas s(variable)
      p_val <- s$s.pv[1]
    } else {
      # cas factor(..) ou variable catégorielle
      tab <- s$p.table
      if (nrow(tab) > 1) {
        p_val <- tab[2, 4]  # p-value du 1er coef non-intercept
      } else {
        p_val <- NA
      }
    }
    
    data.frame(
      variable   = var_name,
      p_value    = p_val,
      significant_5pct = ifelse(!is.na(p_val) & p_val < 0.05, TRUE, FALSE),
      error      = NA_character_
    )
  })
  return(res)
}

results <- do.call(rbind, lapply(vars_all, pvalue_solo_test))
results

vars_s<- subset(results, significant_5pct)$variable
vars_s

#"veh_years_claim_free" "veh_age"              "veh_value"           
# "veh_weight"           "cont_seniority"       "driv_m_age"          
# "veh_fuel"             "veh_type"             "veh_use"             
# "geo_region_fr"        "geo_postcode_lat"     "geo_postcode_lng "   


transfo_var_s <- sapply(vars_s, transfo, data = train_set)

fit_s <- gam(as.formula(paste("claim_nb_tpl_md ~", 
                              paste(transfo_var_s, collapse = " + ")))
             ,family = poisson(link = "log"), data = train_set, 
             offset = offset,  method = "REML")
summary(fit_s)
#Significativ : veh_fuelgasoil(0.05) + veh_usepersonal(0.001) +veh_years_claim_free(0.0001)
#veh_value(0.05) + cont_seniority(0.0001) + driv_m_age (0.0001)+ veh_age (0.001)
#New list of vars
vars<- c("veh_years_claim_free", "veh_age",
         "veh_value", "veh_weight"
         ,"cont_seniority","driv_m_age",
         "veh_fuel","veh_type"
         ,"veh_use" ,"geo_region_fr"
         ,"geo_postcode_lat","geo_postcode_lng"   
)


# Third : Nested model

vars<- c("veh_years_claim_free", "veh_make",
         "veh_age","geo_postcode_2digits",
         "veh_value", "veh_weight"
         ,"cont_seniority","driv_m_age",
         "veh_fuel","veh_type"
         ,"veh_use" ,"geo_region_fr"
         ,"geo_postcode_lat","geo_postcode_lng"   
)


#Choose:"geo_postcode_2digits" "veh_make"             "veh_age"             
#"cont_seniority"       "veh_value"            "driv_m_age"          
#"veh_years_claim_free" "veh_seats"(No way)            "driv_y_age"          
# "veh_fuel

#Among : "veh_years_claim_free" "veh_age"              "veh_power"           
#"veh_value"            "veh_weight"           "cont_seniority"      
#"driv_m_age"           "driv_y_age"           "veh_fuel"            
#"veh_make"             "veh_seats"            "veh_type"            
#"veh_use"              "geo_postcode_2digits"

offset<-log(train_set$admi_risk_exposure)
compare_models <- function(base_model, v, data = train_set) {
  +   base_formula_str <- paste(deparse(formula(base_model)), collapse = " ")
  +   
    +   new_formula_str <- paste0(base_formula_str, " + ", transfo(v, data))
    +   new_formula <- as.formula(new_formula_str)
    +   
      +   new_model <- gam(new_formula, family = poisson(link = "log"), data = data, offset = offset_link, method = "REML")
      +   
        +   # Test de rapport de vraisemblance
        +   lrtest <- anova(base_model, new_model, test = "Chisq")
        +   
          +   # AIC
          +   aic_diff <- AIC(new_model) - AIC(base_model)
          +   
            +   list(
              +     variable = v,  
              +     new_model = new_model,
              +     p_value = lrtest$`Pr(>Chi)`[2],
              +     aic_change = aic_diff,
              +     significant = lrtest$`Pr(>Chi)`[2] < 0.05
              +   )
          + }
# Intercept model
base_model <- fit0
#New: s_vars
#[1] ""              "geo_postcode_lng" wtf only that    "veh_type"            
#[4] ""             ""           ""           
#[7] "" ""       ""
#[10] "veh_use"              "driv_m_age"           "veh_fuel"            
#Choose:"" ""             ""             
#""       ""            "driv_m_age"          
#"" "veh_seats"(No way)            "driv_y_age"          
# "veh_fuel
#veh_weight in only 1 tech no the other
vars<- c("veh_years_claim_free", "veh_make",
         "veh_age","geo_postcode_2digits",
         "veh_value","cont_seniority", "veh_weight"
         ,"driv_m_age",
         "veh_fuel","veh_type"
         ,"veh_use" ,"geo_region_fr"
         ,"geo_postcode_lat","geo_postcode_lng"   
)


# Interactions bivariées pour var signi

#MAtrice de corrélation pour catégorielle ??
cor_matrix <- cor(train_set[vars],use = "pairwise.complete.obs",method = "pearson")

library(corrplot)

# Créer une visualisation claire
corrplot(cor_matrix, 
         method = "color",
         type = "upper",
         order = "hclust",  
         tl.cex = 0.8,      
         tl.col = "black",
         addCoef.col = "black",  
         number.cex = 0.7,
         title = "Matrice de corrélation de Pearson",
         mar = c(0,0,1,0))

upper.tri(cor_matrix)

# Clustering pour identifier des groupes de variables corrélées
hclust_cor <- hclust(as.dist(1 - abs(cor_matrix)))  # Distance = 1 - |correlation|

# Visualiser le dendrogramme
plot(hclust_cor, main = "Clustering des variables par corrélation",
     xlab = "", sub = "")

# Découper en groupes
groups <- cutree(hclust_cor, k = 3)  # 3 groupes
cat("\nGroupes de variables corrélées:\n")
for(i in 1:3) {
  cat("Groupe", i, ":", names(groups[groups == i]), "\n")
}

interactions_to_test <- c(
  "te(driv_m_age, veh_age)",           # Âge conducteur × âge véhicule
  "te(veh_value, geo_region_fr)",      # Valeur véhicule × région
  "ti(cont_seniority, driv_m_age)",    # Ancienneté × âge conducteur
  "te(veh_power, driv_m_age)",         # Puissance × âge conducteur
  "ti(veh_type, veh_fuel)"           # Type véhicule × carburant
  
)


# Tester chaque interaction
test_interaction <- function(base_formula, interaction_term, data = train_set) {
  interaction_formula <- as.formula(paste(deparse(base_formula), "+", interaction_term))
  interaction_model <- gam(interaction_formula, family = poisson(link = "log"), 
                           data = data, offset = offset_link, method = "REML")
  
  base_model <- gam(base_formula, family = poisson(link = "log"), 
                    data = data, offset = offset_link, method = "REML")
  
  lrtest <- anova(base_model, interaction_model, test = "Chisq")
  
  list(
    interaction = interaction_term,
    p_value = lrtest$`Pr(>Chi)`[2],
    aic_change = AIC(interaction_model) - AIC(base_model),
    significant = lrtest$`Pr(>Chi)`[2] < 0.05
  )
}

# Appliquer les tests d'interaction
base_formula <- formula(current_model)  # Le modèle final de l'étape 1

for (interaction in interactions_to_test) {
  result <- test_interaction(base_formula, interaction)
  cat("Interaction", interaction, ": p =", result$p_value, 
      ", AIC change =", result$aic_change, "\n")
}


#Forth : Cross-Validation gam


folds <- createFolds(train_set$claim_nb_tpl_md, k = 10, returnTrain = TRUE)

total_loss_cv_mean <- 0
for(i in 1:10) {
  train_indices <- folds[[i]]
  train_fold <- train_set[train_indices, ]
  test_fold <- train_set[-train_indices, ]
  
  gam_model <- gam(Tot_claim ~ 
                     s(admi_risk_exposure) + 
                     s(veh_years_claim_free) +
                     s(veh_age) +
                     s(veh_power) +
                     s(veh_value) +
                     s(veh_weight) +
                     s(cont_seniority) +
                     s(driv_m_age) +
                     factor(veh_fuel) +
                     factor(veh_type) +
                     factor(veh_use) +
                     factor(geo_postcode_2digits),
                   data = train_fold,
                   family = poisson(link = "log"),
                   method = "ML")
  
  cv_prediction <- predict(gam_model, newdata = test_fold, type = "response")
  
  total_loss_cv <- 0
  for (i in length(cv_prediction)) {
    loss <- 2*(test_fold$Tot_claim[i]*log(test_fold$tot_claim[i]/cv_prediction[i])-(test_fold$Tot_claim[i]-cv_prediction[i]))
    total_loss_cv = total_loss_cv + loss
  }
  total_loss_cv = total_loss_cv/length(test_fold$Tot_claim)
  total_loss_cv_mean <- total_loss_cv_mean + total_loss_cv
} 



results <- sapply(vars, test_variable)
print(results)





#Fifth : all the same as before but with binomial negativ


#Validation on the training set & compute loss_function
training_predictions <-predict(gam_model, newdata = train_set, type = "response")

total_loss_training <- 0
for (i in length(training_predictions)) {
  loss <- 2*(train_set$Tot_claim[i]*log(train_set$tot_claim[i]/training_predictions[i])-(Cdata$Tot_claom[i]-training_predictions[i]))
  total_loss_training = total_loss_training + loss
}
total_loss_training = total_loss_training/length(train_set$Tot_claim)
total_loss_training

#Validation on the validation set & compute loss_function
validation_predictions <- predict(gam_model, newdata = val_set, type = "response")
validation_predictions

total_loss_validation <- 0
for (i in length(validation_predictions)) {
  loss <- 2*(val_set$Tot_claim[i]*log(val_set$Tot_claim[i]/validation_predictions[i])-(Val_set$Tot_claim[i]-validation_predictions[i]))
  total_loss_validation = total_loss_validation + loss
}
total_loss_validation = total_loss_validation/length(validation_set$claim_nb_tpl_md)
total_loss_validation

AIC(m_pois)
AIC(m_nb)
pred_pois <- predict(m_pois, newdata = val_set, type="response")
RMSE_pois <- sqrt(mean((val_set$claim_nb_tpl_md - pred)^2))
pred_nb <- predict(m_nb, newdata = val_set, type="response")
RMSE_nb <- sqrt(mean((val_set$claim_nb_tpl_md - pred)^2))

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
