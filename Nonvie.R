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
#quest : este juste ? Estce qu'il faudrait pas aussi verif body injured ?


#exposition au risque sup à 1
Cdata <- data_clean %>%
  mutate(admi_risk_exposure = pmin(admi_risk_exposure, 1))

# Nous faisons cela pour conserver l'information (supposant que NA signifie 0 année sans sinistre)
data_clean <- data_clean %>% 
  mutate(veh_years_claim_free = ifelse(is.na(veh_years_claim_free), 0, veh_years_claim_free))

#categoriel to numeric
data_clean <- data_clean %>% 
  mutate(
    claim_nb_tpl_md = as.numeric(claim_nb_tpl_md), 
    claim_nb_tpl_bi = as.numeric(claim_nb_tpl_bi)  
  )
#Ajouter tot_claim
data_clean <- data_clean %>%
  mutate(Tot_claim = if_else(
    claim_nb_tpl_md == 1 | claim_nb_tpl_bi == 1, 
    1, 
    0
  ))

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

#Observed claim frequencysum(data_clean$Tot_claim) / sum(data_clean$admi_risk_exposure)

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

#2nd selection dataset de ce qu'on a besoin et rename car nom horrible
Cdata <- data_clean %>% 
  dplyr::select(
    -geo_munty_fr, 
    -geo_province_fr, 
    -geo_region_fr, 
    -geo_postcode_lat,
    -geo_postcode_lng, 
    -id
  ) 
##########################################################################
#GAM felix
Cdata <- data_clean

set.seed(123) # For reproducibility
train_index <- createDataPartition(Cdata$Tot_claim, p = 0.8, list = FALSE)
train_set <- Cdata[train_index, ]
val_set <- Cdata[-train_index, ]

# Check the sizes of each set
cat("Original training set size:", nrow(Cdata), "\n")
cat("New training set size:", nrow(train_set), "\n")
cat("Validation set size:", nrow(val_set), "\n")

#Gam model
#Prepare the data
Cdata$veh_make <- as.factor(Cdata$veh_make)
Cdata$veh_type <- as.factor(Cdata$veh_type)
Cdata$veh_use <- as.factor(Cdata$veh_use)

vars <- c("Tot_claim", "veh_age", "veh_power", "veh_value", "driv_m_age", 
          "cont_seniority", "veh_seats", "veh_weight", "veh_make", "veh_type", 
          "veh_use", "admi_risk_exposure")

for (var in vars) {
  cat(var, ": length =", length(Cdata[[var]]), 
      ", NA count =", sum(is.na(Cdata[[var]])), 
      ", Unique values =", length(unique(Cdata[[var]])), "\n")
}

test_variable <- function(var_name) {
  tryCatch({
    # Simple model with just one variable
    test_model <- gam(Tot_claim ~ s(get(var_name)) + offset(log(admi_risk_exposure)),
                      family = poisson, data = Cdata)
    return(paste(var_name, ": OK"))
  }, error = function(e) {
    return(paste(var_name, ": ERROR -", e$message))
  })
}

results <- sapply(vars, test_variable)
print(results)

# GAM-Model
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
                 data = Cdata,
                 family = poisson(link = "log"),
                 method = "ML")
plot(gam_model, pages = 1, residuals = TRUE, all.terms = TRUE)

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

#Validation on Cross-Validation & compute loss_function
folds <- createFolds(train_set$Tot_claim, k = 10, returnTrain = TRUE)

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
    loss <- 2*(test_fold$Tot_claim[i]*log(test_fold$Tot_claim[i]/cv_prediction[i])-(test_fold$Tot_claim[i]-cv_prediction[i]))
    total_loss_cv = total_loss_cv + loss
  }
  total_loss_cv = total_loss_cv/length(test_fold$Tot_claim)
  total_loss_cv_mean <- total_loss_cv_mean + total_loss_cv
} 



results <- sapply(vars, test_variable)
print(results)

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
