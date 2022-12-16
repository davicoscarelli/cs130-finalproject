###############################################################################
#Adapted version from original paper
###############################################################################

#import necessary libraries
library(MatchIt)
library(tcltk)
library(cem)
library(tidyverse)


#filter data for the week of the disaster
df <- read_rds("raw_data/ca_dataset.rds") %>%
  filter(week == "2020-10-12")

#match data for demographics using CEM
matched <- df %>%
  matchit(formula = active_fire ~ pop_density + social_capital + svi +
            employee_muni_int,
          method = "cem")

summary(matched)

#save weights
df %>%
  mutate(weights = matched$weights) %>%
  filter(weights > 0) %>%
  select(csub, weights) %>%
  saveRDS("raw_data/ca_disaster_match.rds")

#join raw-dataset with weights and other transformations from the original study
data <- read_rds("raw_data/ca_dataset.rds") %>%
  mutate_at(vars(case_rate, case_rate_lag2), 
            funs(if_else(
              condition = . == 0,
              true = sort(unique(.))[2] / 2,
              false = .))) %>%
  mutate(health_care_capacity_int = ntile(health_care_capacity_int, 4),
         health_quality_int = ntile(health_quality_int, 4),
         svi_socioeconomic = ntile(svi_socioeconomic, 4),
         svi_minority = svi_minority,
         bonding = bonding) %>%
  mutate_at(vars(
    contains("evacuation"),
    case_rate_lag2,
    burn_rate_int, bonding, bridging, linking, 
    svi_socioeconomic, svi_household_disability, 
    svi_minority, svi_housing_tranport, workplaces_int,
    health_care_capacity_int, health_quality_int, 
    employee_muni_int, democrat_2016_int, weathertrans),
    funs(scale(.))) %>%
  left_join(by = c("csub"), y = read_rds("raw_data/ca_disaster_match.rds")) %>%
  filter(weights > 0)


#run OLS regression
plm1 <- data %>%
  plm(formula = log(case_rate.x) ~ 
        case_rate_lag2.x + 
        evacuation_more.x + 
        burn_rate_int.x +
        bonding.x + bridging.x + linking.x + 
        svi_socioeconomic.x + svi_household_disability.x + 
        svi_housing_tranport.x +
        workplaces_int.x +
        employee_muni_int.x + democrat_2016_int.x,
      model = "within", effect = "time", index = c("csub", "week.x"), weights = weights)

summary(plm1)


###############################################################################
#Attempt Model with Genetic Matching 
###############################################################################

#import necessary libraries
library(tidyverse)
library(xts)
library(Matching)

#read raw data and peform same transformations
data <- read_rds("raw_data/ca_dataset.rds") %>%
  filter(week == "2020-10-12") %>%
  mutate_at(vars(case_rate, case_rate_lag2), 
            funs(if_else(
              condition = . == 0,
              true = sort(unique(.))[2] / 2,
              false = .))) %>%
  mutate_at(vars(
    contains("evacuation"),
    case_rate_lag2,
    burn_rate_int, bonding, bridging, linking, 
    svi_socioeconomic, svi_household_disability, 
    svi_minority, svi_housing_tranport, workplaces_int,
    health_care_capacity_int, health_quality_int, 
    employee_muni_int, democrat_2016_int, weathertrans),
    funs(scale(.)))


#run OLS without matching for comparison
lm1 <- lm(log(case_rate) ~ 
            case_rate_lag2 + 
            evacuation_more + 
            burn_rate_int +
            bonding + bridging + linking + 
            svi_socioeconomic + svi_household_disability + 
            svi_minority + svi_housing_tranport +
            workplaces_int + health_care_capacity_int + health_quality_int + 
            employee_muni_int + democrat_2016_int, data=df)

summary(lm1)

#resume original transformations
data <- data %>%
  filter(week == "2020-10-12") %>%
  mutate(health_care_capacity_int = ntile(health_care_capacity_int, 4),
         health_quality_int = ntile(health_quality_int, 4),
         svi_socioeconomic = ntile(svi_socioeconomic, 4),
         svi_minority = ntile(svi_minority, 4)) 

#perform genetic matching
X <- cbind(data$pop_density, data$social_capital, data$svi, data$employee_muni_int)

genout <- GenMatch(X = X, Tr = data$active_fire, pop.size=10)

matchout.gen <- Match(X = X, Tr = data$active_fire, Weight.matrix=genout)
summary(matchout.gen)

#check for balance and get estimated treatment effect
genetic <- MatchBalance(active_fire ~ pop_density + social_capital + svi +
                           employee_muni_int, data=data, match.out = matchout.gen, nboots=1000)


summary(genectic)

matchout.gen2 <- Match(X = X, Tr = data$active_fire, Y = log(data$case_rate), Weight.matrix=genout)
summary(matchout.gen2)


tr.units <- data[matchout.gen2$index.treated,]

matched.controls <- data[matchout.gen2$index.control,]

matched.data <- rbind(tr.units, matched.controls)


#run OLS with matched dataset
lm2 <- lm(log(case_rate) ~ 
            case_rate_lag2 + 
            evacuation_more + 
            burn_rate_int +
            bonding + bridging + linking + 
            svi_socioeconomic + svi_household_disability + 
            svi_minority + svi_housing_tranport +
            workplaces_int + health_care_capacity_int + health_quality_int + 
            employee_muni_int + democrat_2016_int, data=matched.data, weights = c(matchout.gen2$weights, matchout.gen2$weights))

summary(lm2)


