#install.packages("readxl")
#install.packages("dplyr")
#install.packages("eRm")
#install.packages("lubridate")
#install.packages("WrightMap")
#install.packages("ggplot2")
#install.packages("tibble")

library(readxl)
library(dplyr)
library(lubridate)
library(psych)
library(ggplot2)
library(tibble)
library(WrightMap)
library(eRm)


######### Import dataset ######### 
data_raw <- read_excel("data.xlsx", sheet=2) 


######### Manipulating dataset ######### 

# Transform perceptions into binary
data <- data_raw %>%
  mutate(across(Lighting:AccessibleBusStops, ~ case_when(
    . == "Disagree" ~ 1,
    . == "Agree" ~ 0,
    TRUE ~ NA_real_
  )))


# # Filtering time periods 
peak_filtered <- data %>%
  filter((hour(`Date`) >= 8 & hour(`Date`) < 10) |
           (hour(`Date`) >= 17 & hour(`Date`) < 20))

offpeak_filtered <- data %>%
  filter((hour(`Date`) >= 10 & hour(`Date`) < 12) |
           (hour(`Date`) >= 15 & hour(`Date`) < 17)) 

lunch_filtered <- data %>%
  filter(hour(`Date`) >= 12 & hour(`Date`) < 15) #Lunch time extended to 3 p.m. (15 hrs) to accommodate late responses


# Filtering columns to run the rasch model

peak <- peak_filtered %>% select('Lighting':'AccessibleBusStops')
offpeak <- offpeak_filtered %>% select('Lighting':'AccessibleBusStops')
lunch <- lunch_filtered %>% select('Lighting':'AccessibleBusStops')


######### Initial Rasch Model with all items #########

# Unidimensionality analysis

## Unidimensionality analysis using EFA
###Note: Unidimensionality is met if the first factor has a proportion explained variance equal or above 0.2 (20%).

### Peak hour
efa_result_peak <- fa(peak, nfactors = ncol(peak), rotate = "none")
efa_result_peak$Vaccounted 

### Off-peak hour
efa_result_offpeak <- fa(offpeak, nfactors = ncol(offpeak), rotate = "none")
efa_result_offpeak$Vaccounted 

### Lunch hour
efa_result_lunch <- fa(lunch, nfactors = ncol(lunch), rotate = "none")
efa_result_lunch$Vaccounted 


## Estimation of RM and person parameters during peak-hours
rm_peak_results <- RM(peak)
student.locations_peak <- person.parameter(rm_peak_results)

## Estimation of RM and person parameters during off-peak-hour
rm_offpeak_results <- RM(offpeak)
student.locations_offpeak <- person.parameter(rm_offpeak_results)

## Estimation of RM and person parameters during lunch-hour
rm_lunch_results <- RM(lunch)
student.locations_lunch <- person.parameter(rm_lunch_results)

# Reliability of item separation
# Note: There is not a direct function in eRM package to calculate Reliability of item separation.
## Note: eRM package calculates item easiness (positive values are items more easy to overcome). 
### Therefore we will use the negative sign to invert for difficulties.

## Peak hour
### Extract item parameters and standard errors
item_difficulties_peak <- -coef(rm_peak_results)
item_SE_peak <- rm_peak_results$se.beta

### Compute variance of item difficulties
var_items_peak <- var(item_difficulties_peak)

### Compute mean squared error
mean_SE2_peak <- mean(item_SE_peak^2)

### Compute item separation reliability
item_sep_reliability_peak <- var_items_peak / (var_items_peak + mean_SE2_peak)
item_sep_reliability_peak 

## Off-peak hour
### Extract item parameters and standard errors
item_difficulties_offpeak <- -coef(rm_offpeak_results)
item_SE_offpeak <- rm_offpeak_results$se.beta

### Compute variance of item difficulties
var_items_offpeak <- var(item_difficulties_offpeak)

### Compute mean squared error
mean_SE2_offpeak <- mean(item_SE_offpeak^2)

### Compute item separation reliability
item_sep_reliability_offpeak <- var_items_offpeak / (var_items_offpeak + mean_SE2_offpeak)
item_sep_reliability_offpeak 

## Lunch hour
### Extract item parameters and standard errors
item_difficulties_lunch <- -coef(rm_lunch_results)
item_SE_lunch <- rm_lunch_results$se.beta

### Compute variance of item difficulties
var_items_lunch <- var(item_difficulties_lunch)

### Compute mean squared error
mean_SE2_lunch <- mean(item_SE_lunch^2)

### Compute item separation reliability
item_sep_reliability_lunch <- var_items_lunch / (var_items_lunch + mean_SE2_lunch)
item_sep_reliability_lunch 


# Reliability of person separation

## Peak-hour
person_sep_reliability_peak <- SepRel(student.locations_peak)
person_sep_reliability_peak 

## off-peak hour
person_sep_reliability_offpeak <- SepRel(student.locations_offpeak)
person_sep_reliability_offpeak 

## Lunch hour
person_sep_reliability_lunch <- SepRel(student.locations_lunch)
person_sep_reliability_lunch 

# Check item fit

## Peak hour
### Z Outfit
item.fit_peak$i.outfitZ
### MSQ Outfit
item.fit_peak$i.outfitMSQ
### Z infit
item.fit_peak$i.infitZ
### MSQ infit
item.fit_peak$i.infitMSQ

## OffPeak hour
### Z Outfit
item.fit_offpeak$i.outfitZ
### MSQ Outfit
item.fit_offpeak$i.outfitMSQ
### Z infit
item.fit_offpeak$i.infitZ
### MSQ infit
item.fit_offpeak$i.infitMSQ

## Lunch hour
### Z Outfit
item.fit_lunch$i.outfitZ
### MSQ Outfit
item.fit_lunch$i.outfitMSQ
### Z infit
item.fit_lunch$i.infitZ
### MSQ infit
item.fit_lunch$i.infitMSQ

## Note: Some items are not within the Infit and outfit parameters suitable for the analysis, having:
          ## a) Infit or Outfit Z values not within the range -2 < Zst < 2, or;
          ## b) Infit or Outfit MSQ values not within the range 0.6 < MSQ < 1.4.

# We excluded some of the items in all periods that were not suitable, until we obtained the final models.

######### Final Rasch Model with treated datasets #########

# Dataset treatment (filtering items) 

agg_treated <- agg[,-c(2,3,11,18,19,23)] #6, 18
peak_treated <- peak[,-c(2,3,11,18,19,23)] 
offpeak_treated <- offpeak[,-c(2,3,11,18,19,23)] 
lunch_treated <- lunch[,-c(2,3,11,18,19,23)] 


# Unidimensionality analysis to treated datasets

## Unidimensionality analysis using EFA
###Note: Unidimensionality is met if the first factor has a proportion explained variance equal or above 0.2 (20%).
 
 ## Peak hour
 efa_result_peak_treated <- fa(peak_treated, nfactors = ncol(peak_treated), rotate = "none")
 efa_result_peak_treated$Vaccounted 
 
 ## Off-peak hour
 efa_result_off_peak_treated <- fa(offpeak_treated, nfactors = ncol(offpeak_treated), rotate = "none")
 efa_result_off_peak_treated$Vaccounted 
 
 ## Lunch hour
 efa_result_lunch_treated <- fa(lunch_treated, nfactors = ncol(lunch_treated), rotate = "none")
 efa_result_lunch_treated$Vaccounted 


## Estimation of RM and person parameters for Peak-hour treated dataset
rm_peak_results_treated <- RM(peak_treated)
student.locations_peak_treated <- person.parameter(rm_peak_results_treated)

## Estimation of RM and person parameters for Off-peak-hour treated dataset
rm_offpeak_results_treated <- RM(offpeak_treated)
student.locations_offpeak_treated <- person.parameter(rm_offpeak_results_treated)

## Estimation of RM and person parameters during lunch-hour
rm_lunch_results_treated <- RM(lunch_treated)
student.locations_lunch_treated <- person.parameter(rm_lunch_results_treated)

# Check itemfit

## Peak hour
### Z Outfit
item.fit_peak_treated$i.outfitZ
### MSQ Outfit
item.fit_peak_treated$i.outfitMSQ
### Z infit
item.fit_peak_treated$i.infitZ
### MSQ infit
item.fit_peak_treated$i.infitMSQ

## OffPeak hour
### Z Outfit
item.fit_offpeak_treated$i.outfitZ
### MSQ Outfit
item.fit_offpeak_treated$i.outfitMSQ
### Z infit
item.fit_offpeak_treated$i.infitZ
### MSQ infit
item.fit_offpeak_treated$i.infitMSQ

## Lunch hour
### Z Outfit
item.fit_lunch_treated$i.outfitZ
### MSQ Outfit
item.fit_lunch_treated$i.outfitMSQ
### Z infit
item.fit_lunch_treated$i.infitZ
### MSQ infit
item.fit_lunch_treated$i.infitMSQ

# Reliability of item separation

## Peak hour
### Extract item parameters and standard errors
item_difficulties_peak_treated <- -coef(rm_peak_results_treated)
item_SE_peak_treated <- rm_peak_results_treated$se.beta

### Compute variance of item difficulties
var_items_peak_treated <- var(item_difficulties_peak_treated)

### Compute mean squared error
mean_SE2_peak_treated <- mean(item_SE_peak_treated^2)

### Compute item separation reliability
item_sep_reliability_peak_treated <- var_items_peak_treated / (var_items_peak_treated + mean_SE2_peak_treated)
item_sep_reliability_peak_treated 


## Off-peak hour
### Extract item parameters and standard errors
item_difficulties_offpeak_treated <- -coef(rm_offpeak_results_treated)
item_SE_offpeak_treated <- rm_offpeak_results_treated$se.beta

### Compute variance of item difficulties
var_items_offpeak_treated <- var(item_difficulties_offpeak_treated)

### Compute mean squared error
mean_SE2_offpeak_treated <- mean(item_SE_offpeak_treated^2)

### Compute item separation reliability
item_sep_reliability_offpeak_treated <- var_items_offpeak_treated / (var_items_offpeak_treated + mean_SE2_offpeak_treated)
item_sep_reliability_offpeak_treated 

## Lunch hour
### Extract item parameters and standard errors
item_difficulties_lunch_treated <- -coef(rm_lunch_results_treated)
item_SE_lunch_treated <- rm_lunch_results_treated$se.beta

### Compute variance of item difficulties
var_items_lunch_treated <- var(item_difficulties_lunch_treated)

### Compute mean squared error
mean_SE2_lunch_treated <- mean(item_SE_lunch_treated^2)

### Compute item separation reliability
item_sep_reliability_lunch_treated <- var_items_lunch_treated / (var_items_lunch_treated + mean_SE2_lunch_treated)
item_sep_reliability_lunch_treated 


# Reliability of person separation

## Peak-hour
person_sep_reliability_peak_treated <- SepRel(student.locations_peak_treated)
person_sep_reliability_peak 

## off-peak hour
person_sep_reliability_offpeak_treated <- SepRel(student.locations_offpeak_treated)
person_sep_reliability_offpeak_treated 

## Lunch hour
person_sep_reliability_lunch_treated <- SepRel(student.locations_lunch_treated)
person_sep_reliability_lunch_treated 

# Recap item difficulties and ability parameters

## Item difficulties estimates (betas on a logit scale)
item_difficulties_peak_treated
item_difficulties_offpeak_treated
item_difficulties_lunch_treated

## Item standard errors
item_SE_peak_treated
item_SE_offpeak_treated
item_SE_lunch_treated

## Individual ability estimates ("theta" estimates)
student.locations_peak_treated
student.locations_offpeak_treated
student.locations_lunch_treated


######### Generating Comparative Graphics #########

## Extract item parameters and SEs from eRm models

extract_item_table <- function(model, class_label) {
  item_diffs <- -coef(model, "beta")
  item_se <- model$se.beta
  
  tibble(
    item = names(item_diffs),
    xsi = as.numeric(item_diffs),
    se.xsi = as.numeric(item_se),
    lower = xsi - 1.96 * se.xsi,
    upper = xsi + 1.96 * se.xsi,
    class = class_label
  )
}

## Generate data frames for each group

peak_final <- extract_item_table(rm_peak_results_treated, "Peak")
offpeak_final <- extract_item_table(rm_offpeak_results_treated, "Off-Peak")
lunch_final <- extract_item_table(rm_lunch_results_treated, "Lunch")

## Combine datasets

compare_periods <- bind_rows(peak_final, offpeak_final, lunch_final)

## Remove label "beta" from names of items. 
compare_periods$item <- gsub("^beta\\s*", "", compare_periods$item)

## Reorder items by difficulty for plotting

compare_periods$item <- reorder(compare_periods$item, -compare_periods$xsi)

## Generate plot

ggplot(compare_periods, aes(x = factor(item), y = xsi, color = class)) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.5,
    position = position_dodge(width = 0.6),
    linewidth = 0.8
  ) +
  geom_hline(yintercept = 0, color = "darkgray") +
  labs(y = "Item Difficulty (logits)", x = "Items") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9, 0.8),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid.major = element_line(linewidth = 0.8),
    panel.grid.minor = element_line(linewidth = 0.5)
  )

######### Generating Person-Item Maps (Wright Maps) #########

## Peak hour
par(mar = c(5, 2, 4, 2)) # bottom, left, top, right
plotPImap(rm_peak_results_treated,
          sorted = TRUE,      # sort items by difficulty (top easy; below:Difficult)
          cex.gen = 0.3,      # font size for general text
          main = "Person-Item Map (Peak hour)"
)

## Off-peak hour
plotPImap(rm_offpeak_results_treated,
          sorted = TRUE,      # sort items by difficulty (top easy; below:Difficult)
          cex.gen = 0.3,      # font size for general text
          main = "Person-Item Map (Off-peak hour)"
)

## Lunch hour
plotPImap(rm_lunch_results_treated,
          sorted = TRUE,      # sort items by difficulty (top easy; below:Difficult)
          cex.gen = 0.3,      # font size for general text
          main = "Person-Item Map (Lunch hour)"
)

