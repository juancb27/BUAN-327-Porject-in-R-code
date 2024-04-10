library(ggplot2)
library(dplyr)
library(lm)  

hospitals = read.csv("hospitals.csv")

#How big is the dataset?
dim(hospitals)

#What are the names of the columns?
names(hospitals)

#What data types are each columns?
str(hospitals)

#Are there missing values?
summary(hospitals)



#Which hospital has the lowest number of beds?
hospitals %>% slice_min(Beds)
summary(lm(Total.Expense ~ Beds, hospitals))
hospital_min_beds <- hospitals %>%
  arrange(Beds) %>%  
  head(1) %>%                
  select(Hospital.Number, Beds)  

print(hospital_min_beds)

#Which hospital has the lowest expense?
hospital_min_expense <- hospitals %>%
  arrange(Total.Expense) %>%  
  head(1) %>%                
  select(Hospital.Number, Total.Expense)  

print(hospital_min_expense)

#How many hospitals deliver babies?
num_delivery_hospitals <- hospitals %>%
  filter(Births == 1) %>%  
  nrow()                   

cat("Number of Hospitals Delivering Babies:", num_delivery_hospitals)

#Using ggplot, scatterplot number of beds vs Total Expense
ggplot(hospitals, aes(x = Beds, y = Total.Expense)) +
  geom_point(aes(color = Control.Number), size = 3) +  
  labs(title = "Number of Beds vs. Total Expense",
       x = "Number of Beds",
       y = "Total Expense") +
  theme_classic()

#Using ggplot, scatterplot Admissions vs Total Expense
ggplot(hospitals, aes(x = Admissions, y = Total.Expense)) +
  geom_point(aes(color = Control.Number), size = 3) +  
  labs(title = "Admissions vs. Total Expense",
       x = "Admissions",
       y = "Total Expense") +
  theme_classic()

#Using dplyr and ggplot, scatterplot beds vs Total Expense but only for hospitals that deliver babies
delivery_hospitals <- hospitals %>%
  filter(Births.or.Not %in% c("No Births", 0))  

ggplot(delivery_hospitals, aes(x = Beds, y = Total.Expense)) +
  geom_point(aes(color = Control.Number), size = 3) +  
  labs(title = "Beds vs. Total Expense (Delivery Hospitals)",
       x = "Number of Beds",
       y = "Total Expense") +
  theme_classic()

#One more question that you believe would be useful.
##What is the lowest and highest Payroll Expense?
hospital_payroll_extremes <- hospitals %>%
  arrange(Payroll.Expense) %>%  # Arrange ascending (lowest to highest)
  slice(1, nrow(hospitals)) %>%    # Select first and last rows (lowest and highest)
select(Hospital.Number , Payroll.Expense)  # Select relevant columns
print(hospital_payroll_extremes)


#2.Descriptive Analysis

#Pie Chart:Admissions x Out Patient Visits
# Pie Chart: Admissions vs. Outpatient Visits
total_admissions <- sum(hospitals$Admissions)
total_outpatient_visits <- sum(hospitals$Outpatient.Visits)

hospital_data <- data.frame(
  category = c("Admissions", "Outpatient Visits"),
  value = c(total_admissions, total_outpatient_visits)
)

ggplot(hospital_data, aes(x = "Outpatient Visits", y = value, fill = category)) + 
  geom_bar(stat = "identity") +  
  coord_polar(theta = "y") +  
  labs(title = "Admissions vs. Outpatient Visits Distribution",
       y = "Admissions") +
  theme_classic()

#Bar Chart: Admissions x Region
possible_regions <- c("East South Central", "Pacific", "Mountain", 
                      "West South Central", "New England", 
                      "South Atlantic", "West North Central", 
                      "East North Central")

admissions_by_region <- hospitals %>%
  group_by(Region) %>%
  summarize(total_admissions = sum(Admissions))

ggplot(admissions_by_region, aes(x = Region, y = total_admissions, fill = Region)) +
  geom_bar(stat = "identity") + 
  labs(title = "Admissions by Region",
       x = "Region",
       y = "Total Admissions") +
  theme_classic() 

#Line-Chart
ggplot(hospitals, aes(x = Personnel)) +  
  geom_line(aes(y = Total.Expense, color = "Total Expense")) +  
  geom_line(aes(y = Payroll.Expense, color = "Payroll Expense")) + 
  labs(title = "Expense Trends by Personnel Size",
       x = "Personnel",
       y = "Expense") +
  theme_classic()


#SIMPLE REGRESSION
# Assuming "hospitals" is your data frame and "Beds" is the chosen variable
regression_model <- lm(Total.Expense ~ Beds, data = hospitals)

#What is the value of the R^2?
summary(regression_model)$r.squared

#What does the R^2 measure in this case? 
##An R-squared of 0.60 means that 60% of the variation in total expense can be explained by the number of beds, while 40% remains unexplained.
    
#What are the pvalues? How many pvalues are reported, why? What does each pvalue mean?
summary(regression_model)
## p-value is < 2.2e-16, other p-values are 0.00688 and < 2e-16 for both the Intercept and Beds.
##intercept: Assesses the statistical significance of the intercept term.
##Beds: Assesses the statistical significance of the "Beds" variable's effect on total expense.

#Explain R square, pvalues.
##R-squared: Measures goodness of fit, indicating how well the model explains the relationship between variables.
##P-value: Measures statistical significance, indicating the likelihood that an observed relationship is due to chance.

#What would be the right attribute size (independent variable) that seems most appropriate to lead you in the expense range of $55–$75 million?
##Strength of relationship (R-squared)
##Statistical significance (p-value)
new_beds <- 51000
predicted_expense <- predict(regression_model, data.frame(Beds = new_beds))

cat("Predicted total expense for a hospital with", new_beds, "beds:", predicted_expense, "\n")
new_beds <- 70000
predicted_expense <- predict(regression_model, data.frame(Beds = new_beds))

cat("Predicted total expense for a hospital with", new_beds, "beds:", predicted_expense, "\n")


#MULTIPLE REGRESSION
regression_model <- lm(Total.Expense ~ Service + Control, data = hospitals)
summary(regression_model)
summary(regression_model)$r.squared
#An R-squared of 0.14 means that 14% of the variation in total expense can be explained by the number of beds, while 86% remains unexplained.
#p-value: < 2.2e-16
#P-Values:0.03972, 0.97249, 0.06586, 0.48329, 0.89905, 0.74725, 0.33687, 0.32606, 0.84290, 0.72469, 0.76076, 0.74073, 0.60718, 0.62720, 0.01083, 0.00101, 0.02204, 0.32989.   
#Each independent variable in the model will have its own coefficient and corresponding p-value.
#The p-values help you assess the generalizability of the observed relationships between variables in your sample data to the broader population of hospitals. A low p-value suggests the relationship is unlikely to be due to chance in the sample and increases confidence that it might hold true in the population. The population is the entire group of hospitals we are using to get our findings. 
#The low p-values suggest statistically significant relationships between the corresponding variables and Total Expense, while the higher p-values indicate less conclusive evidence for a relationship.
#R-Square: approximately 14.61% of the variation in Total Expense is explained by the independent variables included in the model.


