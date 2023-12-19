library(readr)
library(tidyverse)
library(openintro)
library(dplyr)
library(ggplot2)
library(tidyr)

fuel <- read.csv("/Users/tatianagodorogea/Desktop/Semestrul V/Analiza Datelor/Laborator 1/Fuel_Consumption_Ratings_2022.csv")

glimpse(fuel)

levels(fuel$CO2.Emissions..g.km.)

fuel_test <- fuel

levels(fuel_test$CO2.Emissions..g.km.) <- c("Small", "Medium", "High")

levels(fuel_test$CO2.Emissions..g.km.)

fuel_test1 <- fuel %>%
  count(CO2.Rating, Make) %>%
  pivot_wider(names_from = CO2.Rating, values_from = n)

print(fuel_test1)

fuel_filtered <- fuel %>%
  filter(Engine.Size..L. != "NA") %>%
  droplevels()


ggplot(fuel_filtered, aes(x = CO2.Rating, fill = Fuel.Type)) +
  geom_bar(position = "dodge") +
  labs(x = "CO2 Rating", y = "Count", fill = "Fuel Type") +
  ggtitle("Distribution")

# Creează boxplot pentru Smog.Rating și CO2.Emissions..g.km.
boxplot_plot <- fuel %>%
  ggplot(aes(x = as.factor(Smog.Rating), y = CO2.Emissions..g.km.)) +
  geom_boxplot() +
  labs(title = "Boxplot of Smog Rating and CO2 Emissions",
       x = "Smog Rating",
       y = "CO2 Emissions")

# Afișează boxplot
print(boxplot_plot)



# Elimină valorile lipsă din setul de date
fuel_filtered <- fuel[complete.cases(fuel$CO2.Emissions..g.km.), ]

# Extrage datele de CO2 Emissions din setul filtrat
co2_emissions_filtered <- fuel_filtered$CO2.Emissions..g.km.

# Generează cuartile teoretice pentru distribuția normală
theoretical_quantiles <- qnorm(ppoints(length(co2_emissions_filtered)))

# Creează un dataframe pentru datele reale și cuartele teoretice
qq_data <- data.frame(Theoretical = theoretical_quantiles, Observed = sort(co2_emissions_filtered))


# Q-Q plot
qq_plot <- ggplot(fuel, aes(sample = CO2.Emissions..g.km.)) +
  stat_qq(aes(sample = Fuel.Consumption..Comb...L.100.km.)) +
  geom_abline(intercept = mean(fuel$Fuel.Consumption..Comb...L.100.km.),
              slope = sd(fuel$Fuel.Consumption..Comb...L.100.km.),
              linetype = "dashed", color = "red") +
  labs(title = "Q-Q Plot: Consum de Combustibil vs. Emisii CO2",
       x = "Emisii CO2",
       y = "Consum de Combustibil") +
  theme_minimal()

print(qq_plot)


fuel_diesel <- fuel %>% filter(Fuel.Type == "D")
summary_measures_diesel <- fuel_diesel %>%
  summarize(
    mean_CO2_Emissions = mean(CO2.Emissions..g.km.),
    median_CO2_Emissions = median(CO2.Emissions..g.km.),
    sd_CO2_Emissions = sd(CO2.Emissions..g.km.),
    iqr_CO2_Emissions = IQR(CO2.Emissions..g.km.),
    count_fuel = n()
  )

fuel <- na.omit(fuel)
fuel$Engine.Size..L.[is.na(fuel$Engine.Size..L.)] <- mean(fuel$Engine.Size..L., na.rm = TRUE)

# Verificarea variabilității
variance <- apply(fuel[, c("Engine.Size..L.", "Cylinders", "Fuel.Consumption..City...L.100.km.", "CO2.Emissions..g.km.")], 2, var)

# Eliminarea variabilelor fără variație
selected_variables <- names(variance[variance > 0])

# Calculul matricei de corelație pentru variabilele selectate
correlation_matrix <- cor(fuel[, selected_variables])

# Afișarea matricei de corelație
print(correlation_matrix)


density_plot_diesel <- fuel_diesel %>%
  ggplot(aes(x = CO2.Emissions..g.km.)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of CO2 Emission for cars with diesel fuel type",
       x = "CO2 emission",
       y = "Density")


print(density_plot_diesel)

density_plot_original <- fuel %>%
  ggplot(aes(x = Smog.Rating)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of Smog Rating",
       x = "Smog Rating",
       y = "Density")

print(density_plot_original)

fuel_test1 <- fuel %>%
  mutate(log_smog = log(Smog.Rating))

density_plot_log <- fuel_test1 %>%
  ggplot(aes(x = log_smog)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of Log-Transformed Smog Rating",
       x = "Smog Rating",
       y = "Density")


print(density_plot_log)

#### Data Preparation ####
# For simplicity, let's use only numerical predictors
# You may need to handle categorical variables appropriately (e.g., one-hot encoding)

# Data Splitting
set.seed(123)
split <- initial_split(fuel, prop = 0.7, strata = 'Fuel.Consumption..Comb...mpg.')
fuel_train <- training(split)
fuel_test <- testing(split)

#### Linear Regression Models ####
# Model 1: Using 'Engine.Size..L.' as the predictor
model_simple1 <- lm(Fuel.Consumption..Comb...mpg. ~ Engine.Size..L., data = fuel_train)
summary(model_simple1)
# Model 2: Using 'Cylinders' as the predictor
model_simple2 <- lm(Fuel.Consumption..Comb...mpg. ~ Cylinders, data = fuel_train)
summary(model_multiple2)
# Model 3: Using all numerical predictors
model_multiple1 <- lm(Fuel.Consumption..Comb...mpg. ~ Engine.Size..L. + Cylinders + Fuel.Consumption..City...L.100.km. + 
                        Fuel.Consumption..Hwy...L.100.km. + CO2.Emissions..g.km., data = fuel_train)
# Realizați predicții pe setul de date de antrenament
fuel_train$Predictions <- predict(model_multiple1, newdata = fuel_train)

# Trasează predicțiile împotriva valorilor reale
plot(fuel_train$Fuel.Consumption..Comb...mpg., fuel_train$Predictions,
     xlab = "Valori reale", ylab = "Predicții",
     main = "Plot pentru Modelul Multiple Regression",
     col = "blue", pch = 16)

# Adaugă o linie de identitate (linie diagonală)
abline(0, 1, col = "red", lty = 2)

# Adaugă o legendă
legend("topleft", legend = "Linia de identitate", col = "red", lty = 2, cex = 0.8)







# Model 4: Using a subset of predictors
model_multiple2 <- lm(Fuel.Consumption..Comb...mpg. ~ Engine.Size..L. + Fuel.Consumption..Hwy...L.100.km., data = fuel_train)

# Model 5: Using a different subset of predictors
model_multiple3 <- lm(Fuel.Consumption..Comb...mpg. ~ Cylinders + CO2.Emissions..g.km., data = fuel_train)

#### Plots for Model Interpretation ####
# Plot for Model 1
plot1 <- ggplot(fuel_train, aes(Engine.Size..L., Fuel.Consumption..Comb...mpg.)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = 'lm', se = F, color = 'blue') +
  xlab('Engine Size (L)') +
  ylab('Fuel Consumption (Comb) (mpg)') +
  ggtitle('Model 1')

print(plot1)


# Plot for Model 2
plot2 <- ggplot(fuel_train, aes(Cylinders, Fuel.Consumption..Comb...mpg.)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = 'lm', se = F, color = 'red') +
  xlab('Cylinders') +
  ylab('Fuel Consumption (Comb) (mpg)') +
  ggtitle('Model 2')

print(plot2)

# Plot for Model 3
plot3 <- ggplot(fuel_train, aes(x = Engine.Size..L., y = Fuel.Consumption..Comb...mpg.)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = 'lm', se = F, color = 'green') +
  xlab('Engine Size (L)') +
  ylab('Fuel Consumption (Comb) (mpg)') +
  ggtitle('Model 3')

print(plot3)

# Efectuarea predicțiilor pe setul de date de test
predictions_model3 <- predict(model_multiple3, newdata = fuel_test)

# Afișarea rezultatelor
head(predictions_model3)



# Plot for Model 4
plot4 <- ggplot(fuel_train, aes(x = Engine.Size..L., y = Fuel.Consumption..Comb...mpg.)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = 'lm', se = F, color = 'purple') +
  xlab('Engine Size (L)') +
  ylab('Fuel Consumption (Comb) (mpg)') +
  ggtitle('Model 4')

print(plot4)

# Plot for Model 5
plot5 <- ggplot(fuel_train, aes(x = Cylinders, y = Fuel.Consumption..Comb...mpg.)) +
  geom_point(size = 1, alpha = .4) +
  geom_smooth(method = 'lm', se = F, color = 'orange') +
  xlab('Cylinders') +
  ylab('Fuel Consumption (Comb) (mpg)') +
  ggtitle('Model 5')

print(plot5)

#model 1
# Calculează reziduurile
residuals <- resid(model_simple1)

# Trasează diagrama de dispersie a reziduurilor față de variabila independentă
plot(fuel_train$Engine.Size..L., residuals,
     xlab = "Engine.Size..L.", ylab = "Residuals",
     main = "Diagrama de Dispersie a Reziduurilor pentru modelul 1",
     pch = 16, col = "blue")

# Adaugă o linie orizontală pentru reziduurile zero
abline(h = 0, col = "red", lty = 2)


#model 2
# Calculează reziduurile pentru Modelul 2
residuals_model2 <- resid(model_simple2)

# Trasează diagrama de dispersie a reziduurilor față de variabila independentă (Cylinders)
plot(fuel_train$Cylinders, residuals_model2,
     xlab = "Cylinders", ylab = "Residuals",
     main = "Diagrama de Dispersie a Reziduurilor pentru Modelul 2",
     pch = 16, col = "red")

# Adaugă o linie orizontală pentru reziduurile zero
abline(h = 0, col = "blue", lty = 2)


#model 3
residuals_model3 <- resid(model_multiple1)

plot(fuel_train$Engine.Size..L., residuals_model3,
     xlab = "Engine.Size..L.", ylab = "Residuals",
     main = "Diagrama de Dispersie a Reziduurilor pentru Modelul 3",
     pch = 16, col = "green")

abline(h = 0, col = "purple", lty = 2)


#model 4
# Calculează reziduurile pentru Modelul 4
residuals_model4 <- resid(model_multiple2)

# Trasează diagrama de dispersie a reziduurilor față de variabila independentă (Engine.Size..L.)
plot(fuel_train$Engine.Size..L., residuals_model4,
     xlab = "Engine.Size..L.", ylab = "Residuals",
     main = "Diagrama de Dispersie a Reziduurilor pentru Modelul 4",
     pch = 16, col = "purple")

# Adaugă o linie orizontală pentru reziduurile zero
abline(h = 0, col = "orange", lty = 2)


#model 5
# Calculează reziduurile pentru Modelul 5
residuals_model5 <- resid(model_multiple3)

# Trasează diagrama de dispersie a reziduurilor față de variabila independentă (Cylinders)
plot(fuel_train$Cylinders, residuals_model5,
     xlab = "Cylinders", ylab = "Residuals",
     main = "Diagrama de Dispersie a Reziduurilor pentru Modelul 5",
     pch = 16, col = "orange")

# Adaugă o linie orizontală pentru reziduurile zero
abline(h = 0, col = "green", lty = 2)


# Instalează/ încarcă pachetul glmnet (dacă nu este deja instalat)
library(glmnet)

# Crează matricea de caracteristici X și vectorul de răspuns y
X <- as.matrix(fuel_train[, c("Engine.Size..L.", "Fuel.Consumption..Hwy...L.100.km.")])
y <- fuel_train$Fuel.Consumption..Comb...mpg.

# Aplică regresia Ridge cu funcția glmnet
ridge_model <- cv.glmnet(x = X, y = y, alpha = 0, nfolds = 10)

# Afișează coeficienții optimi
print(ridge_model$glmnet.fit)

# Plot pentru selectarea valorii optimale a λ (parametrul de regularizare)
plot(ridge_model)




# Instalează și încarcă pachetul glmnet (dacă nu este deja instalat)
install.packages("glmnet")
library(glmnet)

# Crează matricea de caracteristici X și vectorul de răspuns y
X <- as.matrix(fuel_train[, c("Engine.Size..L.", "Fuel.Consumption..Hwy...L.100.km.")])
y <- fuel_train$Fuel.Consumption..Comb...mpg.

# Aplică regresia LASSO cu funcția glmnet
lasso_model <- cv.glmnet(x = X, y = y, alpha = 1, nfolds = 10)

# Afișează coeficienții optimi
print(lasso_model$glmnet.fit)

# Plot pentru selectarea valorii optimale a λ (parametrul de regularizare)
plot(lasso_model)


# Crează matricea de caracteristici X și vectorul de răspuns y pentru Smog.Rating
X_smog <- as.matrix(fuel_train[, c("Engine.Size..L.", "Fuel.Consumption..Hwy...L.100.km.")])
y_smog <- fuel_train$Smog.Rating

# Aplică regresia LASSO cu funcția glmnet pentru Smog.Rating
lasso_model_smog <- cv.glmnet(x = X_smog, y = y_smog, alpha = 1, nfolds = 10)

# Afișează coeficienții optimi pentru regresia LASSO cu Smog.Rating
print(lasso_model_smog$glmnet.fit)

# Plot pentru selectarea valorii optime a λ (parametrul de regularizare) pentru Smog.Rating
plot(lasso_model_smog)



# încarcă pachetul yardstick 
library(yardstick)

# Calculează RMSE pentru modelul_simple1
rmse_value <- rmse(predict(model_simple1, newdata = fuel_test), fuel_test$Fuel.Consumption..Comb...mpg.)
print(paste("RMSE:", rmse_value))
