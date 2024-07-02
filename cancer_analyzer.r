breast_cancer_data <- read.csv("data.csv")




# ********************** FULL MODEL ********************** #
cat("FULL MODEL \n")
# Fit the linear regression model
full_breast_cancer_model <- lm(radius_mean ~ compactness_mean + concavity_mean + texture_mean
    + compactness_mean * concavity_mean
    + compactness_mean * texture_mean
    + concavity_mean * texture_mean, data = breast_cancer_data)




# Display the summary of the model
summary(full_breast_cancer_model)




# ********************** REDUCED MODEL ********************** #
cat("REDUCED MODEL \n")
reduced_breast_cancer_model <- lm(radius_mean ~ compactness_mean + concavity_mean + texture_mean, data = breast_cancer_data)
summary(reduced_breast_cancer_model)


# ********************** ANOVA ********************** #
cat("ANOVA RESULT \n")
# Perform the hypothesis test
anova_result <- anova(reduced_breast_cancer_model, full_breast_cancer_model)


# Display the result
print(anova_result)




# ********************** QQ PLOTS ********************** #
# Create QQ plot for the residuals of the reduced model
qq_reduced <- qqnorm(residuals(reduced_breast_cancer_model))
qqline(residuals(reduced_breast_cancer_model), col = "red")




# QQ plot for the full model
qq_full <- qqnorm(residuals(full_breast_cancer_model))
qqline(residuals(full_breast_cancer_model), col = "blue")


resid_reduced <- residuals(reduced_breast_cancer_model)
resid_full <- residuals(full_breast_cancer_model)


cat("RESIDUALS OF REDUCED MODEL", resid_reduced, "\n")
cat("RESIDUALS OF FULL MODEL", resid_full, "\n")




fitted_reduced <- fitted(reduced_breast_cancer_model)
fitted_full <- fitted(full_breast_cancer_model)


cat("FITTED VALUES OF REDUCED MODEL ", fitted_reduced, "\n")
cat("FITTED VALUES OF FULL MODEL", fitted_full, "\n")




# ********************** SCATTERPLOTS ********************** #
# List of predictor variables
predictor_variables <- c("compactness_mean", "concavity_mean", "texture_mean")
# Set up the layout for multiple plots
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
# Define colors for each variable
variable_colors <- c("compactness_mean" = "red", "concavity_mean" = "blue", "texture_mean" = "green")
# Initialize an empty vector to store correlations
correlations <- numeric()
# Define data_subset before the loop
data_subset <- breast_cancer_data[, c("radius_mean", predictor_variables)]


for (variable in predictor_variables) {
    # Scatterplot for reduced model
    plot(data_subset[, variable], data_subset$radius_mean,
        xlab = variable,
        ylab = "Radius Mean",
        pch = 16,
        col = variable_colors[variable]
    )


    # Compute correlation
    correlation <- cor(data_subset[, c("radius_mean", variable)], use = "complete.obs")
    correlations <- c(correlations, correlation)


    # Print correlation with variable name
    cat(paste(variable, "correlation: ", round(correlation, 2), "\n"))
}
