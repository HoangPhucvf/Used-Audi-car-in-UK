# Audi Used Car Price Analysis (UK)
This project aims to analyze and predict the selling prices of used Audi cars in the UK based on various car attributes using econometric techniques. It uses real-world data and applies data preprocessing, feature engineering, and multiple linear regression modeling to identify key factors that influence price.

📂 Project Structure
- audi.csv / audi_cleaned.csv: Dataset containing details of used Audi cars (features like model, year, mileage, fuel type, engine size, etc.).

- Audi_car.r: R script for data preprocessing, feature engineering, regression modeling, residual analysis, and statistical testing.

- Audi_car.ipynb: Jupyter notebook version for exploratory analysis and visualization (in Python, if needed for cross-reference or experimentation).

🧪 Key Methods & Tests
- Linear Regression: Log-linear model log(price) ~ predictors to normalize skewed price distribution.

- Feature Engineering:

    + Polynomial terms: mpg^2, tax^3

    + Interaction terms: year × mileage, tax × mpg

- Model Diagnostics:

    + Residual plots for linearity and homoscedasticity.

    + Breusch-Pagan and White tests to detect heteroskedasticity.

    + Ramsey RESET test to check for model misspecification.

    + VIF (Variance Inflation Factor) to detect multicollinearity.

📊 Visualizations
- Residual plots (vs year, mileage, tax, etc.)

- Distribution of residuals with histogram & KDE.

- Boxplots of residuals by fuel type and transmission.

- Scatter plots with regression lines:

    + Price vs year, mileage, tax, etc.

    + log(Price) vs numerical predictors.

📌 Objectives
- Understand which factors (year, mileage, tax, engine size, etc.) significantly affect used Audi prices.

- Detect signs of non-linearity, multicollinearity, and heteroskedasticity.

- Improve the reliability of predictive modeling using robust econometric tools.

📈 Model Example
The model takes the form:
lm(log(price) ~ year + mileage + transmission + fuelType +
    tax + mpg + engineSize + model +
    year*mileage + mpg^2 + tax*mpg + tax^3)
⚙️ Requirements (R Script)
Make sure the following R packages are installed:
install.packages(c("dplyr", "ggplot2", "lmtest", "car", "sandwich", "ggcorrplot", "reshape2"))

Group Project from an Econometrics Course
