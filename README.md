# SUPERVISED-LEARNING

# Supervised Learning Using Logistic and Decision Tree Regression

## Overview
This project explores supervised learning techniques, specifically logistic regression and decision tree regression, to analyze and predict customer subscription to term deposits in a bank marketing dataset. The dataset used for this analysis comes from Kaggle and contains 45,211 observations with 16 variables.

## Dataset
- **Source:** [Kaggle Banking Dataset - Marketing Targets](https://www.kaggle.com/datasets/prakharrathi25/banking-dataset-marketing-targets?datasetId=223954&searchQuery=R)
- **Target Variable:** `y` (indicating if the client subscribed to a term deposit)
- **Independent Variables:** 15 features including age, job, marital status, education, balance, etc.

## Methodology
### Data Preprocessing
- Dropped the variable `previous` as it was not relevant for analysis.
- Split the dataset into **training (80%)** and **testing (20%)**.
- Conducted exploratory data analysis (EDA) using:
  - Descriptive statistics
  - Correlation matrices
  - Box plots, histograms, and GG plots for visualization

### Model Implementation
Implemented and compared:
- **Logistic Regression**
- **Decision Tree Regression**

Performance was evaluated using:
- Accuracy scores
- AUC-ROC curves
- Confusion matrices

## Results
- Logistic regression achieved **87.70% accuracy**.
- Decision tree regression achieved **75.25% accuracy**.
- Logistic regression outperformed the decision tree model in predicting term deposit subscriptions.

## Key Insights & Recommendations
1. **Marketing Timing:** Marketing campaigns should focus on **March, September, October, and December** rather than May, which had the lowest conversion rates.
2. **Seasonality Effect:** Customers were more likely to subscribe to term deposits during fall and winter.
3. **Target Age Groups:** Customers in their **20s and 60s+** were more likely to subscribe.
4. **Call Frequency:** Limiting the number of calls to potential customers to a maximum of **three calls** improves efficiency and reduces rejection rates.

## Future Improvements
- Use **deep learning** techniques to improve the AUC score.
- Implement **feature engineering** for better variable selection.
- Experiment with **ensemble learning models** such as Random Forest and Gradient Boosting.

## How to Run the Project
1. Download the dataset from Kaggle.
2. Install dependencies: `pip install pandas numpy sklearn matplotlib seaborn`
3. Run the script to preprocess data and train models.
4. Evaluate model performance using accuracy and AUC-ROC curves.

## References
- [Jovian Banking Marketing Analysis](https://jovian.ai/pro-dut2000/banking-market-analysis)
- [A Beginner's Guide to Machine Learning](https://medium.com/@randylaosat/a-beginners-guide-to-machine-learning-5d87d1b06111)

## Author
**Anusha Girish**

