# R Machine Learning Project: YouTube Dataset Analysis

## Project Overview
In this project, we aim to demonstrate the modeling process using R's machine learning capabilities. Our goal is to comprehend and implement various phases of data science, including data preparation, model building, and model evaluation. We have chosen the YouTube dataset provided by our university coordinator, which we also used in Project 1. The responsibilities are evenly distributed, with each member contributing equally (50-50).

## Update Log
- **Framework Update**: 10 Oct, 9:45 PM by Wiz

## Workflow

### Data Preparation
1. **Objective and Feature Selection**:
   - **Data Integration**: Consider integrating with external data sources such as the World Bank, adding data like GDP or GDP per capita for each YouTuber's country to better differentiate between high and low income.
   - **Feature Engineering**: Create new features from existing data, such as views per subscriber to indicate the average number of views per subscriber.
   - **Irrelevant Column Removal**: Remove all unnecessary columns, such as those with unique values (e.g., rankings, YouTuber names).
   - **Data Splitting**: Use R's `caret` package to split the data into training and testing sets.

### Model Building
2. **Model Types**:
   - **Null Model**: Build and evaluate a Null Model by predicting the major class for each sample based on the training set.
   - **Single Variable Models**: Model each variable individually to identify the most predictive features.
   - **Multivariable Models**:
     - **Decision Tree**: Build using the `rpart` package with parameters set to prevent overfitting.
     - **Alternative Classifiers**: Consider logistic regression, Naive Bayes, or K-Nearest Neighbors.
     - **Model Optimization**: Adjust hyperparameters and consider using cross-validation to enhance model performance.

### Feature Selection
3. Use two different methods to select features:
   - Start with information gain to assess the importance of each feature.
   - Then, apply a forward feature selection method, incrementally adding features until performance no longer significantly improves.

### Model Evaluation
4. Evaluate models using various metrics:
   - **Confusion Matrix**: Calculate accuracy, recall, precision, etc.
   - **ROC and AUC**: Plot ROC curves and calculate the area under the curve (AUC).
   - **Cross-Validation**: Employ k-fold cross-validation to assess model stability and reliability.
   - **LIME Analysis**: Analyze why the model makes certain predictions for specific samples using the LIME tool.

### Clustering Analysis
5. Perform clustering using selected features and methods like K-means. Determine the optimal number of clusters using the elbow method or other techniques.

### Shiny App Development
6. **Interactive Application**:
   - **User Interface Design**: Provide a user-friendly interface allowing parameter selection.
   - **Results Display**: Visualize classification and clustering outcomes.
   - **Interactivity**: Let users adjust model parameters and see results in real-time.

### Reporting
7. Document all stages of the analysis:
   - **Introduction**: Briefly introduce the analysis purpose.
   - **Data Preparation**: Describe the data cleaning and preprocessing steps.
   - **Model Selection**: Discuss the rationale behind the chosen models and parameters.
   - **Results**: Provide detailed accounts of model performance.
   - **Conclusion**: Summarize the findings and offer recommendations.
   - **Appendix**: Include all R code and other technical details for reproducibility.

### Achieving High Distinction
To secure a high distinction (80%-100%), ensure:
   - Exceptional understanding of model comparison and selection principles.
   - Effective data treatment strategies, especially for imbalanced datasets.
   - Meticulous data provenance for reproducible data science.
   - Advanced handling and selection of feature variables.
   - Proper use of annotations in diagrams.
