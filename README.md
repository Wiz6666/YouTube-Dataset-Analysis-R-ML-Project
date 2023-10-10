
- 老师给的大框架：
1. Data Preparation 
   - Data cleaning
   - Data transformation
   - Training and Testing dataset 
2. Classification (Week 8, 9 and 10 slides)
   - Dependent variable and independent variables
   - Attributes selection
   - Null module
   - 2 X classifier techniques
   - Results and Evaluation
3. Clustering (Week 11 slides)
   - 1 clustering technique
4. Shiny APP
   - Present your findings to audiences.



- 作业框架（9:45pm 10 Oct 更新-Wiz）
了解您的背景和需求后，接下来为您提供一个简单的步骤，帮助您完成机器学习作业。

### 3.建模

#### 3.1 分类

##### 3.1.1 定义目标变量和特征

由于您的目标是基于多个特征预测Youtuber是否为高收入，您可以将`highest_yearly_earnings`或`highest_monthly_earnings`作为目标变量，并根据某个阈值将其分类为高收入或低收入。

特征可以是：`subscribers`、`video.views`、`uploads`、`created_year`、`channel_type`等。

##### 3.1.2 数据分割

使用例如`caret`包中的`createDataPartition`函数，将数据分为训练集和测试集。

#### 3.2 决策树分类器

##### 3.2.1 使用`rpart`包建立决策树模型。
##### 3.2.2 使用训练集对模型进行训练。
##### 3.2.3 使用训练好的模型在测试集上进行预测。

#### 3.3 k近邻算法 (kNN)

##### 3.3.1 使用`class`包中的`knn`函数。
##### 3.3.2 尝试不同的k值，并选择最优的k值。

#### 3.4 模型评估

对于分类问题，您可以考虑以下评估指标：

- 精确度
- 召回率
- F1得分
- ROC曲线和AUC

#### 3.5 LIME解释

使用`lime`包来解释您的模型。这对于理解您的模型如何工作，并识别任何可能的问题是非常有用的。

#### 3.6 聚类

选择合适的特征并使用聚类算法，例如k-means，对数据进行聚类。然后，可视化聚类结果并解释您发现的模式。

### 4. Shiny应用

构建一个Shiny应用，用户可以交互地查看单变量模型性能、两个分类模型性能和聚类结果。您可以使用`shiny`包来完成。

### 5. 报告撰写

- **数据准备**：描述您如何清洁和处理数据的。
- **模型构建**：解释您选择的模型、所使用的特征和为什么。
- **模型评估**：使用各种指标和可视化工具描述模型的性能。
- **结论**：基于您的分析，总结您的发现和结论。

### 6. 总结

回顾您完成的所有步骤，确保您的工作是完整的，满足老师的要求，并且能够清楚地传达给其他人。


To obtain marks in the HD range (80%-100%), you should aim at having the following elements in your project:

- Well demonstrated exceptional understandings of the principles for model comparison and selection (NOTE: simply building a few more models will not automatically warrant marks in the HD range).
- Thorough and effective treatment of data to improve model performance, e.g., imbalanced dataset treatment.
- Exceptional data provenance for reproducible data science.
- Exceptional understanding of feature variable treatment and selection.
- Appropriate use of short text descriptions in diagrams (e.g., annotations).
