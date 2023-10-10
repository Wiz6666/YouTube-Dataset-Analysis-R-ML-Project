- 作业框架（9:45pm 10 Oct 更新-Wiz）
  
数据准备
1.1 目标和特征选择
    1.1 数据整合：考虑与World Bank或其他公开数据源整合，例如增加每个Youtuber所在国家的GDP或GDP per capita数据，以更好地界定高收入和低收入。
    1.2 特征工程：根据原始特征创建新的特征，例如观看数/订阅者数来表示每个订阅者的平均观看次数
    1.3 删除无关列：删除所有不必要的列，如具有唯一值的列（例如，排名、YouTuber等）。
    1.4 数据划分：使用R的caret包，将数据划分为训练集和测试集。

2. 模型构建
  2.1 Null Model
   构建Null Model：根据训练集的目标变量的主要类别，为每个样本预测这一主要类别。
   评估Null Model： 使用混淆矩阵计算准确率和其他相关指标
2.2 单一变量模型
   选择变量： 对每个变量进行建模，以确定哪个变量最具有预测能力
   评估单一变量模型： 使用混淆矩阵、roc、auc等指标评估

2.3 多变量模型
2.3.1 决策树
    使用rpart包进行构建：
        设置模型参数。
        选择一个合适的深度以防止过拟合。
        使用训练数据训练模型。
2.3.2 选择另一个分类器
    例如，可以选择逻辑回归、朴素贝叶斯或K最近邻。
        设置模型参数。
        使用训练数据训练模型。
2.3.3 模型优化：调整超参数，考虑使用交叉验证来优化模型性能。

3. 特征选择
    采用两种不同的方法进行特征选择。
        例如，首先使用信息增益来计算每个特征的重要性。
        其次，可以使用前向特征选择方法，逐步增加特征直到性能不再显著增加。

4. 模型评估
4.1 使用混淆矩阵
    从混淆矩阵中计算准确率、召回率、精确度等指标。
4.2 ROC和AUC
    画出ROC曲线，计算曲线下的面积(AUC)。
4.3 交叉验证
    使用k折交叉验证来评估模型的稳定性和可靠性。
4.4 LIME分析 ：使用LIME工具分析模型为什么对某些样本做出了某些预测
   
6. 聚类分析
    选择适当的特征变量进行聚类。
    使用K均值或其他聚类方法进行聚类。
    选择合适的距离度量。
    使用肘部方法或其他方法确定最佳的簇数量。

7. Shiny App开发
6.1 用户界面设计
    为用户提供一个直观的界面，允许他们选择不同的输入参数。
6.2 结果展示
    可视化分类和聚类的结果。
6.3 交互性
    允许用户调整模型参数并立即看到结果。

8. 报告撰写
    简介：简要介绍分析的目的。
    数据准备：描述数据清洗和预处理的步骤。
    模型选择：描述为什么选择这些模型和参数。
    结果：详细描述模型的性能。
    结论：总结分析结果，并给出建议或建议。
    附录：包括所有的R代码和其他技术细节。


To obtain marks in the HD range (80%-100%), you should aim at having the following elements in your project:

- Well demonstrated exceptional understandings of the principles for model comparison and selection (NOTE: simply building a few more models will not automatically warrant marks in the HD range).
- Thorough and effective treatment of data to improve model performance, e.g., imbalanced dataset treatment.
- Exceptional data provenance for reproducible data science.
- Exceptional understanding of feature variable treatment and selection.
- Appropriate use of short text descriptions in diagrams (e.g., annotations).
