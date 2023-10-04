作业框架（26号下午4点30更新-Wiz）
如何开始project2 https://csse-uwa-1.gitbook.io/cits4009-computational-data-analysis/week-10-single-variable-models-and-naive-bayes-model/week-10-project-2-classification
还有wei给的ppt instruction
1. 引言
   - 1.1 项目背景和目的： 简要介绍项目的背景、目的和重要性。如果是团队项目，简要描述团队成员和各自的贡献。
   - 1.2 数据来源和特征： 描述数据的来源和主要特征。解释为什么选择这个数据集。
   
2. 数据准备和预处理 
   - 2.1 数据清理： 上传并导入数据集。描述数据清理过程，包括处理缺失值、异常值和重复值。检查缺失值、异常值、重复值，进行必要的处理。
   - 2.2 数据转换： 描述数据转换过程，如分类变量的编码、数值变量的标准化等。用适当的方法转换数据类型。对数据进行整理，使之成为干净、整洁的数据集。

3. 探索性数据分析（EDA）
   - 3.1 变量分析： 通过可视化和统计方法分析各个变量的分布和相互关系。
   - 3.2 特征选择： 描述如何基于EDA的结果选择模型的特征变量。
   
4. 建模
   - 4.1 Null model 和single variable model（Transforming the categorical variables into numerical for single-variate model selection）
   - 4.2 分类模型： 说明选择的分类模型和原因。描述模型的训练和评估过程。
   - 4.3 聚类模型： 说明选择的聚类模型和原因。描述模型的训练和评估过程。

5. 模型评估
   - 5.1 分类模型评估：使用混淆矩阵、ROC曲线等方法评估分类模型的性能。
   - 5.2 聚类模型评估： 使用轮廓系数、戴维森堡丁指数等方法评估聚类模型的性能。

6. 结果和讨论
   - 6.1 模型结果：总结模型的主要发现和结果。
   - 6.2 结果讨论： 分析结果的意义和局限性。提出可能的改进方向和建议。

7. Shiny应用
   - 7.1 应用设计：描述Shiny应用的设计和功能。
   - 7.2 用户界面（UI）： 描述应用的用户界面设计。
   - 7.3 服务器逻辑： 描述应用的服务器逻辑和响应。
   
8. 总结
   - 8.1 主要发现： 总结报告的主要发现和结论。
   - 8.2 对未来工作的建议： 基于项目结果，提出未来研究的方向和建议。

9. 参考文献： 列出报告中引用的所有文献。

之雅部分
3.8 处理异常值（10.1更新-之雅）
 - 3.8.1 识别
 - 3.8.2 散点图呈现
 - 3.8.3 利用尾缩处理前后5%内容
 - 3.8.4 提出部分model问题
 - 3.8.5 做visualization去看哪些变量有明显关系
      - 目标变量的分布：首先，你可以绘制目标变量的直方图或密度图，以查看它的分布。这有助于你了解目标变量是否服从正态分布或其他特定分布。
      - 目标变量与预测变量的关系：如果你有多个预测变量，你可以绘制散点图或箱线图，以查看目标变量与每个预测变量之间的关系。这可以帮助你确定是否存在线性或非线性关系。
      - 时间趋势分析：如果你的数据包含时间序列信息，你可以绘制时间序列图，以查看目标变量随时间的变化趋势。
      - 分类问题的类别分布：如果你的目标变量是分类问题的类别，你可以绘制饼图或条形图，以查看每个类别的分布情况。
      - 相关性分析：你还可以计算目标变量与每个预测变量之间的相关性，并绘制相关性矩阵或热力图，以查看它们之间的关系强度。

3. 模型准备
 - 3.1 选择目标变量和特征变量。
 - 3.2 将数据分为训练集和测试集。
 - 3.3 确定并实现适当的分类模型，例如决策树、逻辑回归等。
 - 3.4 对模型进行训练和测试。

4. 模型评估
 - 4.1 使用混淆矩阵、ROC曲线等来评估模型的性能。
 - 4.2 比较不同模型的性能。
 - 4.3 根据需要优化模型。

5. Shiny应用开发
 - 5.1 设计用户界面，以展示模型性能和聚类结果。
 - 5.2 实现用户界面的功能。
 - 5.3 测试应用以确保其按预期工作。
 - 5.4 对应用进行调整以提高用户体验。

6. 报告与展示
 - 6.1 编写详细的报告，总结项目的各个阶段。
 - 6.2 准备并进行项目展示，向利益相关者展示你的发现和结论。

To obtain marks in the HD range (80%-100%), you should aim at having the following elements in your project:

- Well demonstrated exceptional understandings of the principles for model comparison and selection (NOTE: simply building a few more models will not automatically warrant marks in the HD range).
- Thorough and effective treatment of data to improve model performance, e.g., imbalanced dataset treatment.
- Exceptional data provenance for reproducible data science.
- Exceptional understanding of feature variable treatment and selection.
- Appropriate use of short text descriptions in diagrams (e.g., annotations).
