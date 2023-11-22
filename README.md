[toc]

The data set “IBM HR Analytics Employee Attrition & Performance” was
downloaded from this [link](https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset/).

# Import Libraries


```R
library(dplyr)
library(tidyverse)
library(plotrix)
library(caret)
library(rpart) 
library(partykit)
library(h2o)
library(cowplot)
library(ggcorrplot)
library(visNetwork)
library(plotrix)
library(RColorBrewer)
library(viridis)
library(sparkline)
```

# Data prepare

## Data import


```R
df <- read.csv("./data/WA_Fn-UseC_-HR-Employee-Attrition.csv")
head(df)
```


<table class="dataframe">
<caption>A data.frame: 6 × 35</caption>
<thead>
    <tr><th></th><th scope=col>Age</th><th scope=col>Attrition</th><th scope=col>BusinessTravel</th><th scope=col>DailyRate</th><th scope=col>Department</th><th scope=col>DistanceFromHome</th><th scope=col>Education</th><th scope=col>EducationField</th><th scope=col>EmployeeCount</th><th scope=col>EmployeeNumber</th><th scope=col>⋯</th><th scope=col>RelationshipSatisfaction</th><th scope=col>StandardHours</th><th scope=col>StockOptionLevel</th><th scope=col>TotalWorkingYears</th><th scope=col>TrainingTimesLastYear</th><th scope=col>WorkLifeBalance</th><th scope=col>YearsAtCompany</th><th scope=col>YearsInCurrentRole</th><th scope=col>YearsSinceLastPromotion</th><th scope=col>YearsWithCurrManager</th></tr>
    <tr><th></th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>⋯</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;int&gt;</th></tr>
</thead>
<tbody>
    <tr><th scope=row>1</th><td>41</td><td>Yes</td><td>Travel_Rarely    </td><td>1102</td><td>Sales                 </td><td>1</td><td>2</td><td>Life Sciences</td><td>1</td><td>1</td><td>⋯</td><td>1</td><td>80</td><td>0</td><td> 8</td><td>0</td><td>1</td><td> 6</td><td>4</td><td>0</td><td>5</td></tr>
    <tr><th scope=row>2</th><td>49</td><td>No </td><td>Travel_Frequently</td><td> 279</td><td>Research &amp; Development</td><td>8</td><td>1</td><td>Life Sciences</td><td>1</td><td>2</td><td>⋯</td><td>4</td><td>80</td><td>1</td><td>10</td><td>3</td><td>3</td><td>10</td><td>7</td><td>1</td><td>7</td></tr>
    <tr><th scope=row>3</th><td>37</td><td>Yes</td><td><span style=white-space:pre-wrap>Travel_Rarely    </span></td><td>1373</td><td>Research &amp; Development</td><td>2</td><td>2</td><td><span style=white-space:pre-wrap>Other        </span></td><td>1</td><td>4</td><td>⋯</td><td>2</td><td>80</td><td>0</td><td> 7</td><td>3</td><td>3</td><td> 0</td><td>0</td><td>0</td><td>0</td></tr>
    <tr><th scope=row>4</th><td>33</td><td>No </td><td>Travel_Frequently</td><td>1392</td><td>Research &amp; Development</td><td>3</td><td>4</td><td>Life Sciences</td><td>1</td><td>5</td><td>⋯</td><td>3</td><td>80</td><td>0</td><td> 8</td><td>3</td><td>3</td><td> 8</td><td>7</td><td>3</td><td>0</td></tr>
    <tr><th scope=row>5</th><td>27</td><td>No </td><td><span style=white-space:pre-wrap>Travel_Rarely    </span></td><td> 591</td><td>Research &amp; Development</td><td>2</td><td>1</td><td><span style=white-space:pre-wrap>Medical      </span></td><td>1</td><td>7</td><td>⋯</td><td>4</td><td>80</td><td>1</td><td> 6</td><td>3</td><td>3</td><td> 2</td><td>2</td><td>2</td><td>2</td></tr>
    <tr><th scope=row>6</th><td>32</td><td>No </td><td>Travel_Frequently</td><td>1005</td><td>Research &amp; Development</td><td>2</td><td>2</td><td>Life Sciences</td><td>1</td><td>8</td><td>⋯</td><td>3</td><td>80</td><td>0</td><td> 8</td><td>2</td><td>2</td><td> 7</td><td>7</td><td>3</td><td>6</td></tr>
</tbody>
</table>




```R
# 创建副本
original_df <- df

# Using an insightful summary with skim and kable
df %>% glimpse()
```

    Rows: 1,470
    Columns: 35
    $ Age                      [3m[90m<int>[39m[23m 41, 49, 37, 33, 27, 32, 59, 30, 38, 36, 35, 2…
    $ Attrition                [3m[90m<chr>[39m[23m "Yes", "No", "Yes", "No", "No", "No", "No", "…
    $ BusinessTravel           [3m[90m<chr>[39m[23m "Travel_Rarely", "Travel_Frequently", "Travel…
    $ DailyRate                [3m[90m<int>[39m[23m 1102, 279, 1373, 1392, 591, 1005, 1324, 1358,…
    $ Department               [3m[90m<chr>[39m[23m "Sales", "Research & Development", "Research …
    $ DistanceFromHome         [3m[90m<int>[39m[23m 1, 8, 2, 3, 2, 2, 3, 24, 23, 27, 16, 15, 26, …
    $ Education                [3m[90m<int>[39m[23m 2, 1, 2, 4, 1, 2, 3, 1, 3, 3, 3, 2, 1, 2, 3, …
    $ EducationField           [3m[90m<chr>[39m[23m "Life Sciences", "Life Sciences", "Other", "L…
    $ EmployeeCount            [3m[90m<int>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    $ EmployeeNumber           [3m[90m<int>[39m[23m 1, 2, 4, 5, 7, 8, 10, 11, 12, 13, 14, 15, 16,…
    $ EnvironmentSatisfaction  [3m[90m<int>[39m[23m 2, 3, 4, 4, 1, 4, 3, 4, 4, 3, 1, 4, 1, 2, 3, …
    $ Gender                   [3m[90m<chr>[39m[23m "Female", "Male", "Male", "Female", "Male", "…
    $ HourlyRate               [3m[90m<int>[39m[23m 94, 61, 92, 56, 40, 79, 81, 67, 44, 94, 84, 4…
    $ JobInvolvement           [3m[90m<int>[39m[23m 3, 2, 2, 3, 3, 3, 4, 3, 2, 3, 4, 2, 3, 3, 2, …
    $ JobLevel                 [3m[90m<int>[39m[23m 2, 2, 1, 1, 1, 1, 1, 1, 3, 2, 1, 2, 1, 1, 1, …
    $ JobRole                  [3m[90m<chr>[39m[23m "Sales Executive", "Research Scientist", "Lab…
    $ JobSatisfaction          [3m[90m<int>[39m[23m 4, 2, 3, 3, 2, 4, 1, 3, 3, 3, 2, 3, 3, 4, 3, …
    $ MaritalStatus            [3m[90m<chr>[39m[23m "Single", "Married", "Single", "Married", "Ma…
    $ MonthlyIncome            [3m[90m<int>[39m[23m 5993, 5130, 2090, 2909, 3468, 3068, 2670, 269…
    $ MonthlyRate              [3m[90m<int>[39m[23m 19479, 24907, 2396, 23159, 16632, 11864, 9964…
    $ NumCompaniesWorked       [3m[90m<int>[39m[23m 8, 1, 6, 1, 9, 0, 4, 1, 0, 6, 0, 0, 1, 0, 5, …
    $ Over18                   [3m[90m<chr>[39m[23m "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", …
    $ OverTime                 [3m[90m<chr>[39m[23m "Yes", "No", "Yes", "Yes", "No", "No", "Yes",…
    $ PercentSalaryHike        [3m[90m<int>[39m[23m 11, 23, 15, 11, 12, 13, 20, 22, 21, 13, 13, 1…
    $ PerformanceRating        [3m[90m<int>[39m[23m 3, 4, 3, 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 3, …
    $ RelationshipSatisfaction [3m[90m<int>[39m[23m 1, 4, 2, 3, 4, 3, 1, 2, 2, 2, 3, 4, 4, 3, 2, …
    $ StandardHours            [3m[90m<int>[39m[23m 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 8…
    $ StockOptionLevel         [3m[90m<int>[39m[23m 0, 1, 0, 0, 1, 0, 3, 1, 0, 2, 1, 0, 1, 1, 0, …
    $ TotalWorkingYears        [3m[90m<int>[39m[23m 8, 10, 7, 8, 6, 8, 12, 1, 10, 17, 6, 10, 5, 3…
    $ TrainingTimesLastYear    [3m[90m<int>[39m[23m 0, 3, 3, 3, 3, 2, 3, 2, 2, 3, 5, 3, 1, 2, 4, …
    $ WorkLifeBalance          [3m[90m<int>[39m[23m 1, 3, 3, 3, 3, 2, 2, 3, 3, 2, 3, 3, 2, 3, 3, …
    $ YearsAtCompany           [3m[90m<int>[39m[23m 6, 10, 0, 8, 2, 7, 1, 1, 9, 7, 5, 9, 5, 2, 4,…
    $ YearsInCurrentRole       [3m[90m<int>[39m[23m 4, 7, 0, 7, 2, 7, 0, 0, 7, 7, 4, 5, 2, 2, 2, …
    $ YearsSinceLastPromotion  [3m[90m<int>[39m[23m 0, 1, 0, 3, 2, 3, 0, 0, 1, 7, 0, 0, 4, 1, 0, …
    $ YearsWithCurrManager     [3m[90m<int>[39m[23m 5, 7, 0, 0, 2, 6, 0, 0, 8, 7, 3, 8, 3, 2, 3, …
    


```R
# Look up for missing values
any(is.na(df))
```


FALSE


## Prepare data for visualization


```R
# Translate into Chinese

df_zh <- df %>%
  mutate(Gender = case_when(
    Gender == "Male" ~ "男",
    Gender == "Female" ~ "女",
    TRUE ~ Gender
  )) %>%
  mutate(Attrition = case_when(
    Attrition == "Yes" ~ "已流失",
    Attrition == "No" ~ "未流失",
    TRUE ~ Attrition
  )) %>%
  mutate(Over18 = case_when(
    Over18 == "Y" ~ "已成年",
    TRUE ~ Over18
  )) %>%
  mutate(OverTime = case_when(
    OverTime == "Yes" ~ "有加班",
    OverTime == "No" ~ "无加班",
    TRUE ~ OverTime
  )) %>%
  mutate(Department = case_when(
    Department == "Research & Development" ~ "研发部门",
    Department == "Sales" ~ "销售部门",
    Department == "Human Resources" ~ "人力部门",
    TRUE ~ Department
  )) %>%
  mutate(JobRole = case_when(
    JobRole == "Sales Executive" ~ "销售主管",
    JobRole == "Research Scientist" ~ "研究科学家",
    JobRole == "Laboratory Technician" ~ "实验室技术员",
    JobRole == "Manufacturing Director" ~ "制造总监",
    JobRole == "Healthcare Representative" ~ "医疗代表",
    JobRole == "Manager" ~ "经理",
    JobRole == "Sales Representative" ~ "销售代表",
    JobRole == "Research Director" ~ "研究总监",
    JobRole == "Human Resources" ~ "人力资源",
    TRUE ~ JobRole
  )) %>%
  mutate(BusinessTravel = case_when(
    BusinessTravel == "Travel_Rarely" ~ "很少出差",
    BusinessTravel == "Travel_Frequently" ~ "频繁出差",
    BusinessTravel == "Non-Travel" ~ "从不出差",
    TRUE ~ BusinessTravel
  )) %>%
  mutate(EducationField = case_when(
    EducationField == "Life Sciences" ~ "生命科学",
    EducationField == "Human Resources" ~ "人力资源",
    EducationField == "Technical Degree" ~ "技术学位",
    EducationField == "Marketing" ~ "市场营销",
    EducationField == "Medical" ~ "医疗",
    EducationField == "Other" ~ "其他",
    TRUE ~ EducationField
  )) %>%
  mutate(MaritalStatus = case_when(
    MaritalStatus == "Single" ~ "单身",
    MaritalStatus == "Married" ~ "已婚",
    MaritalStatus == "Divorced" ~ "离异",
    TRUE ~ MaritalStatus
  ))
df_zh %>% glimpse()

# 重命名列名
col_names <- c('年龄', '流失情况', '出差情况', '日薪', '所属部门', '公里离家距离', '教育水平', '教育领域', '员工数量（恒1）', '员工数量', '环境满意度', '性别', '小时薪', '工作投入度', '岗位级别', '岗位名称', '工作满意度', '婚姻状态', '月收入', '月费率', '在多少家公司工作过', '是否成年', '是否加班', '薪资增长百分比', '绩效评级', '人际关系满意度', '标准工作小时数', '股票期权级别', '总工作年数', '去年接受培训的次数', '工作与生活的平衡情况', '在当前公司的工作年数', '在当前职位的工作年数', '自上次升职以来的年数', '与当前经理合作的年数')
df_zh <- rename(df_zh, !!!setNames(names(df_zh), col_names))
df_zh %>% glimpse()
```

    Rows: 1,470
    Columns: 35
    $ Age                      [3m[90m<int>[39m[23m 41, 49, 37, 33, 27, 32, 59, 30, 38, 36, 35, 2…
    $ Attrition                [3m[90m<chr>[39m[23m "已流失", "未流失", "已流失", "未流失", "未流…
    $ BusinessTravel           [3m[90m<chr>[39m[23m "很少出差", "频繁出差", "很少出差", "频繁出差…
    $ DailyRate                [3m[90m<int>[39m[23m 1102, 279, 1373, 1392, 591, 1005, 1324, 1358,…
    $ Department               [3m[90m<chr>[39m[23m "销售部门", "研发部门", "研发部门", "研发部门…
    $ DistanceFromHome         [3m[90m<int>[39m[23m 1, 8, 2, 3, 2, 2, 3, 24, 23, 27, 16, 15, 26, …
    $ Education                [3m[90m<int>[39m[23m 2, 1, 2, 4, 1, 2, 3, 1, 3, 3, 3, 2, 1, 2, 3, …
    $ EducationField           [3m[90m<chr>[39m[23m "生命科学", "生命科学", "其他", "生命科学", "…
    $ EmployeeCount            [3m[90m<int>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    $ EmployeeNumber           [3m[90m<int>[39m[23m 1, 2, 4, 5, 7, 8, 10, 11, 12, 13, 14, 15, 16,…
    $ EnvironmentSatisfaction  [3m[90m<int>[39m[23m 2, 3, 4, 4, 1, 4, 3, 4, 4, 3, 1, 4, 1, 2, 3, …
    $ Gender                   [3m[90m<chr>[39m[23m "女", "男", "男", "女", "男", "男", "女", "男…
    $ HourlyRate               [3m[90m<int>[39m[23m 94, 61, 92, 56, 40, 79, 81, 67, 44, 94, 84, 4…
    $ JobInvolvement           [3m[90m<int>[39m[23m 3, 2, 2, 3, 3, 3, 4, 3, 2, 3, 4, 2, 3, 3, 2, …
    $ JobLevel                 [3m[90m<int>[39m[23m 2, 2, 1, 1, 1, 1, 1, 1, 3, 2, 1, 2, 1, 1, 1, …
    $ JobRole                  [3m[90m<chr>[39m[23m "销售主管", "研究科学家", "实验室技术员", "研…
    $ JobSatisfaction          [3m[90m<int>[39m[23m 4, 2, 3, 3, 2, 4, 1, 3, 3, 3, 2, 3, 3, 4, 3, …
    $ MaritalStatus            [3m[90m<chr>[39m[23m "单身", "已婚", "单身", "已婚", "已婚", "单身…
    $ MonthlyIncome            [3m[90m<int>[39m[23m 5993, 5130, 2090, 2909, 3468, 3068, 2670, 269…
    $ MonthlyRate              [3m[90m<int>[39m[23m 19479, 24907, 2396, 23159, 16632, 11864, 9964…
    $ NumCompaniesWorked       [3m[90m<int>[39m[23m 8, 1, 6, 1, 9, 0, 4, 1, 0, 6, 0, 0, 1, 0, 5, …
    $ Over18                   [3m[90m<chr>[39m[23m "已成年", "已成年", "已成年", "已成年", "已成…
    $ OverTime                 [3m[90m<chr>[39m[23m "有加班", "无加班", "有加班", "有加班", "无加…
    $ PercentSalaryHike        [3m[90m<int>[39m[23m 11, 23, 15, 11, 12, 13, 20, 22, 21, 13, 13, 1…
    $ PerformanceRating        [3m[90m<int>[39m[23m 3, 4, 3, 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 3, …
    $ RelationshipSatisfaction [3m[90m<int>[39m[23m 1, 4, 2, 3, 4, 3, 1, 2, 2, 2, 3, 4, 4, 3, 2, …
    $ StandardHours            [3m[90m<int>[39m[23m 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 8…
    $ StockOptionLevel         [3m[90m<int>[39m[23m 0, 1, 0, 0, 1, 0, 3, 1, 0, 2, 1, 0, 1, 1, 0, …
    $ TotalWorkingYears        [3m[90m<int>[39m[23m 8, 10, 7, 8, 6, 8, 12, 1, 10, 17, 6, 10, 5, 3…
    $ TrainingTimesLastYear    [3m[90m<int>[39m[23m 0, 3, 3, 3, 3, 2, 3, 2, 2, 3, 5, 3, 1, 2, 4, …
    $ WorkLifeBalance          [3m[90m<int>[39m[23m 1, 3, 3, 3, 3, 2, 2, 3, 3, 2, 3, 3, 2, 3, 3, …
    $ YearsAtCompany           [3m[90m<int>[39m[23m 6, 10, 0, 8, 2, 7, 1, 1, 9, 7, 5, 9, 5, 2, 4,…
    $ YearsInCurrentRole       [3m[90m<int>[39m[23m 4, 7, 0, 7, 2, 7, 0, 0, 7, 7, 4, 5, 2, 2, 2, …
    $ YearsSinceLastPromotion  [3m[90m<int>[39m[23m 0, 1, 0, 3, 2, 3, 0, 0, 1, 7, 0, 0, 4, 1, 0, …
    $ YearsWithCurrManager     [3m[90m<int>[39m[23m 5, 7, 0, 0, 2, 6, 0, 0, 8, 7, 3, 8, 3, 2, 3, …
    Rows: 1,470
    Columns: 35
    $ 年龄                 [3m[90m<int>[39m[23m 41, 49, 37, 33, 27, 32, 59, 30, 38, 36, 35, 29, 3…
    $ 流失情况             [3m[90m<chr>[39m[23m "已流失", "未流失", "已流失", "未流失", "未流失",…
    $ 出差情况             [3m[90m<chr>[39m[23m "很少出差", "频繁出差", "很少出差", "频繁出差", "…
    $ 日薪                 [3m[90m<int>[39m[23m 1102, 279, 1373, 1392, 591, 1005, 1324, 1358, 216…
    $ 所属部门             [3m[90m<chr>[39m[23m "销售部门", "研发部门", "研发部门", "研发部门", "…
    $ 公里离家距离         [3m[90m<int>[39m[23m 1, 8, 2, 3, 2, 2, 3, 24, 23, 27, 16, 15, 26, 19, …
    $ 教育水平             [3m[90m<int>[39m[23m 2, 1, 2, 4, 1, 2, 3, 1, 3, 3, 3, 2, 1, 2, 3, 4, 2…
    $ 教育领域             [3m[90m<chr>[39m[23m "生命科学", "生命科学", "其他", "生命科学", "医疗…
    $ `员工数量（恒1）`    [3m[90m<int>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ 员工数量             [3m[90m<int>[39m[23m 1, 2, 4, 5, 7, 8, 10, 11, 12, 13, 14, 15, 16, 18,…
    $ 环境满意度           [3m[90m<int>[39m[23m 2, 3, 4, 4, 1, 4, 3, 4, 4, 3, 1, 4, 1, 2, 3, 2, 1…
    $ 性别                 [3m[90m<chr>[39m[23m "女", "男", "男", "女", "男", "男", "女", "男", "…
    $ 小时薪               [3m[90m<int>[39m[23m 94, 61, 92, 56, 40, 79, 81, 67, 44, 94, 84, 49, 3…
    $ 工作投入度           [3m[90m<int>[39m[23m 3, 2, 2, 3, 3, 3, 4, 3, 2, 3, 4, 2, 3, 3, 2, 4, 4…
    $ 岗位级别             [3m[90m<int>[39m[23m 2, 2, 1, 1, 1, 1, 1, 1, 3, 2, 1, 2, 1, 1, 1, 3, 1…
    $ 岗位名称             [3m[90m<chr>[39m[23m "销售主管", "研究科学家", "实验室技术员", "研究科…
    $ 工作满意度           [3m[90m<int>[39m[23m 4, 2, 3, 3, 2, 4, 1, 3, 3, 3, 2, 3, 3, 4, 3, 1, 2…
    $ 婚姻状态             [3m[90m<chr>[39m[23m "单身", "已婚", "单身", "已婚", "已婚", "单身", "…
    $ 月收入               [3m[90m<int>[39m[23m 5993, 5130, 2090, 2909, 3468, 3068, 2670, 2693, 9…
    $ 月费率               [3m[90m<int>[39m[23m 19479, 24907, 2396, 23159, 16632, 11864, 9964, 13…
    $ 在多少家公司工作过   [3m[90m<int>[39m[23m 8, 1, 6, 1, 9, 0, 4, 1, 0, 6, 0, 0, 1, 0, 5, 1, 0…
    $ 是否成年             [3m[90m<chr>[39m[23m "已成年", "已成年", "已成年", "已成年", "已成年",…
    $ 是否加班             [3m[90m<chr>[39m[23m "有加班", "无加班", "有加班", "有加班", "无加班",…
    $ 薪资增长百分比       [3m[90m<int>[39m[23m 11, 23, 15, 11, 12, 13, 20, 22, 21, 13, 13, 12, 1…
    $ 绩效评级             [3m[90m<int>[39m[23m 3, 4, 3, 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3…
    $ 人际关系满意度       [3m[90m<int>[39m[23m 1, 4, 2, 3, 4, 3, 1, 2, 2, 2, 3, 4, 4, 3, 2, 3, 4…
    $ 标准工作小时数       [3m[90m<int>[39m[23m 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 8…
    $ 股票期权级别         [3m[90m<int>[39m[23m 0, 1, 0, 0, 1, 0, 3, 1, 0, 2, 1, 0, 1, 1, 0, 1, 2…
    $ 总工作年数           [3m[90m<int>[39m[23m 8, 10, 7, 8, 6, 8, 12, 1, 10, 17, 6, 10, 5, 3, 6,…
    $ 去年接受培训的次数   [3m[90m<int>[39m[23m 0, 3, 3, 3, 3, 2, 3, 2, 2, 3, 5, 3, 1, 2, 4, 1, 5…
    $ 工作与生活的平衡情况 [3m[90m<int>[39m[23m 1, 3, 3, 3, 3, 2, 2, 3, 3, 2, 3, 3, 2, 3, 3, 3, 2…
    $ 在当前公司的工作年数 [3m[90m<int>[39m[23m 6, 10, 0, 8, 2, 7, 1, 1, 9, 7, 5, 9, 5, 2, 4, 10,…
    $ 在当前职位的工作年数 [3m[90m<int>[39m[23m 4, 7, 0, 7, 2, 7, 0, 0, 7, 7, 4, 5, 2, 2, 2, 9, 2…
    $ 自上次升职以来的年数 [3m[90m<int>[39m[23m 0, 1, 0, 3, 2, 3, 0, 0, 1, 7, 0, 0, 4, 1, 0, 8, 0…
    $ 与当前经理合作的年数 [3m[90m<int>[39m[23m 5, 7, 0, 0, 2, 6, 0, 0, 8, 7, 3, 8, 3, 2, 3, 8, 5…
    


```R
# 创建副本
original_df_zh <- df_zh
```

## Prepare data for machine learning

### Convert categorical type data to integer encoding


```R
df_train <- df %>%
  mutate(Gender = ifelse(Gender == "Male", 1, ifelse(Gender == "Female", 0, Gender))) %>%
  mutate(Attrition = ifelse(Attrition == "Yes", 0, ifelse(Attrition == "No", 1, Attrition))) %>%
  mutate(Over18 = ifelse(Over18 == "Y", 1, Over18)) %>%
  mutate(OverTime = ifelse(OverTime == "Yes", 1, ifelse(OverTime == "No", 0, OverTime))) %>%
  mutate(Department = ifelse(Department == "Research & Development", 0,
                             ifelse(Department == "Sales", 1,
                                    ifelse(Department == "Human Resources", 2, Department)))) %>%
  mutate(JobRole = ifelse(JobRole == "Sales Executive", 0,
                          ifelse(JobRole == "Research Scientist", 1,
                                 ifelse(JobRole == "Laboratory Technician", 2,
                                        ifelse(JobRole == "Manufacturing Director", 3,
                                               ifelse(JobRole == "Healthcare Representative", 4,
                                                      ifelse(JobRole == "Manager", 5,
                                                             ifelse(JobRole == "Sales Representative", 6,
                                                                    ifelse(JobRole == "Research Director", 7,
                                                                           ifelse(JobRole == "Human Resources", 8, JobRole)))))))))) %>%
  mutate(BusinessTravel = ifelse(BusinessTravel == "Travel_Rarely", 1,
                                 ifelse(BusinessTravel == "Travel_Frequently", 2,
                                        ifelse(BusinessTravel == "Non-Travel", 0, BusinessTravel)))) %>%
  mutate(EducationField = ifelse(EducationField == "Life Sciences", 0,
                                 ifelse(EducationField == "Human Resources", 1,
                                        ifelse(EducationField == "Technical Degree", 2,
                                               ifelse(EducationField == "Marketing", 3,
                                                      ifelse(EducationField == "Medical", 4,
                                                             ifelse(EducationField == "Other", 5, EducationField))))))) %>%
  mutate(MaritalStatus = ifelse(MaritalStatus == "Single", 0,
                                ifelse(MaritalStatus == "Married", 1,
                                       ifelse(MaritalStatus == "Divorced", 2, MaritalStatus))))

df_train$Gender <- as.integer(df_train$Gender)
df_train$Attrition <- as.integer(df_train$Attrition)
df_train$Over18 <- as.integer(df_train$Over18)
df_train$OverTime <- as.integer(df_train$OverTime)
df_train$Department <- as.integer(df_train$Department)
df_train$JobRole <- as.integer(df_train$JobRole)
df_train$BusinessTravel <- as.integer(df_train$BusinessTravel)
df_train$EducationField <- as.integer(df_train$EducationField)
df_train$MaritalStatus <- as.integer(df_train$MaritalStatus)


df_train %>% glimpse()
```

    Rows: 1,470
    Columns: 35
    $ Age                      [3m[90m<int>[39m[23m 41, 49, 37, 33, 27, 32, 59, 30, 38, 36, 35, 2…
    $ Attrition                [3m[90m<int>[39m[23m 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, …
    $ BusinessTravel           [3m[90m<int>[39m[23m 1, 2, 1, 2, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, …
    $ DailyRate                [3m[90m<int>[39m[23m 1102, 279, 1373, 1392, 591, 1005, 1324, 1358,…
    $ Department               [3m[90m<int>[39m[23m 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    $ DistanceFromHome         [3m[90m<int>[39m[23m 1, 8, 2, 3, 2, 2, 3, 24, 23, 27, 16, 15, 26, …
    $ Education                [3m[90m<int>[39m[23m 2, 1, 2, 4, 1, 2, 3, 1, 3, 3, 3, 2, 1, 2, 3, …
    $ EducationField           [3m[90m<int>[39m[23m 0, 0, 5, 0, 4, 0, 4, 0, 0, 4, 4, 0, 0, 4, 0, …
    $ EmployeeCount            [3m[90m<int>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    $ EmployeeNumber           [3m[90m<int>[39m[23m 1, 2, 4, 5, 7, 8, 10, 11, 12, 13, 14, 15, 16,…
    $ EnvironmentSatisfaction  [3m[90m<int>[39m[23m 2, 3, 4, 4, 1, 4, 3, 4, 4, 3, 1, 4, 1, 2, 3, …
    $ Gender                   [3m[90m<int>[39m[23m 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, …
    $ HourlyRate               [3m[90m<int>[39m[23m 94, 61, 92, 56, 40, 79, 81, 67, 44, 94, 84, 4…
    $ JobInvolvement           [3m[90m<int>[39m[23m 3, 2, 2, 3, 3, 3, 4, 3, 2, 3, 4, 2, 3, 3, 2, …
    $ JobLevel                 [3m[90m<int>[39m[23m 2, 2, 1, 1, 1, 1, 1, 1, 3, 2, 1, 2, 1, 1, 1, …
    $ JobRole                  [3m[90m<int>[39m[23m 0, 1, 2, 1, 2, 2, 2, 2, 3, 4, 2, 2, 1, 2, 2, …
    $ JobSatisfaction          [3m[90m<int>[39m[23m 4, 2, 3, 3, 2, 4, 1, 3, 3, 3, 2, 3, 3, 4, 3, …
    $ MaritalStatus            [3m[90m<int>[39m[23m 0, 1, 0, 1, 1, 0, 1, 2, 0, 1, 1, 0, 2, 2, 0, …
    $ MonthlyIncome            [3m[90m<int>[39m[23m 5993, 5130, 2090, 2909, 3468, 3068, 2670, 269…
    $ MonthlyRate              [3m[90m<int>[39m[23m 19479, 24907, 2396, 23159, 16632, 11864, 9964…
    $ NumCompaniesWorked       [3m[90m<int>[39m[23m 8, 1, 6, 1, 9, 0, 4, 1, 0, 6, 0, 0, 1, 0, 5, …
    $ Over18                   [3m[90m<int>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    $ OverTime                 [3m[90m<int>[39m[23m 1, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, …
    $ PercentSalaryHike        [3m[90m<int>[39m[23m 11, 23, 15, 11, 12, 13, 20, 22, 21, 13, 13, 1…
    $ PerformanceRating        [3m[90m<int>[39m[23m 3, 4, 3, 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 3, …
    $ RelationshipSatisfaction [3m[90m<int>[39m[23m 1, 4, 2, 3, 4, 3, 1, 2, 2, 2, 3, 4, 4, 3, 2, …
    $ StandardHours            [3m[90m<int>[39m[23m 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 8…
    $ StockOptionLevel         [3m[90m<int>[39m[23m 0, 1, 0, 0, 1, 0, 3, 1, 0, 2, 1, 0, 1, 1, 0, …
    $ TotalWorkingYears        [3m[90m<int>[39m[23m 8, 10, 7, 8, 6, 8, 12, 1, 10, 17, 6, 10, 5, 3…
    $ TrainingTimesLastYear    [3m[90m<int>[39m[23m 0, 3, 3, 3, 3, 2, 3, 2, 2, 3, 5, 3, 1, 2, 4, …
    $ WorkLifeBalance          [3m[90m<int>[39m[23m 1, 3, 3, 3, 3, 2, 2, 3, 3, 2, 3, 3, 2, 3, 3, …
    $ YearsAtCompany           [3m[90m<int>[39m[23m 6, 10, 0, 8, 2, 7, 1, 1, 9, 7, 5, 9, 5, 2, 4,…
    $ YearsInCurrentRole       [3m[90m<int>[39m[23m 4, 7, 0, 7, 2, 7, 0, 0, 7, 7, 4, 5, 2, 2, 2, …
    $ YearsSinceLastPromotion  [3m[90m<int>[39m[23m 0, 1, 0, 3, 2, 3, 0, 0, 1, 7, 0, 0, 4, 1, 0, …
    $ YearsWithCurrManager     [3m[90m<int>[39m[23m 5, 7, 0, 0, 2, 6, 0, 0, 8, 7, 3, 8, 3, 2, 3, …
    

### Delete constant columns


```R
df_train <- select(df_train, -'Over18')
df_train <- select(df_train, -'StandardHours')
df_train <- select(df_train, -'EmployeeCount')
```

# Static


```R
red <- "#a6dcc3"
darkred <- "#df0017"
green <- "#ffdac2"
yellow <- "#fcffc0"
brown <- "#fed097"
st_y <- "员工数量"
st_y_per <- "员工数量占比"
font <- "serif"
```

# Analyze basic factors
1. Demographic characteristics(sex, age, marital status, educational level)
2. Department and job role

## Demographic characteristics
Stacked Bar Charts and Percent Stacked Bar Charts

### Sex -> Attrition


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_sex_attrition <- 
  df_zh %>%
  select(性别, 流失情况) %>% 
  group_by(性别, 流失情况) %>% 
  summarize(n=n()) %>% 
    ggplot(aes(x=性别, y=n, fill = 流失情况)) +
    geom_bar(stat="identity", color="black", size=.2, width=.6, position="stack") +
    geom_label(aes(label=n, y=n), 
               color = "black", family = font) +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y) +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10)) 
```

```R
options(repr.plot.width=6, repr.plot.height=4) 

p_sex_attrition_pct <- 
  df_zh %>%
  select(性别, 流失情况) %>% 
  group_by(性别, 流失情况) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% arrange(desc(pct)) %>%
    ggplot(aes(x=fct_reorder(性别,pct), y=pct, fill=流失情况, color=流失情况)) + 
    geom_bar(stat="identity", color="black", size=.2, width=.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 流失情况), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per, x="性别") +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```


### Age -> Attrition
Devide age into 4 generations


```R
df_zh$代际 <- ifelse(df_zh$年龄<37,"千禧一代",
ifelse(df_zh$年龄>=38 & df_zh$年龄<54,"X世代",
ifelse(df_zh$年龄>=54 & df_zh$年龄<73,"婴儿潮一代",
       "沉默一代"
)))
unique(df_zh$代际)
```


<style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>'X世代'</li><li>'沉默一代'</li><li>'千禧一代'</li><li>'婴儿潮一代'</li></ol>




```R
options(repr.plot.width=6, repr.plot.height=4) 

p_age_attrition <- 
  df_zh %>%
  select(代际, 流失情况) %>% 
  group_by(代际, 流失情况) %>% 
  summarize(n=n()) %>% 
  mutate(代际 = factor(代际, levels=c("千禧一代", "X世代", "婴儿潮一代", "沉默一代"))) %>%
    ggplot(aes(x=代际, y=n, fill = 流失情况)) +
    geom_bar(stat="identity", color="black", size=.2, width=.6, position="stack") +
    geom_label(aes(label=n, y=n), 
               color = "black", family = font) +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y) +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_age_attrition_pct <- 
  df_zh %>%
  select(代际, 流失情况) %>% 
  group_by(代际, 流失情况) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% arrange(desc(pct)) %>%
  mutate(代际 = factor(代际, levels=c("千禧一代", "X世代", "婴儿潮一代", "沉默一代"))) %>%
    ggplot(aes(x=fct_reorder(代际,pct), y=pct, fill=流失情况, color=流失情况)) + 
    geom_bar(stat="identity", color="black", size=.2, width=0.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 流失情况), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per, x="代际") +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```


### Marital status -> Attrition


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_marry_attrition <- 
  df_zh %>%
  select(婚姻状态, 流失情况) %>% 
  group_by(婚姻状态, 流失情况) %>% 
  summarize(n=n()) %>% 
  mutate(婚姻状态 = factor(婚姻状态, levels=c("已婚", "单身", "离异"))) %>%
    ggplot(aes(x=婚姻状态, y=n, fill = 流失情况)) +
    geom_bar(stat="identity", color="black", size=.2, width=.6, position="stack") +
    geom_label(aes(label=n, y=n), 
               color = "black", family = font) +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y) +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10)) 
```



```R
options(repr.plot.width=6, repr.plot.height=4) 

p_marry_attrition_pct <- 
  df_zh %>%
  select(婚姻状态, 流失情况) %>% 
  group_by(婚姻状态, 流失情况) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% arrange(desc(pct)) %>%
  mutate(婚姻状态 = factor(婚姻状态, levels=c("已婚", "单身", "离异"))) %>%
    ggplot(aes(x=fct_reorder(婚姻状态,pct), y=pct, fill=流失情况, color=流失情况)) + 
    geom_bar(stat="identity", color="black", size=.2, width=0.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 流失情况), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per, x="婚姻状态") +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```



### Educational level -> Attrition


```R
df_zh$教育水平_文字 <- 
ifelse(df_zh$教育水平 == 1, "本科以下学历",
ifelse(df_zh$教育水平 == 2, "本科学历",
ifelse(df_zh$教育水平 == 3, "学士学位",
ifelse(df_zh$教育水平 == 4, "硕士学位", 
       "博士学位"))))
unique(df_zh$教育水平_文字)

```


<style>
.list-inline {list-style: none; margin:0; padding: 0}
.list-inline>li {display: inline-block}
.list-inline>li:not(:last-child)::after {content: "\00b7"; padding: 0 .5ex}
</style>
<ol class=list-inline><li>'本科学历'</li><li>'本科以下学历'</li><li>'硕士学位'</li><li>'学士学位'</li><li>'博士学位'</li></ol>




```R
options(repr.plot.width=6, repr.plot.height=4) 

p_edu_attrition <- 
  df_zh %>%
  select(教育水平_文字, 流失情况) %>% 
  group_by(教育水平_文字, 流失情况) %>% 
  summarize(n=n()) %>% 
  mutate(教育水平_文字 = factor(教育水平_文字, levels=c("学士学位", "硕士学位", "本科学历", "本科以下学历", "博士学位"))) %>%
    ggplot(aes(x=教育水平_文字, y=n, fill = 流失情况)) +
    geom_bar(stat="identity", color="black", size=.2, width=.6, position="stack") +
    geom_label(aes(label=n, y=n), 
               color = "black", family = font) +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y, x="教育水平") +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_edu_attrition_pct <- 
  df_zh %>%
  select(教育水平_文字, 流失情况) %>% 
  group_by(教育水平_文字, 流失情况) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% arrange(desc(pct)) %>%
  mutate(教育水平_文字 = factor(教育水平_文字, levels=c("学士学位", "硕士学位", "本科学历", "本科以下学历", "博士学位"))) %>%
    ggplot(aes(x=fct_reorder(教育水平_文字,pct), y=pct, fill=流失情况, color=流失情况)) + 
    geom_bar(stat="identity", color="black", size=.2, width=0.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 流失情况), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per, x="教育水平") +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```



```R
options(repr.plot.width=8, repr.plot.height=20) 

plot_grid(p_age_attrition, p_age_attrition_pct, p_sex_attrition, p_sex_attrition_pct, p_marry_attrition, p_marry_attrition_pct, p_edu_attrition, p_edu_attrition_pct, ncol=2, nrow=4)
```


    
![png](pic/basic_attrition.png)
    


## Department and job role
Pyramid scheme

### Department -> Attrition


```R
options(repr.plot.width=10, repr.plot.height=4) 

attr.dep <- df_zh %>% 
select(所属部门, 流失情况) %>% 
group_by(所属部门, 流失情况) %>% 
summarize(amount=n()) %>%
mutate(pct=round(prop.table(amount),2) * 100) %>% 
arrange(pct)

nofunc <- colorRampPalette(c("#A9F5A9", "#58FA58", "#01DF01"))
yesfunc <- colorRampPalette(c("#F5A9A9", "#FE2E2E", "#B40404"))

yes.attr <- attr.dep %>% 
filter(流失情况 == "已流失") %>% 
arrange(所属部门) 

no.attr <- attr.dep %>% 
filter(流失情况 == "未流失") %>% 
arrange(所属部门)

# 使用par指定字体、线粗等
par(family = font, lwd=0.5)
par(mar = pyramid.plot(no.attr$pct, yes.attr$pct, 
                       labels = unique(attr.dep$所属部门),
                       top.labels=c("未流失","","已流失"),
                       gap=30, show.values = T, 
                       rxcol = yesfunc(9), lxcol = nofunc(9)))
```
    86 86 
    


    
![png](pic/pic/department_attrition.png)
    


### Job role -> Attrition


```R
options(repr.plot.width=10, repr.plot.height=6) 

attr.job <- df_zh %>% 
select(岗位名称, 流失情况) %>% 
group_by(岗位名称, 流失情况) %>% 
summarize(amount=n()) %>%
mutate(pct=round(prop.table(amount),2) * 100) %>% 
arrange(pct)

nofunc <- colorRampPalette(c("#A9F5A9", "#58FA58", "#01DF01"))
yesfunc <- colorRampPalette(c("#F5A9A9", "#FE2E2E", "#B40404"))

yes.attr <- attr.job %>% 
filter(流失情况 == "已流失") %>% 
arrange(岗位名称) 

no.attr <- attr.job %>% 
filter(流失情况 == "未流失") %>% 
arrange(岗位名称)

# 使用par指定字体、线粗等
par(family = font, lwd=0.5)
par(mar = pyramid.plot(no.attr$pct, yes.attr$pct, 
                       labels = unique(attr.job$岗位名称),
                       top.labels=c("未流失","","已流失"),
                       gap=30, show.values = T, 
                       rxcol = yesfunc(9), lxcol = nofunc(9)))
```
    98 98 
    


    
![png](pic/job_attrition.png)
    


#  Ranking of feature importance
1. Decision Tree
2. H2O AutoML

## Decision Tree

A decision tree is a machine learning algorithm for building predictive models and performing classification or regression analysis. It creates a tree-like structure by selecting the best features from a dataset, where each internal node represents a feature and each leaf node represents a category or a prediction.


```R
set.seed(142)
# I personally prefer to shuffle my data before splitting.
df_zh <- df_zh[sample(nrow(df_zh)),]

# Let's encode the ordinal variables
df_zh$出差情况 = factor(df_zh$出差情况,
                         levels = c('频繁出差', '很少出差', '从不出差'),
                         labels = c(1, 2, 3))

# Changing the datatype from integer to factors from the ordinal variables.
cols <- c("教育水平", "环境满意度", "工作投入度", "岗位级别",
         "工作满意度", "绩效评级", "人际关系满意度", 
         "股票期权级别", "去年接受培训的次数", "工作与生活的平衡情况")

df_zh[cols] <- lapply(df_zh[cols], factor)

# Splitting our data
trainIndex <- createDataPartition(df_zh$流失情况, p=0.8, list=FALSE, times=1)

train <- df_zh[trainIndex,]
test <- df_zh[-trainIndex,]
```


```R
# Checking that both the training and testing sets have the same label proportions.
prop_train <- train %>% select(流失情况) %>% group_by(流失情况) %>% summarize(n=n()) %>%
mutate(pct=round(prop.table(n), 2))

prop_test <- test %>% select(流失情况) %>% group_by(流失情况) %>% summarize(n=n()) %>%
mutate(pct=round(prop.table(n), 2))

prop_train
prop_test
```


<table class="dataframe">
<caption>A tibble: 2 × 3</caption>
<thead>
    <tr><th scope=col>流失情况</th><th scope=col>n</th><th scope=col>pct</th></tr>
    <tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
    <tr><td>已流失</td><td>190</td><td>0.16</td></tr>
    <tr><td>未流失</td><td>987</td><td>0.84</td></tr>
</tbody>
</table>




<table class="dataframe">
<caption>A tibble: 2 × 3</caption>
<thead>
    <tr><th scope=col>流失情况</th><th scope=col>n</th><th scope=col>pct</th></tr>
    <tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;int&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
    <tr><td>已流失</td><td> 47</td><td>0.16</td></tr>
    <tr><td>未流失</td><td>246</td><td>0.84</td></tr>
</tbody>
</table>




```R
options(repr.plot.width=15, repr.plot.height=10) 

# 构建了一个决策树模型
tree <- rpart(流失情况 ~ ., data=train)

visTree(tree,height = "800px",
        colorY = c("green","red","blue"))
```


![png](pic/visTree.png)


```R
prune.tree <- prune(tree, cp=0.02) # pruning the tree

visTree(prune.tree,height = "600px",
        colorY = c("green","red","blue"))

rparty.tree <- as.party(tree)
rparty.tree
```


![png](pic/pruneTree.png)


    
    Model formula:
    流失情况 ~ 年龄 + 出差情况 + 日薪 + 所属部门 + 
        公里离家距离 + 教育水平 + 教育领域 + `员工数量（恒1）` + 
        员工数量 + 环境满意度 + 性别 + 小时薪 + 工作投入度 + 
        岗位级别 + 岗位名称 + 工作满意度 + 婚姻状态 + 
        月收入 + 月费率 + 在多少家公司工作过 + 是否成年 + 
        是否加班 + 薪资增长百分比 + 绩效评级 + 人际关系满意度 + 
        标准工作小时数 + 股票期权级别 + 总工作年数 + 
        去年接受培训的次数 + 工作与生活的平衡情况 + 
        在当前公司的工作年数 + 在当前职位的工作年数 + 
        自上次升职以来的年数 + 与当前经理合作的年数 + 
        代际 + 教育水平_文字
    
    Fitted party:
    [1] root
    |   [2] 总工作年数 >= 2.5
    |   |   [3] 是否加班 in 无加班
    |   |   |   [4] 股票期权级别 in 1, 2, 3: 未流失 (n = 462, err = 5.0%)
    |   |   |   [5] 股票期权级别 in 0
    |   |   |   |   [6] 工作满意度 in 2, 3, 4: 未流失 (n = 256, err = 9.0%)
    |   |   |   |   [7] 工作满意度 in 1
    |   |   |   |   |   [8] 人际关系满意度 in 2, 4: 未流失 (n = 34, err = 8.8%)
    |   |   |   |   |   [9] 人际关系满意度 in 1, 3
    |   |   |   |   |   |   [10] 岗位名称 in 经理, 人力资源, 医疗代表, 制造总监: 未流失 (n = 10, err = 0.0%)
    |   |   |   |   |   |   [11] 岗位名称 in 实验室技术员, 销售代表, 销售主管, 研究科学家: 已流失 (n = 19, err = 26.3%)
    |   |   [12] 是否加班 in 有加班
    |   |   |   [13] 月收入 >= 3751.5
    |   |   |   |   [14] 婚姻状态 in 离异, 已婚: 未流失 (n = 158, err = 10.8%)
    |   |   |   |   [15] 婚姻状态 in 单身
    |   |   |   |   |   [16] 岗位名称 in 经理, 人力资源, 研究科学家, 研究总监, 医疗代表, 制造总监: 未流失 (n = 32, err = 12.5%)
    |   |   |   |   |   [17] 岗位名称 in 实验室技术员, 销售主管
    |   |   |   |   |   |   [18] 月收入 < 9430
    |   |   |   |   |   |   |   [19] 工作投入度 in 1, 3: 未流失 (n = 17, err = 29.4%)
    |   |   |   |   |   |   |   [20] 工作投入度 in 2, 4: 已流失 (n = 7, err = 14.3%)
    |   |   |   |   |   |   [21] 月收入 >= 9430: 已流失 (n = 7, err = 0.0%)
    |   |   |   [22] 月收入 < 3751.5
    |   |   |   |   [23] 环境满意度 in 2, 3, 4
    |   |   |   |   |   [24] 日薪 >= 1129: 未流失 (n = 17, err = 5.9%)
    |   |   |   |   |   [25] 日薪 < 1129
    |   |   |   |   |   |   [26] 出差情况 in 2
    |   |   |   |   |   |   |   [27] 年龄 >= 33.5: 未流失 (n = 18, err = 16.7%)
    |   |   |   |   |   |   |   [28] 年龄 < 33.5: 已流失 (n = 18, err = 33.3%)
    |   |   |   |   |   |   [29] 出差情况 in 1, 3: 已流失 (n = 12, err = 8.3%)
    |   |   |   |   [30] 环境满意度 in 1: 已流失 (n = 17, err = 11.8%)
    |   [31] 总工作年数 < 2.5
    |   |   [32] 是否加班 in 无加班
    |   |   |   [33] 环境满意度 in 3, 4: 未流失 (n = 44, err = 25.0%)
    |   |   |   [34] 环境满意度 in 1, 2
    |   |   |   |   [35] 小时薪 >= 58.5: 未流失 (n = 10, err = 30.0%)
    |   |   |   |   [36] 小时薪 < 58.5: 已流失 (n = 10, err = 10.0%)
    |   |   [37] 是否加班 in 有加班: 已流失 (n = 29, err = 20.7%)
    
    Number of inner nodes:    18
    Number of terminal nodes: 19



```R
# Complicated DecisionTree, Is there a way to determine variable importance?
var_imp <- data.frame(tree$variable.importance)
var_imp$features <- rownames(var_imp)
var_imp <- var_imp[, c(2, 1)]
var_imp$importance <- round(var_imp$tree.variable.importance, 2)
var_imp$tree.variable.importance <- NULL

colorCount <- length(unique(var_imp$features))
feature_importance <- var_imp %>%
ggplot(aes(x=reorder(features, importance), y=importance, fill=features)) + 
geom_bar(stat='identity', color="black", size=0.2) + 
coord_flip() + 
scale_fill_manual(values = colorRampPalette(brewer.pal(24, "Pastel1"))(colorCount)) + 
theme_minimal() +
theme(# 全图字体&字号
      text = element_text(family = font, size=11),
      # 不显示图例
      legend.position="none", 
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10)) + 
geom_label(aes(label=paste0(importance, "%")), color = "black", family = font) + 
labs(x="特征", y="重要性")

feature_importance
```

    
![png](pic/dt_feature_importance.png)
    


The order of importance of the characteristics is shown. Monthly income has the highest impact of most attrition, followed by total years of work, and in descending order of importance by whether or not overtime is worked, position, and age.


```R
options(repr.plot.width=8, repr.plot.height=6) 

predictions <- predict(tree, test, type="class")
conf_df <- data.frame(table(test$流失情况, predictions))

ggplot(data = conf_df, mapping = aes(x = predictions, y = Var1)) +
geom_tile(aes(fill = Freq)) +
geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1, family = "serif", size=6) +
scale_fill_gradient(low = "#fcf4ed", high = "#f47b2a") +
theme_minimal() +
theme(# 全图字体&字号
      text = element_text(family = "serif", size=11),
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10)) + 
labs(y="流失情况", x="预测", fill="频率")
```


    
![png](pic/conf_df.png)
    


The model performs poorly in terms of accuracy, precision and specificity, while performing better in terms of recall. This could mean that the model has some ability to recognize positive categories but is less capable of distinguishing negative categories.

## H2O AutoML

H2O is an open source distributed machine learning platform that provides a rich set of machine learning algorithms and tools that make building and deploying machine learning models more efficient and convenient.

H2O machine learning models have the advantages of high performance, diverse algorithm support, and automated feature engineering, making them a powerful tool for analyzing the factors affecting employee turnover. By leveraging the H2O platform, the causes of employee turnover can be analyzed more quickly and accurately, and corresponding management measures can be taken to improve employee satisfaction and retention, thus enhancing the stability and competitiveness of the organization.

Use H2O AutoML to run an automated machine learning process that automatically builds and evaluates multiple models based on a given training dataset with a target variable of churning and generates a model leaderboard. 

The appearance of a progress bar to 100% is considered a successful build.


```R
# 启动模型
h2o.init()

# 将数据框改成h2o格式
h2o_df <- as.h2o(df_train)

# 拆分训练集、测试集、验证集
split_df <- h2o.splitFrame(h2o_df, c(0.7, 0.15), seed=12)
h2o_train <- h2o.assign(split_df[[1]], "train")
h2o_validation <- h2o.assign(split_df[[2]], "validation")
h2o_test <- h2o.assign(split_df[[2]], "test")

h2o.describe(h2o_train)
```

    
    H2O is not running yet, starting it now...
    
    Note:  In case of errors look at the following log files:
        C:\Users\XQ\AppData\Local\Temp\RtmpaMIXY6\fileed41b227f97/h2o_XQ_started_from_r.out
        C:\Users\XQ\AppData\Local\Temp\RtmpaMIXY6\fileed475e76f9e/h2o_XQ_started_from_r.err
    
    
    Starting H2O JVM and connecting: . Connection successful!
    
    R is connected to the H2O cluster: 
        H2O cluster uptime:         4 seconds 823 milliseconds 
        H2O cluster timezone:       Asia/Shanghai 
        H2O data parsing timezone:  UTC 
        H2O cluster version:        3.40.0.4 
        H2O cluster version age:    6 months and 24 days 
        H2O cluster name:           H2O_started_from_R_XQ_mas852 
        H2O cluster total nodes:    1 
        H2O cluster total memory:   3.95 GB 
        H2O cluster total cores:    8 
        H2O cluster allowed cores:  8 
        H2O cluster healthy:        TRUE 
        H2O Connection ip:          localhost 
        H2O Connection port:        54321 
        H2O Connection proxy:       NA 
        H2O Internal Security:      FALSE 
        R Version:                  R version 4.3.0 (2023-04-21 ucrt) 
      |======================================================================| 100%
    


<table class="dataframe">
<caption>A data.frame: 32 × 11</caption>
<thead>
    <tr><th scope=col>Label</th><th scope=col>Type</th><th scope=col>Missing</th><th scope=col>Zeros</th><th scope=col>PosInf</th><th scope=col>NegInf</th><th scope=col>Min</th><th scope=col>Max</th><th scope=col>Mean</th><th scope=col>Sigma</th><th scope=col>Cardinality</th></tr>
    <tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
    <tr><td>Age                     </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>  18</td><td>   60</td><td>3.695873e+01</td><td>   9.2341317</td><td>NA</td></tr>
    <tr><td>Attrition               </td><td>int</td><td>0</td><td>168</td><td>0</td><td>0</td><td>   0</td><td>    1</td><td>8.387716e-01</td><td>   0.3679181</td><td>NA</td></tr>
    <tr><td>BusinessTravel          </td><td>int</td><td>0</td><td>105</td><td>0</td><td>0</td><td>   0</td><td>    2</td><td>1.087332e+00</td><td>   0.5305760</td><td>NA</td></tr>
    <tr><td>DailyRate               </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td> 102</td><td> 1499</td><td>7.901727e+02</td><td> 405.2567110</td><td>NA</td></tr>
    <tr><td>Department              </td><td>int</td><td>0</td><td>684</td><td>0</td><td>0</td><td>   0</td><td>    2</td><td>3.905950e-01</td><td>   0.5765409</td><td>NA</td></tr>
    <tr><td>DistanceFromHome        </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>   1</td><td>   29</td><td>9.029750e+00</td><td>   8.0638707</td><td>NA</td></tr>
    <tr><td>Education               </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>   1</td><td>    5</td><td>2.901152e+00</td><td>   1.0180025</td><td>NA</td></tr>
    <tr><td>EducationField          </td><td>int</td><td>0</td><td>448</td><td>0</td><td>0</td><td>   0</td><td>    5</td><td>2.002879e+00</td><td>   1.8931575</td><td>NA</td></tr>
    <tr><td>EmployeeNumber          </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>   1</td><td> 2065</td><td>1.031714e+03</td><td> 608.6139027</td><td>NA</td></tr>
    <tr><td>EnvironmentSatisfaction </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>   1</td><td>    4</td><td>2.708253e+00</td><td>   1.1094343</td><td>NA</td></tr>
    <tr><td>Gender                  </td><td>int</td><td>0</td><td>427</td><td>0</td><td>0</td><td>   0</td><td>    1</td><td>5.902111e-01</td><td>   0.4920308</td><td>NA</td></tr>
    <tr><td>HourlyRate              </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>  30</td><td>  100</td><td>6.549328e+01</td><td>  20.3880334</td><td>NA</td></tr>
    <tr><td>JobInvolvement          </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>   1</td><td>    4</td><td>2.729367e+00</td><td>   0.7106176</td><td>NA</td></tr>
    <tr><td>JobLevel                </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>   1</td><td>    5</td><td>2.053743e+00</td><td>   1.1066954</td><td>NA</td></tr>
    <tr><td>JobRole                 </td><td>int</td><td>0</td><td>232</td><td>0</td><td>0</td><td>   0</td><td>    8</td><td>2.518234e+00</td><td>   2.3181516</td><td>NA</td></tr>
    <tr><td>JobSatisfaction         </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>   1</td><td>    4</td><td>2.727447e+00</td><td>   1.0951798</td><td>NA</td></tr>
    <tr><td>MaritalStatus           </td><td>int</td><td>0</td><td>331</td><td>0</td><td>0</td><td>   0</td><td>    2</td><td>8.973129e-01</td><td>   0.7229014</td><td>NA</td></tr>
    <tr><td>MonthlyIncome           </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>1009</td><td>19999</td><td>6.428012e+03</td><td>4646.3237140</td><td>NA</td></tr>
    <tr><td>MonthlyRate             </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>2097</td><td>26999</td><td>1.444260e+04</td><td>7074.4172133</td><td>NA</td></tr>
    <tr><td>NumCompaniesWorked      </td><td>int</td><td>0</td><td>143</td><td>0</td><td>0</td><td>   0</td><td>    9</td><td>2.606526e+00</td><td>   2.4251872</td><td>NA</td></tr>
    <tr><td>OverTime                </td><td>int</td><td>0</td><td>751</td><td>0</td><td>0</td><td>   0</td><td>    1</td><td>2.792706e-01</td><td>   0.4488562</td><td>NA</td></tr>
    <tr><td>PercentSalaryHike       </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>  11</td><td>   25</td><td>1.513244e+01</td><td>   3.6348357</td><td>NA</td></tr>
    <tr><td>PerformanceRating       </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>   3</td><td>    4</td><td>3.146833e+00</td><td>   0.3541093</td><td>NA</td></tr>
    <tr><td>RelationshipSatisfaction</td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>   1</td><td>    4</td><td>2.702495e+00</td><td>   1.0771282</td><td>NA</td></tr>
    <tr><td>StockOptionLevel        </td><td>int</td><td>0</td><td>439</td><td>0</td><td>0</td><td>   0</td><td>    3</td><td>8.119002e-01</td><td>   0.8617903</td><td>NA</td></tr>
    <tr><td>TotalWorkingYears       </td><td>int</td><td>0</td><td>  7</td><td>0</td><td>0</td><td>   0</td><td>   40</td><td>1.122265e+01</td><td>   7.8146360</td><td>NA</td></tr>
    <tr><td>TrainingTimesLastYear   </td><td>int</td><td>0</td><td> 44</td><td>0</td><td>0</td><td>   0</td><td>    6</td><td>2.762956e+00</td><td>   1.2922774</td><td>NA</td></tr>
    <tr><td>WorkLifeBalance         </td><td>int</td><td>0</td><td>  0</td><td>0</td><td>0</td><td>   1</td><td>    4</td><td>2.757198e+00</td><td>   0.7186088</td><td>NA</td></tr>
    <tr><td>YearsAtCompany          </td><td>int</td><td>0</td><td> 29</td><td>0</td><td>0</td><td>   0</td><td>   37</td><td>7.065259e+00</td><td>   6.0897516</td><td>NA</td></tr>
    <tr><td>YearsInCurrentRole      </td><td>int</td><td>0</td><td>167</td><td>0</td><td>0</td><td>   0</td><td>   18</td><td>4.308061e+00</td><td>   3.7143583</td><td>NA</td></tr>
    <tr><td>YearsSinceLastPromotion </td><td>int</td><td>0</td><td>406</td><td>0</td><td>0</td><td>   0</td><td>   15</td><td>2.216891e+00</td><td>   3.2407383</td><td>NA</td></tr>
    <tr><td>YearsWithCurrManager    </td><td>int</td><td>0</td><td>190</td><td>0</td><td>0</td><td>   0</td><td>   17</td><td>4.139155e+00</td><td>   3.6105862</td><td>NA</td></tr>
</tbody>
</table>



A Warning: "AutoML: XGBoost is not available; skipping it."

Because AutoML is unable to build XGBoost models on Windows.


```R
# Establish X and Y (Features and Labels)
y <- "Attrition"
x <- setdiff(names(h2o_train), y)

auto_ml <- h2o.automl(
    # 指定目标变量
    y = y,
    # 指定特征变量
    x = x,
    # 指定用于训练的数据集
    training_frame = h2o_train,
    # 指定用于生成排行榜的数据集
    leaderboard_frame = h2o_validation,
    project_name = "Attribution",
    max_models = 10,
    seed = 12
)

# Check for the top models
top_models <- auto_ml@leaderboard
print(top_models)
```

      |==                                                                    |   3%
    23:37:53.999: AutoML: XGBoost is not available; skipping it.
    23:37:54.78: _response param, We have detected that your response column has only 2 unique values (0/1). If you wish to train a binary model instead of a regression model, convert your target column to categorical before training.
      |====                                                                  |   6%nly 2 unique values (0/1). If you wish to train a binary model instead of a regression model, convert your target column to categorical before training.
    23:37:57.203: _response param, We have detected that your response column has only 2 unique values (0/1). If you wish to train a binary model instead of a regression model, convert your target column to categorical before training.
      |========                                                              |  12%ly 2 unique values (0/1). If you wish to train a binary model instead of a regression model, convert your target column to categorical before training.
    23:38:00.238: _response param, We have detected that your response column has only 2 unique values (0/1). If you wish to train a binary model instead of a regression model, convert your target column to categorical before training.
    23:38:01.154: _response param, We have detected that your response column has only 2 unique values (0/1). If you wish to train a binary model instead of a regression model, convert your target column to categorical before training.
      |==============                                                        |  21%nly 2 unique values (0/1). If you wish to train a binary model instead of a regression model, convert your target column to categorical before training.
    23:38:04.188: _response param, We have detected that your response column has only 2 unique values (0/1). If you wish to train a binary model instead of a regression model, convert your target column to categorical before training.
      |=================================                                     |  47%nly 2 unique values (0/1). If you wish to train a binary model instead of a regression model, convert your target column to categorical before training.
    23:38:06.791: _response param, We have detected that your response column has only 2 unique values (0/1). If you wish to train a binary model instead of a regression model, convert your target column to categorical before training.
      |======================================================================| 100%nly 2 unique values (0/1). If you wish to train a binary model instead of a regression model, convert your target column to categorical before training.
                                                     model_id      rmse       mse
    1 StackedEnsemble_BestOfFamily_1_AutoML_1_20231121_233753 0.3210262 0.1030578
    2    StackedEnsemble_AllModels_1_AutoML_1_20231121_233753 0.3216969 0.1034889
    3                 DeepLearning_1_AutoML_1_20231121_233753 0.3217492 0.1035226
    4                          DRF_1_AutoML_1_20231121_233753 0.3279511 0.1075519
    5                          GBM_4_AutoML_1_20231121_233753 0.3316593 0.1099979
    6             GBM_grid_1_AutoML_1_20231121_233753_model_1 0.3323328 0.1104451
            mae     rmsle mean_residual_deviance
    1 0.2188025 0.2227513              0.1030578
    2 0.2166384 0.2232295              0.1034889
    3 0.2105720 0.2232538              0.1035226
    4 0.2248094 0.2291632              0.1075519
    5 0.2242110 0.2289432              0.1099979
    6 0.2268220 0.2327375              0.1104451
    
    [12 rows x 6 columns] 
    


```R
# Get the best model
# Our aim is to determine the feature importance
model_id <- as.data.frame(top_models$model_id)[,1]
best_family <- h2o.getModel(grep("StackedEnsemble_BestOfFamily", model_id, value=TRUE)[1])
obtain_model <- h2o.getModel(best_family@model$metalearner$name)
```


```R
# How important is each model to the StackEnsemble
h2o.varimp(obtain_model)
```


<table class="dataframe">
<caption>A H2OTable: 5 × 4</caption>
<thead>
    <tr><th scope=col>variable</th><th scope=col>relative_importance</th><th scope=col>scaled_importance</th><th scope=col>percentage</th></tr>
    <tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
    <tr><td>GBM_4_AutoML_1_20231121_233753         </td><td>0.055622578</td><td>1.0000000</td><td>0.27299205</td></tr>
    <tr><td>GLM_1_AutoML_1_20231121_233753         </td><td>0.053264495</td><td>0.9576057</td><td>0.26141873</td></tr>
    <tr><td>DRF_1_AutoML_1_20231121_233753         </td><td>0.049367692</td><td>0.8875477</td><td>0.24229347</td></tr>
    <tr><td>DeepLearning_1_AutoML_1_20231121_233753</td><td>0.035710063</td><td>0.6420066</td><td>0.17526270</td></tr>
    <tr><td>XRT_1_AutoML_1_20231121_233753         </td><td>0.009786812</td><td>0.1759503</td><td>0.04803304</td></tr>
</tbody>
</table>



The model importance ranking is shown, and the top three ranked models (GLM, GBM, DRF) will be used next for feature importance analysis.


```R
xgb <- h2o.getModel(grep("GBM", model_id, value = TRUE)[1])

# Examine the variable importance of the top XGBoost model
# XGBoost can show the feature importance as oppose to the stack ensemble
h2o.varimp(xgb)

# We can also plot the base learner contributions to the ensemble.
par(family=font)
h2o.varimp_plot(xgb)
```


<table class="dataframe">
<caption>A H2OTable: 31 × 4</caption>
<thead>
    <tr><th scope=col>variable</th><th scope=col>relative_importance</th><th scope=col>scaled_importance</th><th scope=col>percentage</th></tr>
    <tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
    <tr><td>MonthlyIncome           </td><td>58.647167</td><td>1.00000000</td><td>0.099727934</td></tr>
    <tr><td>OverTime                </td><td>44.447372</td><td>0.75787757</td><td>0.075581564</td></tr>
    <tr><td>Age                     </td><td>37.004238</td><td>0.63096378</td><td>0.062924714</td></tr>
    <tr><td>DailyRate               </td><td>29.859972</td><td>0.50914602</td><td>0.050776081</td></tr>
    <tr><td>MaritalStatus           </td><td>28.639126</td><td>0.48832923</td><td>0.048700065</td></tr>
    <tr><td>MonthlyRate             </td><td>25.045486</td><td>0.42705364</td><td>0.042589178</td></tr>
    <tr><td>EnvironmentSatisfaction </td><td>24.644251</td><td>0.42021213</td><td>0.041906887</td></tr>
    <tr><td>NumCompaniesWorked      </td><td>22.847263</td><td>0.38957147</td><td>0.038851158</td></tr>
    <tr><td>JobSatisfaction         </td><td>22.775900</td><td>0.38835465</td><td>0.038729807</td></tr>
    <tr><td>HourlyRate              </td><td>21.006285</td><td>0.35818072</td><td>0.035720624</td></tr>
    <tr><td>YearsWithCurrManager    </td><td>20.197224</td><td>0.34438532</td><td>0.034344837</td></tr>
    <tr><td>TotalWorkingYears       </td><td>19.666626</td><td>0.33533804</td><td>0.033442570</td></tr>
    <tr><td>StockOptionLevel        </td><td>19.543367</td><td>0.33323634</td><td>0.033232972</td></tr>
    <tr><td>DistanceFromHome        </td><td>19.226494</td><td>0.32783329</td><td>0.032694137</td></tr>
    <tr><td>YearsAtCompany          </td><td>18.560036</td><td>0.31646943</td><td>0.031560843</td></tr>
    <tr><td>YearsSinceLastPromotion </td><td>17.884302</td><td>0.30494742</td><td>0.030411776</td></tr>
    <tr><td>YearsInCurrentRole      </td><td>17.537079</td><td>0.29902687</td><td>0.029821332</td></tr>
    <tr><td>Department              </td><td>17.401680</td><td>0.29671817</td><td>0.029591090</td></tr>
    <tr><td>PercentSalaryHike       </td><td>16.783663</td><td>0.28618028</td><td>0.028540168</td></tr>
    <tr><td>EmployeeNumber          </td><td>15.172945</td><td>0.25871574</td><td>0.025801186</td></tr>
    <tr><td>JobRole                 </td><td>13.863920</td><td>0.23639539</td><td>0.023575224</td></tr>
    <tr><td>BusinessTravel          </td><td>12.105301</td><td>0.20640896</td><td>0.020584739</td></tr>
    <tr><td>EducationField          </td><td>11.981455</td><td>0.20429725</td><td>0.020374142</td></tr>
    <tr><td>TrainingTimesLastYear   </td><td> 9.942945</td><td>0.16953838</td><td>0.016907712</td></tr>
    <tr><td>JobLevel                </td><td> 9.924556</td><td>0.16922481</td><td>0.016876441</td></tr>
    <tr><td>JobInvolvement          </td><td> 9.373492</td><td>0.15982856</td><td>0.015939372</td></tr>
    <tr><td>WorkLifeBalance         </td><td> 7.519458</td><td>0.12821520</td><td>0.012786637</td></tr>
    <tr><td>RelationshipSatisfaction</td><td> 6.412500</td><td>0.10934033</td><td>0.010904285</td></tr>
    <tr><td>Education               </td><td> 5.568669</td><td>0.09495205</td><td>0.009469372</td></tr>
    <tr><td>Gender                  </td><td> 4.488841</td><td>0.07653977</td><td>0.007633153</td></tr>
    <tr><td>PerformanceRating       </td><td> 0.000000</td><td>0.00000000</td><td>0.000000000</td></tr>
</tbody>
</table>




    
![png](pic/h2o_GBM.png)
    



```R
xgb <- h2o.getModel(grep("DRF", model_id, value = TRUE)[1])

# Examine the variable importance of the top XGBoost model
# XGBoost can show the feature importance as oppose to the stack ensemble
h2o.varimp(xgb)

# We can also plot the base learner contributions to the ensemble.
par(family=font)
h2o.varimp_plot(xgb)
```


<table class="dataframe">
<caption>A H2OTable: 31 × 4</caption>
<thead>
    <tr><th scope=col>variable</th><th scope=col>relative_importance</th><th scope=col>scaled_importance</th><th scope=col>percentage</th></tr>
    <tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
    <tr><td>OverTime                </td><td>249.612961</td><td>1.00000000</td><td>0.075939889</td></tr>
    <tr><td>MonthlyIncome           </td><td>219.615662</td><td>0.87982475</td><td>0.066813794</td></tr>
    <tr><td>Age                     </td><td>200.710754</td><td>0.80408787</td><td>0.061062344</td></tr>
    <tr><td>DailyRate               </td><td>167.691483</td><td>0.67180599</td><td>0.051016872</td></tr>
    <tr><td>MonthlyRate             </td><td>150.094620</td><td>0.60130940</td><td>0.045663369</td></tr>
    <tr><td>YearsAtCompany          </td><td>141.339081</td><td>0.56623294</td><td>0.042999667</td></tr>
    <tr><td>TotalWorkingYears       </td><td>135.195480</td><td>0.54162043</td><td>0.041130596</td></tr>
    <tr><td>DistanceFromHome        </td><td>133.649551</td><td>0.53542713</td><td>0.040660277</td></tr>
    <tr><td>HourlyRate              </td><td>132.320740</td><td>0.53010364</td><td>0.040256012</td></tr>
    <tr><td>NumCompaniesWorked      </td><td>121.135658</td><td>0.48529394</td><td>0.036853168</td></tr>
    <tr><td>MaritalStatus           </td><td>118.674751</td><td>0.47543505</td><td>0.036104485</td></tr>
    <tr><td>EmployeeNumber          </td><td>111.937408</td><td>0.44844390</td><td>0.034054780</td></tr>
    <tr><td>PercentSalaryHike       </td><td>110.396973</td><td>0.44227260</td><td>0.033586132</td></tr>
    <tr><td>StockOptionLevel        </td><td>108.019821</td><td>0.43274925</td><td>0.032862930</td></tr>
    <tr><td>YearsWithCurrManager    </td><td> 98.802956</td><td>0.39582462</td><td>0.030058878</td></tr>
    <tr><td>YearsInCurrentRole      </td><td> 98.256874</td><td>0.39363691</td><td>0.029892743</td></tr>
    <tr><td>YearsSinceLastPromotion </td><td> 98.189308</td><td>0.39336623</td><td>0.029872188</td></tr>
    <tr><td>EnvironmentSatisfaction </td><td> 97.886963</td><td>0.39215497</td><td>0.029780205</td></tr>
    <tr><td>JobRole                 </td><td> 97.289917</td><td>0.38976308</td><td>0.029598565</td></tr>
    <tr><td>JobInvolvement          </td><td> 82.291710</td><td>0.32967723</td><td>0.025035652</td></tr>
    <tr><td>WorkLifeBalance         </td><td> 82.014610</td><td>0.32856711</td><td>0.024951350</td></tr>
    <tr><td>JobSatisfaction         </td><td> 77.473228</td><td>0.31037342</td><td>0.023569723</td></tr>
    <tr><td>RelationshipSatisfaction</td><td> 70.713516</td><td>0.28329265</td><td>0.021513212</td></tr>
    <tr><td>BusinessTravel          </td><td> 70.592636</td><td>0.28280838</td><td>0.021476437</td></tr>
    <tr><td>Department              </td><td> 64.073570</td><td>0.25669168</td><td>0.019493138</td></tr>
    <tr><td>Education               </td><td> 63.344151</td><td>0.25376948</td><td>0.019271226</td></tr>
    <tr><td>TrainingTimesLastYear   </td><td> 62.551231</td><td>0.25059288</td><td>0.019029996</td></tr>
    <tr><td>JobLevel                </td><td> 51.189217</td><td>0.20507435</td><td>0.015573324</td></tr>
    <tr><td>EducationField          </td><td> 49.190155</td><td>0.19706571</td><td>0.014965148</td></tr>
    <tr><td>Gender                  </td><td> 12.799197</td><td>0.05127617</td><td>0.003893907</td></tr>
    <tr><td>PerformanceRating       </td><td>  9.926663</td><td>0.03976822</td><td>0.003019994</td></tr>
</tbody>
</table>




    
![png](pic/h2o_DRF.png)
    



```R
xgb <- h2o.getModel(grep("GLM", model_id, value = TRUE)[1])

# Examine the variable importance of the top XGBoost model
# XGBoost can show the feature importance as oppose to the stack ensemble
h2o.varimp(xgb)

# We can also plot the base learner contributions to the ensemble.
par(family=font)
h2o.varimp_plot(xgb)
```


<table class="dataframe">
<caption>A H2OTable: 31 × 4</caption>
<thead>
    <tr><th scope=col>variable</th><th scope=col>relative_importance</th><th scope=col>scaled_importance</th><th scope=col>percentage</th></tr>
    <tr><th scope=col>&lt;chr&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th><th scope=col>&lt;dbl&gt;</th></tr>
</thead>
<tbody>
    <tr><td>OverTime                </td><td>8.685281e-02</td><td>1.0000000000</td><td>0.1312032664</td></tr>
    <tr><td>BusinessTravel          </td><td>4.396939e-02</td><td>0.5062517655</td><td>0.0664218853</td></tr>
    <tr><td>NumCompaniesWorked      </td><td>4.366436e-02</td><td>0.5027397261</td><td>0.0659610942</td></tr>
    <tr><td>MaritalStatus           </td><td>4.096025e-02</td><td>0.4716053281</td><td>0.0618761595</td></tr>
    <tr><td>Age                     </td><td>3.883078e-02</td><td>0.4470871489</td><td>0.0586592943</td></tr>
    <tr><td>JobSatisfaction         </td><td>3.691411e-02</td><td>0.4250192178</td><td>0.0557639097</td></tr>
    <tr><td>Department              </td><td>3.515900e-02</td><td>0.4048112551</td><td>0.0531125589</td></tr>
    <tr><td>JobInvolvement          </td><td>3.515749e-02</td><td>0.4047939267</td><td>0.0531102854</td></tr>
    <tr><td>EnvironmentSatisfaction </td><td>3.375427e-02</td><td>0.3886375847</td><td>0.0509905206</td></tr>
    <tr><td>YearsSinceLastPromotion </td><td>3.332363e-02</td><td>0.3836793559</td><td>0.0503399848</td></tr>
    <tr><td>YearsInCurrentRole      </td><td>2.400944e-02</td><td>0.2764383037</td><td>0.0362696084</td></tr>
    <tr><td>WorkLifeBalance         </td><td>2.369764e-02</td><td>0.2728482437</td><td>0.0357985808</td></tr>
    <tr><td>YearsWithCurrManager    </td><td>2.177465e-02</td><td>0.2507074391</td><td>0.0328936349</td></tr>
    <tr><td>DistanceFromHome        </td><td>1.994589e-02</td><td>0.2296516518</td><td>0.0301310469</td></tr>
    <tr><td>MonthlyIncome           </td><td>1.773381e-02</td><td>0.2041823855</td><td>0.0267893959</td></tr>
    <tr><td>TotalWorkingYears       </td><td>1.715916e-02</td><td>0.1975659308</td><td>0.0259212955</td></tr>
    <tr><td>StockOptionLevel        </td><td>1.578641e-02</td><td>0.1817605525</td><td>0.0238475782</td></tr>
    <tr><td>DailyRate               </td><td>1.473849e-02</td><td>0.1696949702</td><td>0.0222645344</td></tr>
    <tr><td>RelationshipSatisfaction</td><td>1.289405e-02</td><td>0.1484586397</td><td>0.0194782585</td></tr>
    <tr><td>JobLevel                </td><td>1.222306e-02</td><td>0.1407330087</td><td>0.0184646304</td></tr>
    <tr><td>Gender                  </td><td>9.848037e-03</td><td>0.1133876573</td><td>0.0148768310</td></tr>
    <tr><td>YearsAtCompany          </td><td>9.411927e-03</td><td>0.1083664095</td><td>0.0142180269</td></tr>
    <tr><td>EmployeeNumber          </td><td>9.205841e-03</td><td>0.1059935922</td><td>0.0139067055</td></tr>
    <tr><td>TrainingTimesLastYear   </td><td>8.363591e-03</td><td>0.0962961412</td><td>0.0126343683</td></tr>
    <tr><td>Education               </td><td>5.920730e-03</td><td>0.0681696944</td><td>0.0089440866</td></tr>
    <tr><td>HourlyRate              </td><td>3.875905e-03</td><td>0.0446261285</td><td>0.0058550938</td></tr>
    <tr><td>PerformanceRating       </td><td>2.341583e-03</td><td>0.0269603556</td><td>0.0035372867</td></tr>
    <tr><td>JobRole                 </td><td>2.067334e-03</td><td>0.0238027354</td><td>0.0031229966</td></tr>
    <tr><td>PercentSalaryHike       </td><td>1.232610e-03</td><td>0.0141919369</td><td>0.0018620285</td></tr>
    <tr><td>MonthlyRate             </td><td>1.077840e-03</td><td>0.0124099568</td><td>0.0016282269</td></tr>
    <tr><td>EducationField          </td><td>7.733564e-05</td><td>0.0008904218</td><td>0.0001168262</td></tr>
</tbody>
</table>


    
![png](pic/h2o_GLM.png)
    


Combining the results of the models, the factors that most affect employee turnover are overtime, monthly income, and age.

# Analyze important factors
1. Important factors -> Attrition
2. Basic factors -> Important factors
3. Correlation coefficient analysis

## Important factors -> Attrition
1. Overtime work
2. Monthly income

### Overtime work -> Attrition


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_overwork_attrition <- 
  df_zh %>%
  select(是否加班, 流失情况) %>% 
  group_by(是否加班, 流失情况) %>% 
  summarize(n=n()) %>% 
    ggplot(aes(x=是否加班, y=n, fill = 流失情况)) +
    geom_bar(stat="identity", color="black", size=.2, width=.6, position="stack") +
    geom_label(aes(label=n, y=n), 
               color = "black", family = font) +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y) +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10)) +
    # 不显示图例
    theme(legend.position="none")
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_overwork_attrition_pct <- 
  df_zh %>%
  select(是否加班, 流失情况) %>% 
  group_by(是否加班, 流失情况) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% arrange(desc(pct)) %>%
    ggplot(aes(x=fct_reorder(是否加班,pct), y=pct, fill=流失情况, color=流失情况)) + 
    geom_bar(stat="identity", color="black", size=.2, width=.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 流失情况), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per, x="是否加班") +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))]
```


```R
options(repr.plot.width=8, repr.plot.height=4) 

plot_grid(p_overwork_attrition, p_overwork_attrition_pct, ncol=2)
```

![png](pic/overwork_attrition.png)
    

### Monthly income -> Attrition


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_income_attrition <- 
  df_zh %>%
    ggplot(aes(x = 月收入, fill = 流失情况, group = 流失情况)) +
    geom_density(alpha=.6, size=.3, position="fill") +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per) +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10)) 

p_income_attrition
```


    
![png](pic/p_income_attrition.png)
    


## Basic factors -> Important factors
1. Demographic characteristics
2. Department and job role

### Demographic characteristics -> Overtime work


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_sex_overwork <- 
  df_zh %>%
  select(性别, 是否加班) %>% 
  group_by(性别, 是否加班) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% 
  arrange(desc(pct)) %>%
    ggplot(aes(x=fct_reorder(性别,pct), y=pct, fill=是否加班, color=是否加班)) + 
    geom_bar(stat="identity", color="black", size=.1, width=.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 是否加班), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    coord_flip() +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per, x="性别") +
    # 填充颜色
    scale_fill_manual(values=c(yellow, brown))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10)) +
    # 不显示图例
    theme(legend.position="none")
```



```R
options(repr.plot.width=6, repr.plot.height=4) 

p_age_overwork <- 
  df_zh %>%
  select(代际, 是否加班) %>% 
  group_by(代际, 是否加班) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% 
  arrange(desc(pct)) %>%
    ggplot(aes(x=fct_reorder(代际,pct), y=pct, fill=是否加班, color=是否加班)) + 
    geom_bar(stat="identity", color="black", size=.1, width=.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 是否加班), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    coord_flip() +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per, x="代际") +
    # 填充颜色
    scale_fill_manual(values=c(yellow, brown))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_marry_overwork <- 
  df_zh %>%
  select(婚姻状态, 是否加班) %>% 
  group_by(婚姻状态, 是否加班) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% 
  arrange(desc(pct)) %>%
    ggplot(aes(x=fct_reorder(婚姻状态,pct), y=pct, fill=是否加班, color=是否加班)) + 
    geom_bar(stat="identity", color="black", size=.1, width=.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 是否加班), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    coord_flip() +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per, x="婚姻状态") +
    # 填充颜色
    scale_fill_manual(values=c(yellow, brown))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10)) +
    # 不显示图例
    theme(legend.position="none")
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_edu_overwork <- 
  df_zh %>%
  select(教育水平_文字, 是否加班) %>% 
  group_by(教育水平_文字, 是否加班) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% 
  arrange(desc(pct)) %>%
    ggplot(aes(x=fct_reorder(教育水平_文字,pct), y=pct, fill=是否加班, color=是否加班)) + 
    geom_bar(stat="identity", color="black", size=.1, width=.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 是否加班), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    coord_flip() +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per, x="教育水平") +
    # 填充颜色
    scale_fill_manual(values=c(yellow, brown))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```


```R
options(repr.plot.width=8, repr.plot.height=4) 

plot_grid(p_sex_overwork, p_age_overwork, p_marry_overwork, p_edu_overwork, ncol=2, nrow=2)
```

    
![png](pic/basic_overwork.png)
    


### Demographic characteristics -> Monthly income


```R
options(repr.plot.width=8, repr.plot.height=8) 

p_income_sex_attrition <- df_zh %>% 
select(流失情况, 月收入, 性别) %>% 
ggplot(aes(x=月收入, y=性别)) + 
geom_jitter(aes(col=流失情况), alpha=0.5) + 
scale_color_manual(values=c("#a1dfc3", darkred)) + 
labs(x="月收入（$）") + 
theme_minimal() + 
theme(text = element_text(family = font, size=11),
      panel.border = element_rect(color = "black", fill = NA, size = 0.3),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12)) +
# 不显示图例
theme(legend.position="none")
```


```R
options(repr.plot.width=8, repr.plot.height=8) 

p_income_age_attrition <- df_zh %>% 
select(流失情况, 月收入, 代际) %>% 
ggplot(aes(x=月收入, y=代际)) + 
geom_jitter(aes(col=流失情况), alpha=0.5) + 
scale_color_manual(values=c("#a1dfc3", darkred)) + 
labs(x="月收入（$）") + 
theme_minimal() + 
theme(text = element_text(family = font, size=11),
      panel.border = element_rect(color = "black", fill = NA, size = 0.3),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12))
```


```R
options(repr.plot.width=8, repr.plot.height=8) 

p_income_marry_attrition <- df_zh %>% 
select(流失情况, 月收入, 婚姻状态) %>% 
ggplot(aes(x=月收入, y=婚姻状态)) + 
geom_jitter(aes(col=流失情况), alpha=0.5) + 
scale_color_manual(values=c("#a1dfc3", darkred)) + 
labs(x="月收入（$）") + 
theme_minimal() + 
theme(text = element_text(family = font, size=11),
      panel.border = element_rect(color = "black", fill = NA, size = 0.3),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12)) +
# 不显示图例
theme(legend.position="none")
```


```R
options(repr.plot.width=8, repr.plot.height=8) 

p_income_edu_attrition <- df_zh %>% 
select(流失情况, 月收入, 教育水平_文字) %>% 
ggplot(aes(x=月收入, y=教育水平_文字)) + 
geom_jitter(aes(col=流失情况), alpha=0.5) + 
scale_color_manual(values=c("#a1dfc3", darkred)) + 
labs(x="月收入（$）", y="教育水平") + 
theme_minimal() + 
theme(text = element_text(family = font, size=11),
      panel.border = element_rect(color = "black", fill = NA, size = 0.3),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12))
```


```R
options(repr.plot.width=16, repr.plot.height=16) 

plot_grid(p_income_sex_attrition, p_income_age_attrition, p_income_marry_attrition, p_income_edu_attrition, ncol=2, nrow=2)
```

    
![png](pic/income_basic_attrition.png)
    


### Department and job role -> Overtime work


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_depart_overwork <- 
  df_zh %>%
  select(所属部门, 是否加班) %>% 
  group_by(所属部门, 是否加班) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% 
  arrange(desc(pct)) %>%
    ggplot(aes(x=fct_reorder(所属部门,pct), y=pct, fill=是否加班, color=是否加班)) + 
    geom_bar(stat="identity", color="black", size=.1, width=.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 是否加班), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    coord_flip() +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per, x="部门") +
    # 填充颜色
    scale_fill_manual(values=c(yellow, brown))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10)) +
    # 不显示图例
    theme(legend.position="none")
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_job_overwork <- 
  df_zh %>%
  select(岗位名称, 是否加班) %>% 
  group_by(岗位名称, 是否加班) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% 
  arrange(desc(pct)) %>%
    ggplot(aes(x=fct_reorder(岗位名称,pct), y=pct, fill=是否加班, color=是否加班)) + 
    geom_bar(stat="identity", color="black", size=.1, width=.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 是否加班), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    coord_flip() +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per, x="岗位") +
    # 填充颜色
    scale_fill_manual(values=c(yellow, brown))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```

```R
options(repr.plot.width=8, repr.plot.height=4) 

plot_grid(p_depart_overwork, p_job_overwork, ncol=2)
```

    
![png](pic/dp&job_overwork.png)
    


### Department and job role -> Monthly income


```R
options(repr.plot.width=8, repr.plot.height=8) 

p_depart_income_attrition <- df_zh %>% 
select(流失情况, 所属部门, 月收入) %>% 
ggplot(aes(x=月收入, y=所属部门)) + 
geom_jitter(aes(col=流失情况), alpha=0.5) + 
scale_color_manual(values=c("#a1dfc3", darkred)) + 
labs(y="所属部门", x="月收入（$）") + 
theme_minimal() + 
theme(text = element_text(family = font, size=11),
      panel.border = element_rect(color = "black", fill = NA, size = 0.3),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12)) +
# 不显示图例
theme(legend.position="none")
```


```R
options(repr.plot.width=8, repr.plot.height=8) 

p_job_income_attrition <- df_zh %>% 
select(流失情况, 岗位名称, 月收入) %>% 
ggplot(aes(x=月收入, y=岗位名称)) + 
geom_jitter(aes(col=流失情况), alpha=0.5) + 
scale_color_manual(values=c("#a1dfc3", darkred)) + 
labs(y="岗位", x="月收入（$）") + 
theme_minimal() + 
theme(text = element_text(family = font, size=11),
      panel.border = element_rect(color = "black", fill = NA, size = 0.3),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12)) +
# 不显示图例
theme(legend.position="none")
```


```R
options(repr.plot.width=16, repr.plot.height=8) 

plot_grid(p_depart_income_attrition, p_job_income_attrition, ncol=2)
```


    
![png](pic/dp&job_income_attrition.png)
    


## Correlation coefficient analysis
1. Draw the correlation matrix
2. Bivariate analysis

### Draw the correlation matrix


```R
options(repr.plot.width=13, repr.plot.height=10) 
# 筛选数值列
nums <- select_if(df_train, is.numeric)
#计算相关系数
corr <- round(cor(nums), 1)
# 记录显著差异
p.mat <- cor_pmat(nums)
```


```R
options(repr.plot.width=10, repr.plot.height=7) 

nums <- select_if(df_zh, is.numeric)

corr <- round(cor(nums), 1)

ggcorrplot(corr, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("#95001c", "white", "#1c642d"),
           ggtheme=theme_minimal()) + 
theme(text = element_text(family = font, size=10))
```

    
![png](pic/correlation.png)
    


### Bivariate analysis


```R
options(repr.plot.width=6, repr.plot.height=4) 

    p_allwy_income <- df_zh %>% 
    ggplot(aes(x=总工作年数, y=月收入)) + 
    geom_point(color = "#c4e3ba", alpha=1/2) +
    geom_smooth(method="loess", color="red") + 
theme_minimal() +
theme(# 全图字体&字号
      text = element_text(family = font, size=11),
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_age_income <- df_zh %>% 
ggplot(aes(x=年龄, y=月收入)) + 
geom_point(color = "#c4e3ba", alpha=1/2) +
geom_smooth(method="loess", color="red") + 
theme_minimal() +
theme(# 全图字体&字号
      text = element_text(family = font, size=11),
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```



```R
options(repr.plot.width=6, repr.plot.height=4) 

p_companywy_income <- df_zh %>% 
ggplot(aes(x=在当前公司的工作年数, y=月收入)) + 
geom_point(color = "#c4e3ba", alpha=1/2) +
geom_smooth(method="loess", color="red") + 
theme_minimal() +
theme(# 全图字体&字号
      text = element_text(family = font, size=11),
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```
    

```R
options(repr.plot.width=6, repr.plot.height=4) 

p_jobwy_income <- df_zh %>% 
ggplot(aes(x=在当前职位的工作年数, y=月收入)) + 
geom_point(color = "#c4e3ba", alpha=1/2) +
geom_smooth(method="loess", color="red") + 
theme_minimal() +
theme(# 全图字体&字号
      text = element_text(family = font, size=11),
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```


```R
options(repr.plot.width=22, repr.plot.height=8) 

plot_grid(p_allwy_income, p_age_income, p_companywy_income, p_jobwy_income, ncol=2, nrow=2)
```
    
![png](pic/workyear_income.png)
    


# Satisfaction analysis
1. Satisfaction -> Attrition
2. Department and job role -> Satisfaction

## Satisfaction -> Attrition


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_environ_attrition <- 
  df_zh %>%
  select(环境满意度, 流失情况) %>% 
  group_by(环境满意度, 流失情况) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% 
  arrange(desc(pct)) %>%
    ggplot(aes(x=环境满意度, y=pct, fill=流失情况, color=流失情况)) + 
    geom_bar(stat="identity", color="black", size=.1, width=.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 流失情况), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    coord_flip() +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per) +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10)) +
    # 不显示图例
    theme(legend.position="none")
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_relation_attrition <- 
  df_zh %>%
  select(人际关系满意度, 流失情况) %>% 
  group_by(人际关系满意度, 流失情况) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% 
  arrange(desc(pct)) %>%
    ggplot(aes(x=人际关系满意度, y=pct, fill=流失情况, color=流失情况)) + 
    geom_bar(stat="identity", color="black", size=.1, width=.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 流失情况), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    coord_flip() +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per) +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10)) +
    # 不显示图例
    theme(legend.position="none")
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_worksatis_attrition <- 
  df_zh %>%
  select(工作满意度, 流失情况) %>% 
  group_by(工作满意度, 流失情况) %>% 
  summarize(n=n()) %>% 
  mutate(pct=round(prop.table(n),2) * 100) %>% 
  arrange(desc(pct)) %>%
    ggplot(aes(x=工作满意度, y=pct, fill=流失情况, color=流失情况)) + 
    geom_bar(stat="identity", color="black", size=.1, width=.6, position="fill") +
    geom_label(aes(label=paste0(pct, "%"), fill = 流失情况), 
               color = "black", family = font, 
               position = position_fill(vjust = 0.5)) + 
    coord_flip() +
    theme_minimal() +
    # 坐标轴标题
    labs(y=st_y_per) +
    # 填充颜色
    scale_fill_manual(values=c(red, green))+
    # 全图字体&字号
    theme(text = element_text(family = font, size=11)) +
    # 坐标轴外观
    theme(
      # 坐标轴标题距离其他元素间距&字号
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      # 坐标轴刻度字号
      axis.text = element_text(size = 10))
```



```R
options(repr.plot.width=15, repr.plot.height=4) 

plot_grid(p_environ_attrition, p_relation_attrition, p_worksatis_attrition, ncol=3)
```

![png](pic/satisf_attrition.png)
    


## Department and job role -> Satisfaction


```R
df_zh <- original_df_zh
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_depart_environ <- df_zh %>% 
ggplot(aes(x=所属部门, group=环境满意度, fill=环境满意度)) +
geom_density(adjust=1.5, position="fill", size=0.5) + 
# 颜色集
scale_fill_viridis(alpha=0.8) +
theme_minimal() + 
# 设置图例顺序
guides(fill = guide_legend(reverse=FALSE)) +
labs(y=st_y_per) +
theme(text = element_text(family = font, size=11),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12)) +
# 不显示图例
theme(legend.position="none") 
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_depart_relation <- df_zh %>% 
ggplot(aes(x=所属部门, group=人际关系满意度, fill=人际关系满意度)) +
geom_density(adjust=1.5, position="fill", size=0.5) + 
# 颜色集
scale_fill_viridis(alpha=0.8) +
theme_minimal() + 
# 设置图例顺序
guides(fill = guide_legend(reverse=FALSE)) +
labs(y=st_y_per) +
theme(text = element_text(family = font, size=11),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12)) +
# 不显示图例
theme(legend.position="none") 
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_depart_worksatif <- df_zh %>% 
ggplot(aes(x=所属部门, group=工作满意度, fill=工作满意度)) +
geom_density(adjust=1.5, position="fill", size=0.5) + 
# 颜色集
scale_fill_viridis(alpha=0.8) +
theme_minimal() + 
# 设置图例顺序
guides(fill = guide_legend(reverse=FALSE)) +
labs(y=st_y_per) +
theme(text = element_text(family = font, size=11),
      axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12)) +
# 不显示图例
theme(legend.position="none") 
```



```R
options(repr.plot.width=6, repr.plot.height=4) 

p_job_environ <- df_zh %>% 
ggplot(aes(x=岗位名称, group=环境满意度, fill=环境满意度)) +
geom_density(adjust=1.5, position="fill", size=0.5) + 
# 颜色集
scale_fill_viridis(alpha=0.8) +
theme_minimal() + 
# 设置图例顺序
guides(fill = guide_legend(reverse=FALSE)) +
labs(y=st_y_per) +
theme(text = element_text(family = font, size=11),
      axis.title.x = element_text(margin = margin(t = -10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), angle=45)) +
theme(plot.margin = margin(t = 0, r = .5, b = 0, l = 0, unit = "cm"))
```

```R
options(repr.plot.width=6, repr.plot.height=4) 

p_job_relation <- df_zh %>% 
ggplot(aes(x=岗位名称, group=人际关系满意度, fill=人际关系满意度)) +
geom_density(adjust=1.5, position="fill", size=0.5) + 
# 颜色集
scale_fill_viridis(alpha=0.8) +
theme_minimal() + 
# 设置图例顺序
guides(fill = guide_legend(reverse=FALSE)) +
labs(y=st_y_per) +
theme(text = element_text(family = font, size=11),
      axis.title.x = element_text(margin = margin(t = -10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), angle=45)) +
theme(plot.margin = margin(t = 0, r = .5, b = 0, l = 0, unit = "cm"))
```


```R
options(repr.plot.width=6, repr.plot.height=4) 

p_job_worksatif <- df_zh %>% 
ggplot(aes(x=岗位名称, group=工作满意度, fill=工作满意度)) +
geom_density(adjust=1.5, position="fill", size=0.5) + 
# 颜色集
scale_fill_viridis(alpha=0.8) +
theme_minimal() + 
# 设置图例顺序
guides(fill = guide_legend(reverse=FALSE)) +
labs(y=st_y_per) +
theme(text = element_text(family = font, size=11),
      axis.title.x = element_text(margin = margin(t = -10, r = 0, b = 0, l = 0), size=12), 
      axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size=12),
      axis.text.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0), angle=45)) +
theme(plot.margin = margin(t = 0, r = .5, b = 0, l = 0, unit = "cm"))
```


```R
options(repr.plot.width=13, repr.plot.height=13) 

plot_grid(p_depart_environ, p_depart_relation, p_depart_worksatif, p_job_environ, p_job_relation, p_job_worksatif, ncol=2, nrow=3)
```

    
![png](pic/dp&job_satisf.png)
    


# Conclusion

The factors with the most significant impact on turnover are overtime, income and age, with satisfaction and total years of service being the next most influential factors. Surprisingly, age has a very significant effect on turnover, yet gender has almost no effect on turnover for the same underlying demographic factors. For the other demographic characteristics, marital status has an insignificant level of influence, and relatively low levels of education are more likely to cause attrition. Department and position have some effect, but not to a significant degree. Despite the significant effect of income, performance ratings have little effect on attrition. In terms of work status, only environmental satisfaction had a more significant effect on turnover for the three satisfaction scores, and work-life balance had a non-significant effect.


The above findings suggest the following management insights for managers to help reduce employee attrition:
1. Overtime Situation Management: Reduce excessive overtime work and provide appropriate working hours and workload management to avoid employee fatigue and dissatisfaction due to excessive overtime work.
2. Income Management: Ensure that employees are paid at a reasonable level and provide competitive remuneration packages to attract and retain good employees.
3. Age management: Emphasize the age characteristics of employees and provide development opportunities and benefits that suit the needs of employees of different age groups to enhance their sense of belonging and job satisfaction.
4. Satisfaction management: Pay attention to the job satisfaction of employees, and enhance their satisfaction and work motivation by providing good working environment, training and development opportunities.
5. total working years management: pay attention to employees' work experience and development, provide them with promotion opportunities, career development planning, etc. in order to increase their job stability and loyalty.
6. Management of other demographic characteristics: Although gender and marital status have less impact on employee turnover, it is still necessary to pay attention to individual needs and equal treatment of employees to avoid unfairness caused by demographic factors.
7. Department and position management: Pay attention to the characteristics of departments and positions, and provide employees with appropriate resource support and development opportunities to enhance their sense of belonging and job satisfaction.
8. Performance Rating Management: Emphasize the fairness and accuracy of performance ratings to ensure that performance ratings are in line with the actual performance of employees and to avoid the negative impact of performance ratings on employee turnover.
9. Work-life balance management: Pay attention to the work-life balance of employees and provide flexible work arrangements and support measures to reduce employee turnover due to excessive work pressure.
