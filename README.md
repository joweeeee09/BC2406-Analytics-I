# BC2406-Analytics-I: Enhancing Workplace Safety in the Oil Pipeline Sector: A Data-Driven Approach for Saudi Aramco

Saudi Aramco, a global industry leader, is committed to enhancing workplace safety amid rising global workplace accidents. Despite existing safety measures, the company is exploring advanced data analytics to identify and mitigate factors contributing to high accident rates in the oil pipeline sector. This project, conducted as part of the Nanyang Business School Coursework, serves as a Proof of Concept for Aramco.

## Objectives
This project leverages advanced data analytics to predict accident-prone scenarios, reduce accidents, cut costs, and enhance safety in the oil pipeline sector. The five-stage approach includes data cleaning, exploratory data analysis, variable selection, model building, and proposing business solutions.

## Key Findings
Predictive models highlight significant predictors, including 'Cause Category', 'Liquid Type', and 'Pipeline Shutdown', indicating opportunities for targeted interventions.

## Proposed Solution
Recommendations include pre- and post-accident strategies such as infrastructure reinforcement, increased emergency drills, automation, targeted educational programs, and a Risk Assessment Matrix. These solutions complement Aramco’s existing safety management systems and aim to provide a substantial improvement over current safety measures.

## Conclusion
The proactive approach to workplace safety aligns with Aramco's commitment to ensuring safer operations. The analysis acknowledges limitations due to limited public data and an imbalanced dataset but suggests future improvements with more detailed data from Aramco.

## Datasets

### 1. Oil Pipeline Accidents (2010-Present)
- **Source:** [Kaggle](https://www.kaggle.com/usdot/pipeline-accidents)
- **Location:** `database.csv`
- **Description:** 
  - Contains records of oil pipeline leaks or spills reported to the Pipeline and Hazardous Materials Safety Administration from 2010 onwards.
- **Use Case:** 
  - Analyzed to understand the primary causes of pipeline leaks and spills.

## How to Run the Project

1. Execute the R-script `Team1S03.R` alongside the `database.csv` file.
2. Adhere to the code and accompanying comments throughout the analysis for guidance.

## Technologies & Libraries

- **Programming Language:** R
- **Libraries:**
  - **Data Manipulation:** `dplyr`, `tidyr`, `tidyverse`, `data.table`, `datawizard`
  - **Data Visualization:** `ggplot2`, `corrplot`
  - **Model Building & Evaluation:**  `caTools`, `rpart`, `rpart.plot`, `car`, `caret`, `ehaGoF`
  - **Date-Time Data Handling:** `lubridate`
  - **Scaling and Transformation Tools:** `scales`
  - **Machine Learning and Statistics:** `caTools`, `Metrics`, `caret`

## Analytical and Predictive Techniques Utilised
1. Logistic Regression: To predict fatalities and injuries
2. Classification And Regression Tree (CART): To predict fatalities, injuries and the percentile of total costs incurred
3. Linear Regression: To predict the percentile of total costs incurred
