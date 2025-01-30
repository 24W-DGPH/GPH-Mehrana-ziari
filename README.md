# Project Description
This project aims to explore, clean, analyze, and visualize medical costs and healthcare expenses by using an existing dataset. This analysis provides helpful insight for the healthcare industry by comparing how different variables influence medical charges. Researchers, healthcare professionals, and analysts can utilize this dataset to better understand medical expenses, helping to inform healthcare policies and pricing strategies.

# Dataset Information
The Medical Cost Dataset consists of the following columns:
1. **ID**: Every individual has a unique identifier as ID, ensuring efficient data management and analysis.
2. **Age**: Patient’s age is an important factor to consider as it correlates with medical expenses.
3. **Sex**: The gender of the patient can affect the potential costs of healthcare due to biological differences.
4. **BMI**: The Body Mass Index (BMI) is an important factor that impacts healthcare costs.
5. **Children**: The number of children covered by the medical insurance can influence family-related medical expenses.
6. **Smoker**: A binary indicator for whether the patient is a smoker or not, as smoking habits can significantly affect healthcare costs.
7. **Region**: The geographic region of the patient helps us understand regional disparities in healthcare expenditure.
8. **Charges**: The medical charges incurred by the patient, serving as the target variable for analysis and predictions.

# Data Source
The data source is the **Medical Cost Dataset** sourced from Kaggle. The dataset is provided by Nandita Pore and is invaluable for anyone aiming to analyze medical costs and expenses.  
[Kaggle - Medical Cost Dataset](https://www.kaggle.com/datasets/nanditapore/medical-cost-dataset)

# User Persona
The primary user persona for this project is **Healthcare Policy Analysts**, who work for insurance companies or government health agencies. This project can help them in:
- Identifying high-risk groups of people who contribute disproportionately to healthcare costs.
- Developing strategies to reduce healthcare costs while maintaining quality care.
- Discovering regional disparities in healthcare expenditure to allocate resources more effectively.

# Goals of the User Persona:
1. **Reduce Healthcare Costs**: By identifying factors that drive high medical charges, the analyst can design targeted interventions (e.g., smoking cessation programs or weight management initiatives).
2. **Identify High-Risk Groups**: Understanding how variables like smoking status, BMI, and age impact medical costs helps the analyst prioritize at-risk populations.
3. **Inform Policy Decisions**: Insights from the data can guide the development of policies that promote equitable and cost-effective healthcare.

# Key Insights from Analysis
The analysis revealed several key insights critical for decision-making:
1. **Impact of Smoking**: Across all four regions (Northeast, Northwest, Southeast, and Southwest), smokers consistently have much higher mean and median charges compared to non-smokers. For example, in the Southeast, smokers have a mean charge of $34,845, while non-smokers pay an average of $8,032—a difference of over 4 times.
2. **BMI and Medical Charges**: Patients with higher BMI values are shown to have higher medical charges. This emphasizes the importance of weight management initiatives. On the other hand, underweight patients have the lowest mean and median charges.
3. **Age and Charges**: Older patients generally face higher medical expenses, suggesting a need for preventive care programs that target older demographics.
4. **Regional Disparities**: Medical charges vary across regions, with some areas showing consistently higher costs. This could indicate differences in healthcare access, pricing, or population health. For example, the Southeast region has the highest average charges for smokers ($34,845) and the lowest for non-smokers ($8,032), while the Northwest and Southwest regions have the lowest non-smoker charges, both around $8,000.

These insights provide actionable and valuable information for healthcare policy analysts to design targeted interventions and allocate resources effectively.

# Technologies Used
- **R**: Language for statistical computing and graphics.
- **Shiny**: Framework for building interactive web applications with R.
- **ggplot2**: A data visualization package for creating plots and charts.
- **dplyr**: A package for data manipulation and cleaning.

# Links
- GitHub Repository: [Medical Cost](https://github.com/Mehrana-Ziari/Medical-Cost)

- [Shiny Application](https://fatemehziari.shinyapps.io/med_cost/)

  
# Installation & Setup
 Follow these steps to set up and run the application:

### 1. Clone the Repository
```sh
git clone https://github.com/Mehrana-Ziari/Medical-Costs-Analysis.git
```

### 2. Install Required R Packages

```r
install.packages(c("shiny", "ggplot2", "dplyr"))
```

### 3. Run the Shiny Application
```r
shiny::runApp("path/to/your/app")
```

### 4. Open in Browser
Navigate to the displayed URL in your browser to interact with the visualizations.

# Usage
 After opening the shiny app, you can:
- Select a region from the dropdown menu to filter the data by region.
- Filter the smokers in the visualizations by using the checkbox.
- Explore various data visualizations such as scatter plots and box plots which help to analyze the relationships between medical charges and our different variables like age, BMI, smoking status, and region.

