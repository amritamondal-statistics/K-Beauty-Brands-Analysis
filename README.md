# K-Beauty Brands Analysis
Explorative Analysis, NLP, Recommendation Engine & Predictive Modelling using ML techniques on Nykaa’s Top 20 K-Beauty Brands in 2024

📌 Project Overview

This project explores Nykaa’s K-Beauty market in India by combining statistical analysis, machine learning, NLP, and business intelligence.

It includes:

Exploratory Data Analysis (EDA): R-based hypothesis testing, Bayesian rating models, Power BI dashboards

Recommendation Engine: Python + Streamlit (deployed as an interactive app)

Machine Learning Models: Regression & predictive analysis in R

Natural Language Processing (NLP): Text mining & feature analysis in R

Reports & Documentation: PDF reports, .Rmd to .HTML outputs, project documentation

🔗 Live App: https://k-beauty-brands-analysis-coczqyokhyuwf2ppk9cvnz.streamlit.app/

🔗 Power BI Dashboard (https://github.com/amritamondal-statistics/K-Beauty-Brands-Analysis/blob/main/Exploratory%20Data%20Analysis/EDA%20Power%20BI.pbix)

📂 Repository Structure
K-Beauty-Brands-Analysis/
│
├── Exploratory Data Analysis/         
│   ├── Hypotheses.R                  # Hypothesis testing in R
│   ├── Bayesian_Rating.R             # Bayesian rating analysis
│   ├── *.R                           # Other EDA scripts
│   ├── *.html                        # Rendered R Markdown outputs
│   ├── EDA_Report.pdf                # EDA findings
│   └── PowerBI_Dashboard_Report.pdf  # Power BI report
│
├── Machine Learning & Regression/
│   ├── *.R                           # ML scripts (regression, prediction)
│   ├── *.html                        # Rendered R Markdown
│   └── ML_Report.pdf                 # ML findings
│
├── NLP (Natural Language Processing)/
│   ├── *.R                           # NLP scripts
│   ├── *.html                        # Rendered R Markdown
│   └── NLP_Report.pdf                # NLP findings
│
├── app/
│   └── app.py                        # Streamlit recommendation engine
│
├── data/
│   └── cosmetics.csv                 # Raw dataset
│
├── requirements.txt                  # Python dependencies (Streamlit app)
├── Final_Report.pdf                  # End-to-end project documentation
├── Project_Outline.pdf                # High-level workflow & structure
├── README.md
└── LICENSE

📸 Screenshots
🔹 Streamlit Recommendation App: https://github.com/amritamondal-statistics/K-Beauty-Brands-Analysis/blob/main/Streamlit%20app%20screenshot.png

🔹 Power BI Dashboard: https://github.com/amritamondal-statistics/K-Beauty-Brands-Analysis/blob/main/Dashboard%20screenshot%20(EDA).png


⚙️ Installation & Setup
🔹 For Python App (Streamlit)
# Clone repo
git clone https://github.com/amritamondal-statistics/K-Beauty-Brands-Analysis.git
cd K-Beauty-Brands-Analysis

# Create virtual environment
python -m venv .venv
.venv\Scripts\activate   # Windows
source .venv/bin/activate # Mac/Linux

# Install requirements
pip install -r requirements.txt

# Run app
streamlit run app/app.py

🔹 For R Scripts

Open .R files in RStudio

Open .Rmd → knitted as HTML

Install required packages:

install.packages(c("tidyverse", "caret", "ggplot2", "tm", "wordcloud"))

📑 Deliverables

EDA: Hypotheses testing, Bayesian rating, Power BI dashboard

ML Models: Regression, prediction results

NLP: Text mining & feature analysis

App: Streamlit-based recommendation system

Reports: Individual reports per section + Final integrated report

🚀 Future Enhancements

Hybrid recommendation (content + collaborative filtering)

Sentiment analysis of customer reviews

API deployment of R ML models

Expansion of dataset

👩‍💻 Author

Amrita Mondal
📊 Data Analytics & ML Enthusiast
🔗 GitHub- https://github.com/amritamondal-statistics