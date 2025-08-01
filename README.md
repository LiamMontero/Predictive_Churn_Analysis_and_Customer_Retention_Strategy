
# Data Scientist (Internship Project) – Predictive Analytics and Customer Retention Strategy for a Telco

## Development of a Predictive Model and Business Strategy in R to Predict the Churn Rate

I faced a critical business problem for a telecommunications company: a high and costly customer churn rate of 26.5%. My goal was not only to predict who would leave, but to understand why and create a quantifiable retention strategy that
would allow for revenue recovery.

My role was comprehensive. I began with a thorough data cleansing and Exploratory Data Analysis (EDA) that allowed me to diagnose the epicenter of the problem: new customers (less than 12 months) with flexible (month-to-month) contracts. I went beyond the analysis and created a
rule-based segmentation to isolate this group, which I called "New Customers at Risk." 
This step was crucial, as it allowed me to quantify the problem: I identified 1,994 customers representing more than $59,615 in Monthly Recurring Revenue (MRR) at risk, with an alarming churn rate of 51.4%. 

For modeling, I used the tidymodels framework in R and developed a Logistic Regression (GLM) model. I didn't stop at a base model; I performed advanced feature engineering to capture nonlinear and interaction relationships, improving the model's predictive capacity.
However, the most important step was the strategic optimization of the decision threshold. Instead of using the default threshold (0.5), I set it to 0.2415.
This decision, although it reduced overall accuracy, was deliberate and aligned with the business: it allowed me to increase Recall Sensitivity by 48%, ensuring that we correctly identified 82.1% of customers who were planning to leave. 

The end result was not just a model, but a complete action plan. I presented a four-pillar retention strategy based on the model's evidence and, most importantly, calculated its financial impact. With a conservative assumption of reducing churn by 20% within the most vulnerable segment, my plan demonstrated the potential to recover more than $143,000 in annual revenue. 

Measurable financial and business achievements: 
+ I quantified a revenue recovery potential of more than $143,220 annually, demonstrating a clear Return on Investment (ROI) for my project and justifying its implementation. 
+ I identified and measured a $59,615 risk in Monthly Recurring Revenue (MRR) concentrated in the "New Customers at Risk" segment, allowing the company to focus its efforts precisely and profitably.
+ I increased the ability to detect at-risk customers by 48% (from 55% to 82.1% Recall) through strategic optimization of the model's threshold. This means that the company can now act on 8 out of 10 customers who plan to churn, instead of just 5.
+ I designed a comprehensive four-pillar business strategy, including a phased implementation plan with A/B testing, demonstrating not only technical skills but also strategic and product vision.
+ I developed a robust and interpretable model (AUC of 0.858) that not only predicts but also explains key churn drivers, enabling smarter marketing and product interventions.
