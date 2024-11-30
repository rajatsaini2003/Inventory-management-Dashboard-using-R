# Medical Sales Dashboard and Inventory Management system
### **Authors**: Parth Nuwal and Rajat Saini

![Screenshot 2024-11-30 234653](https://github.com/user-attachments/assets/12a81be8-12ca-422f-9f44-40dc9bc030a9) ![image](https://github.com/user-attachments/assets/89de84a4-1532-42eb-ad41-d4037d061e9f) ![image](https://github.com/user-attachments/assets/4dadee50-6f31-419d-9caa-f3175fde2929)




This project is a data-driven application developed in R, designed to manage and analyze medical sales and inventory data. It provides:

  - Insights into sales trends, inventory levels, and product performance, enabling better decision-making for healthcare providers and medical store owners.

  - Sales Forecasting, equipped with seasonal trends and patient inflow data to better optimize inventory for upcoming traffic.

  - Customer Demographics Insights to optimize their business strategies and more.

## Table of contents
  - Project Overview
  - Installation
  - Usage
  - Data Requirements
  - Technologies Used
  - Folder Structure
  - Future Enhancements
  - Contributing
  - License

## Project Overview
This system integrates sales and inventory data to provide real-time insights into medical store operations. The dashboard offers:

- Visualizations of sales trends and seasonal patterns.
- Predictive inventory analysis to minimize stockouts or overstocking.
- A user-friendly interface for querying sales or inventory reports.
- Account access control.
It leverages data visualization, predictive modeling, and statistical analysis capabilities in R.

## Installation
1. Clone this repository:
```bash
git clone https://github.com/yourusername/medical-sales-dashboard.git 
cd medical-sales-dashboard
```

2. Install necessary R packages:
```R
install.packages("package-name")
```
3. Run the Shiny app:
``` R
library(shiny)  
runApp("app.R")
```
4. Log-In/Sign-up in the system.

## Navigation 
1. On Opening, is the Analysis section, where one can see all the KPI's and metrices being tracked and their breakdwon in charts. You can also choose custom date and can also upload your data (according to template) for dashboard to show the analysis of it.
![Screenshot 2024-11-30 234653](https://github.com/user-attachments/assets/12a81be8-12ca-422f-9f44-40dc9bc030a9)

2. Then Navigate to Forecast Section to see all the forecasts and predictions.
![image](https://github.com/user-attachments/assets/4dadee50-6f31-419d-9caa-f3175fde2929)

3. And if you want to see the data, you can move to data pane.
![image](https://github.com/user-attachments/assets/5381d9eb-8bf6-4c94-9026-c9b170643cf4)

## Tech Stack:
1. For webapp and dashboarding, we used R, RShiny and Libraries of R
2. For ML Model: XGBoost (Extreme Gradient Boosting)
We 
 
Model performances:

![Screenshot 2024-11-30 223858](https://github.com/user-attachments/assets/ea3d1068-28ad-4a7e-9442-45d44307bb4b)

## Future Works:
1. Further tweeks to the model for better generalizability.
2. Inclusion of geographicrelated features
Open to suggestions :) 

## Contributing
Contributions are welcome! Please follow these steps:

- Fork the repository.
- Create a feature branch (git checkout -b feature-name).
- Commit your changes (git commit -m 'Add feature').
- Push to the branch (git push origin feature-name).
- Open a pull request.

## Thanks for reading! May the gods watch over you
