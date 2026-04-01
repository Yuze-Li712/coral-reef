# Coral Reef: Predicting Crown-of-Thorns Starfish Outbreak Risk

This project builds a reproducible data analysis pipeline to study and predict Crown-of-Thorns Starfish (COTS) outbreak severity on the Great Barrier Reef, with a case study focus on the Lizard Island region. The repository combines exploratory spatial analysis, environmental data processing, predictive modelling, and a Shiny app for interactive risk prediction. The core workflow and final report are preserved in this repository, while some original data files were omitted or replaced because the project was originally hosted on a private group server.

## Project Goal

COTS outbreaks are a major threat to coral reef health. The aim of this project is to examine whether environmental conditions and coral-related indicators can help predict outbreak severity, so that reef managers can identify risk earlier and respond before outbreaks become more severe. The final modelling task treats outbreak severity as an ordered outcome with four classes: **no outbreak**, **potential outbreak**, **outbreak**, and **severe outbreak**. 

## Data Used

The project brings together multiple data sources:

- **AIMS COTS survey data** (`manta-tow-by-reef.csv`), used to measure COTS counts, tow effort, and coral conditions.
- **eReefs environmental data** (`annual.nc`), accessed directly from the eReefs THREDDS server, used to extract environmental predictors.
- Key environmental variables used in the final model are:
  - dissolved inorganic nitrogen (**DIN**)
  - dissolved inorganic phosphorus (**DIP**)
  - dissolved inorganic carbon (**DIC**)
  - chlorophyll-a
- Some exploratory work also uses Queensland land use data and clean water data to visually examine possible spatial relationships between coastal activity and reef conditions. 

## Project Workflow

The project has four main stages.

### 1. Exploratory Data Analysis

The EDA first maps sugar land use in Queensland and compares it with the spatial distribution of COTS observations. It then overlays the two to visually inspect whether COTS activity appears more concentrated near coastal farming regions. The EDA also filters COTS observations near the coastline and creates year-based animated maps to show how outbreaks vary across time and space. 

### 2. Data Preparation

The modelling pipeline starts by cleaning the COTS dataset and renaming core variables such as longitude, latitude, survey date, total COTS, mean COTS per tow, and coral cover. It keeps observations after 2009. Then it downloads annual eReefs data and extracts DIN, DIP, DIC, and chlorophyll values across latitude, longitude, time, and depth. To avoid duplicate depth layers, the workflow keeps the shallowest available depth for each location-date combination. 

### 3. Spatial Joining and Feature Engineering

Environmental observations are aggregated into **hexagonal grid cells** using DGGS. The reef survey data are then spatially joined to those same hexagons by year. After the join, the pipeline computes reef-level features such as total COTS, number of tows, mean COTS, and mean live coral cover. It also applies transformations used in the final model, including:

- `log(din + 1)`
- `log(dip + 1)`
- `sqrt(chlorophyll)`

These transformed predictors are used together with DIC and live coral cover in the final classification models. 

### 4. Modelling and Deployment

The project compares several supervised learning methods:

- Random Forest
- Support Vector Machine
- k-Nearest Neighbours
- Ordinal Logistic Regression

All models are trained with **5-fold cross-validation**, and the workflow uses **SMOTE** during training to help address class imbalance. The final selected model is **ordinal logistic regression (OREG)** because the outcome is naturally ordered and because this model showed stronger local performance for the Lizard Island case study while maintaining good interpretability. The trained model is then deployed in a **Shiny app**, where users can adjust environmental inputs and view the predicted probability of each outbreak severity class. 

## Main Results

The final report concludes that ordinal logistic regression was the most appropriate model for this application. Although its overall mean sensitivity across the full Great Barrier Reef dataset was modest, it performed better in the Lizard Island region and was able to identify examples from every outbreak class without collapsing predictions into only the dominant class. The report also notes that the model achieved a specificity of **0.752** for no-outbreak conditions. 

In the model interpretation stage, **DIN** emerged as the strongest predictor of outbreak risk, while chlorophyll-a had a weaker overall SHAP contribution even though its direction of effect remained ecologically sensible. The report argues that this makes the final model useful as an early-warning tool, while also noting that some predictor relationships did not fully align with prior literature and may reflect confounding or local ecological complexity. 

## Repository Structure

```text
coral-reef/
├── EDA.Rmd                         # exploratory spatial analysis and visualisation
├── final_models_final.qmd         # data cleaning, joining, feature engineering, and model training
├── Reef_09_Reproducible_Report.Rmd # final written report
├── Reef_09_Reproducible_Report.html# rendered report output
├── app.R                          # Shiny app for interactive prediction
└── README.md
