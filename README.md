Overview
--------

Diagnose, explore and identify data quality and model performance data with `ModelEvaluation`.

Features:

-   Diagnose data quality.
-   Find appropriate scenarios to pursuit the follow-up analysis through data exploration and understanding.
-   Derive classification and numeric model performance with automatic visualizatoin
-   Automatically generate reports for the above above tasks.



Usage
-----

ModelEvaluation includes several functions files:


The Qual_report() report the information of Exploratory
data analysis for object inheriting from the DBMS table through tbl_dbi


The Data Quality process will report the following information:

-   Missing and Unique Values
-   Descriptive Statistics
-   Categorical Breakdown
-   Histograms
-   Population Stability Index
-   Trended Numerical Variables
-   Trended Categorical Variables
-   Correlation Matrix



The Perf_report() produces and saves objects for evaluating
performance of a two stage modeling process. Object must be of form data.frame.

The Model Performance process will report the following information:

-   PD K-fold Cross Validation
-   PD Model Coefficients / Significance
-   Concordance
PD Model Discrimination
-   KS Tables
-   KS Plots
-   Gini Curve/Lorenz Curve
-   ROC Curve

-   Backtesting by Bucket
-   Loss Forecast Backtesting
 