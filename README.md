## Analysis of Open Unemployment Rates in North Sumatera in 2022 Using Spatial Autoregression (SAR)

![SAR](https://cdn.antaranews.com/cache/350x233/2013/06/20130627Bursa-Kerja-270613-af-2.jpg) <br>

### Introduction
Spatial analysis plays a pivotal role in understanding the dynamics of socio-economic phenomena, particularly in regions like North Sumatra, Indonesia. In this project, we delve into the analysis of the Open Unemployment Rate (TPK) dataset for North Sumatra in the year 2022. Unemployment rates serve as crucial indicators of economic health, reflecting the labor market conditions and socio-economic disparities across different regions. By employing various spatial econometric methods, including Spatial Autoregression (SAR), Spatial Error Model (SEM), Spatial Durbin Model (SDM), Spatial Durbin Error Model (SDEM), Spatial Lag Model (SLX), Spatial Autoregressive Combined (SAC), and General Nested Spatial Model (GNSM), we aim to explore the spatial relationships and drivers of unemployment across different districts in North Sumatra.

### Methods
The methodology employed in this analysis revolves around a suite of spatial econometric techniques tailored for modeling spatially dependent data. These techniques account for the spatial autocorrelation and heterogeneity present in the unemployment rate data for North Sumatra.

Firstly, the TPK dataset for North Sumatra in 2022 is preprocessed, including data cleaning, integration with spatial reference data, and geocoding to ensure compatibility with spatial analysis techniques. Spatial weights matrices are constructed to capture the spatial relationships between different districts or administrative units within North Sumatra.

Subsequently, a range of spatial econometric models is applied to the preprocessed dataset. This includes SAR models, which account for spatial autocorrelation in the dependent variable; SEM models, which consider spatially correlated errors; SDM models, which incorporate lagged values of the dependent variable as explanatory variables; SDEM models, which combine aspects of SEM and SDM; SLX models, which include a spatially lagged dependent variable as a covariate; SAC models, which address cross-sectional dependence in spatial data; and GNSM, which allow for spatially varying coefficients.

Each model is estimated and evaluated using appropriate diagnostic tests and goodness-of-fit measures to assess model performance and identify the most suitable model for explaining variations in unemployment rates across North Sumatra.

Through the application of spatial econometric methods, this study aims to provide insights into the spatial determinants of unemployment in North Sumatra and inform targeted policy interventions aimed at addressing unemployment disparities and fostering regional development.

### Credit to Team Members
This project was a collaborative effort, and credit goes to the entire team for their contributions to data collection, analysis, and interpretation.

### Uploaded File Description
- `data_sumut.xlsx`: Raw dataset containing information about open unemployment rate in North Sumatera in 2022.
- `Tugas 3 Topik Khusus I_Kelompok 1.R`: R script for performing spatial regressions analysis on open unemployment rate data.
- `Tugas 3 Topik Khusus I_Kelompok 1.pdf`: Comprehensive report summarizing the findings and conclusions of spatial regressions analysis on open unemployment rate data.
