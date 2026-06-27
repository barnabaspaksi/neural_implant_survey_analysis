# Neural Implant Survey Analysis
This project is part of 105.708 Data Acquisition and Survey Methods at TU Wien. We surveyed fellow students about their attitudes toward neural implants.

The authors formed Group 13 for this assignment (Assignment 2).

Main tasks include:
- Exploratory Data Analysis: Choose appropriate methods to explore the categorical and quantitative features in your data set (one visualization per research question). Briefly explain the methods and comment on your findings.
- Descriptive Inference: Compute suitable summary statistics and display the results in a table.
- Analytic Inference: Select an appropriate test statistic to test your hypothesis. Summarize the results and explain how your findings are connected to your research question.

## Key Methods:
- Students of the course were required to take the following Datacamp courses: Analyzing Survey Data in R as well as Factor Analysis in R. They cover intermediate approaches to analyze survey data. Key approaches include:
- **Survey Package Framework** (svy): Uses svymean() and svyby() to calculate weight-adjusted proportions, ensuring the dominant groups (males, Data Science) don't mask minority trends. It applies svychisq() for design-corrected independence tests and svyglm() for logistic regressions that handle sample imbalances.

- **Experimental Design Framework**: Treats demographic traits as active factors. It uses Factorial Experiments (lm() with interaction terms) to test how traits jointly affect outcomes (e.g., age variations across gender). Alternatively, Randomized Complete Block Designs (RCBD) treat uneven traits as blocking factors to isolate variance and validate model assumptions.

## License
- Code: subject to GNU GPL.
- Data: All rights reserved.
