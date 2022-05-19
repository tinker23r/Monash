# Retail project
![](<https://lms.monash.edu/theme/image.php/monash/assign/1651085835/icon>) [Retail forecasting project](https://lms.monash.edu/mod/assign/view.php?id=9680200)

**Objective:** To forecast a real time series using ETS and ARIMA models.

**Data:** Each student will be use a different time series, selected using their student ID number as follows.

```{r}
# Use your student ID as the seed (31455034)
set.seed(31455034)
myseries <- aus_retail %>%
    # Remove discontinued series
    filter(!(`Series ID` %in% c(
        "A3349561R", "A3349883F", "A3349499L", "A3349902A",
        "A3349588R", "A3349763L", "A3349372C", "A3349450X",
        "A3349679W", "A3349378T", "A3349767W", "A3349451A"
    ))) %>%
    # Select a series at random
    filter(`Series ID` == sample(`Series ID`, 1))
```

**Assignment value:** This assignment is worth 20% of the overall unit assessment.

**Report:**

You should produce forecasts of the series using ETS and ARIMA models. Write a report in RMarkdown format of your analysis explaining carefully what you have done and why you have done it. Your report should include the following elements.

- ~~A discussion of the statistical features of the original data.~~
- ~~Explanation of transformations and differencing used. You should use a unit-root test as part of the discussion.~~
- ~~A description of the methodology used to create a short-list of appropriate ARIMA models and ETS models. Include discussion of AIC values as well as results from applying the models to a test-set consisting of the last 24 months of the data provided.~~
- Choose one ARIMA model and one ETS model based on this analysis and show parameter estimates, residual diagnostics, forecasts and prediction intervals for both models. Diagnostic checking for both models should include ACF graphs and the Ljung-Box test.
- Comparison of the results from each of your preferred models. Which method do you think gives the better forecasts? Explain with reference to the test-set.
- Apply your two chosen models to the full data set and produce out-of-sample point forecasts and 80% prediction intervals for each model for two years past the end of the data provided.
- Obtain up-to-date data from the [ABS website (Cat. 8501.0, Table 11)](http://www.abs.gov.au/ausstats/abs@.nsf/mf/8501.0), and compare your forecasts with the actual numbers. How well did you do? [Hint: the readabs package can help in getting the data into R.]
- A discussion of benefits and limitations of the models for your data.

Your submission must include the Rmarkdown file (.Rmd), and should run without error. You can also include a knitted version of the document (HTML preferred), although you **must** include a copy of the .Rmd file.

Graphs should be properly labelled, including appropriate units of measurement.
