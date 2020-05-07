# Survival Analysis

## Summary
Has the transition to online learning in response to COVID-19 affected student engagement, specifically student online attendance? With attendance data collected from Zoom, this project conducts a survival analysis testing for differential patterns of attrition between two Harvard University Courses (S052 and GOV1005) with different course attendance philosophies. The results find statistically significant evidence that course philosophy affects synchronous online lecture attendance, providing interesting insight for future research into how different student characteristics and course logistics impact student learning behavior.

## Repository Guide
### survival_analysis directory
data_analysis.rmd: R script cleaning and manipulating Attendance.csv for analysis
#### raw-data subdirectory
Attendance.xlsx: Original person-level data collected from Zoom
Attendance.csv: Converted csv file for data loading
#### shiny_app subdirectory
app: script that generates shiny app in R
##### raw-data subdirectory
data_all.rds: Saved rds file from data_wrangling that includes all observations
data_students.rds: Saved rds file from data_wrangling that includes all student observations
data.rds: Saved rds file from data_wrangling that includes all students who attended at least once online lecture synchronously
data_pp.rds: Saved rds file from data_wrangling that converts data.rds into person-period data for surivival analysis

### Link to Published Shiny App 
https://yongrlee3.shinyapps.io/survival_analysis/?_ga=2.249723780.325576854.1588359995-705379825.1582475664
