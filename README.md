# nlpPredictorSwiftkey
This is the final capstone project for the Johns Hopkins University Data Science Specialization.

## Goal
The goal of this project is to create a Shiny app that takes user text input and predicts the next word.

## Data
The data used for this project is provided by Swiftkey. There are files for twitter, blogs, and news sources in four different languages; I focus only on english

**Note that I do not include the data sets as exceed Github's size requirement.**

## Basic instructions
The app is located online at https://stevebachmeier.shinyapps.io/nextWordPredictor/.

To run the app within R:
1. Download the raw data files from https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
2. Run 01_clean_data.R. This cleans the data using f_cleanWords.R.
3. Run 02_analysis_prep.R. This prepares the data for the model and saves the relevent files as .txt files.
4. (optional) Run 03_EDA.R if desired.
5. Source f_nlpPredictor_3grams.R. This creates the nlpPredictor() function.
6. In R, run nlpPredictor("..."), where "..." is any text.