# prediction-interface
This project is a prediction interface for classification and regression using gradient boosting models. It has been developed using the Shiny package offered by R in the RStudio IDE.

The original purpose of this project was to create an Airport Delay Prediction Model that focused on the Chicago O'hare airport. 2 datasets are considered, one pertaining to flight scheduling of the airport and the other to the weather conditions in Chicago for the duration of the airport schedule provided. The interface allows for 2 datasets to be uploaded, and they will be merged based on the user selecting the "key" (column to merge on) of the primary and secondary dataset. 

The interface has 6 tabs, each performing a unique functionality that makes the interface easy to use and understand. Each tab is described below: 
1) Upload Data - This tab allows you to upload the datasets. You can upload one, or two if you have a "secondary" dataset that is required. Once uploaded, a preview is depicted on the screen.
