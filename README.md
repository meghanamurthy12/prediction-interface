# prediction-interface
This project is a prediction interface for classification and regression using gradient boosting models. It has been developed using the Shiny package offered by R in the RStudio IDE.

The original purpose of this project was to create an Airport Delay Prediction Model that focused on the Chicago O'hare airport. 2 datasets are considered, one pertaining to flight scheduling of the airport and the other to the weather conditions in Chicago for the duration of the airport schedule provided. The interface allows for 2 datasets to be uploaded, and they will be merged based on the user selecting the "key" (column to merge on) of the primary and secondary dataset. 

The interface has 6 tabs, each performing a unique functionality that makes the interface easy to use and understand. Each tab is described below: 
1) Upload Data - This tab allows you to upload the datasets (they must be of extension .csv). You can upload one, or two if you have a "secondary" dataset that is required. Once uploaded, a preview is depicted on the screen.
2) Summary of Data - This tab displays a summary of the data. For example, max, min, avg values of all the columns. This allows the user to get a better understanding of the dataset. 
3) Split Data - Here you choose whether your prediction task is Regression or Classification. Once you choose, select the training features and the prediction Label. If you have uploaded two datasets, choose the column that will join the datasets together. If not, proceed to click the 'split data' button to split your data into training and testing. Once the split is complete, the Training Data will be visible on the screen.
4) Train Model - Finally, the time has arrived for the actual model training. Choose which gradient boosting you want to train - gbm, xgbDART, xgbLinear or xgbTree. For each model you choose, the parameter valuesl to fill in will change. If you wish to create a parameter grid, you can fill in the values accordingly and press "Create Grid" to visualize the grid. Once you click "Train model", the model results will be printed below. 
5) User Test Data - Click the button prompting the user to enter test data. NOw you can enter your own values for each training feature. For example, if you are predicting the classification of Yes/No for susceptibility to Diabetes, you might enter the bio of a new patient to predict if they are at risk. Hit the predict button, and your results will be disiplayed below,
6) Test Model - THe last tab will run the test data on the model and proceed to print a graph depicted the feature importance towards the prediction. 

In this way, the modules of the application can be used to carry out your prediction for any use case. 

There are 2 files required to run this application 
1) app.R - this is the web application that you can launch in your web browser or within the RStudio interface
2) splitData.r - this is the file carrying out the parameter grid creation based on which model you have selected

Store both files in the same directory. 
