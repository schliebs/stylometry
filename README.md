# stylometry
2017 Studienstiftung Natur- und Ingenieurswissenschaftliches Forschungskolleg

An Attempt to Implement the Algorithm from "d. Vel et al. 2001: Mining E-mail Content for Author Identification Forensics"

The repository consists of several files. 

data_prep.R loads and prepares the data.\n
model_fitting.R builds the model and tunes it using cross validation.\n
predict.R predicts out-of-sample observations and summarizes the model success.\n
viz.R includes some visualizations for the presentation.\n

The Data cannot be made availeble here, but can be retrieved from the data source listed below.
The code assumes the data to be stored one folder level above the github repository location. 
Data Source: https://www.kaggle.com/kaggle/hillary-clinton-emails/version/1
