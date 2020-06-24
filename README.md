# AF COVID Dashboard

The AF COVID Dashboard is an R Shiny App. It includes interactive visualizations 
and summary information that can be used to visualize and explore COVID data across
many countries in Africa.

The current structure of this app reads pre-written .csv files in order to reduce
the runtime for the app. This directory will include the code for the app itself
and for the code used to generate the files it uses. 

* Test includes the .csv files included for the app, and **app.R**, the file for the ShinyApp itself.
* **ReadData.R** reads COVID data directly from the JHU database, and cleans and formats data from a 
WPP population data (which I'm having trouble pushing due to its size).
* **ModelInput.R** includes the coding that produces predictive models, and creates forecast data for the model. Currently, it hosts
 Instrumental Variable Regression models and Linear Mixed Effects models to account for both time and effects for each country.
* stan_fit.rds and stan_fit4.rds are both linear mixed effects models run through STAN. These are used for more rigorous predictions.
* TestApp.Rmd is an initial first draft Rmd file including interactive features. This can basically be ignored!

For this app's workflow, **ReadData.R** and **ModelInput.R** are both run, and then models are put into the Test folder.
The app is then tested and published to https://tmh741.shinyapps.io/AFCOVIDDashboard/
