# AF COVID Dashboard

The AF COVID Dashboard is an R Shiny App that visualizes COVID-19 data in Africa from the JHU COVID-19 Data Repository.

This repository contains the files that download and process the data, and ready them for visualization. 

The process can be run in one sequence with **ModelPipeline.R**.

* **ReadData.R** reads COVID data directly from the JHU COVID-19 data repositories, and also reads in cleaned and formatted 
* population data from UN World Population Prospects. All files are saved in the *Test* folder, which includes the shinyapp.
* **ModelFiles.R** prepares and creates files as preparation for modeling. Again, everything is saved in *Test*.
* **ModelRun.R** develops a model to project upcoming COVID data. I'll talk about the model later in the readme!
* Finally, the constructed files and data in *Test* is displayed in the shinyapp. The app is then tested and published to https://tmh741.shinyapps.io/AFCOVIDDashboard/

# Model

To project COVID cases, we wanted to make a model that would consider differences between countries.
Since COVID trends and so many factors were also changing so fast, we also wanted to make sure our model
didn't make projections from too far in the past, and apply trends that were no longer applicable.

We also wanted our model to include a good degree of uncertainty in its predictions.

The current model we picked is a Linear Mixed Effects model developed in STAN. 
Linear Mixed Effects models are hierarchical and can identify the difference between groups,
so it allows us to pick differences between countries.
By building it in STAN, we can also apply a Bayesian framework for our model, incorporating uncertainty
into our projections. However, it makes it run for a very long time.
Lastly, we also made sure to include very recent COVID data into our measurements. This way,
we could make sure we're only capturing really recent trends. 
