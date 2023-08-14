# Distinciveness or Asymmetry?
Code and data for the project on the influence of facial sextypicality, asymmetry, and distinciveness on attractiveness. Extended model contains also body height and BMI.

It is a good idea to start from **Distinctiveness.Rproj** file and than go consecutively from script **01_data_preparation.R** onwards.
The **data.clean.txt** file contains the data that were used to fit the models. Note, that to execute the code, you need to have **rethinking** packege with **Stan** HMC sampler installed on your machine (more info here: https://github.com/rmcelreath/rethinking).

We are not authorized to share publicly the raw data of facial coordinates because of the recognizability of the target faces. The .tps datasets that are processed within the **00_calculate_metrics.R** script into the composite metrics in **data.clean.txt** can be obtained upon request with the authors at karel.kleisner@natur.cuni.cz or petr.turecek@natur.cuni.cz

A subsample of data that allows to replicate the demonstration of change of distinciveness with asymmetry is stored in Czech_demo_asym.Rdata file. The demonstration code is in files **06...R** and **07...R**.

Folder **by rating** contains equivalent model to m1 where individual rating (1,2,...,or 7) is the unit of analysis, whcih requires ordered-logit link function with 6 threshold probabilities describing the discrete rating distribution instead of normal. There is a **by_rating.Rproj** file that can be used to initiate the session. (The **01_data_preparation.R** file is identical to the one in root folder, start by running **01_data_by_rating_prepare.R** file that automatically sources this file as well) There is also another folder **ind_ratings** that contains all the files necessary for reproducting the analysis. 
