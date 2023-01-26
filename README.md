# Distinciveness or Asymmetry?
Code and data for the project on the influence of facial sextypicality, asymmetry, and distinciveness on attractiveness. Extended model contains also body height and BMI.

It is a good idea to start from **Distinctiveness.Rproj** file and than go consecutively from script **01.analysis.R** onwards.
The **data.clean.txt** file contains the data that were used to fit the models. Note, that to execute the code, you need to have **retinking** packege with **Stan** sampler installed on your machine (more info here: https://github.com/rmcelreath/rethinking).

We are not authorized to share publicly the raw data of facial coordinates because of the recognizability of the target faces. The .tps datasets that are processed within the **00_data_arrange.R** script into the composite metrics in **data.clean.txt** can be obtained upon request with the authors at karel.kleisner@natur.cuni.cz or petr.turecek@natur.cuni.cz

A subsample of data that allows to replicate the demonstration of change of distinciveness with asymmetry is stored in Czech_demo_asym.Rdata file. The demonstration code is in files **06...R** and **07...R**.
