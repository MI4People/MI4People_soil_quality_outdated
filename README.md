# Soil Quality Evaluation System
This repo is the work in progress on MI4people's Soil Quality Evaluation System. The focus of this repo is data gathering and model building. For the actual Application (MVP stage), see repo https://github.com/PaulSpringerMI4People/MI4People_Soil_Quality_Streamlit and the app itself https://share.streamlit.io/paulspringermi4people/mi4people_soil_quality_streamlit/app.py.

Currently the system is able to predict content of organic carbon in the soil in Africa (depending on location and depth of interest)

## Data Gathering
The Scripts that are used to collect required data are:
 - Soil_Data_Preparation_v0.2.R: Deals with target data (variables to be predicted) and input data from OpenLandMap.org API. These input data is data such as temperature, vegetation indexes, terrain structure etc. I.e. this is data that was derived from satelite raw data. Thereby various satellite sources were used. Note that this data is tabular, i.e., per location of interest and input variable there is only one value
 - Download_Sentinel_2_Pixels.ipynb: Deals with gathering of input data from Sentinel-2 satellite. This is raw data. It includes all 13 spectral bands of Sentinel-2 (incl. visual RGB bands). Here we also gather only tabular data
 - Download_Sentinel_2_RGB_images.ipynb: Basically the same as Download_Sentinel_2_Pixels.ipynb but deals with only RGB bands an download actual images (4.5km by 4.5 km rectangles with coordinates of location of interes in the center, resolution is 20m)

For input files required by this scripts and output files produced by them, take a look at the scripts. They are documented/commented in detail.

## Building models
Currently, two approaches to build a model are covered in this repo:
1. Approach similar to the one in reference paper: https://www.nature.com/articles/s41598-021-85639-y. I.e., only tabular data are used to produce predictions. Creation and evaluation of this model is covered in the script As in Paper v1.1.ipynb
2. The second approach is to use actual RGB Sentinel-2 images in addition to tabular data. This a approach and evaluation of the model are covered in Combined Input Source v1.1.ipynb.

For input files required by this scripts and output files produced by them, take a look at the scripts. They are documented/commented in detail.

Note that in order to evaluate the errors of accuracy in the models we repeat calculations 10 additional times with another random split in training/validation/test datra sets. Especially for models utilizing images it is a very time consuming calculations.

Also note that models utilizing images are quite heavy - ca. 130MB. Since standardly GitHub allows to upload only files less than 100MB, this models are compressed to zip-files.
