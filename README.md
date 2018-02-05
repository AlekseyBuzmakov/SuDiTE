# SuDiTE
A set of programms implementing algorithms on Subgroup Discovery under Treatment Effect.
The name of the library come from **SU**bgroup **DI**scovery under **T**reatment **E**ffect. Apparently in Russian the word SuDiTE is very close to "you judge". It is close to the goal of the library which is deciding which algorithm is the best one.

## Package for comparison of algos

A special package in R is created for testing and comparing of different algos.

### Install

In order to install the package one is need to install 'devtools' library.
> install.packages("devtools")

Then our package can be installed from GitHub
> devtools::install_github("AlekseyBuzmakov/SuDiTE", subdir = "SuDiTE.R") 

### Usage
The following functions are provided by the package

#### Train and test different models
> evaluateAlgos(trainModelFuncs, predictModelFuncts, subgroupQualityFunc, trainY, trainTrt, TrainX, testY, testTrt, testX)

The first two arguments are a vector of functions of the same length. The first ones train models, and the second ones predict with the corresponding models.

_subgroupQualityFunc_ is the function that measure the quality of the subgroup. The simplest case as Average Treatment Effect is given by function SuDiTE::subgroupAverageTreatmentEffect()

_train\*_  are response, treatment and covariates of the training dataset
_test\*_ are response, treatment and covariates of the test dataset

The result is the quality measured in the test dataset for the models build in the train dataset.

#### Crossvalidation of the model
> crossValidateAlgos(trainModelFuncs, predictModelFuncs, subgroupQualityFunc, dbY, dbTrt, dbX, numTrials, testProportion)

The first three arguments are the same as for SuDiTE::evaluateAlgos function.
_db\*_ is response, treatment and covariates for the dataset.
*numTrials* is the number of divisions to train-hold\_out datasets
*testProportion* is the 0-1 number specifying the proportion of the whole dataset to be considered as the hold-out set

#### Avereage treatment effect function
> subgroupQualityFunc(subgroup, Y, Trt)

Function measure the quality of a subgroup
_subgroup_  is a vector of logical flags specifying if i-th object is included into the subgroup
_Y_, _Trt_ is the value of response and treatment varibale for every object

