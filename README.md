# IPIP

IPIP method to train and test machine learning models with unbalanced datasets.

IPIP main functions and their usability are commented in the file **COVID/ipip_functions.R**.

Let's suppose that we want to make a binary (YES/NO) classification, where the 'YES' class is the minority class.

## How to use IPIP functions

Firstly, to comment on the different functions that are used in the IPIP algorithm, let's detail the variables that are used on these functions:

- **bs**: Number of balanced subsets (subproblems) where each one will have a perfectly balanced data set associated to it, formed by samples of the original set.

- **r.min**: The r.min variable refers to the percent of the original dataset's examples of the minority class that will be in each balanced subset.

- **dt**: The dt (decision threshold) variable refers to the percent of models that have to classify an example as 'NO' to make the final ensemble predict 'NO'.  

- **b**: For each of the 'bs' subsets, a certain number 'b' of basic models will be trained, which will form an ensemble. This number 'b' will be chosen so that each of the samples of the minority class will be used to train one of the 'b' models.

- **r.b**: The r.b variable refers to the percent of the examples of each balanced subset that we want to use to train each basic model.

- **mt**: Maximum number of possible attempts to further expand the ensemble, given by a function mt(|M|), whose value will depend on the number of basic models 'M' that is already added for a specific perfectly balanced subset.

About the IPIP functions:

- **createSubsets**: This function creates the 'bs' balanced subsets. The inputs of this function are the examples of the minority class of the train set and the examples of the majority class of the train set.

- **prediction**: This function makes predictions using a set of basic models. This function is used to create the final ensemble and to create the 'final.prediction' function.

- **final.prediction**: This function makes the prediction on the test data using the final ensemble generated with the 'createEnsemble' function.

- **metrics**: This function obtain the accuracy, sensitivity, specificity, positive predictive value, negative predictive value, and Cohen's Kappa over the predictions makes with the 'prediction' or 'final.prediction' functions.

- **numberModels**: This function obtains the number of basic models that form the final ensemble.

# About

Autors: Antonio Guillén, Marcos Caracena, Alejandro Cisterna-García, Enrique Pérez, Fernando Jiménez, Francisco J. Francisco-Verdú, Gabriel Reina, Enrique González-Billalabeitia, José T. Palma, Álvaro Sánchez, Juan A. Botía.
