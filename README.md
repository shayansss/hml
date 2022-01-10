# Hybrid machine learning for numerical simulation
Typically, for a range of numerical simulations, the numerical results are generated and gathered in a dataset by expensive numerical solvers to train a machine learning model. Then, the machine learning model is used, instead of the numerical solver, which is significantly faster. Our developed hybrid algorithm used a similar method, however it also uses a reduced-order (but still fast enough) numerical solver to get some rough estimation of the results. Subsequently, the machine learning model focuses on merely improving the accuracy of the results. This renders our hybrid method similar to the regular machine learning methods, except for the fast reduced-order model that should be inserted before the machine learning part.

# Dependency
We have the following software dependency:

- Python >= 3.8.8
- Jupyter Notebook >= 6.3.0
- matplotlib >= 3.3.4
- numpy >= 1.20.1
- scikit-learn >= 0.24.1
- tensorflow >= 2.5.0
- keras-tuner} >= 1.0.1

# Experiment workflow

## Installation
Download the archive file using `git clone https://github.com/shayansss/hml`, and then, use the Jupyter Notebook to run the script (in `Hybrid_ml.ipynb`).

## Dataset preprepration
The datasets need to be loaded before training, and they should be scaled using the `scaler` function. Besides this, the odd and even rows of the input files refer to two different simulations. For the output datasets, the labels of each sample, which are in a matrix form, are located below each other, associating to each of the input row. They can be reshaped to create separate datasets using the `preprocessing` function. Note that as the outputs of the reduced-order numerical models is fed into the machine learning model as input values, the relevant dataset is named `input_mono.csv`.

## Implentation of experiments
The `ml_tools` constructor is used to run hyperparameter tuning after preprocessing the data. It also uses the `newHyperModel` constructor to generate different deep learning architectures. We used the `run_tuner` function to run the experiments on each dataset while using the `report_tuner` function, we could return results both by subplots and text files. Once the results were saved, the subplots were also generated.

#Evaluation and expected results
We ran analysis on different simulation, ranges, machine learning results, and the results were the similar, validating our major finding: the hybrid version could outperform their machine learning counterparts. We also used the early stopping algorithm, ensuring the models do not overfit the validation sets. For emulation, the mean squared errors were used.

The results are in a form of four subplots, showing the metric results vs. the number of training samples. The hyperparameters' tuning results are saved separately (in `tuned_parameters.txt`).

# Experiment customization
The hyperparameter tuning was simplified to reduce running time. One can increase the values assigned to `max_trialsList` to get more tuning iterations, and therefore, more accurate results.
