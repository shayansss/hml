# HML: Hybrid machine learning for finite element surrogate modeling of soft tissues
This repository contains the implementation code, described in our *under-review* paper, entitled **Multi-fidelity surrogate modeling through hybrid machine learning for biomechanical and finite element analysis of soft tissues**.

Typically, for a range of numerical simulations, the numerical results are generated and gathered in a dataset by expensive numerical solvers to train a machine learning model. Then, the machine learning model is used, instead of the numerical solver, which is significantly faster. Our developed hybrid algorithm, which we called here HML, used a similar method, however, it has a first a reduced-order (but faster) numerical submodel that can give some rough estimation of the results. Subsequently, a machine learning submodel within this HML implementation can then improve the accuracy of this model to the level of the high-fidelity results.

The main benefit of such a multi-fidelity approach is the non-destructive methodology we proposed in the paper that can develop the low-fidelity model, potentially applicable in many of the soft tissue biomechanical and finite element simulations. While further details is available in the paper, you may reproduce the results with this repository.

# Dependency
For the machine learning implementation:

- Python >= 3.8.8
- Jupyter Notebook >= 6.3.0
- matplotlib >= 3.3.4
- numpy >= 1.20.1
- scikit-learn >= 0.24.1
- tensorflow >= 2.5.0
- keras >= 2.4.3
- h5py = 2.10.0
- keras-tuner >= 1.0.1

For generation of datasets, custom subroutines (used in numerical implementation), and contour plots:
- Visual Studio = 2019 (at least the Community edition)
- Intel® Parallel Studio XE = 2020 with Update 4
- Abaqus/CAE = 2020 (full version)

## Installation
Firstly, install all the dependencies, with the versions specified, which were tested on Window 10. The python libraries can easily be installed by [(ana)conda](https://www.anaconda.com/). For the subroutine implementation, install the software packages for the Fortran subroutine implementation before the Abaqus installation, which can be tricky, as they should be linked properly. This is descibed (with some other similar versions) in [this tutorial](http://dx.doi.org/10.13140/RG.2.2.33539.32800). Next, download the archive file using `git clone https://github.com/shayansss/hml`, and then, set the address of the root directory (in your machine) by changing the default values that are fed to: 1) the `parentFolder` local variable of the `abaqus` function inside the .ipynb file; 2) the `os.chdir` function in the `.py` files. You may also open the `NEW.cae` file and set the exact address of the user subroutine file, which is accessible by first opening the file via Abaqus/CAE, and then fiding on the `PlaneStrain` Abaqus Job setting, where you can set the new address on your machine.

## Dataset preparation

First generate the datasets by applying these commands: `abaqus cae noGui=data_generation_2d.py`, `abaqus cae noGui=data_generation_2d_lowfidelity.py`, `abaqus cae noGui=data_generation_3d.py`, and `abaqus cae noGui=data_generation_3d_lowfidelity.py`, which automatically generates them. Note that depending on your system it may take between minutes to hours to have them all.

Using `Hybrid_ml.ipynb`, the datasets are loaded before training, and then they were scaled using the `scaler` function. Besides this, the odd and even rows of the for the datasets of the 2D models refer to two different simulations (namely axial and shear tests). Regarding this, for the output datasets, the labels of each sample, which are in a matrix form, are located below each other, associating to each of the input row. Thus, they are reshaped to create separate datasets using the `preprocessing` function.

# Experiment workflow
By running the Jupyter Notebook, i.e., the `.ipynb` file, the model is trained by the `run` function, while we extract all the visualization data using `matplotlib`. In addition, for the PMSE, we use also the `Abaqus` function that calls Abaqus/CAE to save the contours via another script, i.e., `Visualization.py`. All the results are then saved in the root directory.

## Implementation of experiments
The `ml_tools` constructor is used to run hyperparameter tuning after preprocessing the data. It also uses the `newHyperModel` constructor to generate different deep learning architectures. We used the `run_tuner` function to run the experiments on each dataset while using the `report_tuner` function, we could return results both by subplots and text files. Once the results were saved, the subplots were also generated.

## Evaluation and expected results
We ran tests for different simulations, hyperparameter settings, random seed parameters, etc., and the results were illustrated and evaluated. These results should demonstrate that the hybrid versions can outperform their machine learning counterparts, as explained in the paper. We also used the early stopping algorithm, ensuring the models do not overfit. Besides, some metrics are used to approximate the errors; in particular, the *point-wise mean square error* was described in our [previous work](http://dx.doi.org/10.13140/RG.2.2.33539.32800).

## Experiment customization
In the `.ipynb` file:

- The hyperparameter tuning conditions may be changed by its variable, e.g, using the `max_trialsList` to get more tuning iterations. However, note that this might be very expensive and time-consuming since with this setting it took us several days to finish the tuning search.
- You may also change the arguments of the `train_model` function to set a different architecture of the neural networks for the 3D models.

Furthermore, the numerical model and physics problem can be changed using options available Abaqus/CAE, and just ensure that change both the high- and low-fidelity models together. This is procedure is straightforward, as explained in the paper, but to generate the code automatically the `.py` files should also be changed, which might not be trivial, and you may need to refer to the <a href="https://www.3ds.com/products-services/simulia/services-support/support/documentation/" target="_blank">Abaqus manual</a> for this.
