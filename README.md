# Hybrid machine learning for finite element surrogate modeling of soft tissues
This repository contains the research code of our *under review* paper entitled "**Multi-fidelity surrogate modeling through hybrid machine learning for biomechanical and finite element analysis of soft tissues**".

Surrogates are typically used to speed up numerical simulations, based on the generated numerical results gathered inside some datasets by an expensive numerical solver to then train a machine learning model that can be used instead of the slow numerical model. Our developed hybrid algorithm utilized a similar method; however, it firstly has a reduced-order (but fast enough) numerical submodel that can give a rough estimate of the results, and secondly a machine learning submodel within this hybrid implementation improves the accuracy of the low-fidelity results to the level of the high-fidelity results. The main benefits of it, which is potentially applicable in many of the soft tissues' finite element simulations, are its non-destructive and efficient implementation (since it does not complicate learning and can work with limited data of multi-physics problems). Here, we performed some empirical tests, as elaborated in the paper.

## Dependency
For machine learning implementation:
- Jupyter notebook
- Python = 3.6.13
- numpy
- matplotlib
- tensorflow-gpu >= 1.15, < 2
- dm-sonnet < 2
- pandas
- sklearn

For generation of datasets and contour plots:
- Visual Studio = 2019 (at least the Community edition)
- Intel® Parallel Studio XE = 2020 with Update 4
- Abaqus/CAE = 2021 (full version)

## Installation
You should install all the dependencies (tested successfully on Windows 10). For a simple installation of the Python libraries and relevant tools, use [(Ana)conda](https://www.anaconda.com/). For the numerical implementation, install the other software packages before the Abaqus installation, which can be tricky, as they should be linked properly (for the Fortran subroutine implementation). This is described (using some other similar versions) in [this tutorial](http://dx.doi.org/10.13140/RG.2.2.33539.32800). You can download the archive by

    git clone https://github.com/shayansss/hml

As the address of the root directory of the local repository in your system may not be the same as the one set in the code, you should change it to your local address by correcting the values passed to: 1) the `parentFolder` local variable of the `abaqus` function inside the `hybrid_ml.ipynb` file; 2) the `os.chdir` function inside `3d.py`, `2d.py`, and `visualization.py`; 3) the `FilLoc` variable in `NONLIPLS.for`. Likewise, you can open the `NEW.cae` file and update the directory address of the Fortran subroutine file, accessible by finding the defined `Jobs` settings, where you can modify the address of the subroutine, and finally save this file.

## Dataset generation
In the root directory, create the datasets automatically by the numerical solver using these commands:

    abaqus cae noGui=2d.py -- id
    abaqus cae noGui=2d.py -- ood
    abaqus cae noGui=3d.py -- lf
    abaqus cae noGui=3d.py -- hf

 Note that depending on your system, it may take between minutes to hours to have them all.

## Experiment workflow
By running the `hybrid_ml.ipynb` file, the runtimes of the numerical simulations are compared, and after that the datasets are preprocessed for the subsequent training steps. This file contains the training code for only the 3D models, whereas the main part of the 2D training code is available at `gnn.py` (to avoid any conflict between `sonnet` and `keras`). Also, for the contour plots, it employs the `Abaqus` function that calls Abaqus/CAE to create them via another script of Abaqus/CAE, i.e., `visualization.py`. All the results are then saved in the root directory.

## Evaluation and expected results
We ran tests for different simulations, hyperparameters, etc., and the results are illustrated and compared. These results demonstrate that the hybrid versions outperform their machine learning counterparts. Some metrics are selected to approximate the errors, in particular, the *point-wise mean square error* described in our [previous work](https://shayansss.github.io/files/2021_11.pdf).

## Experiment customization
In the `hybrid_ml.ipynb` file, the hyperparameters can be changed. For this, you can modify the arguments of the `run_2d_experiments_on_samples` and `train_model` functions to define different architectures of the graph-based and deep learning models, respectively. On the other hand, the numerical models and physics problems can be changed using the options available in Abaqus. You can increase the fidelity of the constitutive equations by the `NONLIPLS.for` and `*.cae` files, although it may increase the computational costs extremely, covered in our previous studies of the [bone-cartilage modeling](https://shayansss.github.io/files/2019_09_preprint.pdf) and [pre-stress modeling](https://shayansss.github.io/files/2021_02.pdf). This procedure is straightforward, but regardless of any surrogate you implement using Abaqus, some knowledge of the Abaqus scripting is needed.
