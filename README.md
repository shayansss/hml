# HML: Hybrid Machine Learning
This repository contains the research code of our "HML" method, recently *accepted* for publication, entitled "[Multi-fidelity surrogate modeling through hybrid machine learning for biomechanical and finite element analysis of soft tissues](https://shayansss.github.io/files/2022_06.pdf)".

Surrogates are typically used to speed up numerical simulations, based on the generated numerical results gathered inside some datasets by an expensive numerical solver to then train a machine learning model that can be used instead of the slow numerical model. Our HML algorithm utilized a similar method; However, it firstly has a reduced-order (but fast enough) numerical submodel that can give a rough estimate of the results, and secondly a machine learning submodel within this hybrid implementation improves the accuracy of the low-fidelity results to the level of the high-fidelity results. The main benefits of it, which is potentially applicable in many of the soft tissues' finite element simulations, are its non-destructive and efficient implementation (since it does not complicate learning and can work with limited data of multi-physics problems). Here, we performed some empirical tests, as elaborated in the paper.

## Citation
If this research data is useful for your work, kindly please consider citing our work ([DOI](https://doi.org/10.1016/j.compbiomed.2022.105699) | [PDF](https://shayansss.github.io/files/2022_06.pdf)):

```
@article{sajjadinia2022a,
    title = {Multi-fidelity surrogate modeling through hybrid machine learning for biomechanical and finite element analysis of soft tissues},
    author = {Seyed Shayan Sajjadinia and Bruno Carpentieri and Duraisamy Shriram and Gerhard A. Holzapfel},
    journal = {Computers in Biology and Medicine},
    pages = {105699},
    year = {2022},
    issn = {0010-4825},
    doi = {10.1016/j.compbiomed.2022.105699},
}
```

## Dependency
For machine learning implementation:
- Jupyter notebook
- Python = 3.6.13
- tensorflow-gpu >= 1.15, < 2
- dm-sonnet < 2
- numpy
- matplotlib
- pandas
- sklearn

For generation of datasets and contour plots:
- Visual Studio = 2019 (at least the Community edition)
- Intel?? Parallel Studio XE = 2020 with Update 4
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
We ran tests for different simulations, hyperparameters, etc., and the results are illustrated and compared. These results demonstrate that the hybrid versions outperform their machine learning counterparts. Some metrics are selected to approximate the errors, in particular, the *pointwise mean square error* described in our [previous work](https://shayansss.github.io/files/2021_11.pdf).

## Experiment customization
In the `hybrid_ml.ipynb` file, the hyperparameters can be changed. For this, you can modify the arguments of the `run_2d_experiments_on_samples` and `train_model` functions to define different architectures of the graph-based and deep learning models, respectively. On the other hand, the numerical models and physics problems can be changed using the options available in Abaqus. You can increase the fidelity of the constitutive equations by the `NONLIPLS.for` and `*.cae` files, although it may increase the computational costs extremely, covered in our previous studies of the [bone-cartilage modeling](https://shayansss.github.io/files/2019_09_preprint.pdf) and [pre-stress modeling](https://shayansss.github.io/files/2021_02.pdf). This procedure is straightforward, but regardless of any surrogate you implement using Abaqus, some knowledge of the Abaqus scripting is needed.

## FAQ

**Does this method outperform the physics-informed machine learning algorithms?**

We have not carried out any comparative studies between these two groups of algorithms, as it is outside the scope of this research. But in general, physics-informed surrogates have typically more complex training implementation, while they might be faster (although not always) in inference. On the other hand, multi-fidelity surrogates, like what we developed, have usually straightforward training, but their efficiency is extremely dependent on the definition of a relevant and efficient low-fidelity numerical model (which might not be always available). Thus, both techniques have their own pros and cons. Indeed, we expect, in the future, to see studies with a combination of these techniques for very complex physics problems.

**Why do I receive different results?**

If you correctly run the same code, you may observe a slight change in the results, as the learning algorithm has randomness in initialization, optimization, etc. This issue of reproducibility is ignored in our implementation, as its impact on the results is mostly minor and unimportant. On the other hand, if you change the modeling or training conditions, you should of course get different results, since our surrogate modelings have special assumptions, i.e., they are performed with limited computational resources; thus, few training samples are provided with very efficient and typical training conditions (as we typically expect when a surrogate is used for simulation acceleration).

**Is this research code ready to be used in my biomechanical research?**

The code is particularly developed for the implementation of our experiments and is open-sourced for more clarification of the paper. Therefore, application of them in other studies may require further modifications. 

**Can this repository be used to learn surrogate modeling by Abaqus and TensorFlow?**

No. Please, do not use this repository to learn the basics of surrogate modeling, as you should already be familiar with it to understand the code here. For a simple tutorial of surrogate modeling, check out our other simpler open-sourced [project](https://github.com/shayansss/pmse).

## Other contributions
I would like to thank everyone who contributed to this project, including the co-authors of the papers, anonymous reviewers, and others who provide useful feedback. Special thanks go to [Prof. Duraisamy Shriram](https://scholar.google.com/citations?user=HtBrxbsAAAAJ&hl=en) for his help in the implementation of the numerical 3D model.
