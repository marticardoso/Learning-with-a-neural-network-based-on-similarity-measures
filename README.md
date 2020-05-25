# Learning with a neural network based on similarity measures
## Master thesis
## Master in Innovation and Research in Informatics - FIB - MIRI
## Data science speciality


 Author: Marti Cardoso i Sabé
 
 ## Abstract 

 Two new learning algorithms are studied: a neural network based on similarity measures and an Ensemble of the first method. The goal of this thesis is to understand these two methods, derive training algorithms, analyse the impact of the similarity layer and compare them with traditional methods.

## Folder structure

In this folder, there is all code used to develop my master thesis.
It contains the implementations of the SNN and EnsSNN. 
These are the main files:

- SNN.R: 
	This file contains all functions related to the SNNs.

- SNNBagging.R: 
	This file contains all functions related to the Ensemble of SNNs.

- fp_utils.R: 
	This file contains all functions related to the activation function (fp) and the way to set it.

- MoE.R: 
	This file contains all functions related to the Mixture of Experts (MoE) ensemble model.

- daisy/* (folder):
	This folder contains the modifications of the daisy function (from cluster package).

- datasets/* (folder):
	This folder contains all datasets used in the experiments and tests.
		
- experiments/* (folder):
	This folder contains the source files of the thesis experiments.

- test/* (folder):
	This folder contains some useful functions to test the implementation and also some unit-tests to check the right functionality of the main functions.