# angleClosure_CNN_finetuning

`git clone https://github.com/petteriTeikari/angleClosure_CNN_finetuning`

or download as .zip as backup solution if you do not have a working git in your organization

## Prerequisites

* R, https://www.r-project.org/
* (optional) RStudio IDE, https://www.rstudio.com/
* (optional) Git, https://git-scm.com/
* Tensorflow, https://www.tensorflow.org/ [Keras]
* Python3, https://www.anaconda.com/

## How-to-use

### 1) Data Wrangling

Start with this type of data structure (data is one folder backwards `..`, and then there in `Data_ASOCT`:

![](https://github.com/petteriTeikari/angleClosure_CNN_finetuning/blob/master/figures/data_organization.png)

And let the R script `MAIN.R` hunt for the "SL" and "1" wildcards

![](https://github.com/petteriTeikari/angleClosure_CNN_finetuning/blob/master/figures/R_wildcards.png)

### 2) Quick'n'dirty deep learning

Follow the example _"Fine-tune InceptionV3 on a new set of classes"_ 
* https://keras.io/applications/#fine-tune-inceptionv3-on-a-new-set-of-classes
* https://blog.keras.io/building-powerful-image-classification-models-using-very-little-data.html
* https://github.com/danielvarga/keras-finetuning


