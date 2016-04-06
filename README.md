# Shiny example of SIMEX 

In order to run this Shiny App, you will need to install the `PKSFC` and `Rgraphviz` packages. Because neither are stored in CRAN, you need to use the commands described hereunder to install them.

Once this is done, you should be able to run the Shiny application by opening either the `ui.R` or `server.R` file and clicking on the Run App button appearing on the top left window (in R Studio).

## PKSFC

Download the [PK-SFC package](data/PKSFC_1.3.tar.gz) on your computer and store it in a folder of your choice. Make sure that the name of the package is "PKSFC_1.3.tar.gz". Then run the following comand line. It will install the package from your local folder where 'pathToYourFolder' represent the path to the folder where you downloaded the package.

```{r, eval=F}
install.packages("pathToYourFolder/PKSFC_1.3.tar.gz",repos = NULL, type="source")
```

## Rgraphviz

According to the latest [README](http://www.bioconductor.org/packages/2.11/bioc/readmes/Rgraphviz/README):

```{r,eval=FALSE}
Rgraphviz now comes bundles with Graphviz. This should greatly simplify installation 
on all platforms, compared with earlier versions.
```

Bioconductor 2.11 contains a lot of libraries that you might not want or need, but it does seem to be the easiest path to achieving what you want. These are the instructions on the [Rgraphviz homepage](http://www.bioconductor.org/packages/2.11/bioc/html/Rgraphviz.html):

```{r,eval=F}
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
```

In order to test for the success of the installation:
```{r}
library("Rgraphviz")
```