# MESOCP: Modern Elementary Statistics Online Computing Platform

![R](https://img.shields.io/badge/R-V4.1-green)
[![DOI](https://zenodo.org/badge/443600562.svg)](https://zenodo.org/badge/latestdoi/443600562)

A online statistics teaching assisted platform, Proudly powered by `R` and [`shiny`](https://shiny.rstudio.com/) 

> Code of [SETE2021 #21](https://sete2021.uic.edu.cn/index.htm):
> 
> citation: LIU, J., Peng, X, & DENG, Y. (2021). Online Statistics Teaching-assisted Platform with Interactive Web Applications using R Shiny. (accepted by) The 6th International Symposium on emerging Technologies for Education

## To run the code

For now, you can clone the repo:

```bash
git clone https://github.com/Bayes-Cluster/MESOCP.git
```
or just download the [zip file](https://github.com/Bayes-Cluster/MESOCP/archive/refs/heads/master.zip) and open the folder with [RStudio](https://www.rstudio.com/). 

Alternatively, if you has a self-host Shiny-server, you can simply move the apps into directory of application (by default, the directory should be `/srv/shiny-server/`), subsitute user to `shiny`/`root` with `sudo su shiny`/`sudo su` and install the preliminary packages:

```R
packages_list <- c("shiny", "knitr", "bslib", "ggplot2", "plotly", "leaflet", "scales", "rgeos", "tidyr", "DT", "shinydashboard", "dplyr", "shinydisconnect")
install.packages(packages_list, dep = T)
```

For Mainland China's users, you can download these packages with the BFSU/Tsinghua mirrors for getting a faster speed,
```R
packages_list <- c("shiny", "knitr", "bslib", "ggplot2", "plotly", "leaflet", "scales", "rgeos", "tidyr", "DT", "shinydashboard", "dplyr", "shinydisconnect")
install.packages(packages_list, dep = T, repos="https://mirrors.bfsu.edu.cn/CRAN/") # with BFSU mirrors
install.packages(packages_list, dep = T, repos="https://mirrors.tuna.tsinghua.edu.cn/CRAN/") # with Tsinghua mirrors
```

## To cite this code
```shell
@software{terencelau_2022_5813042,
  author       = {TerenceLau},
  title        = {TerenceLiu98/MESOCP: MESOCP},
  month        = jan,
  year         = 2022,
  publisher    = {Zenodo},
  version      = {V0.1},
  doi          = {10.5281/zenodo.5813042},
  url          = {https://doi.org/10.5281/zenodo.5813042}
}
```
