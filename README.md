# Fundamental-Statistics

`R` version 4.1.0

To install libraries:

```R
requirements <- read.csv('requirements.csv')
baseR <- as.data.frame(installed.packages())
toInstall <- setdiff(requirements, baseR)
install.packages(toInstall)
```