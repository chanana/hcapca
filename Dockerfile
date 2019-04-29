FROM rocker/shiny:3.5.2
FROM rocker/tidyverse:3.5.2

RUN echo 'install.packages(c("data.table", "dendextend", "data.tree", "plotly", "config"), repos="http://cran.us.r-project.org", dependencies=TRUE)' > /tmp/packages.R \
  Rscript /tmp/packages.R
