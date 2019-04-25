FROM rocker/tidyverse:3.5.2
FROM rocker/rstudio:3.5.2

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev

RUN echo 'install.packages(c("data.table", "dendextend", "data.tree", "plotly", "config"), repos="http://cran.us.r-project.org", dependencies=TRUE)' > /tmp/install_packages.R \
  && Rscript /tmp/install_packages.R

