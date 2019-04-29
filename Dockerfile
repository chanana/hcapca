ARG VERSION=3.5.2
FROM rocker/shiny:${VERSION}
FROM rocker/tidyverse:${VERSION}

RUN echo 'install.packages(c("data.table", "dendextend", "data.tree", "plotly", "config"), repos="http://cran.us.r-project.org", dependencies=TRUE)' > /tmp/packages.R \
  Rscript /tmp/packages.R
