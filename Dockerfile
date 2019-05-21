ARG VERSION=3.5.2
FROM rocker/tidyverse:${VERSION}

# Install extra libraries
RUN echo 'install.packages(c("dendextend", "data.table", "data.tree", "plotly", "config","ape","shinydashboard","networkD3"))' > /tmp/packages.R && Rscript /tmp/packages.R

# Location of scripts inside container: /script/
COPY "./src/*.R" "/script/"

# Add scripts to PATH
ENV PATH=$PATH:/script
