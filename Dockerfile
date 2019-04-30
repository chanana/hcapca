ARG VERSION=3.5.2
FROM rocker/tidyverse:${VERSION}

RUN echo 'install.packages(c("dendextend", "data.table", "data.tree", "plotly", "config"))' > /tmp/packages.R && Rscript /tmp/packages.R

# setup filesystem and copy scripts and data
RUN mkdir -p /hcapca/src
RUN mkdir -p /hcapca/data
COPY "./src/*.R" "hcapca/src/"
#COPY $(pwd)/data/*.dat /usr/local/src/hcapca/data
#COPY $(pwd)/config_file.yaml /usr/local/src/hcapca
