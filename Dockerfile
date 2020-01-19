FROM rocker/shiny-verse:3.5.2

RUN install2.r --error \
  collapsibleTree \
  config \
  data.table \
  data.tree \
  dendextend \
  plotly \
  shinydashboard \
  DT

ENV PATH="/srv/shiny-server/hcapca:${PATH}"
