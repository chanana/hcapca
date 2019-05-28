FROM rocker/shiny-verse:3.5.2

RUN install2.r --error \
  collapsibleTree \
  config \
  data.table \
  data.tree \
  dendextend \
  plotly \
  shinydashboard

#COPY *.R /srv/shiny-server/hcapca/
#RUN chmod 
