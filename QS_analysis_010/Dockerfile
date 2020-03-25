# QS analysis 001 -
# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:3.5.1

# For ubuntu
RUN apt-get update && apt-get install libcurl4-openssl-dev libv8-3.14-dev -y &&\
  mkdir -p /var/lib/shiny-server/bookmarks/shiny

## gnupg is needed to add new key
RUN apt-get update && apt-get install -y gnupg2

RUN apt-get update \
  && apt-get install -y default-jdk
  
RUN R CMD javareconf \
  && apt-get update && apt-get install -y r-cran-rjava

RUN R -e "install.packages(c('rJava'),repos='http://cran.us.r-project.org')"

# RUN apt-get install labxml2
#RUN apt-get install r-cran-xml
#RUN apt-get install libssl-dev

# Download and install library
RUN R -e "install.packages(c('shiny'),repos='http://cran.us.r-project.org')"
RUN R -e "install.packages(c('shinydashboard'),repos='http://cran.us.r-project.org')"
RUN R -e "install.packages(c('shinyFiles'),repos='http://cran.us.r-project.org')"
RUN R -e "install.packages(c('dplyr'),repos='http://cran.us.r-project.org')"
RUN R -e "install.packages(c('ggplot2'),repos='http://cran.us.r-project.org')"
RUN R -e "install.packages(c('xlsx'),repos='http://cran.us.r-project.org')"
RUN R -e "install.packages(c('DT'),repos='http://cran.us.r-project.org')"
RUN R -e "install.packages(c('tidyr'),repos='http://cran.us.r-project.org')"
RUN R -e "install.packages(c('readxl'),repos='http://cran.us.r-project.org')"

# copy files
COPY shiny /srv/shiny-server/

# Make all app files readable
RUN chmod -R +r /srv/shiny-server/

# Make the shiny server readable
RUN chown shiny:shiny /var/lib/shiny-server

# expose port
EXPOSE 3838

# start shiny server
CMD ["/usr/bin/shiny-server.sh"]