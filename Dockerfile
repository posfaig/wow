FROM rocker/tidyverse:3.3.2 
MAINTAINER Gergely Pósfai "posfaig@gmail.com"

# Install additional packages
RUN R -e "install.packages(c('lubridate', 'data.table', 'dummies', 'pROC', 'cvTools', 'randomForest', 'igraph', 'scales', 'xgboost', 'RColorBrewer'), repos='https://cran.rstudio.com/')"

# Create directories and get codes
RUN mkdir -p git/wow; git clone https://github.com/posfaig/wow/paper_201701_conf git/wow

# Copy datasets to the docker image
COPY data/ git/wow/data/

# Execute computations
CMD cd git/wow && Rscript guild_quitting/00_run_process.R

