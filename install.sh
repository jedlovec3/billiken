#!/bin/bash
set -e

# Install R
apt-get update
apt-get install -y r-base

# Install R packages
R --slave -e "install.packages(c('jsonlite', 'data.table', 'httr'), repos='http://cran.r-project.org')"

# Install Node dependencies
npm install
