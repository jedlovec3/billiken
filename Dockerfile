FROM oven/bun:latest

# Install R and build dependencies
RUN apt-get update && apt-get install -y \
    r-base \
    r-base-dev \
    build-essential \
    gfortran \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY package*.json ./
RUN bun install

COPY . .

# Install R packages
RUN Rscript -e "install.packages(c('tidyverse', 'jsonlite', 'data.table', 'httr'), repos='http://cran.r-project.org', dependencies=TRUE)"

EXPOSE 3000

CMD ["bun", "run", "server.js"]
