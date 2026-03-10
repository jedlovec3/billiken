FROM oven/bun:latest

# Install R and system dependencies
RUN apt-get update && apt-get install -y \
    r-base \
    r-base-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R --slave -e "install.packages(c('tidyverse', 'jsonlite', 'data.table', 'httr'), repos='http://cran.r-project.org')"

WORKDIR /app

COPY package*.json ./
RUN bun install

COPY . .

EXPOSE 3000

CMD ["bun", "run", "server.js"]
