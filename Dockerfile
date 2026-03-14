FROM node:20-slim

# Install R
RUN apt-get update && apt-get install -y \
    r-base \
    --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

# Install R packages one by one with error handling
RUN R --slave -e "install.packages('jsonlite', repos='http://cran.r-project.org')" 2>&1 || echo "jsonlite install attempted"
RUN R --slave -e "install.packages('data.table', repos='http://cran.r-project.org')" 2>&1 || echo "data.table install attempted"
RUN R --slave -e "install.packages('httr', repos='http://cran.r-project.org')" 2>&1 || echo "httr install attempted"

WORKDIR /app

COPY package*.json ./
RUN npm install

COPY . .

EXPOSE 3000

CMD ["node", "server.js"]