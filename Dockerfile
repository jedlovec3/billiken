FROM oven/bun:latest

# Install R and minimal dependencies
RUN apt-get update && apt-get install -y \
    r-base \
    && rm -rf /var/lib/apt/lists/*

# Install R packages with timeout and simpler approach
RUN timeout 600 R --slave -e "install.packages('jsonlite', repos='http://cran.r-project.org')" || true
RUN timeout 600 R --slave -e "install.packages('data.table', repos='http://cran.r-project.org')" || true
RUN timeout 600 R --slave -e "install.packages('httr', repos='http://cran.r-project.org')" || true

WORKDIR /app

COPY package*.json ./
RUN bun install

COPY . .

EXPOSE 3000

CMD ["bun", "run", "server.js"]
