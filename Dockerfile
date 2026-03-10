FROM rocker/r-base:latest

# Install Node/Bun
RUN apt-get update && apt-get install -y \
    curl \
    && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://bun.sh/install | bash
ENV PATH="/root/.bun/bin:$PATH"

# Install R packages
RUN R --slave -e "install.packages(c('jsonlite', 'data.table', 'httr'), repos='http://cran.r-project.org')"

WORKDIR /app

COPY package*.json ./
RUN bun install

COPY . .

EXPOSE 3000

CMD ["bun", "run", "server.js"]
