FROM rocker/r-ver:4.4.1

# Install system libraries needed by common R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    libicu-dev \
    libx11-dev \
    pandoc \
    nodejs \
    npm \
    && rm -rf /var/lib/apt/lists/*

# Install Bun
RUN npm install -g bun

WORKDIR /app

# Install JS dependencies first (better caching)
COPY package*.json ./
RUN bun install

# Copy project files
COPY . .

# Install renv and restore R packages
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"
RUN R -e "renv::restore()"

EXPOSE 3000

CMD ["bun", "server.js"]
