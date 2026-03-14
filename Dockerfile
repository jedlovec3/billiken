FROM oven/bun:latest

# Install R and system dependencies required by tidyverse/systemfonts
RUN apt-get update && apt-get install -y \
    r-base \
    r-base-dev \
    build-essential \
    gfortran \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff5-dev \
    pkg-config \
    --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY package*.json ./
RUN bun install

COPY . .

# Install renv and restore packages during build (not runtime)
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"
RUN R -e "if (file.exists('renv.lock')) renv::restore()"

EXPOSE 3000

CMD ["bun", "server.js"]
