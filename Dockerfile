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
    libglpk-dev \
    libwebp-dev \
    libharfbuzz-dev \
    libfribidi-dev \
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

# ---- STEP 1: install renv ----
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')"

# ---- STEP 2: copy only dependency files ----
COPY renv.lock renv.lock
COPY renv/ renv/

# ---- STEP 3: restore packages (cached layer) ----
RUN R -e "renv::restore()"

# ---- STEP 4: copy rest of project ----
COPY . .

EXPOSE 3000

CMD ["bun", "server.js"]
