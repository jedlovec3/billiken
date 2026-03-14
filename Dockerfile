FROM oven/bun:latest

# Install R
RUN apt-get update && apt-get install -y \
    r-base \
    --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY package*.json ./
RUN bun install

COPY . .

EXPOSE 3000

CMD ["bun", "run", "server.js"]