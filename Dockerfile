# Start with GHC (Glasgow Haskell Compiler) base image
FROM haskell:9.4

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && \
    apt-get install -y \
    libgmp-dev \
    zlib1g-dev \
    libtinfo-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy .cabal file first to cache dependencies
COPY *.cabal ./
RUN cabal update

# Install project dependencies
RUN cabal build --only-dependencies

# Copy the rest of the application
COPY . .

# Build the application
RUN cabal build

# Create a new stage for a smaller final image
FROM debian:bullseye-slim

# Install runtime dependencies
RUN apt-get update && \
    apt-get install -y \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the built executable from the previous stage
# Replace 'your-executable' with your actual executable name
COPY --from=0 /app/dist-newstyle/build/**/its-high-noon/its-high-noon .

# Set the entry point
ENTRYPOINT ["./its-high-noon"]