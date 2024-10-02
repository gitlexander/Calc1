# Use the official R image from the Docker Hub
FROM r-base:4.4.0

# Install system dependencies needed for R packages without specifying versions
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libsodium-dev \
    cmake \
    libnlopt-dev \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('plumber', 'nloptr', 'webutils', 'readr', 'readxl', 'ggplot2', 'data.table', 'tidyr', 'car', 'dplyr', 'caret', 'MASS', 'lsa'), repos='http://cran.us.r-project.org')"

# Copy the Excel sheets with data into the Docker image
COPY Data96.xlsx /Data96.xlsx
COPY Data96.csv /Data96.csv

# Copy the API script into the Docker image
COPY plumber.R /plumber.R

# Expose the port Plumber will run on
EXPOSE 8000

# Optionally create a non-root user
RUN useradd -ms /bin/sh ruser
USER ruser

# Run the Plumber API
CMD ["R", "-e", "pr <- plumber::plumb('/plumber.R'); pr$run(host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 8000)))"]
