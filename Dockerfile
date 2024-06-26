# Select R version
FROM r-base:4.4.1

# Setup environment
ENV R_SHINY_PORT=3838

# Set working directory and copy resources.
WORKDIR /workspace
COPY . /workspace

# Compile R dependencies.
RUN R -e "renv::restore(prompt = FALSE)"

# Set entrypoint
ENTRYPOINT ["Rscript", "app.R"]
