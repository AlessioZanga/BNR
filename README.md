# BNR Project

## Description

This project leverages graphical models to perform queries with constrained bayesian estimation.
This work is developed as a Shiny app in order to provide an easy and clean interaction.

## How to Use

### Option 1 - The Docker container

Build the Docker container using:

    docker build -t maxplank/bnr .

Run the application by binding to a port:

    docker run -p 8080:3838 maxplank/bnr

Open the browser at [http://localhost:8080/](http://localhost:8080/).

### Option 2 - Configure the Rproject

Install system dependencies:

    apt-get install -y \
        libcurl4-openssl-dev \
        libgmp-dev \
        libmpfr-dev \
        libssl-dev \
        libxml2-dev

Then, from `RStudio`, run `renv` to install `R` dependencies:

    renv::restore()

Run the application by executing:

    shiny::runApp()

The browser automatically opens on the right webpage.

## About

This software has been developed by [A. Zanga](mailto:alessio.zanga@unimib.it), [M. Scutari](mailto:scutari@bnlearn.com) and [R. Fernandes](fernandes@shh.mpg.de).
