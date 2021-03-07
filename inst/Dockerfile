FROM rocker/r-ver:4.0.3
RUN apt-get update \
  && apt-get install -y \
  ffmpeg libicu-dev make libavfilter-dev \
  && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(REPO_NAME = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest', download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
# RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never")'
RUN Rscript -e 'remotes::install_version("fs",upgrade="never")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never")'
RUN Rscript -e 'remotes::install_version("shinythemes",upgrade="never")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never")'
RUN Rscript -e 'remotes::install_version("colourpicker",upgrade="never")'

# RUN Rscript -e 'remotes::install_version("EBImage",upgrade="never", version = "4.32.0")'
# RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.6.0")'

# RUN Rscript -e 'remotes::install_version("av",upgrade="never", version = "0.5.1")'

# RUN Rscript -e 'remotes::install_github("emphazis-org/emphazis@main", upgrade="never")'

# RUN Rscript -e 'remotes::install_github("emphazis-org/shinyemphazis@main", upgrade="never")'

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
EXPOSE 3838

CMD R -e "emphazis::emphazis_app(host = '0.0.0.0', port = 3838, launch.browser = FALSE)"

#CMD R -e "shinyemphazis::run_app(host = '0.0.0.0', port = 3838, launch.browser = FALSE)"
