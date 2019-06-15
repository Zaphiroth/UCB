FROM rocker/r-base
MAINTAINER Pharbers <contact@pharbershub>
RUN apt-get update
RUN apt-get install openssl@1.1
RUN R -e 'install.packages("plyr", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("dplyr", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("tidyr", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("DT", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("jsonlite", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("curl", repos = "http://cran.cnr.berkeley.edu/")'
RUN R -e 'install.packages("uuid", repos = "http://cran.cnr.berkeley.edu/")'
COPY Functions.R /Functions.R
COPY Kafka.R /Kafka.R
COPY Calculation.R /Calculation.R
COPY Main.R /Main.R
COPY intermedia.RData /intermedia.RData
CMD Rscript /Main.R
