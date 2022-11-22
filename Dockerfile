# Author: Etienne CAMENEN
# Date: 2022
# Contact: etienne.camenen@gmail.com

FROM rocker/rstudio:4.0

MAINTAINER Etienne CAMENEN (etienne.camenen@gmail.com)

ENV _R_CHECK_FORCE_SUGGESTS_ FALSE
ENV PKGS cmake git libcurl4-openssl-dev libglu1-mesa liblapack-dev libssl-dev libxml2-dev qpdf texlive-fonts-recommended texlive-latex-extra texlive-latex-recommended
ARG TOOL_NAME
ARG TOOL_VERSION

RUN apt-get update --allow-releaseinfo-change -qq && \
    apt-get install -y ${PKGS}
RUN apt-get install -y --no-install-recommends libglpk-dev libxt6
ENV RPKGS ade4 BiocManager corrplot cowplot DataExplorer dendextend devtools dlookr factoextra FactoMineR ggExtra ggforce ggplot2 ggpubr ggrepel ggstatsplot golem heatmaply janitor kableExtra knitr kohonen lintr markdown naniar NbClust pam pheatmap plotly pvclust rlist rmarkdown rstatix styler testthat tidylog tidyverse venn visNetwork
RUN Rscript -e "install.packages(commandArgs(TRUE))" ${RPKGS}
RUN Rscript -e "BiocManager::install('BiocCheck')"
RUN Rscript -e 'BiocManager::install("mixOmics")'
RUN R -e "devtools::install_github('ecamenen/"${TOOL_NAME}"', ref = '"${TOOL_VERSION}"')"
RUN Rscript -e 'devtools::install_github("rgcca-factory/RGCCA")'
RUN Rscript -e 'devtools::install_github("moldach/vapoRwave")'
RUN apt-get purge -y git g++ && \
	apt-get autoremove --purge -y && \
	apt-get clean && \
	rm -rf /var/lib/{cache,log}/ /tmp/* /var/tmp/*
COPY . /home/rstudio
