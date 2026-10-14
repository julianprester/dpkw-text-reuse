# Reproducible R environment for the text-reuse pipeline.
# Build:  docker build -t text-reuse-r analysis/
# Run:    see analysis/README.md
FROM rocker/tidyverse:4.6

RUN install2.r --error --skipinstalled tidytext textreuse tokenizers stopwords fixest lme4 \
    && rm -rf /tmp/downloaded_packages
