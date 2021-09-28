FROM openjdk:11

ARG USERNAME=root

RUN apt-get -y update --no-install-recommends \
    && apt-get -y install --no-install-recommends \
    build-essential \
    curl \
    ca-certificates \
    apt-utils \
    dialog \
    git \
    vim \
	scala \
    && apt-get autoremove -y \
    && apt-get clean -y

