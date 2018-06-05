FROM ubuntu:latest@sha256:e7def0d56013d50204d73bb588d99e0baa7d69ea1bc1157549b898eb67287612

RUN apt-get update && \
      apt-get install -y software-properties-common && \
      apt-add-repository -y ppa:bitcoin/bitcoin && \
      apt-get update && \
      apt-get install -y bitcoind curl make gcc git && \
      curl -sL https://deb.nodesource.com/setup_10.x | bash - && \
      apt-get install -y nodejs && \
      apt-get clean

ENV NPM_CONFIG_PREFIX=/root/.npm-global
ENV PATH=/root/.npm-global/bin:$PATH

RUN mkdir /root/.npm-global && \
      npm -g install yarn


RUN export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)" && \
      echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list && \
      curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add - && \
      apt-get update && apt-get -y install google-cloud-sdk && apt-get clean
