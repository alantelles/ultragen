FROM ubuntu:focal
RUN apt update && apt install openssl -y
WORKDIR /opt/ultragen
COPY ./build .
RUN mkdir /application
WORKDIR /application
ENV ULTRAGEN_HOME="/opt/ultragen"
ENV PATH=$PATH:$ULTRAGEN_HOME/bin
ENTRYPOINT ["ultragen"]
