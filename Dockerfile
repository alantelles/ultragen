FROM ubuntu:focal
RUN apt update && apt install openssl -y
WORKDIR /opt/ultragen
COPY ./build .
RUN mkdir /application
WORKDIR /application
ENV ULTRAGEN_HOME="/opt/ultragen"
ENV PATH=$PATH:$ULTRAGEN_HOME/bin
ENV LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ULTRAGEN_HOME/unix/libsagui-3.3.2/lib64
CMD ["ultragen"]
