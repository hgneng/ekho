FROM ubuntu:latest

# These commands copy your files into the specified directory in the image
# and set that as the working location
COPY . /usr/src/pitch-detection
WORKDIR /usr/src/pitch-detection

# Install dependencies from apt
RUN apt-get update && DEBIAN_FRONTEND="noninteractive" apt-get install -y \
git \
cmake \
gcc \
g++ \
libblas-dev \
liblapack-dev \
libboost-dev \
libarmadillo-dev \
libmlpack-dev \
libgtest-dev \
libbenchmark-dev

# Get and install ffts library
RUN cd /usr/src \
&& git clone https://github.com/anthonix/ffts.git ffts-repo \
&& cd ffts-repo \
&& mkdir build \
&& cd build \
&& cmake .. \
&& make install

# Build and install the pitch-detection library, as well as the tests and benchmarks
RUN cd /usr/src/pitch-detection && make clean all && make -C test clean all && make install

LABEL Name=pitch-detection Version=0.0.1
