FROM amd64/fedora:latest

SHELL ["/bin/bash", "--rcfile", "~/.profile", "-c"]

USER root

RUN yum update -y
RUN yum install -y git \
  git-lfs \
  pkgconfig \
  gcc \
  gcc-c++ \
  tmux \
  gmp \
  gmp-devel \
  make \
  tar \
  xz \
  wget \
  libtool \
  autoconf \
  ncurses \
  ncurses-compat-libs \
  xz-devel \
  perl \
  zlib \
  zlib-devel \
  ncurses \
  ncurses-devel \
  libsodium \
  libsodium-devel \
  systemd-devel \
  which \
  jq \
  openssl-devel \
  lmdb-devel

# We need a custom fork of libsodium which exposes some internal functions and adds some other new functions.
# See https://github.com/input-output-hk/cardano-node/blob/master/doc/getting-started/install.md#installing-libsodium

RUN rm -rf /tmp/src && mkdir /tmp/src

WORKDIR /tmp/src
RUN git clone https://github.com/input-output-hk/libsodium
WORKDIR /tmp/src/libsodium
RUN git checkout 66f017f1 \
  && ./autogen.sh \
  && ./configure \
  && make \
  && sudo make install

# we also need libsecp256k1
WORKDIR /tmp/src
RUN git clone https://github.com/bitcoin-core/secp256k1
WORKDIR /tmp/src/secp256k1
RUN git checkout ac83be33 \
  && ./autogen.sh \
  && ./configure --enable-module-schnorrsig --enable-experimental \
  && make \
  && sudo make install

# make sure we tell the build tools and the link-loader about libsodium-vrf and libsecp256k1
ENV LD_LIBRARY_PATH="/usr/local/lib:${LD_LIBRARY_PATH}"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

# Create USER builder 
RUN useradd -ms /bin/bash builder

# switch to USER = builder and make CWD /home/builder
USER builder
ENV USER=builder

WORKDIR /home/builder

# This is where we will bind-mount the root dir of the repo we're building
RUN mkdir /home/builder/repo

# set up git LFS 
RUN git lfs install

# Install Nix and ghcup and then install the verions of ghc, cabal and stack that we want. 
# The versions are made explicit
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
  && source /home/builder/.ghcup/env \
  && /home/builder/.ghcup/bin/ghcup install ghc 8.10.7 --force --set \
  && /home/builder/.ghcup/bin/ghcup install cabal 3.6.2.0 --force \
  && /home/builder/.ghcup/bin/ghcup set cabal 3.6.2.0 \
  && /home/builder/.ghcup/bin/ghcup install stack 2.7.5 --force \
  && /home/builder/.ghcup/bin/ghcup set stack 2.7.5

# Add /home/builder/.ghcup/bin to PATH so we don't have to remember to do it all the time
ENV PATH="/home/builder/.ghcup/bin:${PATH}"

# Test we can build
USER root
RUN rm -rf /home/builder/test \
  && mkdir -p /home/builder/test
COPY --chown=builder:builder . /home/builder/test
RUN echo "PATH=${PATH}"
RUN  su -s /bin/bash -c 'cd /home/builder/test && CABAL_BUILDDIR=/home/builder/test/docker-dist-newstyle cabal build all' - builder \
  && cd /home/builder \
  && rm -rf /home/builder/test
