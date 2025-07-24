ARG GID=1000
ARG UID=1000

ARG BASE_IMAGE_VERSION=22.04
ARG GHC_VERSION=9.10.2
ARG HLS_VERSION=2.11.0.0
ARG CABAL_VERSION=3.14.1.1
ARG FOURMOLU_VERSION=0.18.0.0
ARG HLINT_VERSION=3.10
ARG APPLY_REFACT_VERSION=0.15.0.0
ARG GHCID_VERSION=0.8.9
ARG GHC_TAGS_VERSION=1.9
ARG POSTGRESQL_MIGRATION_VERSION=0.2.1.8

# This stage installs libraries required to install GHC and other tools
FROM ubuntu:$BASE_IMAGE_VERSION AS base
USER "root"
ENV LANG="en_GB.UTF-8"

RUN apt update && \
  apt install -y  \
    build-essential \
    curl \
    git \
    libffi-dev \
    libffi8 \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    libncurses5 \
    libtinfo5 \
    locales \
    pkg-config \
    zlib1g-dev
RUN localedef -i en_GB -c -f UTF-8 -A /usr/share/locale/locale.alias en_GB.UTF-8
RUN rm -rf /var/lib/apt/lists/* /usr/share/doc /usr/share/man

# This stage installs ghcup, ghc and cabal
FROM base AS ghcup
ARG GHC_VERSION
ARG CABAL_VERSION

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE="YES"
ENV BOOTSTRAP_HASKELL_GHC_VERSION="$GHC_VERSION"
ENV BOOTSTRAP_HASKELL_MINIMAL="YES"
ENV GHCUP_INSTALL_BASE_PREFIX="/out/ghcup"

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

RUN ls -alh /out/ghcup
RUN ls -alh /out/ghcup/.ghcup/bin
ENV PATH="/out/ghcup/.ghcup/bin:$PATH"
RUN echo $PATH

RUN ghcup install cabal $CABAL_VERSION

# This stage installs haskell tools
FROM base as setup-haskell-tools
ENV PATH="/opt/ghcup/.ghcup/bin:$PATH"
RUN echo "$PATH"

COPY --from=ghcup /out/ghcup /opt/ghcup

RUN ls -alh /opt/ghcup/
RUN ls -alh /opt/ghcup/.ghcup/bin

RUN cabal update

RUN cabal install --install-method=copy --installdir=out/ --semaphore -j postgresql-migration-$POSTGRESQL_MIGRATION_VERSION
RUN cabal install --install-method=copy --installdir=out/ --semaphore -j hlint-$HLINT_VERSION
RUN cabal install --install-method=copy --installdir=out/ --semaphore -j fourmolu-$FOURMOLU_VERSION
RUN cabal install --install-method=copy --installdir=out/ --semaphore -j ghcid-$GHCID_VERSION
RUN cabal install --install-method=copy --installdir=out/ --semaphore -j ghc-tags-$GHC_TAGS_VERSION
RUN ghcup run --ghc 9.6.7 -- cabal install --install-method=copy --installdir=out/ -j apply-refact-$APPLY_REFACT_VERSION

# This stage is the development environment
FROM base AS devel
ARG GID
ARG UID
ARG USER="local"

COPY --from=ghcup /out/ghcup /opt/ghcup
COPY --from=setup-haskell-tools /out /opt/bin

RUN apt update
RUN apt install -y libpq-dev wget tmux postgresql-client
RUN git config --global --add safe.directory "*"

RUN groupadd -g "$GID" -o "$USER" \
  && useradd -r -u "$UID" -g "$GID" -m -s /bin/bash "$USER"

RUN mkdir /masna3
RUN chown $USER:$USER /masna3

RUN mkdir /home/$USER/.cabal
RUN chown -R $USER:$USER /home/$USER/.cabal

WORKDIR /masna3
COPY --chown=$UID:$GID . .

USER "local"

RUN echo 'export PATH="$PATH:/home/$USER/.cabal/bin"' >> ~/.bashrc
RUN echo "source /opt/ghcup/.ghcup/env" >> ~/.bashrc

RUN ls -alh

USER "local"
