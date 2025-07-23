ARG GID=1000
ARG UID=1000

ARG BASE_IMAGE_VERSION=22.04
ARG GHC_VERSION=9.10.1
ARG CABAL_VERSION=3.14.1.1
ARG FOURMOLU_VERSION=0.18.0.0
ARG HLINT_VERSION=3.10
ARG APPLY_REFACT_VERSION=0.15.0.0
ARG GHCID_VERSION=0.8.9
ARG GHC_TAGS_VERSION=1.9
ARG POSTGRESQL_MIGRATION_VERSION=0.2.1.8

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

ARG GID
ARG UID

RUN groupadd -g "$GID" -o "local" \
  && useradd -r -u "$UID" -g "$GID" -m -s /bin/bash "$USER"

FROM base as setup-haskell
USER "root"
RUN echo 'export PATH="$PATH:/home/$USER/.cabal/bin"' > /etc/profile
ARG USER="local"

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE="YES"
ENV BOOTSTRAP_HASKELL_INSTALL_NO_STACK="YES"
ENV BOOTSTRAP_HASKELL_INSTALL_NO_STACK_HOOK="YES"
ENV PATH="$PATH:/home/$USER/.ghcup/bin"
RUN apt install -y libpq-dev mcpp wget tmux postgresql-client
RUN corepack enable
RUN git config --global --add safe.directory "*"
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

RUN echo $PATH

RUN ghcup install hls $HLS_VERSION \
  && ghcup install ghc $GHC_VERSION \
  && ghcup set ghc $GHC_VERSION \
  && ghcup install cabal $CABAL_VERSION

RUN cabal update
RUN chmod ugo+x /home/$USER/.cabal
RUN cd ~/ && cabal install -j postgresql-migration-$POSTGRESQL_MIGRATION_VERSION
RUN cd ~/ && cabal install -j hlint-$HLINT_VERSION
RUN cd ~/ && ghcup run --ghc 9.6.7 -- cabal install -j apply-refact-$APPLY_REFACT_VERSION
RUN cd ~/ && cabal install -j fourmolu-$FOURMOLU_VERSION
RUN cd ~/ && cabal install -j ghcid-$GHCID_VERSION
RUN cd ~/ && cabal install -j ghc-tags-$GHC_TAGS_VERSION

FROM setup-haskell AS setup-project

# We create the folder explicitly so that we can give nonprivileged user the appropriate access
RUN mkdir /masna3
RUN chown $USER:$USER /masna3

RUN mkdir /home/$USER/.cabal
RUN chown -R $USER:$USER /home/$USER/.cabal

WORKDIR /masna3
COPY --chown=$UID:$GID .

RUN ls -alh
RUN cabal build --only-dependencies -j

USER ${USER}
