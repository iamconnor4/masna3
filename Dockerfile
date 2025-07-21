# this is a pinned ubuntu:24.04
FROM ubuntu:22.04

ARG GID=1000
ARG UID=1000

ARG GHC_VERSION=9.10.1
ARG CABAL_VERSION=3.14.1.1
ARG FOURMOLU_VERSION=0.18.0.0
ARG HLINT_VERSION=3.10
ARG APPLY_REFACT_VERSION=0.15.0.0
ARG GHCID_VERSION=0.8.9
ARG GHC_TAGS_VERSION=1.9
ARG POSTGRESQL_MIGRATION_VERSION=0.2.1.8

# generate a working directory
USER "root"
ARG USER="local"
RUN apt update && apt install -y zsh
RUN groupadd -g "$GID" -o "$USER" \
  && useradd -r -u "$UID" -g "$GID" -m -s /bin/zsh "$USER"

# We create the folder explicitly so that we can give nonprivileged user the appropriate access
RUN mkdir /masna3
RUN chown $USER:$USER /masna3

RUN mkdir /home/$USER/.cabal
RUN chown -R $USER:$USER /home/$USER/.cabal
WORKDIR /masna3

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
    libsodium-dev \
    libtinfo5 \
    locales \
    pkg-config \
    zlib1g-dev
RUN localedef -i en_GB -c -f UTF-8 -A /usr/share/locale/locale.alias en_GB.UTF-8
RUN rm -rf /var/lib/apt/lists/* /usr/share/doc /usr/share/man

ENV LANG="en_GB.UTF-8"

# install dependencies (pg_config, postgresql-client, yarn)
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE="YES"
ENV BOOTSTRAP_HASKELL_INSTALL_NO_STACK="YES"
ENV BOOTSTRAP_HASKELL_INSTALL_NO_STACK_HOOK="YES"
ENV PATH="$PATH:/home/$USER/.ghcup/bin"
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN apt install -y nodejs libpq-dev mcpp wget tmux postgresql-client
RUN corepack enable
USER ${USER}
RUN chmod ugo+x /home/$USER/.cabal
RUN git config --global --add safe.directory "*"
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

RUN ghcup install hls $HLS_VERSION \
  && ghcup install ghc $GHC_VERSION \
  && ghcup set ghc $GHC_VERSION \
  && ghcup install cabal $CABAL_VERSION

USER ${USER}

RUN echo $PATH

RUN cabal update
RUN cd ~/ && cabal install -j postgresql-migration-$POSTGRESQL_MIGRATION_VERSION
RUN cd ~/ && cabal install -j hlint-$HLINT_VERSION
RUN cd ~/ && ghcup run --ghc 9.6.7 -- cabal install -j apply-refact-$APPLY_REFACT_VERSION
RUN cd ~/ && cabal install -j fourmolu-$FOURMOLU_VERSION
RUN cd ~/ && cabal install -j ghcid-$GHCID_VERSION
RUN cd ~/ && cabal install -j ghc-tags-$GHC_TAGS_VERSION

# build Haskell dependencies
COPY --chown=${USER} cabal.project api server cabal.project.freeze ./
RUN ls -alh
RUN cabal build --only-dependencies -j

# copy makefile
COPY --chown=${USER} Makefile ./

COPY --chown=${USER} docs ./

USER root
RUN echo 'export PATH="$PATH:/home/$USER/.cabal/bin"' > /etc/profile
USER ${USER}
