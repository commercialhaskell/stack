ARG BUILD_ON_IMAGE=quay.io/benz0li/ghc-musl
ARG GHC_VERSION=latest
ARG SUBTAG
ARG HLS_VERSION
ARG STACK_VERSION

ARG HLS_GHC_VERSION=${HLS_VERSION:+$GHC_VERSION}
ARG HLS_IMAGE_TAG=${HLS_VERSION:-none}-ghc${HLS_GHC_VERSION:-all}

ARG STACK_VERSION_OVERRIDE=${STACK_VERSION}

FROM ${BUILD_ON_IMAGE}:${GHC_VERSION}${SUBTAG:+-}${SUBTAG} AS files

RUN mkdir /files

COPY conf/shell /files
COPY conf/stack /files
COPY scripts /files

## Ensure file modes are correct
RUN find /files -type d -exec chmod 755 {} \; \
  && find /files -type f -exec chmod 644 {} \; \
  && find /files/usr/local/bin -type f -exec chmod 755 {} \;

FROM quay.io/benz0li/hlssi:${HLS_IMAGE_TAG} AS hlssi

FROM quay.io/benz0li/hlsi:latest AS hlsi

FROM docker.io/koalaman/shellcheck:stable AS sci

FROM ${BUILD_ON_IMAGE}:${GHC_VERSION}${SUBTAG:+-}${SUBTAG}

COPY --from=files /files /

RUN sysArch="$(uname -m)" \
  ## Ensure that common CA certificates
  ## and OpenSSL libraries are up to date
  && apk upgrade --no-cache ca-certificates openssl-dev \
  ## Install pip
  && apk add --no-cache py3-pip \
  ## Install terminal multiplexers
  && apk add --no-cache screen tmux \
  ## Install yamllint
  && apk add --no-cache yamllint \
  ## Install hadolint
  && case "$sysArch" in \
    x86_64) tarArch="x86_64" ;; \
    aarch64) tarArch="arm64" ;; \
    *) echo "error: Architecture $sysArch unsupported"; exit 1 ;; \
  esac \
  && apiResponse="$(curl -sSL \
    https://api.github.com/repos/hadolint/hadolint/releases/latest)" \
  && downloadUrl="$(echo "$apiResponse" | grep -e \
    "browser_download_url.*Linux-$tarArch\"" | cut -d : -f 2,3 | tr -d \")" \
  && echo "$downloadUrl" | xargs curl -sSLo /usr/local/bin/hadolint \
  && chmod 755 /usr/local/bin/hadolint

## Update environment
ARG USE_ZSH_FOR_ROOT
ARG LANG
ARG TZ

ARG LANG_OVERRIDE=${LANG}
ARG TZ_OVERRIDE=${TZ}

ENV LANG=${LANG_OVERRIDE:-$LANG} \
    TZ=${TZ_OVERRIDE:-$TZ}

  ## Change root's shell to ZSH
RUN if [ -n "$USE_ZSH_FOR_ROOT" ]; then \
    apk add --no-cache zsh shadow; \
    fix-chsh.sh; \
    chsh -s /bin/zsh; \
  fi \
  ## Update timezone if requested
  && if [ "$TZ" != "" ]; then \
    apk add --no-cache tzdata; \
  fi \
  ## Info about timezone
  && echo "TZ is set to $TZ" \
  ## Add/Update locale if requested
  && if [ "$LANG" != "C.UTF-8" ]; then \
    if [ -n "$LANG" ]; then \
      apk add --no-cache musl-locales musl-locales-lang; \
    fi; \
    sed -i "s/LANG=C.UTF-8/LANG=$LANG/" /etc/profile.d/*locale.sh; \
    sed -i "s/LANG:-C.UTF-8/LANG:-$LANG/" /etc/profile.d/*locale.sh; \
    sed -i "s/LC_COLLATE=C/LC_COLLATE=$LANG/" /etc/profile.d/*locale.sh; \
    sed -i "s/LC_COLLATE:-C/LC_COLLATE:-$LANG/" /etc/profile.d/*locale.sh; \
  fi \
  ## Info about locale
  && echo "LANG is set to $LANG"

## Copy binaries as late as possible to avoid cache busting
## Install HLS
COPY --from=hlssi /usr/local /usr/local
## Install HLint
COPY --from=hlsi /usr/local /usr/local
## Install ShellCheck
COPY --from=sci --chown=root:root /bin/shellcheck /usr/local/bin

ARG HLS_VERSION
ARG STACK_VERSION

ARG STACK_VERSION_OVERRIDE

ENV HLS_VERSION=${HLS_VERSION} \
    STACK_VERSION=${STACK_VERSION_OVERRIDE:-$STACK_VERSION}

RUN if [ -n "$STACK_VERSION_OVERRIDE" ]; then \
    ## Install Stack
    cd /tmp || exit ;\
    curl -sSLO https://github.com/commercialhaskell/stack/releases/download/v"$STACK_VERSION"/stack-"$STACK_VERSION"-linux-"$(uname -m)".tar.gz; \
    curl -sSLO https://github.com/commercialhaskell/stack/releases/download/v"$STACK_VERSION"/stack-"$STACK_VERSION"-linux-"$(uname -m)".tar.gz.sha256; \
    sha256sum -cs stack-"$STACK_VERSION"-linux-"$(uname -m)".tar.gz.sha256; \
    tar -xzf stack-"$STACK_VERSION"-linux-"$(uname -m)".tar.gz; \
    if dpkg --compare-versions "$GHC_VERSION" lt "9.2.8"; then \
      mv -f stack-"$STACK_VERSION"-linux-"$(uname -m)"/stack /usr/bin/stack; \
    else \
      mv -f stack-"$STACK_VERSION"-linux-"$(uname -m)"/stack /usr/local/bin/stack; \
    fi; \
    ## Clean up
    rm -rf /tmp/*; \
  fi
