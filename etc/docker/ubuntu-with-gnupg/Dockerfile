FROM fpco/ubuntu-with-libgmp:14.04

# PACKAGES
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get -y install curl            ;# needed to install Nix
RUN apt-get -y install expect          ;# needed to automate GPG key trust
RUN apt-get -y install build-essential ;# needed by Stack at runtime
RUN apt-get -y install libgmp-dev      ;# needed by Stack at runtime
RUN apt-get -y install xz-utils        ;# needed by Stack at runtime

# USER
ENV USER user
ENV HOME /home/$USER
RUN useradd -m $USER

# GPG CONFIG & ASCII KEYS
ADD .gnupg/ $HOME/.gnupg/
RUN chown -R $USER:$USER $HOME
RUN chmod -R go-rwsx $HOME

# INSTALL NIX
RUN mkdir /nix
RUN chown -R $USER:$USER /nix
RUN groupadd -r nixbld
CMD /bin/bash -l
WORKDIR $HOME
USER $USER
RUN curl -s https://nixos.org/nix/install | sh
RUN echo "\nsource /nix/var/nix/profiles/default/etc/profile.d/nix.sh" \
  | tee -a $HOME/.bash_profile

# GNUPGs IN PROFILES
RUN bash -lc "nix-env -p /nix/var/nix/profiles/gpg14 -iA nixpkgs.nix nixpkgs.gnupg1orig"
RUN bash -lc "nix-env -p /nix/var/nix/profiles/gpg20 -iA nixpkgs.nix nixpkgs.gnupg20"
RUN bash -lc "nix-env -p /nix/var/nix/profiles/gpg21 -iA nixpkgs.nix nixpkgs.gnupg"
RUN bash -lc "nix-env -S /nix/var/nix/profiles/gpg14" ;# SET CURRENT PROFILE TO GPG 1.4

# IMPORT & TRUST KEYS
RUN bash -lc "gpg --import $HOME/.gnupg/public.asc $HOME/.gnupg/subkey.asc"
RUN bash -lc "expect $HOME/.gnupg/trust"
RUN rm $HOME/.gnupg/*.asc $HOME/.gnupg/trust
