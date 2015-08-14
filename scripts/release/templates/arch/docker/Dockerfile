FROM dock0/arch

# Update system and install packages from main repos
RUN pacman -Syu --noconfirm git binutils fakeroot pkgbuild-introspection make sudo && \
    useradd -m -g users aurbuilder

# Install libtinfo from AUR
RUN cd /tmp && \
    git clone https://aur.archlinux.org/libtinfo.git && \
    chown -R aurbuilder:users libtinfo && \
    cd libtinfo && \
    su aurbuilder -c makepkg && \
    pacman -U libtinfo*pkg.tar.xz --noconfirm && \
    rm -rf /tmp/libtinfo

# Install the current version of stack for boot strapping
RUN cd /tmp && \
    git clone https://aur.archlinux.org/haskell-stack.git && \
    chown -R aurbuilder:users haskell-stack && \
    cd haskell-stack && \
    su aurbuilder -c makepkg && \
    pacman -U haskell-stack*.pkg.tar.xz --noconfirm && \
    rm -rf /tmp/haskell-stack

# Run environment
COPY run.sh /run.sh
RUN chmod a+x /run.sh
CMD /run.sh
