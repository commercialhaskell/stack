FROM dock0/arch

# Update system and install packages from main repos
RUN pacman -Syu --noconfirm git binutils fakeroot pkgbuild-introspection make sudo ghc

RUN useradd -m -g users aurbuilder

RUN mkdir release && chmod 777 release
WORKDIR release

# Install Lib T info from AUR
RUN git clone https://aur4.archlinux.org/libtinfo.git/
RUN chown -R aurbuilder:users libtinfo
WORKDIR libtinfo
RUN su aurbuilder -c makepkg
RUN pacman -U libtinfo*pkg.tar.xz --noconfirm

# Install the current version of stack for boot strapping
WORKDIR /release
RUN git clone https://aur4.archlinux.org/haskell-stack.git/
RUN chown -R aurbuilder:users haskell-stack
WORKDIR haskell-stack
RUN su aurbuilder -c makepkg
RUN pacman -U haskell-stack*.pkg.tar.xz --noconfirm

# Build using the haskell-stack-git pkgbuild
WORKDIR /release
RUN git clone https://github.com/drwebb/haskell-stack-git.git -b WIPnocaball
RUN chown -R aurbuilder:users haskell-stack-git
# we want to specify the version numbers here I guess
WORKDIR haskell-stack-git
RUN su aurbuilder -c makepkg

