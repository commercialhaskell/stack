#!/bin/bash -xe

# BUILD
stack --install-ghc build
[[ "$(stack path --local-install-root)/bin/stack" -nt "$OUTPUT_PKG" ]] || exit 0

# PKG
mkdir -p /fpm/usr/bin
strip -p --strip-unneeded --remove-section=.comment -o /fpm/usr/bin/stack $(stack path --local-install-root)/bin/stack
mkdir -p /fpm/usr/share
#cp -r man /fpm/usr/share
mkdir -p /fpm/etc/bash_completion.d
/fpm/usr/bin/stack --bash-completion-script /usr/bin/stack >/fpm/etc/bash_completion.d/stack
(cd /fpm; tar czvf "$OUTPUT_PKG" ./usr ./etc)
pkg_sha1=$(sha1sum $OUTPUT_PKG | cut -f1 -d' ')

# AUR DIFF
git clone https://aur.archlinux.org/haskell-stack.git /aur
pushd /aur
sed -i -e "s/pkgver='.*'/pkgver='$PKG_VERSION'/" PKGBUILD
### IMPORTANT pkgrel should be set to 1 and incremented everytime the PKGBUILD is updated without bumping version
sed -i -e "s/pkgrel=.*/pkgrel=1/" PKGBUILD
sed -i -e "/_arch='x86_64'/{n; s/'.*'/'$pkg_sha1'/;}" PKGBUILD
mksrcinfo
git diff --no-color >"$OUTPUT_PKG.aur.diff"
## Test the make pkg
#chown -R aurbuilder:users .
#su aurbuilder -c makepkg
popd
