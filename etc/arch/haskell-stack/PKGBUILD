# Maintainer:  Tristan Webb <tristan@fpcomplete.com>
pkgname=haskell-stack
_pkgname=stack
pkgver='0.0.1.1'
pkgrel=1
pkgdesc="The Haskell Tool Stack"
arch=('x86_64' 'i686')
url="https://www.github.com/commercialhaskell/stack"
license=('BSD3')
depends=('libtinfo')
makedepends=()
optdepends=('docker: Use Docker images to build your project in a temporary container')
provides=('haskell-stack')
conflicts=('haskell-stack-git')
if [ "${CARCH}" = 'x86_64' ]; then
    _arch='x86_64'
    sha1sums=('bb2532dfce84f87f8d048d03d4f696b1e900ee41')
elif [ "${CARCH}" = 'i686' ]; then
    _arch='i686'
    sha1sums=('63fee7571bf70a1b04d1f7ac63e1a9d56d9dbccf')
fi
source=("http://download.fpcomplete.com/archlinux/${_pkgname}_${pkgver}-${_arch}.tar.gz")
# install=

package() {
  cd "$srcdir"
  STACK_BIN=usr/bin/stack
  install -Dm755 $STACK_BIN "$pkgdir/usr/bin/stack" 
}

# vim:set ts=2 sw=2 et:
