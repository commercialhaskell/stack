#!/bin/bash

### Vars
release_commit="2bdc88871"
release_ver="0.1.2.0"
release_arch="x86_64"
stack_pkg=stack_${release_ver}-${release_arch}.tar.gz

### Build Dirs
buildDir=`pwd`/build
echo "Build dir set to" $buildDir
stackDir=$buildDir/stack
distDir=`pwd`/dist
aurPkgDir=$distDir/haskell-stack
echo "dist dir set to" $distDir

# Clean everything
rm -rf $buildDir
rm -rf $distDir

# Build Stage
mkdir -p $buildDir && cd $buildDir

## Build and Test stack
git clone https://github.com/commercialhaskell/stack.git
cd $stackDir 
git reset --hard $release_commit
stack clean && stack install --pedantic && stack test --flag stack:integration-test

# Dist Stage
###  This is where we create the tar for upload
echo "Creating " $distDir
mkdir -p $distDir && cd $distDir
mkdir -p usr/bin
cp $HOME/.local/bin/stack $distDir/usr/bin/

### Install Manuals
mkdir -p usr/share
cp -r $stackDir/man usr/share

###  Tar up the package for S3
tar czvf $stack_pkg ./usr

### Upload to S3
aws s3 cp $stack_pkg s3://download.fpcomplete.com/archlinux/ --acl public-read

### Grab the sha1sum
arch_sha1=$(sha1sum $stack_pkg | cut -f1 -d' ')

## Update the Aur Package
git clone https://aur4.archlinux.org/haskell-stack.git
cd $aurPkgDir
git reset --hard HEAD
### Bump the version in the PKGBUILD
sed -i -e "s/pkgver='.*'/pkgver='$release_ver'/" PKGBUILD
### IMPORTANT pkgrel should be set to 1 and incremented everytime the PKGBUILD is updated without bumping version
sed -i -e "s/pkgrel=.*/pkgrel=1/" PKGBUILD
sed -i -e "/_arch='$release_arch'/{n; s/'.*'/'$arch_sha1'/;}" PKGBUILD
mksrcinfo

## Test the make pkg
makepkg
## Make the source version (Temporarily for old aur)
makepkg --source


## Update git repo
git add PKGBUILD .SRCINFO
# commenting this out until bugs are worked out
git commit -m "Version bump to $release_ver"

## Now you just need to run a git push
