#!/bin/sh

# This allows recreating

# Name for GNU tar.
TAR=tar
CHOWN=chown
# Needed on my macOS install with HomeBrew.
#TAR=gtar
#CHOWN=gchown

mkdir -p test1 test2
touch test1/foo
mkfifo test2/bar
sudo mknod test2/devB b 1 0
sudo mknod test2/devC c 3 2
sudo $CHOWN --reference=test2 test2/*

for i in 1 2; do
  $TAR czf test$i.tar.gz --format=posix  test$i
done
for i in 1 2; do
  gtar czf test$i.tar.gz --format=posix  test$i
done

rm -rf test1 test2
