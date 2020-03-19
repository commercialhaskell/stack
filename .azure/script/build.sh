#!/bin/bash
# set -x
set -m

os_name=$1
dir=$2
cache_s3_version=$3
cache_s3_prefix=$4
base_branch=$5
source_branch=$6

clean_resource() {
    rm -rf $HOME/.local/bin
    rm -rf $HOME/.stack-root
    rm -rf /opt/ghc
    rm -rf /opt/happy
    rm -rf /opt/alex
}

ori_stack_root=$STACK_ROOT
ori_path=$PATH

clean_resource

cd $dir
export STACK_ROOT=$HOME/.stack-root;
echo $STACK_ROOT
mkdir -p $HOME/.local/bin
echo "URL:"
echo "https://github.com/fpco/cache-s3/releases/download/${cache_s3_version}/cache-s3-${cache_s3_version}-linux-x86_64.tar.gz"
curl -f -L "https://github.com/fpco/cache-s3/releases/download/${cache_s3_version}/cache-s3-${cache_s3_version}-linux-x86_64.tar.gz" -o $HOME/.local/bin/cache-s3.tar.gz
tar xzf $HOME/.local/bin/cache-s3.tar.gz -C $HOME/.local/bin
export PATH=$HOME/.local/bin:$PATH;
cache-s3 --prefix="${cache_s3_prefix}" --git-branch="${source_branch}" --suffix="${os_name}" restore stack --base-branch="${base_branch}"
cache-s3 --prefix="${cache_s3_prefix}" --git-branch="${source_branch}" --suffix="${os_name}" restore stack work --base-branch="${base_branch}"
etc/scripts/ci-setup.sh
export PATH=$HOME/.local/bin:/opt/ghc/$GHCVER/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.7/bin:$PATH

set -ex
stack etc/scripts/release.hs build
set +ex

if [ "${source_branch}" = "${base_branch}" ]; then
    cache-s3 --prefix="${cache_s3_prefix}" --git-branch="${source_branch}" --suffix="${os_name}" save stack;
fi;
cache-s3 --prefix="${cache_s3_prefix}" --git-branch="${source_branch}" --suffix="${os_name}" save stack work

# Reset env var
export STACK_ROOT=$ori_stack_root
export PATH=$ori_path

clean_resource
