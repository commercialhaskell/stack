DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
PKG_VERSION := $(shell cat stack.cabal|grep -e '^version:'|cut -d':' -f2|sed 's/\s//g')
GIT_REV_COUNT := $(shell git rev-list HEAD --count)
GIT_SHA := $(shell PAGER=cat git log --pretty=%h HEAD~1..HEAD|head -n1)
UBUNTU_VERSION ?= 14.04

default: docker

docker:
	@cp etc/docker/haskell-stack/Dockerfile Dockerfile
	@docker build --tag=haskell-stack:7.8 $(DIR)

ubuntu-stack: docker

target/ubuntu-$(UBUNTU_VERSION):
	@mkdir -p target/ubuntu-$(UBUNTU_VERSION)

target/ubuntu-$(UBUNTU_VERSION)/stack_$(PKG_VERSION)-$(GIT_REV_COUNT)-$(GIT_SHA)_amd64.deb: | target/ubuntu-$(UBUNTU_VERSION)
	@cp etc/docker/ubuntu-packages/Dockerfile Dockerfile
	@perl -p -i -e "s/<<UBUNTU_VERSION>>/$(UBUNTU_VERSION)/g" Dockerfile
	@perl -p -i -e "s/<<PKG_VERSION>>/$(PKG_VERSION)/g" Dockerfile
	@perl -p -i -e "s/<<GIT_REV_COUNT>>/$(GIT_REV_COUNT)/g" Dockerfile
	@perl -p -i -e "s/<<GIT_SHA>>/$(GIT_SHA)/g" Dockerfile
	@docker build --tag=stack-$(UBUNTU_VERSION):$(PKG_VERSION)-$(GIT_REV_COUNT)-$(GIT_SHA) $(DIR)
	@docker run --rm -v $(DIR)/target/ubuntu-$(UBUNTU_VERSION):/mnt stack-$(UBUNTU_VERSION):$(PKG_VERSION)-$(GIT_REV_COUNT)-$(GIT_SHA)

deb: | target/ubuntu-$(UBUNTU_VERSION)/stack_$(PKG_VERSION)-$(GIT_REV_COUNT)-$(GIT_SHA)_amd64.deb

clean:
	@rm -rf Dockerfile target

.PHONY: clean deb docker default ubuntu-stack
