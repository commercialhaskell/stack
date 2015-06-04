DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
PKG_VERSION := $(shell cat stack.cabal|grep -e '^version:'|cut -d':' -f2|sed 's/\s//g')
GIT_REV_COUNT := $(shell git rev-list HEAD --count)
GIT_SHA := $(shell PAGER=cat git log --pretty=%h HEAD~1..HEAD|head -n1)
UBUNTU_VERSION ?= 15.04

default: $(DIR)/target/ubuntu-$(UBUNTU_VERSION)/stack_$(PKG_VERSION)-$(GIT_REV_COUNT)-$(GIT_SHA)_amd64.deb

$(DIR)/target/ubuntu-$(UBUNTU_VERSION):
	@mkdir -p $(DIR)/target/ubuntu-$(UBUNTU_VERSION)

$(DIR)/target/ubuntu-$(UBUNTU_VERSION)/stack_$(PKG_VERSION)-$(GIT_REV_COUNT)-$(GIT_SHA)_amd64.deb: | $(DIR)/target/ubuntu-$(UBUNTU_VERSION)
	@cp $(DIR)/etc/Dockerfile $(DIR)/Dockerfile
	@perl -p -i -e "s/<<UBUNTU_VERSION>>/$(UBUNTU_VERSION)/g" $(DIR)/Dockerfile
	@perl -p -i -e "s/<<PKG_VERSION>>/$(PKG_VERSION)/g" $(DIR)/Dockerfile
	@perl -p -i -e "s/<<GIT_REV_COUNT>>/$(GIT_REV_COUNT)/g" $(DIR)/Dockerfile
	@perl -p -i -e "s/<<GIT_SHA>>/$(GIT_SHA)/g" $(DIR)/Dockerfile
	@docker build --rm=false --tag=stack-$(UBUNTU_VERSION):$(PKG_VERSION)-$(GIT_REV_COUNT)-$(GIT_SHA) $(DIR)
	@docker run --rm -v $(DIR)/target/ubuntu-$(UBUNTU_VERSION):/mnt stack-$(UBUNTU_VERSION):$(PKG_VERSION)-$(GIT_REV_COUNT)-$(GIT_SHA)

clean:
	@rm -rf $(DIR)/Dockerfile $(DIR)/target

.PHONY: clean default
