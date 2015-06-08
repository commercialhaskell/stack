Distribution packages are available for Ubuntu and Arch Linux. For the future, we have plans to support more OSes.

## Ubuntu

![Installing stack on Ubuntu](http://i.imgur.com/FiywTQA.gif)

* Ubuntu 15.04 (amd64)

```sh
wget -q -O- https://fpco.s3.amazonaws.com/fpco.key | sudo apt-key add -
echo 'deb http://fpco.s3.amazonaws.com/ubuntu/vivid stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
```

* Ubuntu 14.10 (amd64)

```sh
wget -q -O- https://fpco.s3.amazonaws.com/fpco.key | sudo apt-key add -
echo 'deb http://fpco.s3.amazonaws.com/ubuntu/utopic stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
```

* Ubuntu 14.04 (amd64)

```sh
wget -q -O- https://fpco.s3.amazonaws.com/fpco.key | sudo apt-key add -
echo 'deb http://fpco.s3.amazonaws.com/ubuntu/trusty stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
```

* Ubuntu 12.04 (amd64)

```sh
wget -q -O- https://fpco.s3.amazonaws.com/fpco.key | sudo apt-key add -
echo 'deb http://fpco.s3.amazonaws.com/ubuntu/precise stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update
sudo apt-get install stack -y
```
## Arch Linux

stack can be found in the AUR:
  - [haskell-stack](https://aur.archlinux.org/packages/haskell-stack/) _latest stable version_
  - [haskell-stack-git](https://aur.archlinux.org/packages/haskell-stack-git/) _git version_
