#!/bin/sh -e
#
# Stack installation script.
#
# This script is meant for quick & easy install via:
#   'curl -sSL https://get.haskellstack.org/ | sh'
# or:
#   'wget -qO- https://get.haskellstack.org/ | sh'
#
# Make pull requests at:
# https://github.com/commercialhaskell/stack/blob/master/etc/scripts/get-stack.sh
#

HOME_LOCAL_BIN="$HOME/.local/bin"
USR_LOCAL_BIN="/usr/local/bin"
QUIET=""
STACK_TEMP_DIR=

# creates a temporary directory, which will be cleaned up automatically
# when the script finishes
make_temp_dir() {
  STACK_TEMP_DIR="$(mktemp -d)"
}

# cleanup the temporary directory if it's been created.  called automatically
# when the script exits.
cleanup_temp_dir() {
  if [ -n "$STACK_TEMP_DIR" ] ; then
    rm -rf "$STACK_TEMP_DIR"
    STACK_TEMP_DIR=
  fi
}

# print a message to stderr and exit with error code
die() {
  echo "$@" >&2
  exit 1
}

# print a message to stdout unless '-q' passed to script
info() {
  if [ -z "$QUIET" ] ; then
    echo "$@"
  fi
}

# print a separator for post-install messages
post_install_separator() {
  info ""
  info "-------------------------------------------------------------------------------"
  info ""
}

# determines 64- or 32-bit architecture
# if getconf is available, it will return the arch of the OS, as desired
# if not, it will use uname to get the arch of the CPU, though the installed
# OS could be 32-bits on a 64-bit CPU
get_arch() {
  if has_getconf ; then
    if getconf LONG_BIT | grep -q 64 ; then
      echo 64
    else
      echo 32
    fi
  else
    case "$(uname -m)" in
      *64)
        echo 64
        ;;
      *)
        echo 32
        ;;
    esac
  fi
}

# exits with code 0 if a 64-bit architecture is detected as described above
is_64_bit() {
  test "$(get_arch)" = 64
}

# Adds a `sudo` prefix if sudo is available to execute the given command
# If not, the given command is run as is
sudocmd() {
  $(command -v sudo) "$@"
}

# Adds the FPCo key to the keyring and adds the given repo to the apt sources
add_apt_repo() {
  echo "$1" | sudocmd tee /etc/apt/sources.list.d/fpco.list > /dev/null
  sudocmd apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
}

# Install dependencies for distros that use Apt
apt_install_dependencies() {
    info "Installing dependencies..."
    info ""
    apt_get_install_pkgs "$@"
}

# Install Stack package on for distros that use Apt
apt_update_and_install() {
    sudocmd apt-get update ${QUIET:+-qq}
    apt_get_install_pkgs stack
    post_install_separator
    info "Installed 'stack' package."
    info ""
}

# Attempts an install on Ubuntu via apt, if possible
# Expects the version (in Major.Minor format, with any sub-minor removed)
# as the first and only argument
# If the version of Ubuntu is unsupported, it attempts to copy the binary
# and install the necessary dependencies explicitly.
do_ubuntu_install() {

  install_dependencies() {
    apt_install_dependencies g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg
  }

  if is_64_bit ; then
    case "$1" in
      16.10)
        add_apt_repo 'deb http://download.fpcomplete.com/ubuntu yakkety main'
        apt_update_and_install
        ;;
      16.04)
        add_apt_repo 'deb http://download.fpcomplete.com/ubuntu xenial main'
        apt_update_and_install
        ;;
      15.10)
        add_apt_repo 'deb http://download.fpcomplete.com/ubuntu wily main'
        apt_update_and_install
        ;;
      14.04)
        add_apt_repo 'deb http://download.fpcomplete.com/ubuntu trusty main'
        apt_update_and_install
        ;;
      12.04)
        add_apt_repo 'deb http://download.fpcomplete.com/ubuntu precise main'
        apt_update_and_install
        ;;
      *)
        install_dependencies
        info ""
        info "No packages available for Ubuntu $1, using generic bindist..."
        info ""
        install_64bit_standard_binary
        ;;
    esac
  else
    install_dependencies
    info ""
    info "No packages available for 32-bit Ubuntu $1, using generic bindist..."
    info ""
    install_32bit_standard_binary
  fi

}

# Attempts an install on Debian via apt, if possible
# Expects the single-number version as the first and only argument
# If the version of Debian is unsupported, it attempts to copy the binary
# and install the necessary dependencies explicitly.
do_debian_install() {
  install_dependencies() {
    apt_install_dependencies g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev
  }

  if is_64_bit ; then
    case "$1" in
      8*)
        add_apt_repo 'deb http://download.fpcomplete.com/debian jessie main'
        apt_update_and_install
        ;;
      7*)
        add_apt_repo 'deb http://download.fpcomplete.com/debian wheezy main'
        apt_update_and_install
        ;;
      *)
        install_dependencies
        info ""
        info "No packages available for Debian $1, using generic bindist..."
        info ""
        install_64bit_standard_binary
        ;;
    esac
  else
    install_dependencies
    info ""
    info "No packages available for 32-bit Debian $1, using generic bindist..."
    info ""
    install_32bit_standard_binary
  fi
}

# Attempts an install on Fedora via dnf, if possible
# Expects the single-number version as the first and only argument
# If the version of Fedora is unsupported, it attempts to copy the binary
# and install the necessary dependencies explicitly.
do_fedora_install() {
  install_dependencies() {
    dnf_install_pkgs perl make automake gcc gmp-devel libffi zlib xz tar
  }
  dnf_install_stack() {
    dnf_install_pkgs stack
    post_install_separator
    info "Installed 'stack' package."
    info ""
  }

  if is_64_bit ; then
    check_dl_tools
    case "$1" in
      "24"*)
        dl_to_stdout https://download.fpcomplete.com/fedora/24/fpco.repo | sudocmd tee /etc/yum.repos.d/fpco.repo >/dev/null
        dnf_install_stack
        ;;
      "23"*)
        dl_to_stdout https://download.fpcomplete.com/fedora/23/fpco.repo | sudocmd tee /etc/yum.repos.d/fpco.repo >/dev/null
        dnf_install_stack
        ;;
      "22"*)
        dl_to_stdout https://download.fpcomplete.com/fedora/22/fpco.repo | sudocmd tee /etc/yum.repos.d/fpco.repo >/dev/null
        dnf_install_stack
        ;;
      *)
        install_dependencies "$1"
        info ""
        info "No packages available for Fedora $1, using generic bindist..."
        info ""
        install_64bit_standard_binary
        ;;
    esac
  else
    install_dependencies "$1"
    info ""
    info "No packages available for 32-bit Fedora $1, using generic bindist..."
    info ""
    install_32bit_standard_binary
  fi
}

# Attempts an install on CentOS via yum, if possible
# Expects the single-number version as the first and only argument
# If the version of CentOS is unsupported, it attempts to copy the binary
# and install the necessary dependencies explicitly.
do_centos_install() {
  install_dependencies() {
    yum_install_pkgs perl make automake gcc gmp-devel libffi zlib xz tar
  }
  install_package() {
    yum_install_pkgs stack
    post_install_separator
    info "Installed 'stack' package."
    info ""
  }

  if is_64_bit ; then
    check_dl_tools
    case "$1" in
      "7"|"7."*)
        dl_to_stdout https://download.fpcomplete.com/centos/7/fpco.repo | sudocmd tee /etc/yum.repos.d/fpco.repo >/dev/null
        install_package
        ;;
      "6"|"6."*)
        dl_to_stdout https://download.fpcomplete.com/centos/6/fpco.repo | sudocmd tee /etc/yum.repos.d/fpco.repo >/dev/null
        install_package
        ;;
      *)
        install_dependencies
        info ""
        info "No packages available for CentOS/RHEL $1, using generic bindist..."
        info ""
        install_64bit_standard_binary
        ;;
    esac
  else
    install_dependencies
    case "$1" in
      "6")
        info ""
        info "No packages available for 32-bit CentOS/RHEL $1, using genergic libgmp4 bindist..."
        info ""
        install_32bit_gmp4_linked_binary
        ;;
      *)
        info ""
        info "No packages available for 32-bit CentOS/RHEL $1, using generic bindist..."
        info ""
        install_32bit_standard_binary
        ;;
    esac
  fi
}

# Attempts to install on Mac OS X.
# If 'brew' exists, installs using Homebrew.  Otherwise, installs
# the generic bindist.
do_osx_install() {
  #if has_brew ; then
  #  info "Since you have 'brew', installing using Homebrew."
  #  info ""
  #  brew update
  #  brew install haskell-stack
  #  post_install_separator
  #  info "Installed Homebrew 'haskell-stack' formula."
  #  info ""
  #else
  #  info "Since you do not have 'brew', using generic bindist..."
    info "Using generic bindist..."
    info ""
    install_64bit_osx_binary
  #fi
  info "NOTE: You may need to run 'xcode-select --install' to set"
  info "      up the Xcode command-line tools, which Stack uses."
  info ""
}

# Attempts to insall on FreeBSD.  Installs dependencies with
# 'pkg install' and then downloads bindist.
do_freebsd_install() {
  install_dependencies() {
    sudocmd pkg install -y devel/gmake perl5 lang/gcc misc/compat8x misc/compat9x converters/libiconv ca_root_nss
  }
  if is_64_bit ; then
    install_dependencies
    install_64bit_freebsd_binary
  else
    die "Sorry, there is currently no 32-bit FreeBSD binary available."
  fi
}

# Alpine distro install
do_alpine_install() {
  install_dependencies() {
    apk_install_pkgs gmp libgcc xz make
  }
  install_dependencies
  if is_64_bit ; then
      install_64bit_static_binary
  else
      install_32bit_standard_binary
  fi
}

# Attempts to install on unsupported Linux distribution by downloading
# the bindist.
do_sloppy_install() {
  info "This installer doesn't support your Linux distribution, trying generic"
  info "bindist..."
  info ""
  if is_64_bit ; then
      install_64bit_standard_binary
  else
      install_32bit_standard_binary
  fi
  info "Since this installer doesn't support your Linux distribution,"
  info "there is no guarantee that 'stack' will work at all!  You may"
  info "need to manually install some system info dependencies for GHC:"
  info "  gcc, make, libffi, zlib, libgmp and libtinfo"
  info "Please see http://docs.haskellstack.org/en/stable/install_and_upgrade/"
  info "Pull requests to add support for this distro would be welcome!"
  info ""
}

# Attempts to determine the running Linux distribution.
# Prints "DISTRO;VERSION" (distribution name and version)"."
distro_info() {
  parse_lsb() {
    lsb_release -a 2> /dev/null | perl -ne "$1"
  }

  try_lsb() {
    if has_lsb_release ; then
      TL_DIST="$(parse_lsb 'if(/Distributor ID:\s+([^ ]+)/) { print "\L$1"; }')"
      TL_VERSION="$(parse_lsb 'if(/Release:\s+([^ ]+)/) { print "\L$1"; }')"
      echo "$TL_DIST;$TL_VERSION"
    else
      return 1
    fi
  }

  try_release() {
    parse_release() {
      perl -ne "$1" /etc/*release 2>/dev/null
    }

    parse_release_id() {
      parse_release 'if(/^(DISTRIB_)?ID\s*=\s*"?([^"]+)/) { print "\L$2"; exit 0; }'
    }

    parse_release_version() {
      parse_release 'if(/^(DISTRIB_RELEASE|VERSION_ID)\s*=\s*"?([^"]+)/) { print $2; exit 0; }'
    }

    TR_RELEASE="$(parse_release_id);$(parse_release_version)"

    if [ ";" = "$TR_RELEASE" ] ; then
      if [ -e /etc/arch-release ] ; then
        # /etc/arch-release exists but is often empty
        echo "arch;"
      elif [ -e /etc/centos-release ] && grep -q "\<6\>" /etc/centos-release ; then
        # /etc/centos-release has a non-standard format before version 7
        echo "centos;6"
      else
        return 1
      fi
    else
      echo "$TR_RELEASE"
    fi
  }

  try_issue() {
    case "$(cat /etc/issue 2>/dev/null)" in
      "Arch Linux"*)
        echo "arch;" # n.b. Version is not available in /etc/issue on Arch
        ;;
      "Ubuntu"*)
        echo "ubuntu;$(perl -ne 'if(/Ubuntu (\d+\.\d+)/) { print $1; }' < /etc/issue)"
        ;;
      "Debian"*)
        echo "debian;$(perl -ne 'if(/Debian GNU\/Linux (\d+(\.\d+)?)/) { print $1; }' < /etc/issue)"
        ;;
      *"SUSE"*)
        echo "suse;$(perl -ne 'if(/SUSE\b.* (\d+\.\d+)/) { print $1; }' < /etc/issue)"
        ;;
      *"NixOS"*)
        echo "nixos;$(perl -ne 'if(/NixOS (\d+\.\d+)/) { print $1; }' < /etc/issue)"
        ;;
      "CentOS"*)
        echo "centos;$(perl -ne 'if(/^CentOS release (\d+)\./) { print $1; }' < /etc/issue)"
        ;;
      *)
    esac
    # others do not output useful info in issue, return empty
  }

  try_lsb || try_release || try_issue
}

# Attempt to install on a Linux distribution
do_distro() {
  if ! has_perl ; then
    if ! try_install_pkgs perl ; then
      #TODO: remove dependence on 'perl', which is not installed by default
      #on some distributions (Fedora and RHEL, in particular).
      die "This script requires 'perl', please install it to continue."
    fi
  fi

  IFS=";" read -r DISTRO VERSION <<GETDISTRO
$(distro_info)
GETDISTRO

  if [ -n "$DISTRO" ] ; then
    info "Detected Linux distribution: $DISTRO"
    info ""
  fi

  case "$DISTRO" in
    ubuntu)
      do_ubuntu_install "$VERSION"
      ;;
    debian|kali)
      do_debian_install "$VERSION"
      ;;
    fedora)
      do_fedora_install "$VERSION"
      ;;
    centos|rhel)
      do_centos_install "$VERSION"
      ;;
    alpine)
      do_alpine_install "$VERSION"
      ;;
    *)
      do_sloppy_install
  esac
}

# Determine operating system and attempt to install.
do_os() {
  case "$(uname)" in
    "Linux")
      do_distro
      ;;
    "Darwin")
      do_osx_install
      ;;
    "FreeBSD")
      do_freebsd_install
      ;;
    *)
      die "Sorry, this installer does not support your operating system: $(uname).
See http://docs.haskellstack.org/en/stable/install_and_upgrade/"
  esac
}

# Download a URL to stdout, for piping to another process or file,
# using 'curl' or 'wget'.
dl_to_stdout() {
  if has_curl ; then
    curl ${QUIET:+-sS} -L "$@"
  elif has_wget ; then
    wget ${QUIET:+-q} -O- "$@"
  else
    # should already have checked for this, otherwise this message will probably
    # not be displayed, since dl_to_stdout will be part of a pipeline
    die "Neither wget nor curl is available, please install one to continue."
  fi
}

# Check for 'curl' or 'wget' and attempt to install 'curl' if neither found,
# or fail the script if that is not possible.
check_dl_tools() {
  if ! has_curl && ! has_wget ; then
    if ! try_install_pkgs curl ; then
      die "Neither wget nor curl is available, please install one to continue."
    fi
  fi
}

# Download a Stack bindst and install it in /usr/local/bin/stack.
install_from_bindist() {
    IFB_URL="https://www.stackage.org/stack/$1"
    check_dl_tools
    #TODO: the checksum or GPG signature should be checked.
    make_temp_dir

    dl_to_stdout "$IFB_URL" | tar xzf - -C "$STACK_TEMP_DIR"
    sudocmd install -c -o 0 -g 0 -m 0755 "$STACK_TEMP_DIR"/*/stack "$USR_LOCAL_BIN/stack"

    post_install_separator
    info "Stack has been installed to: $USR_LOCAL_BIN/stack"
    info ""

    check_usr_local_bin_on_path
}

install_32bit_standard_binary() {
  install_from_bindist "linux-i386"
}

install_64bit_standard_binary() {
  install_from_bindist "linux-x86_64"
}

install_64bit_static_binary() {
  install_from_bindist "linux-x86_64-static"
}

install_32bit_gmp4_linked_binary() {
  install_from_bindist "linux-i386-gmp4"
}

install_64bit_osx_binary() {
  install_from_bindist "osx-x86_64"
}

install_64bit_freebsd_binary() {
  install_from_bindist "freebsd-x86_64"
}

# Attempt to install packages using whichever of apt-get, dnf, or yum is
# available.
try_install_pkgs() {
  if has_apt_get ; then
    apt_get_install_pkgs "$@"
  elif has_dnf ; then
    dnf_install_pkgs "$@"
  elif has_yum ; then
    yum_install_pkgs "$@"
  elif has_apk ; then
    apk_install_pkgs "$@"
  else
    return 1
  fi
}

# Install packages using apt-get
apt_get_install_pkgs() {
  sudocmd apt-get install -y ${QUIET:+-qq} "$@"
}

# Install packages using dnf
dnf_install_pkgs() {
  sudocmd dnf install -y ${QUIET:+-q} "$@"
}

# Install packages using yum
yum_install_pkgs() {
  sudocmd yum install -y ${QUIET:+-q} "$@"
}

# Install packages using apk
apk_install_pkgs() {
  sudocmd apk add --update ${QUIET:+-q} "$@"
}

# Get installed Stack version, if any
stack_version() {
  stack --version | grep -o 'Version \([[:digit:]]\|\.\)\+'
}

# Get installed Stack's path
stack_location() {
  command -v stack
}

# Check whether 'stack' command exists
has_stack() {
  has_cmd stack
}

# Check whether 'wget' command exists
has_wget() {
  has_cmd wget
}

# Check whether 'curl' command exists
has_curl() {
  has_cmd curl
}

# Check whether 'lsb_release' command exists
has_lsb_release() {
  has_cmd lsb_release
}

# Check whether 'sudo' command exists
has_sudo() {
  has_cmd sudo
}

# Check whether 'getconf' command exists
has_getconf() {
  has_cmd getconf
}

# Check whether 'brew' command exists
#has_brew() {
#  has_cmd brew
#}

# Check whether 'perl' command exists
has_perl() {
  has_cmd perl
}

# Check whether 'apt-get' command exists
has_apt_get() {
  has_cmd apt-get
}

# Check whether 'yum' command exists
has_yum() {
  has_cmd yum
}

# Check whether 'apk' command exists
has_apk() {
  has_cmd apk
}

# Check whether 'dnf' command exists
has_dnf() {
  has_cmd dnf
}

# Check whether the given command exists
has_cmd() {
  command -v "$1" > /dev/null 2>&1
}

# Check whether the given path is listed in the PATH environment variable
on_path() {
  echo ":$PATH:" | grep -q :"$1":
}

# Check whether ~/.local/bin is on the PATH, and print a warning if not.
check_home_local_bin_on_path() {
  if ! on_path "$HOME_LOCAL_BIN" ; then
    #TODO: offer to add it for the user (pull requests welcome!)
    info "WARNING: '$HOME_LOCAL_BIN' is not on your PATH."
    info "    For best results, please add it to the beginning of PATH in your profile."
    info ""
  fi
}

# Check whether /usr/local/bin is on the PATH, and print a warning if not.
check_usr_local_bin_on_path() {
  if ! on_path "$USR_LOCAL_BIN" ; then
    info "WARNING: '$USR_LOCAL_BIN' is not on your PATH."
    info ""
  fi
}

# Check whether Stack is already installed, and print an error if it is.
check_stack_installed() {
  if has_stack ; then
    #XXX add a --force flag to reinstall anyway
    die "Stack $(stack_version) already appears to be installed at:
  $(stack_location)
Use 'stack upgrade' or your OS's package manager to upgrade."
  fi
}

trap cleanup_temp_dir EXIT

case "$1" in
  -q|--quiet)
    # This tries its best to reduce output by suppressing the script's own
    # messages and passing "quiet" arguments to tools that support them.
    QUIET="true"
esac

check_stack_installed
do_os
check_home_local_bin_on_path
