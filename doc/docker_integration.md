Docker integration
===============================================================================

`stack` has support for automatically performing builds inside a Docker
container, using volume mounts and user ID switching to make it mostly seamless.
FP Complete provides images for use with stack that include GHC, tools, and
optionally have all of the Stackage LTS packages pre-installed in the global
package database.

The primary purpose for using stack/docker this way is for teams to ensure all
developers are building in an exactly consistent environment without team
members needing to deal with Docker themselves.

See the
[how stack can use Docker under the hood](https://www.fpcomplete.com/blog/2015/08/stack-docker)
blog post for more information about the motivation and implementation of stack's
Docker support.

Prerequisites
-------------------------------------------------------------------------------

### Supported operating systems

**Linux 64-bit**: Docker use requires machine (virtual or metal) running a Linux
distribution
[that Docker supports](https://docs.docker.com/installation/#installation), with
a 64-bit kernel. If you do not already have one, we suggest Ubuntu 14.04
("trusty") since this is what we test with.

**macOS**: [Docker for Mac](https://docs.docker.com/docker-for-mac/) is the
supported way to use Docker integration on macOS (the older Docker Machine
(boot2docker) approach to using Docker on macOS is not supported due to issues
with host volume mounting that make Stack nearly unusable for anything but the
most trivial projects).

Other Un*xen are not officially supported but there are ways to get them working.
See [#194](https://github.com/commercialhaskell/stack/issues/194) for details
and workarounds.

**Windows does not work at all** (see
[#2421](https://github.com/commercialhaskell/stack/issues/2421)).

### Docker

Install the latest version of Docker by following the
[instructions for your operating system](http://docs.docker.com/installation/).

The Docker client should be able to connect to the Docker daemon as a non-root
user. For example (from
[here](http://docs.docker.com/installation/ubuntulinux/#ubuntu-raring-1304-and-saucy-1310-64-bit)):

    # Add the connected user "${USER}" to the docker group.
    # Change the user name to match your preferred user.
    sudo gpasswd -a ${USER} docker

    # Restart the Docker daemon.
    sudo service docker restart

You will now need to log out and log in again for the group addition
to take effect.

Note the above has security implications.  See [security](#security) for more.

Usage
-------------------------------------------------------------------------------

This section assumes that you already have Docker installed and working. If
not, see the [prerequisites](#prerequisites) section. If you run into any
trouble, see the [troubleshooting](#troubleshooting) section.

### Enable in stack.yaml

The most basic configuration is to add this to your project's `stack.yaml`:

    docker:
        enable: true

See [configuration](#configuration) for additional options. You can enable it on
the command-line using `stack --docker`.

Please note that in a docker-enabled configuration, stack uses the GHC installed
in the Docker container by default. To use a compiler installed by stack, add

    system-ghc: false

(see [`system-ghc`](yaml_configuration.md#system-ghc)).

### Use stack as normal

With Docker enabled, most stack sub-commands will automatically launch
themselves in an ephemeral Docker container (the container is deleted as soon as
the command completes). The project directory and `~/.stack` are volume-mounted
into the container, so any build artifacts are "permanent" (not deleted with the
container).

The first time you run a command with a new image, you will be prompted to run
`stack docker pull` to pull the image first. This will pull a Docker
image with a tag that matches your resolver. Only LTS resolvers are supported
(we do not generate images for nightly snapshots).  Not every LTS version is
guaranteed to have an image existing, and new LTS images tend to lag behind
the LTS snapshot being published on stackage.org.  Be warned: these images are
rather large!

Docker sub-commands
-------------------------------------------------------------------------------

These `stack docker` sub-commands have Docker-specific functionality. Most other
`stack` commands will also use a Docker container under the surface if Docker is
enabled.

### pull - Pull latest version of image

`stack docker pull` pulls an image from the Docker registry for the first time,
or updates the image by pulling the latest version.

### cleanup - Clean up old images and containers

Docker images can take up quite a lot of disk space, and it's easy for them to
build up if you switch between projects or your projects update their images.
This sub-command will help to remove old images and containers.

By default, `stack docker cleanup` will bring up an editor showing the images
and containers on your system, with any stack images that haven't been used
in the last seven days marked for removal.  You can add or remove the `R` in
the left-most column to flag or unflag an image/container for removal.  When
you save the file and quit the text editor, those images marked for removal
will be deleted from your system.  If you wish to abort the cleanup, delete
all the lines from your editor.

If you use Docker for purposes other than stack, you may have other images on
your system as well.  These will also appear in a separate section, but they
will not be marked for removal by default.

Run `stack docker cleanup --help` to see additional options to customize its
behaviour.

### reset - Reset the Docker "sandbox"

In order to preserve the contents of the in-container home directory between
runs, a special "sandbox" directory is volume-mounted into the container. `stack
docker reset` will reset that sandbox to its defaults.

Note: `~/.stack` is separately volume-mounted, and is left alone during reset.

Command-line options
-------------------------------------------------------------------------------

The default Docker configuration can be overridden on the command-line. See
`stack --docker-help` for a list of all Docker options, and consult
[configuration](#configuration) section below for more information about
their meanings. These are global options, and apply to all commands (not just
`stack docker` sub-commands).

Configuration
-------------------------------------------------------------------------------

`stack.yaml` contains a `docker:` section with Docker settings.  If this
section is omitted, Docker containers will not be used.  These settings can
be included in project, user, or global configuration.

Here is an annotated configuration file.  The default values are shown unless
otherwise noted.

    docker:

      # Set to false to disable using Docker.  In the project configuration,
      # the presence of a `docker:` section implies docker is enabled unless
      # `enable: false` is set.  In user and global configuration, this is not
      # the case.
      enable: true

      # The name of the repository to pull the image from.  See the "repositories"
      # section of this document for more information about available repositories.
      # If this includes a tag (e.g. "my/image:tag"), that tagged image will be
      # used.  Without a tag specified, the LTS version slug is added automatically.
      # Either `repo` or `image` may be specified, but not both.
      repo: "fpco/stack-build"

      # Exact Docker image name or ID.  Overrides `repo`. Either `repo` or `image`
      # may be specified, but not both.  (default none)
      image: "5c624ec1d63f"

      # Registry requires login.  A login will be requested before attempting to
      # pull.
      registry-login: false

      # Username to log into the registry.  (default none)
      registry-username: "myuser"

      # Password to log into the registry.  (default none)
      registry-password: "SETME"

      # If true, the image will be pulled from the registry automatically, without
      # needing to run `stack docker pull`.  See the "security" section of this
      # document for implications of enabling this.
      auto-pull: false

      # If true, the container will be run "detached" (in the background).  Refer
      # to the Docker users guide for information about how to manage containers.
      # This option would rarely make sense in the configuration file, but can be
      # useful on the command-line.  When true, implies `persist`.
      detach: false

      # If true, the container will not be deleted after it terminates.  Refer to
      # the Docker users guide for information about how to manage containers. This
      # option would rarely make sense in the configuration file, but can be
      # useful on the command-line.  `detach` implies `persist`.
      persist: false

      # What to name the Docker container.  Only useful with `detach` or
      # `persist` true.  (default none)
      container-name: "example-name"

      # Additional arguments to pass to `docker run`.  (default none)
      run-args: ["--net=bridge"]

      # Directories from the host to volume-mount into the container.  If it
      # contains a `:`, the part before the `:` is the directory on the host and
      # the part after the `:` is where it should be mounted in the container.
      # (default none, aside from the project and stack root directories which are
      # always mounted)
      mount:
        - "/foo/bar"
        - "/baz:/tmp/quux"

      # Environment variables to set in the container.  Environment variables
      # are not automatically inherited from the host, so if you need any specific
      # variables, use the `--docker-env` command-line argument version of this to
      # pass them in.  (default none)
      env:
        - "FOO=BAR"
        - "BAR=BAZ QUUX"

      # Location of database used to track image usage, which `stack docker cleanup`
      # uses to determine which images should be kept.  On shared systems, it may
      # be useful to override this in the global configuration file so that
      # all users share a single database.
      database-path: "~/.stack/docker.db"

      # Location of a Docker container-compatible 'stack' executable with the
      # matching version. This executable must be built on linux-x86_64 and
      # statically linked.
      # Valid values are:
      #   host: use the host's executable.  This is the default when the host's
      #     executable is known to work (e.g., from official linux-x86_64 bindist)
      #   download: download a compatible executable matching the host's version.
      #     This is the default when the host's executable is not known to work
      #   image: use the 'stack' executable baked into the image.  The version
      #     must match the host's version
      #   /path/to/stack: path on the host's local filesystem
      stack-exe: host

      # If true (the default when using the local Docker Engine), run processes
      # in the Docker container as the same UID/GID as the host.  The ensures
      # that files written by the container are owned by you on the host.
      # When the Docker Engine is remote (accessed by tcp), defaults to false.
      set-user: true

      # Require the version of the Docker client to be within the specified
      # Cabal-style version range (e.g., ">= 1.6.0 && < 1.9.0")
      require-docker-version: "any"

Image Repositories
-------------------------------------------------------------------------------

FP Complete provides the following public image repositories on Docker Hub:

- [fpco/stack-build](https://registry.hub.docker.com/u/fpco/stack-build/) (the
  default) - GHC (patched), tools (stack, cabal-install, happy, alex, etc.), and
  system developer libraries required to build all Stackage packages.
- [fpco/stack-ghcjs-build](https://registry.hub.docker.com/u/fpco/stack-ghcjs-build/) -
  Like `stack-build`, but adds GHCJS.
- [fpco/stack-full](https://registry.hub.docker.com/u/fpco/stack-full/) -
  Includes all Stackage packages pre-installed in GHC's global package database.
  These images are over 10 GB!
- [fpco/stack-ghcjs-full](https://registry.hub.docker.com/u/fpco/stack-ghcjs-full/) -
  Like `stack-full`, but adds GHCJS.
- [fpco/stack-run](https://registry.hub.docker.com/u/fpco/stack-run/) -
  Runtime environment for binaries built with Stackage. Includes system shared
  libraries required by all Stackage packages. Does not necessarily include all
  data required for every use (e.g. has texlive-binaries for HaTeX, but does not
  include LaTeX fonts), as that would be prohibitively large. Based on
  [phusion/baseimage](https://registry.hub.docker.com/u/phusion/baseimage/).

FP Complete also builds custom variants of these images for their clients.

These images can also be used directly with `docker run` and provide a complete
Haskell build environment.

In addition, most Docker images that contain the basics for running GHC can be
used with Stack's Docker integration. For example, the
[official Haskell image repository](https://hub.docker.com/_/haskell/) works.
See [Custom images](#custom-images) for more details.

Security
-------------------------------------------------------------------------------

Having `docker` usable as a non-root user is always a security risk, and will
allow root access to your system. It is also possible to craft a `stack.yaml`
that will run arbitrary commands in an arbitrary docker container through that
vector, thus a `stack.yaml` could cause stack to run arbitrary commands as root.
While this is a risk, it is not really a greater risk than is posed by the
docker permissions in the first place (for example, if you ever run an unknown
shell script or executable, or ever compile an unknown Haskell package that uses
Template Haskell, you are at equal risk). Nevertheless, there are
[plans to close the stack.yaml loophole](https://github.com/commercialhaskell/stack/issues/260).

One way to mitigate this risk is, instead of allowing `docker` to run as
non-root, replace `docker` with a wrapper script that uses `sudo` to run the
real Docker client as root. This way you will at least be prompted for your root
password. As [@gregwebs](https://github.com/gregwebs) pointed out, put this
script named `docker` in your PATH (and make sure you remove your user from the
`docker` group as well, if you added it earlier):

    #!/bin/bash -e
    # The goal of this script is to maintain the security privileges of sudo
    # Without having to constantly type "sudo"
    exec sudo /usr/bin/docker "$@"

Additional notes
-------------------------------------------------------------------------------

### Volume-mounts and ephemeral containers

Since filesystem changes outside of the volume-mounted project directory are not
persisted across runs, this means that if you `stack exec sudo apt-get install
some-ubuntu-package`, that package will be installed but then the container it's
installed in will disappear, thus causing it to have no effect. If you wish to
make this kind of change permanent, see later instructions for how to create a
[derivative Docker image](#derivative-image).

Inside the container, your home directory is a special location that volume-
mounted from within your project directory's `.stack-work` in such a
way as that installed GHC/cabal packages are not shared between different
Stackage snapshots.  In addition, `~/.stack` is volume-mounted from the host.

### Network

stack containers use the host's network stack within the container
by default, meaning a process running in the container can connect to
services running on the host, and a server process run within the container
can be accessed from the host without needing to explicitly publish its port.
To run the container with an isolated network, use `--docker-run-args` to pass
the `--net` argument to `docker-run`.  For example:

    stack --docker-run-args='--net=bridge --publish=3000:3000' \
          exec some-server

will run the container's network in "bridge" mode (which is Docker's default)
and publish port 3000.

### Persistent container

If you do want to do all your work, including editing, in the container, it
might be better to use a persistent container in which you can install Ubuntu
packages. You could get that by running something like `stack
--docker-container-name=NAME --docker-persist exec --plain bash`. This
means when the container exits, it won't be deleted. You can then restart it
using `docker start -a -i NAME`. It's also possible to detach from a container
while it continues running in the background using by pressing Ctrl-P Ctrl-Q,
and then reattach to it using `docker attach NAME`.

Note that each time you run `stack --docker-persist`, a _new_ persistent
container is created (it will not automatically reuse the previous one).
See the [Docker user guide](https://docs.docker.com/userguide/) for more
information about managing Docker containers.

### Derivative image

Creating your own custom derivative image can be useful if you need to install
additional Ubuntu packages or make other changes to the operating system. Here
is an example (replace `stack-build:custom` if you prefer a different name for
your derived container, but it's best if the repo name matches what you're
deriving from, only with a different tag, to avoid recompilation):

    ;;; On host
    $ sudo stack  --docker-persist --docker-container-name=temp exec bash

    ;;; In container, make changes to OS
    # apt-get install r-cran-numderiv
    [...]
    # exit

    ;;; On host again
    $ docker commit temp stack-build:custom
    $ docker rm temp

Now you have a new Docker image named `stack-build:custom`. To use the new image, run
a command such as the following or update the corresponding values in your
`stack.yaml`:

    stack --docker-image=stack-build:custom <COMMAND>

Note, however, that any time a new image is used, you will have to re-do this
process. You could also use a Dockerfile to make this reusable. Consult the
[Docker user guide](https://docs.docker.com/userguide/) for more
on creating Docker images.

### Custom images

The easiest way to create your own custom image us by extending FP Complete's
images, but if you prefer to start from scratch, most images that include the
basics for building code with GHC will work. The image doesn't even, strictly
speaking, need to include GHC, but it does need to have libraries and tools that
GHC requires (e.g., libgmp, gcc, etc.).

There are also a few ways to set up images that tightens the integration:

* Create a user and group named `stack`, and create a `~/.stack` directory for
  it. Any build plans and caches from it will be copied from the image by Stack,
  meaning they don't need to be downloaded separately.
* Any packages in GHC's global package database will be available. This can be
  used to add private libraries to the image, or the make available a set of
  packages from an LTS release.

Troubleshooting
-------------------------------------------------------------------------------

### "No Space Left on Device", but 'df' shows plenty of disk space

This is likely due to the storage driver Docker is using, in combination with
the large size and number of files in these images. Use `docker info|grep
'Storage Driver'` to determine the current storage driver.

We recommend using either the `overlay` or `aufs` storage driver for stack, as
they are least likely to give you trouble.  On Ubuntu, `aufs` is the default for
new installations, but older installations sometimes used `devicemapper`.

The `devicemapper` storage driver's default configuration limits it to a 10 GB
file system, which the "full" images exceed. We have experienced other
instabilities with it as well on Ubuntu, and recommend against its use for this
purpose.

The `btrfs` storage driver has problems running out of metadata space long
before running out of actual disk space, which requires rebalancing or adding
more metadata space. See
[CoreOS's btrfs troubleshooting page](https://coreos.com/docs/cluster-management/debugging/btrfs-troubleshooting/)
for details about how to do this.

Pass the `-s <driver>` argument to the Docker daemon to set the storage driver
(in `/etc/default/docker` on Ubuntu). See
[Docker daemon storage-driver option](https://docs.docker.com/reference/commandline/cli/#daemon-storage-driver-option)
for more details.

You may also be running out of inodes on your filesystem.  Use `df -i` to check
for this condition.  Unfortunately, the number of inodes is set when creating
the filesystem, so fixing this requires reformatting and passing the `-N`
argument to mkfs.ext4.

### Name resolution doesn't work from within container

On Ubuntu 12.04, by default `NetworkManager` runs `dnsmasq` service, which sets
`127.0.0.1` as your DNS server. Since Docker containers cannot access this
dnsmasq, Docker falls back to using Google DNS (8.8.8.8/8.8.4.4). This causes
problems if you are forced to use internal DNS server. This can be fixed by
executing:

    sudo sed 's@dns=dnsmasq@#dns=dnsmasq@' -i \
        /etc/NetworkManager/NetworkManager.conf && \
    sudo service network-manager restart

If you have already installed Docker, you must restart the daemon for this
change to take effect:

    sudo service docker restart

<small>
The above commands turn off `dnsmasq` usage in NetworkManager
configuration and restart network manager.  They can be reversed by executing
`sudo sed 's@#dns=dnsmasq@dns=dnsmasq@' -i
/etc/NetworkManager/NetworkManager.conf && sudo service network-manager
restart`.  These instructions are adapted from
[the Shipyard Project's QuickStart guide](https://github.com/shipyard/shipyard/wiki/QuickStart#127011-dns-server-problem-on-ubuntu).
</small>

### Cannot pull images from behind firewall that blocks TLS/SSL

If you are behind a firewall that blocks TLS/SSL and pulling images from a
private Docker registry, you must edit the system configuration so that the
`--insecure-registry <registry-hostname>` option is passed to the Docker daemon.
For example, on Ubuntu:

    echo 'DOCKER_OPTS="--insecure-registry registry.example.com"' \
        |sudo tee -a /etc/default/docker
    sudo service docker restart

This does require the private registry to be available over plaintext HTTP.

See
[Docker daemon insecure registries documentation](https://docs.docker.com/reference/commandline/cli/#insecure-registries)
for details.
