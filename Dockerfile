FROM fpco/stack-build:lts-6.4
ENV LANG     C.UTF-8

RUN (groupadd -r nix-dummy-usr && \
     groupadd -r nixbld && \
     mkdir -p /home/nix-dummy-usr && \
     useradd -r -g nix-dummy-usr nix-dummy-usr && \
     chown nix-dummy-usr:nix-dummy-usr /home/nix-dummy-usr && \
     mkdir -m 0755 /nix && chown nix-dummy-usr /nix && \
     su nix-dummy-usr -c "curl https://nixos.org/nix/install | bash")    
RUN (. /home/nix-dummy-usr/.nix-profile/etc/profile.d/nix.sh && \
     nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs && \
     nix-channel --update nixpkgs)

ADD .stack-work/install/x86_64-linux/lts-6.0/7.10.3/bin/stack /usr/bin/stack.copied

RUN ( \
     # echo "#!/bin/bash" > /usr/bin/nix-shell && \
     # echo ". /home/nix-dummy-usr/.nix-profile/etc/profile.d/nix.sh ; nix-shell \"\$@\"" >> /usr/bin/nix-shell && \
     # chmod +x /usr/bin/nix-shell)
     rm /usr/bin/stack && \
#     mv /usr/bin/stack /usr/bin/stack.bin && \
     echo "#!/bin/bash" > /usr/bin/stack && \
     echo "export HOME=/home/_stack && source /home/nix-dummy-usr/.nix-profile/etc/profile.d/nix.sh && nix-channel --update nixpkgs && file \${NIX_PATH/nixpkgs=} && /usr/bin/stack.copied \"\$@\"" >> /usr/bin/stack && \
     chmod +x /usr/bin/stack)

#RUN chmod 777 -R /nix
#RUN usermod -a -G nixbld nix-dummy-usr



# Phheww... Ok, It's still hacky but I managed to make it work with
# `--docker-stack-exe image --no-docker-set-user`
# and by creating a script in the container that takes the place of `/usr/bin/stack`
# and goes like this:

# ```sh
# #!/bin/bash
# export HOME=/home/_stack
# source /home/nix-dummy-usr/.nix-profile/etc/profile.d/nix.sh
# nix-channel --update nixpkgs
# /usr/bin/real-stack-bin "$@"
# ```

# But now I get
# Invalid option `--internal-re-exec-version=1.1.3'

# Problems with stack/docker:
#   HOME variable isn't set in the
#   HOME (/home/_stack) is mounted by stack, so
