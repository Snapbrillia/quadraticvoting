FROM amd64/fedora:latest

SHELL ["/bin/bash", "--rcfile", "~/.profile", "-c"]

USER root

RUN yum install -y xz

# Create USER builder and config for Nix as root
# N.B. we create /nix here to allow installation of Nix.
# when we run the image in a container.
RUN groupadd --system nixbld
RUN useradd -ms /bin/bash builder
RUN usermod -G nixbld -a builder
RUN for i in $(seq 1 30); do useradd -ms /bin/bash nixbld${i} &&  usermod -G nixbld -a nixbld${i}; done \
  && mkdir -m 0755 /nix && chown builder /nix \
  && mkdir -p /etc/nix \
  && echo 'sandbox = false' > /etc/nix/nix.conf \
  && echo 'build-users-group = nixbld' >> /etc/nix/nix.conf \
  && echo 'experimental-features = nix-command flakes' >> /etc/nix/nix.conf \
  && echo 'allow-import-from-derivation = true' >> /etc/nix/nix.conf \
  && echo 'substituters  = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/'  >> /etc/nix/nix.conf \ 
  && echo "trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" >> /etc/nix/nix.conf 

# switch to USER = builder and make CWD /home/builder
USER builder
ENV USER=builder
WORKDIR /home/builder

RUN touch /home/builder/.bash_profile \
  && curl https://releases.nixos.org/nix/nix-2.9.2/install | sh
  
RUN echo '. /home/builder/.nix-profile/etc/profile.d/nix.sh' >> /home/builder/.bashrc
RUN mkdir -p /home/builder/.config/nixpkgs && echo '{ allowUnfree = true; }' >> /home/builder/.config/nixpkgs/config.nix
RUN mkdir /home/builder/repo

# Install cachix
RUN . /home/builder/.nix-profile/etc/profile.d/nix.sh \
  && nix-env -iA cachix -f https://cachix.org/api/v1/install \
  && cachix use cachix 

# Install git
RUN . /home/builder/.nix-profile/etc/profile.d/nix.sh \
  && nix-env -i git git-lfs

# Warm up Nix cache (only needed if we use the container's cache)
COPY . /home/builder/repo
WORKDIR /home/builder/repo
RUN . /home/builder/.nix-profile/etc/profile.d/nix.sh \
    && nix develop 


