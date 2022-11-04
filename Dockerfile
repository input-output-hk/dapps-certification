FROM nixos/nix
COPY docker-files/nix.conf /etc/nix/nix.conf
COPY . /dapps-certification

WORKDIR /dapps-certification
RUN nix profile install .#plutus-certification:exe:plutus-certification
RUN nix profile install .#plutus-certification:exe:plutus-certification-client
RUN nix-collect-garbage
WORKDIR /
RUN rm -rf dapps-certification/

#ENTRYPOINT [ "/bin/sh" ]
CMD [ "plutus-certification","--local" ]


