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
CMD [ "plutus-certification","--local", "--wallet-id", "73857344a0cf884fe044abfe85660cc9a81f6366", "--wallet-address", "addr_test1qphgqts20fhx0yx7ug42xehcnryukchy5k7hpaksgxax2fzt5w2gu33s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asptcrtp", "--wallet-passphrase", "test123456", "--wallet-url", "http://host.docker.internal:8090", "--wallet-certification-price", "1000000", "--unsafe-plain-address-auth", "--ada-usd-price","0.371" ]




