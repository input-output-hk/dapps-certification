#UNSAFE_PLAIN_ADDRESS_AUTH=1 \
 WALLET_ADDRESS=addr_test1qphgqts20fhx0yx7ug42xehcnryukchy5k7hpaksgxax2fzt5w2gu33s8wrw3c9tjs97dr5pulsvf39e56v7c9ar39asptcrtp \
 WALLET_ID=73857344a0cf884fe044abfe85660cc9a81f6366 \
 WALLET_PASSPHRASE=test123456 \
 WALLET_URL="http://192.168.2.128:8090" \
 WALLET_CERTIFICATION_PRICE=1000000 \
 JWT_SECRET2=secret \
 PORT=80 \
 nix run .#dockerApps.runDockerImage
