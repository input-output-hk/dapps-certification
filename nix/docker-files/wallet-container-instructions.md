## 1. To manage cardano-node and cardano-wallet containers

1.1 first to start containers run:

```
NETWORK=preprod docker-compose -f docker-compose.cardano.yml up
```

or as a dameon

```
NETWORK=preprod docker-compose -f docker-compose.cardano.yml up -d
```

## 2. create a wallet 

```
curl --request POST --url http://localhost:8090/v2/wallets \
  --header 'Content-Type: application/json' \
  --data '{
    "name": "test_cf_1",
    "mnemonic_sentence": ["stock","horn","under","crime","acid","tell","repair","brain","shallow","dinosaur","candy","sight","memory","antenna","baby","truck","force","chuckle","elephant","unhappy","sentence","control","hold","camera"],
    "passphrase": "test123456"
}'
```

## 3. await to be synced (ready)
```
curl --url 'http://localhost:8090/v2/wallets/73857344a0cf884fe044abfe85660cc9a81f6366' | jq '.state.status'
```


# TIPS:

## for checking the balance
```
curl --url 'http://localhost:8090/v2/wallets/73857344a0cf884fe044abfe85660cc9a81f6366' | jq '.balance.total.quantity'
```

## in order to stop the containers if you run it as daemon

```
NETWORK=preprod docker-compose -f docker-compose.cardano.yml down
```

