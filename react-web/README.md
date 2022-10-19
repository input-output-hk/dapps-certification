### react-web

Front-end repository for Certification Service integration

## Building and Running

### Installation
```sh
yarn install
```

### Development server with hot reload
```sh
yarn start
```

### Development build
```sh
yarn build
```

### Production build
```sh
yarn build:production
```

### Steps to deploy react app to hosting

1. Navigate to folder `/react-web`
2. Run `yarn build`; to generate the react build files within the folder `build/`
3. `firebase deploy`; to deploy the contents in `build/` folder
