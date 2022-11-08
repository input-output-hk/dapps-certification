### react-web

Front-end repository for Certification Service integration

## Building and Running

### Installation
```sh
npm install
```

### API Integration
```sh
"proxy": "https://testing.dapps.io/" #in package.json 
```
Create file `.env` in `react-web/`, with below content
```sh
REACT_APP_BASE_URL="https://testing.dapps.io/"
```
* Change above URLs to any respective API endpoints for integration testing.
* Also, to load data from mock static JSONs placed within `public/static/data` for FE development purposes


### Development server with hot reload
```sh
"homepage": "http://localhost:3000/" #in package.json for reading assets from public/ folder
```
To start a development server
```sh
npm start
```

### Development build
```sh
npm build
```

### Production build
* Make sure to edit back `"homepage"` within `package.json` to the front-end hosting URL 
```sh
npm build:production
```

### Steps to deploy react app to github-pages
* Navigate to _Actions_
* Navigate to _CI Deployment_ workflow
* Click 'Run workflow' dropover
* Pick your branch from 'Use workflow from'
* Hit the 'Run workflow' button
* Checkout the app at https://input-output-hk.github.io/dapps-certification/