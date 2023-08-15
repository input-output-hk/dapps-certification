module.exports = {
    moduleNameMapper: {
      "@emurgo/cardano-serialization-lib-browser":
        "<rootDir>/src/mocks/serializerMock.js",
      "\\.(scss|sass|css)$": "identity-obj-proxy", // mock all CSS files
      "^pages/(.*)$": "<rootDir>/src/pages/$1",
      "^components/(.*)$": "<rootDir>/src/components/$1",
      "^compositions/(.*)$": "<rootDir>/src/compositions/$1",
      "^routes/(.*)$": "<rootDir>/src/routes/$1",
      "^constants/(.*)$": "<rootDir>/src/constants/$1",
      "^hooks/(.*)$": "<rootDir>/src/hooks/$1",
      "^utils/(.*)$": "<rootDir>/src/utils/$1",
      "^store/(.*)$": "<rootDir>/src/store/$1",
      "^api/(.*)$": "<rootDir>/src/api/$1",
      "^assets/(.*)$": "<rootDir>/src/assets/$1",
      "^test/(.*)$": "<rootDir>/src/test/$1",
    },
    collectCoverageFrom: ["src/**/*.tsx"],
    testEnvironment: "jsdom",
    transform: {
      "^.+\\.tsx?$": "ts-jest",
    },
    setupFilesAfterEnv: ["@testing-library/jest-dom/extend-expect"],
  };