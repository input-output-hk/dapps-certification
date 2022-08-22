import { AxiosError, AxiosRequestConfig } from "axios";

// Add a request interceptor
export function onRequest(config: AxiosRequestConfig) {
  const auth = "";
  var TokenAuth = "Token " + auth;

  if (auth) {
    config.headers = {
      ...config.headers,
      Authorization: TokenAuth,
    };
  }

  return config;
}

export function onRequestError(error: AxiosError) {
  // Do something with request error
  return Promise.reject(error);
}
