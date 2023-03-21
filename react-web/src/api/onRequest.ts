import { AxiosError, AxiosRequestConfig } from "axios";

// Add a request interceptor
export function onRequest(config: AxiosRequestConfig) {
  const address: any = localStorage.getItem('address');
  const TokenAuth = address;

  if (address) {
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


export function onRepoAccessRequest(config: AxiosRequestConfig) {
  const accessToken: any = localStorage.getItem('accessToken')
  const TokenAuth = accessToken;

  if (accessToken) {
    config.headers = {
      ...config.headers,
      Authorization: TokenAuth,
    };
  }

  return config;
}