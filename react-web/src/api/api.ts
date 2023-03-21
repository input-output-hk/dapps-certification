import axios, { AxiosRequestConfig } from "axios";
import { onRequest, onRequestError, onRepoAccessRequest } from "./onRequest";

export const BASE_URL = process.env.REACT_APP_BASE_URL;

export const fetchData = axios.create({
  baseURL: BASE_URL,
  headers: {
    "Content-type": "application/json",
    Accept: "application/json",
  },
});

export const postData = axios.create({
  baseURL: BASE_URL,
  headers: {
    "Content-type": "text/plain;charset=utf-8",
    Accept: "text/plain;charset=utf-8",
  },
});

export const fetchImage = axios.create({
  baseURL: BASE_URL,
  headers: {
    'Cache-Control': 'private'
  }
})

export const getRepoAccess = axios.create({
  baseURL: BASE_URL,
  headers: {
    "Content-type": "application/json",
    Accept: "application/json",
  },
})

export const postExternal = axios.create()

fetchData.interceptors.request.use(
  (config: AxiosRequestConfig) => onRequest(config),
  onRequestError
);

postData.interceptors.request.use(
  (config: AxiosRequestConfig) => onRequest(config),
  onRequestError
);

getRepoAccess.interceptors.request.use(
  (config: AxiosRequestConfig) => onRepoAccessRequest(config),
  onRequestError
)