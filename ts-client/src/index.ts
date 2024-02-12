/* eslint-disable */
/* tslint:disable */
/*
 * ---------------------------------------------------------------
 * ## THIS FILE WAS GENERATED VIA SWAGGER-TYPESCRIPT-API        ##
 * ##                                                           ##
 * ## AUTHOR: acacode                                           ##
 * ## SOURCE: https://github.com/acacode/swagger-typescript-api ##
 * ---------------------------------------------------------------
 */

export interface Type1304 {
  metadata: MetadataUrl[];
  rootHash: Hash;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  schemaVersion: number;
  subject: Subject;
  type: CertificationType;
}

export interface AccessTokenGenerationResponse {
  access_token: string;
  scope?: string;
  token_type?: string;
}

/**
 * Action must be AUDIT or CERTIFY
 * @pattern ^(AUDIT|CERTIFY)$
 */
export type Action = string;

export interface AuditorCertificationInput {
  certificateIssuer?: CertificateIssuer;
  certificationLevel: CertificationLevel;
  disclaimer?: string;
  report?: ReportURL[];
  scripts?: Script[];
  subject: Subject;
  summary?: string;
}

export interface AuditorReportEvent {
  certLevel: CertificationLevel;
  createdAt: UTCTime;
  /**
   * @format int64
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  id: number;
  offchainContentId: string;
  /**
   * @format int64
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  profileId: number;
}

export interface CertOptNumTestsArgs {
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  numCrashTolerance?: number;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  numDLTests?: number;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  numNoLockedFunds?: number;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  numNoLockedFundsLight?: number;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  numStandardProperty?: number;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  numWhiteList?: number;
}

export interface CertificateIssuer {
  logo?: string;
  name: CertificationIssuerName;
  social: Social;
}

export interface CertificationInput {
  certificateIssuer: CertificateIssuer;
  disclaimer: string;
  scripts: Script[];
  summary: string;
}

/** @pattern ^.{1,64}$ */
export type CertificationIssuerName = string;

export enum CertificationLevel {
  Value0 = 0,
  Value1 = 1,
  Value2 = 2,
  Value3 = 3,
}

export type CertificationResult = any;

export interface CertificationTask {
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  index: number;
  name: CertificationTaskName;
}

export interface CertificationTaskName {
  /**
   * @maxItems 0
   * @example []
   */
  CrashToleranceTask?: any[];
  /**
   * @maxItems 0
   * @example []
   */
  DLTestsTask?: any[];
  /**
   * @maxItems 0
   * @example []
   */
  NoLockedFundsLightTask?: any[];
  /**
   * @maxItems 0
   * @example []
   */
  NoLockedFundsTask?: any[];
  /**
   * @maxItems 0
   * @example []
   */
  StandardPropertyTask?: any[];
  /**
   * @maxItems 0
   * @example []
   */
  UnitTestsTask?: any[];
  UnknownTask?: string;
  /**
   * @maxItems 0
   * @example []
   */
  WhitelistTask?: any[];
}

export interface CertificationType {
  /** Action must be AUDIT or CERTIFY */
  action: Action;
  certificateIssuer: CertificationIssuerName;
  certificationLevel: CertificationLevel;
}

export interface CertifyArgs {
  certOptNumTests?: CertOptNumTestsArgs;
}

export interface CertifyingStatus {
  certifyingPlan?: CertificationTask[];
  certifyingProgress?: Progress;
  certifyingState: StepState;
}

export type CommitOrBranch = string;

/** @pattern ^(addr_test1|addr1)[a-zA-Z0-9]{53,}$ */
export type ContractAddress = string;

export interface CreateRunOptions {
  certArgs: CertifyArgs;
  commitOrBranch: CommitOrBranch;
}

export interface DApp {
  githubToken?: GitHubAccessToken;
  name: string;
  owner: string;
  repo: string;
  subject?: string;
  version?: string;
}

/** @pattern ^(?:https?:\/\/)?discord(?:\.gg|app\.com\/invite|\.com\/invite)\/[\w-]+$ */
export type DiscordLink = string;

/** @pattern ^[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-_]+\.[A-Za-z]{2,64}$ */
export type Email = string;

export interface Feature {
  id: FeatureType;
  name: string;
}

export enum FeatureType {
  L1Run = "l1-run",
  L2UploadReport = "l2-upload-report",
  L0UploadReport = "l0-upload-report",
}

export interface FullMetadata {
  offchain: OffChainMetadata;
  onchain: OnChainMetadata;
}

/** @pattern ^gh[oprsu]_[A-Za-z0-9]{36}$ */
export type GitHubAccessToken = string;

/** @pattern ^(?=.{1,39}$)[a-zA-Z0-9]+(-[a-zA-Z0-9]+)*$ */
export type GitHubAccount = string;

/** @pattern ^[A-Fa-f0-9]{64}$ */
export type Hash = string;

export interface IncompleteRunStatus {
  Building?: StepState;
  Certifying?: CertifyingStatus;
  Preparing?: StepState;
  /**
   * @maxItems 0
   * @example []
   */
  Queued?: any[];
}

/** @pattern ^(http(s)?:\/\/)?([\w]+\.)?linkedin\.com\/(pub|in|profile|company)\/([a-zA-Z0-9_-]+)$ */
export type LinkedIn = string;

export interface LoginBody {
  address: string;
  expiration?: number;
  key: string;
  signature: string;
}

/** @pattern ^(https:|http:|ipfs://).*$ */
export type MetadataUrl = string;

export interface OffChainMetadata {
  certificateIssuer: CertificateIssuer;
  certificationLevel: CertificationLevel;
  disclaimer: string;
  report: Report;
  scripts: Script;
  subject: Subject;
  summary: string;
}

export interface OnChainMetadata {
  "1304"?: Type1304;
}

export interface ProfileBody {
  companyName?: string;
  contactEmail?: Email;
  dapp?: DApp;
  email?: Email;
  fullName?: string;
  /**
   * @format int64
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  id?: number;
  linkedin?: LinkedIn;
  twitter?: Twitter;
  website?: Website;
}

export interface ProfileDTO {
  address: ProfileWalletAddress;
  companyName?: string;
  contactEmail?: Email;
  dapp?: DApp;
  email?: Email;
  fullName?: string;
  /**
   * @format int64
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  id?: number;
  linkedin?: LinkedIn;
  role?: UserRole;
  twitter?: Twitter;
  website?: Website;
}

export interface ProfileSummaryDTO {
  address: ProfileWalletAddress;
  companyName?: string;
  contactEmail?: Email;
  dapp?: DApp;
  email?: Email;
  fullName?: string;
  /**
   * @format int64
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  id: number;
  linkedin?: LinkedIn;
  role: UserRole;
  runStats: RunStats;
  subscription?: SubscriptionLite;
  twitter?: Twitter;
  website?: Website;
}

/** @pattern ^(addr_test1|addr1|stake|stake_test1)[a-zA-Z0-9]{53,}$ */
export type ProfileWalletAddress = string;

export interface Progress {
  currentQc: QCProgress;
  currentTask?: CertificationTask;
  finishedTasks: TaskResult[];
  progressIndex: number;
}

export interface QCProgress {
  qcDiscarded: number;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  qcExpected?: number;
  qcFailures: number;
  qcSuccesses: number;
}

export interface Report {
  reportHash: Hash;
  /** Report URL */
  reportURLs: ReportURL;
}

/**
 * Report URL
 * @format uri
 * @pattern ^(https?|ipfs)://.*$
 */
export type ReportURL = string;

export interface RepositoryInfo {
  default_branch: string;
  description?: string;
  name: string;
  owner: RepositoryOwner;
  private: string;
}

export interface RepositoryOwner {
  avatar_url?: string;
  events_url?: string;
  followers_url?: string;
  following_url?: string;
  gists_url?: string;
  gravatar_id?: string;
  html_url?: string;
  id?: string;
  login?: string;
  node_id?: string;
  organizations_url?: string;
  received_events_url?: string;
  repos_url?: string;
  site_admin?: string;
  starred_url?: string;
  subscriptions_url?: string;
  type?: string;
  url?: string;
}

export interface Run {
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  certificationPrice: number;
  commitDate: UTCTime;
  commitHash: string;
  created: UTCTime;
  finishedAt?: UTCTime;
  repoUrl: string;
  reportContentId?: string;
  runId: UUID;
  runStatus: RunStatus;
  syncedAt?: UTCTime;
  withCustomOptions?: boolean;
}

export interface RunIDV1 {
  uuid: UUID;
}

export type RunLog = any;

export interface RunStats {
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  aborted: number;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  certified: number;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  failed: number;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  queued: number;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  readyForCertification: number;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  successful: number;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  total: number;
}

export enum RunStatus {
  Queued = "queued",
  Failed = "failed",
  Succeeded = "succeeded",
  Certified = "certified",
  ReadyForCertification = "ready-for-certification",
  Aborted = "aborted",
}

export interface RunStatusV1 {
  Finished?: CertificationResult;
  Incomplete?: IncompleteRunStatus;
}

export interface RunTimeArguments {
  interval: SlotSelector;
  /** @multipleOf 1e-12 */
  minRunTime?: number;
}

export interface RunTimeMetric {
  certified: boolean;
  endTime: UTCTime;
  /**
   * @format int64
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  profileId: number;
  runId: UUID;
  startTime: UTCTime;
}

export interface Script {
  contractAddress?: ContractAddress;
  scriptHash: Hash;
  smartContractInfo?: SmartContractInfo;
}

export interface SlotSelector {
  from: UTCTime;
  to: UTCTime;
}

export interface SmartContractInfo {
  compiler?: string;
  compilerVersion?: string;
  era?: string;
  optimizer?: string;
  optimizerVersion?: string;
  progLang?: string;
  repository?: string;
}

export interface Social {
  contact: Email;
  discord?: DiscordLink;
  github?: GitHubAccount;
  twitter?: Twitter;
  website: Website;
}

export enum StepState {
  Running = "Running",
  Failed = "Failed",
}

/** @pattern ^[A-Za-z0-9_]{1,64}$ */
export type Subject = string;

export interface SubscriptionDTO {
  /** @format double */
  adaUsdPrice: number;
  endDate: UTCTime;
  features: Feature[];
  /**
   * @format int64
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  id: number;
  name: string;
  /**
   * @format int64
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  price: number;
  /**
   * @format int64
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  profileId: number;
  startDate: UTCTime;
  status: SubscriptionStatus;
  /**
   * @format int64
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  tierId: number;
  type: TierType;
}

export interface SubscriptionLite {
  /** @format double */
  adaUsdPrice: number;
  endDate: UTCTime;
  /**
   * @format int64
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  id: number;
  name: string;
  /**
   * @format int64
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  price: number;
  startDate: UTCTime;
  status: SubscriptionStatus;
  tierType: TierType;
}

export enum SubscriptionStatus {
  Inactive = "inactive",
  Active = "active",
  Pending = "pending",
}

export interface TaskResult {
  qcResult: QCProgress;
  succeeded: boolean;
  task: CertificationTask;
}

export interface TierDTO {
  description: string;
  /**
   * @min -9223372036854776000
   * @max 9223372036854776000
   */
  duration: number;
  enabled: boolean;
  features: Feature[];
  id: string;
  name: string;
  subtitle: string;
  type: TierType;
  /** @format double */
  usdPrice: number;
}

export enum TierType {
  Developer = "developer",
  Auditor = "auditor",
}

/** @pattern ^[A-Za-z0-9_]{1,15}$ */
export type Twitter = string;

/**
 * @format yyyy-mm-ddThh:MM:ssZ
 * @example "2016-07-22T00:00:00Z"
 */
export type UTCTime = string;

/**
 * @format uuid
 * @example "00000000-0000-0000-0000-000000000000"
 */
export type UUID = string;

export enum UserRole {
  NoRole = "no-role",
  Support = "support",
  Admin = "admin",
}

export interface VersionV1 {
  /** @pattern ^\d+(\.\d+)*$ */
  version: string;
}

export enum WalletAddressStatus {
  Reserved = "reserved",
  Overlapping = "overlapping",
}

/** @pattern ^(https?:\/\/)?(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,255}\.[a-z]{2,6}(\b([-a-zA-Z0-9@:%_\+.~#()?&\/\/=]*))?$ */
export type Website = string;

import type { AxiosInstance, AxiosRequestConfig, AxiosResponse, HeadersDefaults, ResponseType } from "axios";
import axios from "axios";

export type QueryParamsType = Record<string | number, any>;

export interface FullRequestParams extends Omit<AxiosRequestConfig, "data" | "params" | "url" | "responseType"> {
  /** set parameter to `true` for call `securityWorker` for this request */
  secure?: boolean;
  /** request path */
  path: string;
  /** content type of request body */
  type?: ContentType;
  /** query params */
  query?: QueryParamsType;
  /** format of response (i.e. response.json() -> format: "json") */
  format?: ResponseType;
  /** request body */
  body?: unknown;
}

export type RequestParams = Omit<FullRequestParams, "body" | "method" | "query" | "path">;

export interface ApiConfig<SecurityDataType = unknown> extends Omit<AxiosRequestConfig, "data" | "cancelToken"> {
  securityWorker?: (
    securityData: SecurityDataType | null,
  ) => Promise<AxiosRequestConfig | void> | AxiosRequestConfig | void;
  secure?: boolean;
  format?: ResponseType;
}

export enum ContentType {
  Json = "application/json",
  FormData = "multipart/form-data",
  UrlEncoded = "application/x-www-form-urlencoded",
  Text = "text/plain",
}

export class HttpClient<SecurityDataType = unknown> {
  public instance: AxiosInstance;
  private securityData: SecurityDataType | null = null;
  private securityWorker?: ApiConfig<SecurityDataType>["securityWorker"];
  private secure?: boolean;
  private format?: ResponseType;

  constructor({ securityWorker, secure, format, ...axiosConfig }: ApiConfig<SecurityDataType> = {}) {
    this.instance = axios.create({ ...axiosConfig, baseURL: axiosConfig.baseURL || "" });
    this.secure = secure;
    this.format = format;
    this.securityWorker = securityWorker;
  }

  public setSecurityData = (data: SecurityDataType | null) => {
    this.securityData = data;
  };

  protected mergeRequestParams(params1: AxiosRequestConfig, params2?: AxiosRequestConfig): AxiosRequestConfig {
    const method = params1.method || (params2 && params2.method);

    return {
      ...this.instance.defaults,
      ...params1,
      ...(params2 || {}),
      headers: {
        ...((method && this.instance.defaults.headers[method.toLowerCase() as keyof HeadersDefaults]) || {}),
        ...(params1.headers || {}),
        ...((params2 && params2.headers) || {}),
      },
    };
  }

  protected stringifyFormItem(formItem: unknown) {
    if (typeof formItem === "object" && formItem !== null) {
      return JSON.stringify(formItem);
    } else {
      return `${formItem}`;
    }
  }

  protected createFormData(input: Record<string, unknown>): FormData {
    return Object.keys(input || {}).reduce((formData, key) => {
      const property = input[key];
      const propertyContent: any[] = property instanceof Array ? property : [property];

      for (const formItem of propertyContent) {
        const isFileType = formItem instanceof Blob || formItem instanceof File;
        formData.append(key, isFileType ? formItem : this.stringifyFormItem(formItem));
      }

      return formData;
    }, new FormData());
  }

  public request = async <T = any, _E = any>({
    secure,
    path,
    type,
    query,
    format,
    body,
    ...params
  }: FullRequestParams): Promise<AxiosResponse<T>> => {
    const secureParams =
      ((typeof secure === "boolean" ? secure : this.secure) &&
        this.securityWorker &&
        (await this.securityWorker(this.securityData))) ||
      {};
    const requestParams = this.mergeRequestParams(params, secureParams);
    const responseFormat = format || this.format || undefined;

    if (type === ContentType.FormData && body && body !== null && typeof body === "object") {
      body = this.createFormData(body as Record<string, unknown>);
    }

    if (type === ContentType.Text && body && body !== null && typeof body !== "string") {
      body = JSON.stringify(body);
    }

    return this.instance.request({
      ...requestParams,
      headers: {
        ...(requestParams.headers || {}),
        ...(type && type !== ContentType.FormData ? { "Content-Type": type } : {}),
      },
      params: query,
      responseType: responseFormat,
      data: body,
      url: path,
    });
  };
}

/**
 * @title Plutus Certification API
 * @version 1.0
 *
 * This is an API for the Plutus Certification Service
 */
export class Api<SecurityDataType extends unknown> extends HttpClient<SecurityDataType> {
  adaUsdPrice = {
    /**
     * @description Get the current ADA/USD price
     *
     * @name AdaUsdPriceList
     * @request GET:/ada-usd-price
     */
    adaUsdPriceList: (params: RequestParams = {}) =>
      this.request<number, any>({
        path: `/ada-usd-price`,
        method: "GET",
        format: "json",
        ...params,
      }),
  };
  auditor = {
    /**
     * @description Fetches the auditor report L0 | L2
     *
     * @name ReportsCreate
     * @request POST:/auditor/reports
     */
    reportsCreate: (
      body: AuditorCertificationInput,
      query?: {
        "dry-run"?: boolean;
      },
      params: RequestParams = {},
    ) =>
      this.request<FullMetadata, void>({
        path: `/auditor/reports`,
        method: "POST",
        query: query,
        body: body,
        type: ContentType.Json,
        format: "json",
        ...params,
      }),
  };
  github = {
    /**
     * @description Generate a github access token
     *
     * @name AccessTokenCreate
     * @request POST:/github/access-token/{code}
     */
    accessTokenCreate: (code: string, params: RequestParams = {}) =>
      this.request<AccessTokenGenerationResponse, void>({
        path: `/github/access-token/${code}`,
        method: "POST",
        format: "json",
        ...params,
      }),

    /**
     * @description Get the application client id
     *
     * @name ClientIdList
     * @request GET:/github/client-id
     */
    clientIdList: (params: RequestParams = {}) =>
      this.request<Action, any>({
        path: `/github/client-id`,
        method: "GET",
        format: "json",
        ...params,
      }),
  };
  login = {
    /**
     * @description Get a jwt token based on the provided credentials
     *
     * @name LoginCreate
     * @request POST:/login
     */
    loginCreate: (body: LoginBody, params: RequestParams = {}) =>
      this.request<Action, void>({
        path: `/login`,
        method: "POST",
        body: body,
        type: ContentType.Json,
        format: "json",
        ...params,
      }),
  };
  metrics = {
    /**
     * @description Get the auditor report metrics
     *
     * @name AuditorReportsCreate
     * @request POST:/metrics/auditor-reports
     */
    auditorReportsCreate: (body: SlotSelector, params: RequestParams = {}) =>
      this.request<AuditorReportEvent[], void>({
        path: `/metrics/auditor-reports`,
        method: "POST",
        body: body,
        type: ContentType.Json,
        format: "json",
        ...params,
      }),

    /**
     * @description Get the run times
     *
     * @name RunTimesCreate
     * @request POST:/metrics/run-times
     */
    runTimesCreate: (body: RunTimeArguments, params: RequestParams = {}) =>
      this.request<RunTimeMetric[], void>({
        path: `/metrics/run-times`,
        method: "POST",
        body: body,
        type: ContentType.Json,
        format: "json",
        ...params,
      }),

    /**
     * @description Get all subscriptions expiring in a given interval
     *
     * @name SubscriptionsEndingInIntervalCreate
     * @request POST:/metrics/subscriptions/ending/in-interval
     */
    subscriptionsEndingInIntervalCreate: (body: SlotSelector, params: RequestParams = {}) =>
      this.request<SubscriptionDTO[], void>({
        path: `/metrics/subscriptions/ending/in-interval`,
        method: "POST",
        body: body,
        type: ContentType.Json,
        format: "json",
        ...params,
      }),

    /**
     * @description Get all subscriptions created in a given interval
     *
     * @name SubscriptionsStartedInIntervalCreate
     * @request POST:/metrics/subscriptions/started/in-interval
     */
    subscriptionsStartedInIntervalCreate: (body: SlotSelector, params: RequestParams = {}) =>
      this.request<SubscriptionDTO[], void>({
        path: `/metrics/subscriptions/started/in-interval`,
        method: "POST",
        body: body,
        type: ContentType.Json,
        format: "json",
        ...params,
      }),
  };
  profile = {
    /**
     * @description Get the current profile information
     *
     * @name CurrentList
     * @request GET:/profile/current
     */
    currentList: (params: RequestParams = {}) =>
      this.request<ProfileDTO, void>({
        path: `/profile/current`,
        method: "GET",
        format: "json",
        ...params,
      }),

    /**
     * @description Update the current profile information
     *
     * @name CurrentUpdate
     * @request PUT:/profile/current
     */
    currentUpdate: (body: ProfileBody, params: RequestParams = {}) =>
      this.request<ProfileDTO, void>({
        path: `/profile/current`,
        method: "PUT",
        body: body,
        type: ContentType.Json,
        format: "json",
        ...params,
      }),

    /**
     * @description Get the current balance of the profile
     *
     * @name CurrentBalanceList
     * @request GET:/profile/current/balance
     */
    currentBalanceList: (params: RequestParams = {}) =>
      this.request<number, void>({
        path: `/profile/current/balance`,
        method: "GET",
        format: "json",
        ...params,
      }),

    /**
     * @description Get the current profile subscriptions. Expiration isn't checked, so it's possible to get expired subscriptions
     *
     * @name CurrentSubscriptionsList
     * @request GET:/profile/current/subscriptions
     */
    currentSubscriptionsList: (
      query?: {
        "just-enabled"?: boolean;
      },
      params: RequestParams = {},
    ) =>
      this.request<SubscriptionDTO[], void>({
        path: `/profile/current/subscriptions`,
        method: "GET",
        query: query,
        format: "json",
        ...params,
      }),

    /**
     * @description Get the active features of the current profile
     *
     * @name CurrentSubscriptionsActiveFeaturesList
     * @request GET:/profile/current/subscriptions/active-features
     */
    currentSubscriptionsActiveFeaturesList: (params: RequestParams = {}) =>
      this.request<FeatureType[], void>({
        path: `/profile/current/subscriptions/active-features`,
        method: "GET",
        format: "json",
        ...params,
      }),

    /**
     * @description Cancel the current profile pending subscriptions
     *
     * @name CurrentSubscriptionsPendingDelete
     * @request DELETE:/profile/current/subscriptions/pending
     */
    currentSubscriptionsPendingDelete: (params: RequestParams = {}) =>
      this.request<number, void>({
        path: `/profile/current/subscriptions/pending`,
        method: "DELETE",
        format: "json",
        ...params,
      }),

    /**
     * @description Create a new profile subscription
     *
     * @name CurrentSubscriptionsCreate
     * @request POST:/profile/current/subscriptions/{tier}
     */
    currentSubscriptionsCreate: (tier: number, params: RequestParams = {}) =>
      this.request<SubscriptionDTO, void>({
        path: `/profile/current/subscriptions/${tier}`,
        method: "POST",
        format: "json",
        ...params,
      }),

    /**
     * @description Get the wallet address of the current profile
     *
     * @name CurrentWalletAddressList
     * @request GET:/profile/current/wallet-address
     */
    currentWalletAddressList: (params: RequestParams = {}) =>
      this.request<(WalletAddressStatus | string)[], void>({
        path: `/profile/current/wallet-address`,
        method: "GET",
        format: "json",
        ...params,
      }),

    /**
     * @description Get the profile information
     *
     * @name ProfileDetail
     * @request GET:/profile/{id}
     */
    profileDetail: (id: number, params: RequestParams = {}) =>
      this.request<ProfileDTO, void>({
        path: `/profile/${id}`,
        method: "GET",
        format: "json",
        ...params,
      }),

    /**
     * @description Update the profile information
     *
     * @name ProfileUpdate
     * @request PUT:/profile/{id}
     */
    profileUpdate: (id: number, body: ProfileBody, params: RequestParams = {}) =>
      this.request<ProfileDTO, void>({
        path: `/profile/${id}`,
        method: "PUT",
        body: body,
        type: ContentType.Json,
        format: "json",
        ...params,
      }),

    /**
     * @description Get the balance of a given profile
     *
     * @name BalanceDetail
     * @request GET:/profile/{id}/balance
     */
    balanceDetail: (id: number, params: RequestParams = {}) =>
      this.request<number, void>({
        path: `/profile/${id}/balance`,
        method: "GET",
        format: "json",
        ...params,
      }),

    /**
     * @description Get the roles of a given profile
     *
     * @name RolesDetail
     * @request GET:/profile/{id}/roles
     */
    rolesDetail: (id: number, params: RequestParams = {}) =>
      this.request<UserRole[], void>({
        path: `/profile/${id}/roles`,
        method: "GET",
        format: "json",
        ...params,
      }),

    /**
     * @description Update the roles of a given profile
     *
     * @name RolesUpdate
     * @request PUT:/profile/{id}/roles
     */
    rolesUpdate: (id: number, body: UserRole[], params: RequestParams = {}) =>
      this.request<void, void>({
        path: `/profile/${id}/roles`,
        method: "PUT",
        body: body,
        type: ContentType.Json,
        ...params,
      }),

    /**
     * @description Create a new testing run attached to a different profile
     *
     * @name PostProfile
     * @request POST:/profile/{id}/run
     */
    postProfile: (id: number, body: CreateRunOptions, params: RequestParams = {}) =>
      this.request<RunIDV1, void>({
        path: `/profile/${id}/run`,
        method: "POST",
        body: body,
        type: ContentType.Json,
        format: "json",
        ...params,
      }),

    /**
     * @description Query through multiple profile runs
     *
     * @name RunsDetail
     * @request GET:/profile/{id}/runs
     */
    runsDetail: (
      id: number,
      query?: {
        /** @format yyyy-mm-ddThh:MM:ssZ */
        after?: string;
        /**
         * @min -9223372036854776000
         * @max 9223372036854776000
         */
        count?: number;
      },
      params: RequestParams = {},
    ) =>
      this.request<Run[], void>({
        path: `/profile/${id}/runs`,
        method: "GET",
        query: query,
        format: "json",
        ...params,
      }),

    /**
     * @description Get the profile subscriptions. Expiration isn't checked, so it's possible to get expired subscriptions
     *
     * @name SubscriptionsDetail
     * @request GET:/profile/{id}/subscriptions
     */
    subscriptionsDetail: (
      id: number,
      query?: {
        "just-enabled"?: boolean;
      },
      params: RequestParams = {},
    ) =>
      this.request<SubscriptionDTO[], void>({
        path: `/profile/${id}/subscriptions`,
        method: "GET",
        query: query,
        format: "json",
        ...params,
      }),

    /**
     * @description Get the active features of a given profile
     *
     * @name SubscriptionsActiveFeaturesDetail
     * @request GET:/profile/{id}/subscriptions/active-features
     */
    subscriptionsActiveFeaturesDetail: (id: number, params: RequestParams = {}) =>
      this.request<FeatureType[], void>({
        path: `/profile/${id}/subscriptions/active-features`,
        method: "GET",
        format: "json",
        ...params,
      }),

    /**
     * @description Cancel a given profile pending subscriptions
     *
     * @name SubscriptionsPendingDelete
     * @request DELETE:/profile/{id}/subscriptions/pending
     */
    subscriptionsPendingDelete: (id: number, params: RequestParams = {}) =>
      this.request<number, void>({
        path: `/profile/${id}/subscriptions/pending`,
        method: "DELETE",
        format: "json",
        ...params,
      }),

    /**
     * @description Get the wallet address of a given profile
     *
     * @name WalletAddressDetail
     * @request GET:/profile/{id}/wallet-address
     */
    walletAddressDetail: (id: number, params: RequestParams = {}) =>
      this.request<(WalletAddressStatus | string)[], void>({
        path: `/profile/${id}/wallet-address`,
        method: "GET",
        format: "json",
        ...params,
      }),
  };
  profiles = {
    /**
     * @description Getting all profiles with their maximum role and their dapp if any
     *
     * @name ProfilesList
     * @request GET:/profiles
     */
    profilesList: (params: RequestParams = {}) =>
      this.request<ProfileSummaryDTO[], void>({
        path: `/profiles`,
        method: "GET",
        format: "json",
        ...params,
      }),
  };
  repo = {
    /**
     * @description Get the github repo information
     *
     * @name RepoDetail
     * @request GET:/repo/{owner}/{repo}
     */
    repoDetail: (owner: string, repo: string, params: RequestParams = {}) =>
      this.request<RepositoryInfo, void>({
        path: `/repo/${owner}/${repo}`,
        method: "GET",
        format: "json",
        ...params,
      }),
  };
  roles = {
    /**
     * @description Get all profile ids by role
     *
     * @name ProfilesSummaryDetail
     * @request GET:/roles/{role}/profiles/summary
     */
    profilesSummaryDetail: (role: "no-role" | "support" | "admin", params: RequestParams = {}) =>
      this.request<number[], void>({
        path: `/roles/${role}/profiles/summary`,
        method: "GET",
        format: "json",
        ...params,
      }),
  };
  run = {
    /**
     * @description Query through multiple runs belonging to the current profile
     *
     * @name GetRun
     * @request GET:/run
     */
    getRun: (
      query?: {
        /** @format yyyy-mm-ddThh:MM:ssZ */
        after?: string;
        /**
         * @min -9223372036854776000
         * @max 9223372036854776000
         */
        count?: number;
      },
      params: RequestParams = {},
    ) =>
      this.request<Run[], void>({
        path: `/run`,
        method: "GET",
        query: query,
        format: "json",
        ...params,
      }),

    /**
     * @description Create a new testing run attached to the current profile
     *
     * @name PostRun
     * @request POST:/run
     */
    postRun: (body: CreateRunOptions, params: RequestParams = {}) =>
      this.request<RunIDV1, void>({
        path: `/run`,
        method: "POST",
        body: body,
        type: ContentType.Json,
        format: "json",
        ...params,
      }),

    /**
     * @description Abort a run and deletes the history entry if query param is provided
     *
     * @name DeleteRun
     * @request DELETE:/run/{id}
     */
    deleteRun: (
      id: string,
      query?: {
        delete?: boolean;
      },
      params: RequestParams = {},
    ) =>
      this.request<void, void>({
        path: `/run/${id}`,
        method: "DELETE",
        query: query,
        ...params,
      }),

    /**
     * @description Get the status of a run
     *
     * @name GetRun2
     * @request GET:/run/{id}
     * @originalName getRun
     * @duplicate
     */
    getRun2: (id: string, params: RequestParams = {}) =>
      this.request<RunStatusV1, void>({
        path: `/run/${id}`,
        method: "GET",
        format: "json",
        ...params,
      }),

    /**
     * @description Store the L1 Report into IPFS and broadcasts the Certificate onchain
     *
     * @name CertificateCreate
     * @request POST:/run/{id}/certificate
     */
    certificateCreate: (
      id: string,
      body: CertificationInput,
      query?: {
        "dry-run"?: boolean;
      },
      params: RequestParams = {},
    ) =>
      this.request<FullMetadata, void>({
        path: `/run/${id}/certificate`,
        method: "POST",
        query: query,
        body: body,
        type: ContentType.Json,
        format: "json",
        ...params,
      }),

    /**
     * @description Get the details of a run
     *
     * @name DetailsDetail
     * @request GET:/run/{id}/details
     */
    detailsDetail: (id: string, params: RequestParams = {}) =>
      this.request<Run, void>({
        path: `/run/${id}/details`,
        method: "GET",
        format: "json",
        ...params,
      }),

    /**
     * @description Get the logs of a run
     *
     * @name LogsDetail
     * @request GET:/run/{id}/logs
     */
    logsDetail: (
      id: string,
      query?: {
        /** @format yyyy-mm-ddThh:MM:ss+hhMM */
        after?: string;
        "action-type"?: "Generate" | "Build" | "Certify";
      },
      params: RequestParams = {},
    ) =>
      this.request<RunLog[], void>({
        path: `/run/${id}/logs`,
        method: "GET",
        query: query,
        format: "json",
        ...params,
      }),
  };
  serverTimestamp = {
    /**
     * @description Get the current server timestamp
     *
     * @name ServerTimestampList
     * @request GET:/server-timestamp
     */
    serverTimestampList: (params: RequestParams = {}) =>
      this.request<number, any>({
        path: `/server-timestamp`,
        method: "GET",
        format: "json",
        ...params,
      }),
  };
  tiers = {
    /**
     * @description Get the available tiers
     *
     * @name TiersList
     * @request GET:/tiers
     */
    tiersList: (params: RequestParams = {}) =>
      this.request<TierDTO[], any>({
        path: `/tiers`,
        method: "GET",
        format: "json",
        ...params,
      }),
  };
  version = {
    /**
     * @description Get the api version
     *
     * @name VersionList
     * @request GET:/version
     */
    versionList: (params: RequestParams = {}) =>
      this.request<VersionV1, any>({
        path: `/version`,
        method: "GET",
        format: "json",
        ...params,
      }),

    /**
     * @description Get the api version (Response Headers only)
     *
     * @name HeadVersion
     * @request HEAD:/version
     */
    headVersion: (params: RequestParams = {}) =>
      this.request<void, any>({
        path: `/version`,
        method: "HEAD",
        ...params,
      }),
  };
  walletAddress = {
    /**
     * @description Get the wallet address the backend operates with
     *
     * @name WalletAddressList
     * @request GET:/wallet-address
     */
    walletAddressList: (params: RequestParams = {}) =>
      this.request<Action, any>({
        path: `/wallet-address`,
        method: "GET",
        format: "json",
        ...params,
      }),
  };
}
