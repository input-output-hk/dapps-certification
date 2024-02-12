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
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __rest = (this && this.__rest) || function (s, e) {
    var t = {};
    for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === "function")
        for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
            if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                t[p[i]] = s[p[i]];
        }
    return t;
};
export var CertificationLevel;
(function (CertificationLevel) {
    CertificationLevel[CertificationLevel["Value0"] = 0] = "Value0";
    CertificationLevel[CertificationLevel["Value1"] = 1] = "Value1";
    CertificationLevel[CertificationLevel["Value2"] = 2] = "Value2";
    CertificationLevel[CertificationLevel["Value3"] = 3] = "Value3";
})(CertificationLevel || (CertificationLevel = {}));
export var FeatureType;
(function (FeatureType) {
    FeatureType["L1Run"] = "l1-run";
    FeatureType["L2UploadReport"] = "l2-upload-report";
    FeatureType["L0UploadReport"] = "l0-upload-report";
})(FeatureType || (FeatureType = {}));
export var RunStatus;
(function (RunStatus) {
    RunStatus["Queued"] = "queued";
    RunStatus["Failed"] = "failed";
    RunStatus["Succeeded"] = "succeeded";
    RunStatus["Certified"] = "certified";
    RunStatus["ReadyForCertification"] = "ready-for-certification";
    RunStatus["Aborted"] = "aborted";
})(RunStatus || (RunStatus = {}));
export var StepState;
(function (StepState) {
    StepState["Running"] = "Running";
    StepState["Failed"] = "Failed";
})(StepState || (StepState = {}));
export var SubscriptionStatus;
(function (SubscriptionStatus) {
    SubscriptionStatus["Inactive"] = "inactive";
    SubscriptionStatus["Active"] = "active";
    SubscriptionStatus["Pending"] = "pending";
})(SubscriptionStatus || (SubscriptionStatus = {}));
export var TierType;
(function (TierType) {
    TierType["Developer"] = "developer";
    TierType["Auditor"] = "auditor";
})(TierType || (TierType = {}));
export var UserRole;
(function (UserRole) {
    UserRole["NoRole"] = "no-role";
    UserRole["Support"] = "support";
    UserRole["Admin"] = "admin";
})(UserRole || (UserRole = {}));
export var WalletAddressStatus;
(function (WalletAddressStatus) {
    WalletAddressStatus["Reserved"] = "reserved";
    WalletAddressStatus["Overlapping"] = "overlapping";
})(WalletAddressStatus || (WalletAddressStatus = {}));
import axios from "axios";
export var ContentType;
(function (ContentType) {
    ContentType["Json"] = "application/json";
    ContentType["FormData"] = "multipart/form-data";
    ContentType["UrlEncoded"] = "application/x-www-form-urlencoded";
    ContentType["Text"] = "text/plain";
})(ContentType || (ContentType = {}));
export class HttpClient {
    constructor(_a = {}) {
        var { securityWorker, secure, format } = _a, axiosConfig = __rest(_a, ["securityWorker", "secure", "format"]);
        this.securityData = null;
        this.setSecurityData = (data) => {
            this.securityData = data;
        };
        this.request = (_b) => __awaiter(this, void 0, void 0, function* () {
            var { secure, path, type, query, format, body } = _b, params = __rest(_b, ["secure", "path", "type", "query", "format", "body"]);
            const secureParams = ((typeof secure === "boolean" ? secure : this.secure) &&
                this.securityWorker &&
                (yield this.securityWorker(this.securityData))) ||
                {};
            const requestParams = this.mergeRequestParams(params, secureParams);
            const responseFormat = format || this.format || undefined;
            if (type === ContentType.FormData && body && body !== null && typeof body === "object") {
                body = this.createFormData(body);
            }
            if (type === ContentType.Text && body && body !== null && typeof body !== "string") {
                body = JSON.stringify(body);
            }
            return this.instance.request(Object.assign(Object.assign({}, requestParams), { headers: Object.assign(Object.assign({}, (requestParams.headers || {})), (type && type !== ContentType.FormData ? { "Content-Type": type } : {})), params: query, responseType: responseFormat, data: body, url: path }));
        });
        this.instance = axios.create(Object.assign(Object.assign({}, axiosConfig), { baseURL: axiosConfig.baseURL || "" }));
        this.secure = secure;
        this.format = format;
        this.securityWorker = securityWorker;
    }
    mergeRequestParams(params1, params2) {
        const method = params1.method || (params2 && params2.method);
        return Object.assign(Object.assign(Object.assign(Object.assign({}, this.instance.defaults), params1), (params2 || {})), { headers: Object.assign(Object.assign(Object.assign({}, ((method && this.instance.defaults.headers[method.toLowerCase()]) || {})), (params1.headers || {})), ((params2 && params2.headers) || {})) });
    }
    stringifyFormItem(formItem) {
        if (typeof formItem === "object" && formItem !== null) {
            return JSON.stringify(formItem);
        }
        else {
            return `${formItem}`;
        }
    }
    createFormData(input) {
        return Object.keys(input || {}).reduce((formData, key) => {
            const property = input[key];
            const propertyContent = property instanceof Array ? property : [property];
            for (const formItem of propertyContent) {
                const isFileType = formItem instanceof Blob || formItem instanceof File;
                formData.append(key, isFileType ? formItem : this.stringifyFormItem(formItem));
            }
            return formData;
        }, new FormData());
    }
}
/**
 * @title Plutus Certification API
 * @version 1.0
 *
 * This is an API for the Plutus Certification Service
 */
export class Api extends HttpClient {
    constructor() {
        super(...arguments);
        this.adaUsdPrice = {
            /**
             * @description Get the current ADA/USD price
             *
             * @name AdaUsdPriceList
             * @request GET:/ada-usd-price
             */
            adaUsdPriceList: (params = {}) => this.request(Object.assign({ path: `/ada-usd-price`, method: "GET", format: "json" }, params)),
        };
        this.auditor = {
            /**
             * @description Fetches the auditor report L0 | L2
             *
             * @name ReportsCreate
             * @request POST:/auditor/reports
             */
            reportsCreate: (body, query, params = {}) => this.request(Object.assign({ path: `/auditor/reports`, method: "POST", query: query, body: body, type: ContentType.Json, format: "json" }, params)),
        };
        this.github = {
            /**
             * @description Generate a github access token
             *
             * @name AccessTokenCreate
             * @request POST:/github/access-token/{code}
             */
            accessTokenCreate: (code, params = {}) => this.request(Object.assign({ path: `/github/access-token/${code}`, method: "POST", format: "json" }, params)),
            /**
             * @description Get the application client id
             *
             * @name ClientIdList
             * @request GET:/github/client-id
             */
            clientIdList: (params = {}) => this.request(Object.assign({ path: `/github/client-id`, method: "GET", format: "json" }, params)),
        };
        this.login = {
            /**
             * @description Get a jwt token based on the provided credentials
             *
             * @name LoginCreate
             * @request POST:/login
             */
            loginCreate: (body, params = {}) => this.request(Object.assign({ path: `/login`, method: "POST", body: body, type: ContentType.Json, format: "json" }, params)),
        };
        this.metrics = {
            /**
             * @description Get the auditor report metrics
             *
             * @name AuditorReportsCreate
             * @request POST:/metrics/auditor-reports
             */
            auditorReportsCreate: (body, params = {}) => this.request(Object.assign({ path: `/metrics/auditor-reports`, method: "POST", body: body, type: ContentType.Json, format: "json" }, params)),
            /**
             * @description Get the run times
             *
             * @name RunTimesCreate
             * @request POST:/metrics/run-times
             */
            runTimesCreate: (body, params = {}) => this.request(Object.assign({ path: `/metrics/run-times`, method: "POST", body: body, type: ContentType.Json, format: "json" }, params)),
            /**
             * @description Get all subscriptions expiring in a given interval
             *
             * @name SubscriptionsEndingInIntervalCreate
             * @request POST:/metrics/subscriptions/ending/in-interval
             */
            subscriptionsEndingInIntervalCreate: (body, params = {}) => this.request(Object.assign({ path: `/metrics/subscriptions/ending/in-interval`, method: "POST", body: body, type: ContentType.Json, format: "json" }, params)),
            /**
             * @description Get all subscriptions created in a given interval
             *
             * @name SubscriptionsStartedInIntervalCreate
             * @request POST:/metrics/subscriptions/started/in-interval
             */
            subscriptionsStartedInIntervalCreate: (body, params = {}) => this.request(Object.assign({ path: `/metrics/subscriptions/started/in-interval`, method: "POST", body: body, type: ContentType.Json, format: "json" }, params)),
        };
        this.profile = {
            /**
             * @description Get the current profile information
             *
             * @name CurrentList
             * @request GET:/profile/current
             */
            currentList: (params = {}) => this.request(Object.assign({ path: `/profile/current`, method: "GET", format: "json" }, params)),
            /**
             * @description Update the current profile information
             *
             * @name CurrentUpdate
             * @request PUT:/profile/current
             */
            currentUpdate: (body, params = {}) => this.request(Object.assign({ path: `/profile/current`, method: "PUT", body: body, type: ContentType.Json, format: "json" }, params)),
            /**
             * @description Get the current balance of the profile
             *
             * @name CurrentBalanceList
             * @request GET:/profile/current/balance
             */
            currentBalanceList: (params = {}) => this.request(Object.assign({ path: `/profile/current/balance`, method: "GET", format: "json" }, params)),
            /**
             * @description Get the current profile subscriptions. Expiration isn't checked, so it's possible to get expired subscriptions
             *
             * @name CurrentSubscriptionsList
             * @request GET:/profile/current/subscriptions
             */
            currentSubscriptionsList: (query, params = {}) => this.request(Object.assign({ path: `/profile/current/subscriptions`, method: "GET", query: query, format: "json" }, params)),
            /**
             * @description Get the active features of the current profile
             *
             * @name CurrentSubscriptionsActiveFeaturesList
             * @request GET:/profile/current/subscriptions/active-features
             */
            currentSubscriptionsActiveFeaturesList: (params = {}) => this.request(Object.assign({ path: `/profile/current/subscriptions/active-features`, method: "GET", format: "json" }, params)),
            /**
             * @description Cancel the current profile pending subscriptions
             *
             * @name CurrentSubscriptionsPendingDelete
             * @request DELETE:/profile/current/subscriptions/pending
             */
            currentSubscriptionsPendingDelete: (params = {}) => this.request(Object.assign({ path: `/profile/current/subscriptions/pending`, method: "DELETE", format: "json" }, params)),
            /**
             * @description Create a new profile subscription
             *
             * @name CurrentSubscriptionsCreate
             * @request POST:/profile/current/subscriptions/{tier}
             */
            currentSubscriptionsCreate: (tier, params = {}) => this.request(Object.assign({ path: `/profile/current/subscriptions/${tier}`, method: "POST", format: "json" }, params)),
            /**
             * @description Get the wallet address of the current profile
             *
             * @name CurrentWalletAddressList
             * @request GET:/profile/current/wallet-address
             */
            currentWalletAddressList: (params = {}) => this.request(Object.assign({ path: `/profile/current/wallet-address`, method: "GET", format: "json" }, params)),
            /**
             * @description Get the profile information
             *
             * @name ProfileDetail
             * @request GET:/profile/{id}
             */
            profileDetail: (id, params = {}) => this.request(Object.assign({ path: `/profile/${id}`, method: "GET", format: "json" }, params)),
            /**
             * @description Update the profile information
             *
             * @name ProfileUpdate
             * @request PUT:/profile/{id}
             */
            profileUpdate: (id, body, params = {}) => this.request(Object.assign({ path: `/profile/${id}`, method: "PUT", body: body, type: ContentType.Json, format: "json" }, params)),
            /**
             * @description Get the balance of a given profile
             *
             * @name BalanceDetail
             * @request GET:/profile/{id}/balance
             */
            balanceDetail: (id, params = {}) => this.request(Object.assign({ path: `/profile/${id}/balance`, method: "GET", format: "json" }, params)),
            /**
             * @description Get the roles of a given profile
             *
             * @name RolesDetail
             * @request GET:/profile/{id}/roles
             */
            rolesDetail: (id, params = {}) => this.request(Object.assign({ path: `/profile/${id}/roles`, method: "GET", format: "json" }, params)),
            /**
             * @description Update the roles of a given profile
             *
             * @name RolesUpdate
             * @request PUT:/profile/{id}/roles
             */
            rolesUpdate: (id, body, params = {}) => this.request(Object.assign({ path: `/profile/${id}/roles`, method: "PUT", body: body, type: ContentType.Json }, params)),
            /**
             * @description Create a new testing run attached to a different profile
             *
             * @name PostProfile
             * @request POST:/profile/{id}/run
             */
            postProfile: (id, body, params = {}) => this.request(Object.assign({ path: `/profile/${id}/run`, method: "POST", body: body, type: ContentType.Json, format: "json" }, params)),
            /**
             * @description Query through multiple profile runs
             *
             * @name RunsDetail
             * @request GET:/profile/{id}/runs
             */
            runsDetail: (id, query, params = {}) => this.request(Object.assign({ path: `/profile/${id}/runs`, method: "GET", query: query, format: "json" }, params)),
            /**
             * @description Get the profile subscriptions. Expiration isn't checked, so it's possible to get expired subscriptions
             *
             * @name SubscriptionsDetail
             * @request GET:/profile/{id}/subscriptions
             */
            subscriptionsDetail: (id, query, params = {}) => this.request(Object.assign({ path: `/profile/${id}/subscriptions`, method: "GET", query: query, format: "json" }, params)),
            /**
             * @description Get the active features of a given profile
             *
             * @name SubscriptionsActiveFeaturesDetail
             * @request GET:/profile/{id}/subscriptions/active-features
             */
            subscriptionsActiveFeaturesDetail: (id, params = {}) => this.request(Object.assign({ path: `/profile/${id}/subscriptions/active-features`, method: "GET", format: "json" }, params)),
            /**
             * @description Cancel a given profile pending subscriptions
             *
             * @name SubscriptionsPendingDelete
             * @request DELETE:/profile/{id}/subscriptions/pending
             */
            subscriptionsPendingDelete: (id, params = {}) => this.request(Object.assign({ path: `/profile/${id}/subscriptions/pending`, method: "DELETE", format: "json" }, params)),
            /**
             * @description Get the wallet address of a given profile
             *
             * @name WalletAddressDetail
             * @request GET:/profile/{id}/wallet-address
             */
            walletAddressDetail: (id, params = {}) => this.request(Object.assign({ path: `/profile/${id}/wallet-address`, method: "GET", format: "json" }, params)),
        };
        this.profiles = {
            /**
             * @description Getting all profiles with their maximum role and their dapp if any
             *
             * @name ProfilesList
             * @request GET:/profiles
             */
            profilesList: (params = {}) => this.request(Object.assign({ path: `/profiles`, method: "GET", format: "json" }, params)),
        };
        this.repo = {
            /**
             * @description Get the github repo information
             *
             * @name RepoDetail
             * @request GET:/repo/{owner}/{repo}
             */
            repoDetail: (owner, repo, params = {}) => this.request(Object.assign({ path: `/repo/${owner}/${repo}`, method: "GET", format: "json" }, params)),
        };
        this.roles = {
            /**
             * @description Get all profile ids by role
             *
             * @name ProfilesSummaryDetail
             * @request GET:/roles/{role}/profiles/summary
             */
            profilesSummaryDetail: (role, params = {}) => this.request(Object.assign({ path: `/roles/${role}/profiles/summary`, method: "GET", format: "json" }, params)),
        };
        this.run = {
            /**
             * @description Query through multiple runs belonging to the current profile
             *
             * @name GetRun
             * @request GET:/run
             */
            getRun: (query, params = {}) => this.request(Object.assign({ path: `/run`, method: "GET", query: query, format: "json" }, params)),
            /**
             * @description Create a new testing run attached to the current profile
             *
             * @name PostRun
             * @request POST:/run
             */
            postRun: (body, params = {}) => this.request(Object.assign({ path: `/run`, method: "POST", body: body, type: ContentType.Json, format: "json" }, params)),
            /**
             * @description Abort a run and deletes the history entry if query param is provided
             *
             * @name DeleteRun
             * @request DELETE:/run/{id}
             */
            deleteRun: (id, query, params = {}) => this.request(Object.assign({ path: `/run/${id}`, method: "DELETE", query: query }, params)),
            /**
             * @description Get the status of a run
             *
             * @name GetRun2
             * @request GET:/run/{id}
             * @originalName getRun
             * @duplicate
             */
            getRun2: (id, params = {}) => this.request(Object.assign({ path: `/run/${id}`, method: "GET", format: "json" }, params)),
            /**
             * @description Store the L1 Report into IPFS and broadcasts the Certificate onchain
             *
             * @name CertificateCreate
             * @request POST:/run/{id}/certificate
             */
            certificateCreate: (id, body, query, params = {}) => this.request(Object.assign({ path: `/run/${id}/certificate`, method: "POST", query: query, body: body, type: ContentType.Json, format: "json" }, params)),
            /**
             * @description Get the details of a run
             *
             * @name DetailsDetail
             * @request GET:/run/{id}/details
             */
            detailsDetail: (id, params = {}) => this.request(Object.assign({ path: `/run/${id}/details`, method: "GET", format: "json" }, params)),
            /**
             * @description Get the logs of a run
             *
             * @name LogsDetail
             * @request GET:/run/{id}/logs
             */
            logsDetail: (id, query, params = {}) => this.request(Object.assign({ path: `/run/${id}/logs`, method: "GET", query: query, format: "json" }, params)),
        };
        this.serverTimestamp = {
            /**
             * @description Get the current server timestamp
             *
             * @name ServerTimestampList
             * @request GET:/server-timestamp
             */
            serverTimestampList: (params = {}) => this.request(Object.assign({ path: `/server-timestamp`, method: "GET", format: "json" }, params)),
        };
        this.tiers = {
            /**
             * @description Get the available tiers
             *
             * @name TiersList
             * @request GET:/tiers
             */
            tiersList: (params = {}) => this.request(Object.assign({ path: `/tiers`, method: "GET", format: "json" }, params)),
        };
        this.version = {
            /**
             * @description Get the api version
             *
             * @name VersionList
             * @request GET:/version
             */
            versionList: (params = {}) => this.request(Object.assign({ path: `/version`, method: "GET", format: "json" }, params)),
            /**
             * @description Get the api version (Response Headers only)
             *
             * @name HeadVersion
             * @request HEAD:/version
             */
            headVersion: (params = {}) => this.request(Object.assign({ path: `/version`, method: "HEAD" }, params)),
        };
        this.walletAddress = {
            /**
             * @description Get the wallet address the backend operates with
             *
             * @name WalletAddressList
             * @request GET:/wallet-address
             */
            walletAddressList: (params = {}) => this.request(Object.assign({ path: `/wallet-address`, method: "GET", format: "json" }, params)),
        };
    }
}
