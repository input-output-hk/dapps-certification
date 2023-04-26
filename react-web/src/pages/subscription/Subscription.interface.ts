export interface TierFeature {
    "id": "l1-run" | "l2-upload-report"
    "name": string;
}
export interface Subscription {
    "endDate": string;
    "features": Array<TierFeature>;
    "id": string;
    "name": string;
    "price": number,
    "adaUsdPrice": number,
    "profileId": string;
    "startDate": string;
    "status": SubscriptionStatus;
    "tierId": string;
}

export type SubscriptionStatus = "inactive" | "active" | "pending";

export interface Tier {
    "duration": number;
    "features": Array<TierFeature>;
    "id": string;
    "name": string;
    "type": "developer" | "auditor";
    "usdPrice": number;
    "description": string;
    "subtitle": string;
}
