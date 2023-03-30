import { BigNum } from "@emurgo/cardano-serialization-lib-browser";

export interface TierFeature {
    "id": string;
    "name": string;
}
export interface Subscription {
    "end_date": string;
    "features": Array<TierFeature>;
    "id": string;
    "name": "Developer" | "Auditor";
    "price": BigNum,
    "profile_id": string;
    "start_date": string;
    "status": "inactive" | "active" | "pending";
    "tier_id": string;
}

export interface Tier {
    "duration": number;
    "enabled": boolean;
    "features": Array<TierFeature>;
    "id": string;
    "tier_id"?: string;
    "name": "Developer" | "Auditor";
    "usd_price": number;
}