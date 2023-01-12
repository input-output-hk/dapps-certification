export interface IUserProfile {
  "authors"?: string,
  "contacts"?: string,
  "dapp": {
    "name": string,
    "owner": string,
    "repo": string,
    "version": string
  } | null,
  "linkedin"?: string,
  "twitter"?: string,
  "vendor"?: string,
  "website"?: string
}