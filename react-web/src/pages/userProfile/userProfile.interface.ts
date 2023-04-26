export interface IUserProfile {
  "authors"?: string,
  "contacts"?: string,
  "dapp": {
    "name": string,
    "owner": string,
    "repo": string,
    "version": string,
    "githubToken"?: string | null
  } | null,
  "linkedin"?: string,
  "twitter"?: string,
  "vendor"?: string,
  "website"?: string
}