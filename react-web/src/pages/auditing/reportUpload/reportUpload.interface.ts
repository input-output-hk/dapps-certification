export interface IAuditReport {
    name: string
}

export interface IScriptObject {
    smartContractInfo: {
      era?: string;
      compiler?: string;
      compilerVersion?: string;
      optimizer?: string;
      optimizerVersion?: string;
      progLang?: string;
      repository?: string;
    },
    scriptHash: string;
    contactAddress: string;
  }

export interface OffChainMetadataSchema {
    subject: any;
    schemaVersion: number;
    certificationLevel: number;
    certificateIssuer: {
        name: any;
        logo: any;
        social: {
            contact: any;
            link: any;
            twitter: any;
            github: any;
            website: any;
        };
    };
    report: {
        reportURLs: any;
    };
    summary: any;
    disclaimer: any;
    scripts: IScriptObject[];
}