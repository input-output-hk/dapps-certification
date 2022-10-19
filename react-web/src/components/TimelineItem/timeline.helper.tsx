import { CertificationTasks } from "pages/certification/Certification.helper";

export const setManyStatus = (
  index: number,
  config: any[],
  currentConfig: any,
  status: any,
  updatedState: string
) =>
  index < config.map((val: { status: any }) => val.status).indexOf(status)
    ? {
        ...currentConfig,
        state:
          currentConfig.state === "running" || currentConfig.state === "outline" // update only running status to updated status to avoid failure being overwritten
            ? updatedState
            : currentConfig.state,
      }
    : currentConfig;


export const indexOfExecutingProcess = (config: any[], state: string): number =>
  config.map((val: { state: any }) => val.state).indexOf(state) - 1;


export const processFinishedJson = (result: { [x: string]: any }): boolean => {
  const filterKeys = (type: string) => {
    return CertificationTasks.filter(item => item.type === type).map(item => item.key);
  }

  const unitTestKeys = filterKeys('array');

  let unitTestFailures = 0, filteredData: any = [];
  unitTestKeys.forEach(key => {
    const CertTaskRef = CertificationTasks.find(item => item.key === key);
    const filtered = result[key].filter((item: any) => {
      // item.tag === 'failure'
      if (key === '_certRes_DLTests' && item[1].tag === 'Failure') {
        return CertTaskRef
      } else if (key === '_certRes_unitTestResults' && item.resultOutcome.tag === 'Failure') {
        return CertTaskRef
      }
      return false;
    })
    filteredData.push(...filtered)
  })
  unitTestFailures += filteredData.length ? 1 : 0
  return unitTestFailures ? false : true
};
