export const CertificationTasks = [{
  label: 'UnitTests',
  key: '_certRes_unitTestResults',
  type: 'array',
  name: 'unit-tests'
}, {
  label: 'StandardProperty',
  key: '_certRes_standardPropertyResult',
  type: 'object',
  name: 'standard-property'
}, {
  label: 'DoubleSatisfaction',
  key: '_certRes_doubleSatisfactionResult',
  type: 'object',
  name: 'double-satisfaction'
}, {
  label: 'NoLockedFunds',
  key: '_certRes_noLockedFundsResult',
  type: 'object',
  name: 'no-locked-funds'
}, {
  label: 'NoLockedFundsLight',
  key: '_certRes_noLockedFundsLightResult',
  type: 'object',
  name: 'no-locked-funds-light'
}, {
  label: 'CrashTolerance',
  key: '_certRes_standardCrashToleranceResult',
  type: 'object',
  name: 'crash-tolerance'
}, {
  label: 'Whitelist',
  key: '_certRes_whitelistResult',
  type: 'object',
  name: 'white-list'
}, {
  label: 'DLTests',
  key: '_certRes_DLTests',
  type: 'array',
  name: 'dl-tests'
}]

export const VisualizableDataKeys = ['Actions', 'Actions rejected by precondition', 'Bad refund attempts', 'ChainEvent type']

export const filterCertificationTaskKeys = (type: string) => {
  return CertificationTasks.filter(item => item.type === type).map(item => item.key);
}

export const isAnyTaskFailure = (result: any) => {
  const resultKeys = filterCertificationTaskKeys('object')
  let flag = 0;
  if (Object.keys(result).length) {
    flag = resultKeys.filter((key) => result[key] && result[key].tag === "Failure").length
  }
  return flag ? true : false;
}

export const processTablesDataForChart = (resultObj: any, tableAttr: string) => {
  let totalCount = 0
  try {
    totalCount = parseInt(resultObj.output.split(tableAttr + ' (')[1].split(' in total):')[0])
  } catch(err) {
    // do nothing
  }

  const data = [[tableAttr, "Percentage"]]
  for (let key in resultObj.tables[tableAttr]) {
    data.push([key, resultObj.tables[tableAttr][key]])
  }

  return {
    data: data,
    totalCount: totalCount
  }
}

export const getPlannedCertificationTaskCount = (plannedTasks: any[]) => {
  return plannedTasks.filter(item => item.name && typeof item.name === 'string').length
}