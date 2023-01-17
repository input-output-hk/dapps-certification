import React from "react";
import FailedStatus from "./FailedStatus";
import { CertificationTasks } from "../Certification.helper";
import SuccessCard from "./SuccessCard";
import UnitTestFailureCard from "./UnitTestFailureCard";

const ResultContainer: React.FC<{ 
  result: { [x: string]: any },
  unitTestSuccess?: boolean 
}> = ({
  result,
  unitTestSuccess = true
}) => {

  const filterKeys = (type: string) => {
    return CertificationTasks.filter(item => item.type === type).map(item => item.key);
  }

  const resultKeys = filterKeys('object');
  const unitTestKeys = filterKeys('array');

  const getCertificationTaskName = (key: string) => {
    const obj: {key: string, label: string} | any = CertificationTasks.find(item => item.key === key)
    return obj.label;
  };

  return (
    <div id="resultContainer">
      <>
        {/* Unit Test results */}
        {unitTestSuccess === false && unitTestKeys.length
          ? unitTestKeys
              .filter(key => result[key].length)
              .map(unitTestKey => {
                // output of each entry in unit test
                if (unitTestKey === '_certRes_unitTestResults') {
                  return result[unitTestKey].map((item: any, index: number) => {
                    return typeof item !== 'string' && item.resultOutcome.tag === 'Failure' ? (
                      <UnitTestFailureCard
                        resultObj={item}
                        key={index}
                      />)
                    : <></>
                  })
                } else if (unitTestKey === '_certRes_DLTests') {
                  return result[unitTestKey].map((item: Array<any>, index: number) => {
                    return item[1].tag === 'Failure' ? (
                      <FailedStatus
                        taskName='DLUnitTest'
                        reason={item[1].reason}
                        output={item[1].output}
                        failingTestCase={item[1].failingTestCase}
                        key={index}
                      />)
                    : <></>
                  })
                } else {
                  return <></>
                }
              })
          : null}

        {unitTestSuccess ? (
          <>
            {/* Failure container */}
            {resultKeys.length
              ? resultKeys
                  .filter((key) => result[key] && result[key].tag === "Failure")
                  .map((key, index) => {
                    let certTaskName = getCertificationTaskName(key);

                    return <FailedStatus
                      taskName={certTaskName}
                      reason={result[key].reason}
                      output={result[key].output}
                      failingTestCase={result[key].failingTestCase}
                      key={index}
                    />
                  })
              : null}

            {/* UnitTest success cards */}
            {unitTestKeys
              .filter(key => result[key].length)
              .map((unitTestKey, index) => {
                return <SuccessCard 
                  resultObj={result[unitTestKey]}
                  taskName={getCertificationTaskName(unitTestKey)}
                  key={index}
                  unitTest={unitTestKey}
                />
              })
            }

            {/* Success container */}
            { resultKeys
                  .filter((key) => result[key] && result[key].tag === "Success")
                  .map((key, index) => {
                    let certTaskName = getCertificationTaskName(key);
                    return <SuccessCard 
                      resultObj={result[key]}
                      taskName={certTaskName}
                      key={index}
                    />
                  })
            }
          </>
        ) : null}
      </>
    </div>
  );
};

export default ResultContainer;
