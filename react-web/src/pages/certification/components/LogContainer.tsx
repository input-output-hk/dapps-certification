import React from "react";
import FailedStatus from "./FailedStatus";
import { CertificationTasks } from "../Certification.helper";
import SuccessCard from "./SuccessCard";

const LogContainer: React.FC<{ result: { [x: string]: any } }> = ({
  result,
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
    <div id="logContainer">
      <>
        {/* Unit Test results */}
        {unitTestKeys.length
          ? unitTestKeys
              .filter(key => result[key].length)
              .forEach(key => {
                // output of each entry in unit test
              })
          : null}

        {/* Failure container */}
        {resultKeys.length
          ? resultKeys
              .filter((key) => result[key] && result[key].tag === "Failure")
              .map((key, index) => {
                let certTaskName = getCertificationTaskName(key);

                return <FailedStatus
                  certTask={certTaskName}
                  reason={result[key].reason}
                  output={result[key].output}
                  failingTestCase={result[key].failingTestCase}
                  key={index}
                />
              })
          : null}

        {/* Success container */}
        { resultKeys
              .filter((key) => result[key] && result[key].tag === "Success")
              .map((key, index) => {
                let certTaskName = getCertificationTaskName(key);
                return <SuccessCard 
                  resultObj={result[key]}
                  certTask={certTaskName}
                  key={index}
                />
              })
        }
      </>
    </div>
  );
};

export default LogContainer;
