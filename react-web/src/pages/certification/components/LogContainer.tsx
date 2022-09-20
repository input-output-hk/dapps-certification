import React from "react";
import FailedStatus from "./FailedStatus";

const LogContainer: React.FC<{ result: { [x: string]: any } }> = ({
  result,
}) => {
  const nonEmptyKeys = Object.keys(result).filter(
    (key) => result[key] && key.endsWith("Result")
  );

  return (
    <div id="logContainer">
      {/* Failure container */}
      {nonEmptyKeys.length
        ? nonEmptyKeys
            .filter((key) => result[key].tag === "Failure")
            .map((key, index) => (
              <FailedStatus
                tag={result[key].tag}
                reason={result[key].reason}
                output={result[key].output}
                failingTestCase={result[key].failingTestCase}
                key={index}
              />
            ))
        : null}

      {/* Success container */}
    </div>
  );
};

export default LogContainer;
