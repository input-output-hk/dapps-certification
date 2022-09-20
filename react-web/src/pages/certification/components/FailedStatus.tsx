import React from "react";

const FailedStatus: React.FC<{
  tag: string;
  reason: string;
  output: string;
  failingTestCase: any[];
}> = ({ tag, reason, output, failingTestCase }) => {
  return (
    <div className="failure">
      <div
        className="error-title"
        onClick={(_) => console.log("handle accordion")}
      >
        <i>{reason}</i>
      </div>

      <div className="accordion-content">
        {failingTestCase.length
          ? failingTestCase.map((val) => <span>{val}</span>)
          : null}

        <span>{output}</span>
      </div>
    </div>
  );
};

export default FailedStatus;
