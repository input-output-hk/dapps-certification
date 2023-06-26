import React, { useState } from "react";

const FailedStatus: React.FC<{
  taskName: string;
  reason: string;
  output: string;
  failingTestCase: any[];
}> = ({ taskName, reason, output, failingTestCase }) => {
  const [isOpen, setIsOpen] = useState(true);
  
  const toggleAccordion = () => {
    setIsOpen(!isOpen)
  }

  return (
    <div className="result-card failure">
      <label>Task: {taskName}</label>
      <span
        className={`error-title accordion-title ${isOpen ? 'open' : ''}`}
        onClick={(_) => toggleAccordion()}
        data-testid="accordion-title"
      >
        <i>{reason}</i>
        <i className={`arrow ${isOpen ? 'up' : 'down'}`} data-testid="arrow"></i>
      </span>

      <div className={`accordion-content ${isOpen ? '' : 'hidden'}`}>
        <span>Failing TestCase(s):</span>
        {failingTestCase.length
          ? failingTestCase.map((val, index) => <span className="failing-testcase" key={index}>{index + 1}. {val}</span>)
          : null}

        <span className="failure-output" data-testid="failure-output">{output}</span>
      </div>
    </div>
  );
};

export default FailedStatus;
