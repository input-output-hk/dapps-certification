import React, { useState } from "react";

const UnitTestFailureCard: React.FC<{
    resultObj: any;
    }> = ({ resultObj }) => {
  
    const [isOpen, setIsOpen] = useState(true);
    const toggleAccordion = () => {
      setIsOpen(!isOpen);
    };

    return (
        <>
            <div className="result-card failure">
                <label>Task: UnitTest</label>
                <span
                    className={`error-title accordion-title ${isOpen ? 'open' : ''}`}
                    onClick={(_) => toggleAccordion()}
                >
                    <i>{resultObj.resultShortDescription}</i>
                    <i className={`arrow ${isOpen ? 'up' : 'down'}`}></i>
                </span>

                <div className={`accordion-content ${isOpen ? '' : 'hidden'}`}>
                    <span className="failure-output">{resultObj.resultDescription}</span>
                </div>
            </div>
        </>
    );
};

export default UnitTestFailureCard;