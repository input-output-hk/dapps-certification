import React, { useState } from "react";
import { processTablesDataForChart, VisualizableDataKeys } from "../Certification.helper";
import PieChart from "components/charts/PieChart/PieChart";
import { formatToTitleCase } from "utils/utils";

const SuccessCard: React.FC<{
  resultObj: any;
  taskName: string;
  unitTest?: null | string;
}> = ({ resultObj, taskName, unitTest = null }) => {

  const [isOpen, setIsOpen] = useState(true);
  const toggleAccordion = () => {
    setIsOpen(!isOpen);
  };

  const renderCharts = (arg?: any) => {
    const dataSet = arg || resultObj;
    if (!dataSet.hasOwnProperty('tables')) {
      return null
    }
    return Object.keys(dataSet.tables).map((key, idx) => {
        if (VisualizableDataKeys.indexOf(key) !== -1) {
            return (
            <div className="chart-wrapper" key={idx}>
                <PieChart
                    title={key}
                    is3D={true}
                    data={processTablesDataForChart(dataSet, key)}
                />
            </div>)
        } else {
            return null
        }
    })
  }

  return (
    <>
      <div className="result-card success">
        <label>Task: {taskName}</label>
        {unitTest ? 
          unitTest === '_certRes_unitTestResults' ? <>
            <span><i>OK, passed {resultObj.length}/{resultObj.length} tests</i></span>
          </> 
          : unitTest === '_certRes_DLTests' ? <>
            <span className="accordion-title" onClick={(_) => toggleAccordion()}>
              <i>OK, passed {resultObj.length}/{resultObj.length} tests</i>
              <i className={`arrow ${isOpen ? "up" : "down"}`}></i>
            </span>
            <div className={`accordion-content ${isOpen ? "" : "hidden"}`}>
              {resultObj.map((item: any, index: number) => {
                return (
                <div key={index}>
                  <label>Test: {formatToTitleCase(item[0])}</label>
                  <span>
                    <i>OK, passed {item[1].numTests} tests</i>
                    {item[1].numDiscarded >= 0 ? <i>; {item[1].numDiscarded} discarded</i> : null}
                  </span>
                  <div>
                    <section className="chart-container">
                      {renderCharts(item[1])}
                    </section>
                  </div>
                </div>)
              })}
            </div>
          </> : null
        : <>
          <span className="accordion-title" onClick={(_) => toggleAccordion()}>
            <i>OK, passed {resultObj.numTests} tests</i>
            {resultObj.numDiscarded >= 0 ? <i>; {resultObj.numDiscarded} discarded</i> : null}
            <i className={`arrow ${isOpen ? "up" : "down"}`}></i>
          </span>

          <div className={`accordion-content ${isOpen ? "" : "hidden"}`}>
            <section className="chart-container">
              {renderCharts()}
            </section>
          </div>
        </>}
      </div>
    </>
  );
};

export default SuccessCard;
