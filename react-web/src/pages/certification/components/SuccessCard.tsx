import React, { useState } from "react";
import { processTablesDataForChart, VisualizableDataKeys } from "../Certification.helper";
import PieChart from "components/charts/PieChart/PieChart";

const SuccessCard: React.FC<{
  resultObj: any;
  certTask: string;
}> = ({ resultObj, certTask }) => {

  const [isOpen, setIsOpen] = useState(true);
  const toggleAccordion = () => {
    setIsOpen(!isOpen);
  };

  const renderCharts = () => {
    return Object.keys(resultObj.tables).map((key, idx) => {
        if (VisualizableDataKeys.indexOf(key) !== -1) {
            return (
            <div className="chart-wrapper" key={idx}>
                <PieChart
                    title={key}
                    is3D={true}
                    data={processTablesDataForChart(resultObj, key)}
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
        <label>Task: {certTask}</label>
        <span className="accordion-title" onClick={(_) => toggleAccordion()}>
          <i>OK, passed {resultObj.numTests} tests</i>
          {resultObj.numDiscarded > 0 ? <i>; {resultObj.numDiscarded} discarded</i> : null}
          <i className={`arrow ${isOpen ? "up" : "down"}`}></i>
        </span>

        <div className={`accordion-content ${isOpen ? "" : "hidden"}`}>
          <section className="chart-container">
            {renderCharts()}
          </section>
        </div>
      </div>
    </>
  );
};

export default SuccessCard;
