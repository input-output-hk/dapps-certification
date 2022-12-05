import React, { useState, useEffect } from "react";
import TableComponent from "components/Table/Table";
// import { dataArray } from "./dummyData";
import moment from "moment";
import Button from "components/Button/Button";
import "./TestHistory.scss";

const TestHistory = () => {
  const [data, setData] = useState<any>([]);

  useEffect(() => {
    // setData(dataArray);
    setData([])
  }, []);

  const columns = [
    {
      Header: "Campaign #n",
      accessor: "campaignId", // accessor is the "key" in the data
    },
    {
      Header: "Date Time",
      accessor: "dateTime",
      Cell: (props: any) => (
        <span>
          {moment(props.row.original.dateTime).format("YYYY-MM-DD HH:mm:ss")}
        </span>
      ),
    },
    {
      Header: "Metric 1",
      accessor: "metric1",
    },
    {
      Header: "Metric 2",
      accessor: "metric2",
    },
    {
      Header: "View Report",
      Cell: (props: any) => {
        if (props.row.original.viewReport) {
          return (
            <Button
              type="submit"
              className="btn btn-test-history"
              buttonLabel={"View Report"}
              onClick={() => {}}
            />
          );
        }
      },
    },
    {
      Header: "View Certificate",
      Cell: (props: any) => {
        if (props.row.original.viewCertificate) {
          return (
            <Button
              type="submit"
              className="btn btn-test-history"
              buttonLabel={"View Certificate"}
              onClick={() => {}}
            />
          );
        }
      },
    },
  ];
  return (
    <div id="testHistory">
      {data.length ? <TableComponent data={data} config={columns} /> : ""}
    </div>
  );
}

export default TestHistory;