import React, { useState, useEffect } from "react";
import TableComponent from "components/Table/Table";
import moment from "moment";
import Button from "components/Button/Button";
import resultData from './mockData'
import "./TestHistory.scss";

const TestHistory = () => {
  const [data, setData] = useState<any>([]);

  useEffect(() => {
    setData(resultData.data);
  }, []);

  const columns = [
    {
      id: "campaignId",
      Header: "Campaign",
      accessor: "campaignId", // accessor is the "key" in the data
      disableSortBy: true,
    },
    {
      id: "repoUrl",
      Header: "Repo URL",
      accessor: "repoUrl",
      disableSortBy: true,
    },
    {
      id: "dateTime",
      Header: "Date Time",
      accessor: "dateTime",
      disableSortBy: true,
      Cell: (props: any) => (
        <span>
          {moment(props.row.original.dateTime).format("YYYY-MM-DD HH:mm:ss")}
        </span>
      ),
    },
    {
      id: "status",
      Header: "Status",
      accessor: "status", // accessor is the "key" in the data
      disableSortBy: true,
      Cell: (props: any) => {
        if (props.row.original.status === "succeeded") {
          return <span>OK</span>
        } else if (props.row.original.status === "failed") {
          return <span>FAILED</span>
        } else if (props.row.original.status === "queued") {
          return (<>
            <span>In Progress</span>
            <img className="icon" src="images/refresh.svg" alt="refresh" />
          </>)
        }
      }
    },
    {
      id: "viewReport",
      Header: "",
      disableSortBy: true,
      Cell: (props: any) => {
        if (props.row.original.viewReport && props.row.original.status !== "queued") {
          return (
            <Button
              type="submit"
              buttonLabel={"View Report"}
              onClick={() => {}}
            />
          );
        }
      },
    },
    {
      id: "viewCertificate",
      Header: "",
      disableSortBy: true,
      Cell: (props: any) => {
        if (props.row.original.viewCertificate && props.row.original.status === "succeeded") {
          return (
            <Button
              type="submit"
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
      {data.length ? <TableComponent dataSet={data} config={columns} /> : ""}
    </div>
  );
}

export default TestHistory;