import React, { useState, useEffect } from "react";
import TableComponent from "components/Table/Table";
import dayjs from "dayjs";
import Button from "components/Button/Button";
import "./TestHistory.scss";
import { fetchData } from "api/api";

interface ICampaign {
  "commitDate": string,
  "commitHash": string,
  "created": string,
  "finishedAt": string,
  "syncedAt": string,
  "repoUrl": string,
  "runStatus": "queued" | "failed" | "succeeded" | "certified",
  "runId": string
}

const TestHistory = () => {
  const [data, setData] = useState<Array<ICampaign>>([]);

  useEffect(() => {
    fetchTableData();
  }, []);

  const columns = [
    {
      Header: "Repo URL",
      accessor: "repoUrl",
    },
    {
      Header: "Commit Hash",
      accessor: "commitHash",
      disableSortBy: true,
      Cell: (props: any) => (
        <span className="trim-cell-text" title={props.row.original.commitHash}>{props.row.original.commitHash}</span>
      )
    },
    {
      Header: "Commit Date",
      accessor: "commitDate",
      Cell: (props: any) => (
        <span>
          {dayjs(props.row.original.commitDate).format("YYYY-MM-DD HH:mm:ss")}
        </span>
      ),
    },
    {
      Header: "Synced At",
      accessor: "syncedAt",
      columnVisible: false,
      Cell: (props: any) => (
        <span>
          {dayjs(props.row.original.syncedAt).format("YYYY-MM-DD HH:mm:ss")}
        </span>
      ),
    },
    {
      Header: "Run Status",
      accessor: "runStatus",
      Cell: (props: any) => {
        if (props.row.original.runStatus === "certified") {
          return <span style={{color: 'green'}}>Certified</span>
        } else if (props.row.original.runStatus === "succeeded") {
          return <span>OK</span>
        } else if (props.row.original.runStatus === "failed") {
          return <span style={{color: 'red'}}>FAILED</span>
        } else if (props.row.original.runStatus === "queued") {
          return (<>
            <span>Running</span>
            {/* TBD */}
            {/* <img className="icon" src="images/refresh.svg" alt="refresh" title="Sync latest Status" /> */}
          </>)
        }
      }
    },
    {
      Header: "",
      disableSortBy: true,
      accessor: "viewReport",
      Cell: (props: any) => {
        if (props.row.original.runStatus !== "queued") {
          return (
            <Button
              size="small"
              type="submit"
              disabled={true} //To be removed
              buttonLabel={"View Report"}
              onClick={() => {}}
            />
          );
        }
      },
    },
    {
      Header: "",
      disableSortBy: true,
      accessor: "viewCertificate",
      Cell: (props: any) => {
        if (props.row.original.runStatus === "certified") {
          return (
            <Button
              size="small"
              type="submit"
              disabled={true} //To be removed
              buttonLabel={"View Certificate"}
              onClick={() => {}}
            />
          );
        }
      },
    }
    // TBD
    // {
    //   Header: "",
    //   disableSortBy: true,
    //   accessor: "delete",
    //   Cell: (props: any) => {
    //     return (<>
    //       <button
    //         className="trash-icon-btn"
    //         onClick={() => {
    //           onDelete(props.row.original.runId);
    //         }}
    //       >
    //         <img className="icon-trash" src="images/trash.svg" alt="delete" title="Delete Campaign" />
    //       </button>
    //     </>);
    //   },
    // }
  ];

  const fetchTableData = async () => {
    const result = await fetchData.get("/run")
    /** For mock */
    // const result = await fetchData.get("static/data/history.json");
    setData(result.data.data);
  };

  // TBD
  // const onDelete = (id: any) => {};
  
  return (
    <div id="testHistory">
      <TableComponent dataSet={data} config={columns} showColViz={true} />
    </div>
  );
}

export default TestHistory;