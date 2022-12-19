import React, { useState, useEffect } from "react";
import TableComponent from "components/Table/Table";
import moment from "moment";
import Button from "components/Button/Button";
import "./TestHistory.scss";
import { fetchData } from "api/api";

const TestHistory = () => {
  const [data, setData] = useState<any>([]);

  useEffect(() => {
    fetchTableData();
  }, []);

  const columns = [
    {
      Header: "Campaign",
      accessor: "campaignId", // accessor is the "key" in the data
      disableSortBy: true,
    },
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
      Header: "Certificate Created At",
      accessor: "certificateCreatedAt",
      columnVisible: false,
      Cell: (props: any) => (
        <span>
          {moment(props.row.original.certificateCreatedAt).format("YYYY-MM-DD HH:mm:ss")}
        </span>
      ),
    },
    {
      Header: "Commit Date",
      accessor: "commitDate",
      Cell: (props: any) => (
        <span>
          {moment(props.row.original.commitDate).format("YYYY-MM-DD HH:mm:ss")}
        </span>
      ),
    },
    {
      Header: "Synced At",
      accessor: "syncedAt",
      columnVisible: false,
      Cell: (props: any) => (
        <span>
          {moment(props.row.original.syncedAt).format("YYYY-MM-DD HH:mm:ss")}
        </span>
      ),
    },
    {
      Header: "Status",
      accessor: "status", // accessor is the "key" in the data
      Cell: (props: any) => {
        if (props.row.original.status === "succeeded") {
          return <span>OK</span>
        } else if (props.row.original.status === "failed") {
          return <span>FAILED</span>
        } else if (props.row.original.status === "queued") {
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
        if (props.row.original.viewReport && props.row.original.status !== "queued") {
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
        if (props.row.original.viewCertificate && props.row.original.status === "succeeded") {
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
    //           onDelete(props.row.original.campaignId);
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