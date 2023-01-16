import React, { useState, useEffect } from "react";
import TableComponent from "components/Table/Table";
import dayjs from "dayjs";
import Button from "components/Button/Button";
import "./TestHistory.scss";
import { fetchData } from "api/api";
import { processFinishedJson } from "components/TimelineItem/timeline.helper";
import { isAnyTaskFailure } from "pages/certification/Certification.helper";

interface ICampaign {
  commitDate: string;
  commitHash: string;
  created: string;
  finishedAt: string;
  syncedAt: string;
  repoUrl: string;
  runStatus: "queued" | "failed" | "succeeded" | "certified";
  runId: string;
}

const TestHistory = () => {
  const [data, setData] = useState<Array<ICampaign>>([]);
  const [skipPageReset, setSkipPageReset] = useState(false);
  const [certificationData, setCertificationData] = useState(null);
  const [currentSelectedRunId, setCurrentSelectedRunId] = useState("");

  useEffect(() => {
    fetchTableData();
  }, []);

  useEffect(() => {
    setSkipPageReset(false);
  }, [data]);

  const updateMyData = (rowIndex: any, columnID: any, value: any) => {
    setSkipPageReset(true); // turn on flag to not reset the page
    setData((old) =>
      old.map((row, index) => {
        if (index === rowIndex) {
          return {
            ...old[rowIndex],
            [columnID]: value,
          };
        }
        return row;
      })
    );
  };

  const RunStatusCell = ({
    value,
    row,
    column: { id },
    updateMyData, // This is a custom function that we supplied to our table instance
  }: any) => {
    const { index, original } = row;
    const triggerApi = async (e: any) => {
      const res = await fetchData.get("/run/" + original.runId);
      /** For mock */
      // const res = await fetchData.get("static/data/certifying.json")
      const status = res.data.status;
      const state = res.data.hasOwnProperty("state") ? res.data.state : "";
      let response: string = 'queued';
      // show failed if either states failed or unitTest/certTasks failed
      if (state === 'failed') {
        response = 'failed'
      } else {
        if (status === 'finished') {
          const isUnitTestSuccess = processFinishedJson(res.data.result);
          const isComplete = isUnitTestSuccess && !isAnyTaskFailure(res.data.result);
          response = isComplete ? 'succeeded' : 'failed';
        } else {
          // retain response='queued'
        }
      } 
      updateMyData(index, id, response);
    };
    if (value === "certified") {
      return <span style={{ color: "green" }}>Certified</span>;
    } else if (value === "succeeded") {
      return <span>OK</span>;
    } else if (value === "failed") {
      return <span style={{ color: "red" }}>FAILED</span>;
    } else if (value === "queued") {
      const id = original.runId; // get the run id
      return (
        <>
          <span>Running</span>
          <button onClick={(e) => triggerApi(id)} className="sync-btn">
            <img
              className="icon"
              src="images/refresh.svg"
              alt="refresh"
              title="Sync latest Status"
            />
          </button>
        </>
      );
    }
  };

  const viewReportOrCertificate = async (type: string, runId: string) => {
    if (currentSelectedRunId !== runId) {
      setCurrentSelectedRunId(runId);
      try {
        const response = await fetchData.get("/run/" + runId + "/certificate");
        /** For mock */
        // const response = await fetchData.get("static/data/certicate.json");
        setCertificationData(response.data);
        triggerNavigation(type, response.data);
      } catch (e) {
        console.log(e);
        // TBD => error handling
      }
    } else {
      triggerNavigation(type, certificationData);
    }
  };

  const triggerNavigation = (type: string, data: any) => {
    const { reportContentId, transactionId } = data;
    let url;
    if (type === "report") {
      url = `https://${reportContentId}.ipfs.w3s.link/`;
      // TBD => if browser protocol ipfs is enabled, make sure to open - `ipfs://${reportContentId}/`
    } else {
      url = `https://preprod.cardanoscan.io/transaction/${transactionId}`;
    }
    window.open(url, "_blank");
  };

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
      cellStyle: {
        display: 'flex'
      },
      Cell: RunStatusCell
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
              buttonLabel={"View Report"}
              onClick={() => {
                viewReportOrCertificate("report", props.row.original.runId);
              }}
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
              buttonLabel={"View Certificate"}
              onClick={() => {
                viewReportOrCertificate("certificate", props.row.original.runId);
              }}
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
    if (result.data.length) {
      setData(result.data);
    }
  };

  // TBD
  // const onDelete = (id: any) => {};
  
  return (
    <div id="testHistory">
      <TableComponent dataSet={data} config={columns} showColViz={true} 
        updateMyData={updateMyData}
        skipPageReset={skipPageReset}
      />
    </div>
  );
}

export default TestHistory;