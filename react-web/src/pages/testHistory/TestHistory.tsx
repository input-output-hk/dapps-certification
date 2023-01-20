import React, { useState, useEffect } from "react";
import dayjs from "dayjs";
import utc from "dayjs/plugin/utc";
import tz from "dayjs/plugin/timezone";

import { fetchData } from "api/api";

import TableComponent from "components/Table/Table";
import Button from "components/Button/Button";
import Toast from "components/Toast/Toast";
import { processFinishedJson } from "components/TimelineItem/timeline.helper";
import { isAnyTaskFailure } from "pages/certification/Certification.helper";
import "./TestHistory.scss";

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

interface ICampaignCertificate {
  runId: string;
  transactionId: string;
  reportContentId: string;
  createdAt: string;
}

interface IRunCertifications {
  [key: string]: ICampaignCertificate
}

dayjs.extend(utc)
dayjs.extend(tz)

const TestHistory = () => {
  const [data, setData] = useState<Array<ICampaign>>([]);
  const [runningSpinner, setRunningSpinner] = useState("");
  const [highlightLabelFor, setHighlightLabelFor] = useState("");
  const [skipPageReset, setSkipPageReset] = useState(false);
  const [errorToast, setErrorToast] = useState<{display: boolean; statusText?: string; message?: string;}>({display: false});

  const certificationData: IRunCertifications = {};
  const timeZone = dayjs.tz.guess()

  useEffect(() => {
    fetchTableData();
  }, []);

  useEffect(() => {
    setSkipPageReset(false);
  }, [data]);

  const setCertificationData = (runId: string, response: ICampaignCertificate) => {
    certificationData[runId] = response;
  }

  const getCertificationData = (runId: string): ICampaignCertificate | null => {
    return certificationData[runId] ? certificationData[runId] : null
  }

  const handleError = (error: any) => {
    if (error.response) {
      setErrorToast({display: true, statusText: error.response.statusText, message: error.response.data || undefined})
    } else {
      setErrorToast({display: true})
    }
    setTimeout(() => { setErrorToast({display: false}) }, 3000)
  }
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
      setRunningSpinner(original.runId)
      const res: any = await fetchData.get("/run/" + original.runId).catch(handleError);
      /** For mock */
      // const res = await fetchData.get("static/data/certifying.json")
      
      if (res) {
        setErrorToast({display: false})
        const status = res.data.status;
        const state = res.data.hasOwnProperty("state") ? res.data.state : "";
        let response: string = 'queued';
        // show failed if either states failed or unitTest/certTasks failed
        if (state === 'failed') {
          response = 'failed'
        } else {
          if (status === 'finished') {
            const isArrayResult = Array.isArray(res.data.result)
            const resultJson = isArrayResult ? res.data.result[0] : res.data.result;
            const isUnitTestSuccess = processFinishedJson(resultJson);
            const isComplete = isUnitTestSuccess && !isAnyTaskFailure(resultJson);
            response = isComplete ? 'succeeded' : 'failed';
          } else {
            // do nothing; retain response='queued'
          }
        } 
        setRunningSpinner(prevValue => {
          if (prevValue !== "") {
            // show a highlight over the label
            setHighlightLabelFor(prevValue)
            setTimeout(() => { setHighlightLabelFor("") }, 1500)
          }
          return ""
        })
        updateMyData(index, id, response);
      } else {
        setRunningSpinner("")
      }
    };
    const runId = original.runId; // get the run id
    if (value === "certified") {
      return <span 
        style={{ color: "green" }}
        className={highlightLabelFor === runId ? "cell-highlight" : ""}>Certified</span>;
    } else if (value === "succeeded") {
      return <span 
        className={highlightLabelFor === runId ? "cell-highlight" : ""}>OK</span>;
    } else if (value === "failed") {
      return <span 
        style={{ color: "red" }}
        className={highlightLabelFor === runId ? "cell-highlight" : ""}>FAILED</span>;
    } else if (value === "queued") {
      return (
        <>
          <span className={highlightLabelFor === runId ? "cell-highlight" : ""}>Running</span>
          <button onClick={() => triggerApi(runId)} className="sync-btn">
            <img
              className={`icon ${runningSpinner === runId ? 'spin' : ''}`}
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
    const certData = getCertificationData(runId)
    if (!certData) {
      const response: any = await fetchData.get("/run/" + runId + "/certificate").catch(handleError);
      /** For mock */
      // const response = await fetchData.get("static/data/certicate.json");
      if (response) {
        setErrorToast({display: false})
        setCertificationData(runId, response.data);
        triggerNavigation(type, response.data);
      }
    } else {
      triggerNavigation(type, certData);
    }
  };

  const triggerNavigation = (type: string, data: any) => {
    const { reportContentId, transactionId } = data;
    let url;
    if (type === "report") {
      // url = `https://${reportContentId}.ipfs.w3s.link/`;
      url = `ipfs://${reportContentId}/`
    } else {
      url = `https://preprod.cardanoscan.io/transaction/${transactionId}`;
    }
    window.open(url, "_blank");
  };

  const columns = React.useMemo(() => [
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
      columnVisible: false,
      Cell: (props: any) => (
        <span>
          {dayjs.utc(props.row.original.commitDate).tz(timeZone).format("YYYY-MM-DD HH:mm:ss")}
        </span>
      ),
    },
    {
      Header: "Finished At",
      accessor: "finishedAt",
      Cell: (props: any) => (
        <span>
          {dayjs.utc(props.row.original.finishedAt).tz(timeZone).format("YYYY-MM-DD HH:mm:ss")}
        </span>
      ),
    },
    {
      Header: "Synced At",
      accessor: "syncedAt",
      columnVisible: false,
      Cell: (props: any) => (
        <span>
          {dayjs.utc(props.row.original.syncedAt).tz(timeZone).format("YYYY-MM-DD HH:mm:ss")}
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
        if (props.row.original.runStatus === "succeeded" || props.row.original.runStatus === "certified") {
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
    ],
    []
  );

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
    <>
      <div id="testHistory">
        <TableComponent dataSet={data} columns={columns} showColViz={true} 
          updateMyData={updateMyData}
          skipPageReset={skipPageReset}
        />
      </div>
      {(errorToast && errorToast.display) ? (
        ((errorToast.message && errorToast.statusText) ? 
        <Toast message={errorToast.message} title={errorToast.statusText}/> :
        <Toast />))
      : null}
    </>
  );
}

export default TestHistory;