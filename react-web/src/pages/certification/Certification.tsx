import React, { useEffect, useState } from "react";
import classNames from "classnames";

import { fetchData, postData } from "api/api";
import Button from "components/Button/Button";
import { Input } from "compositions/Form/components/Input";
import { Form } from "compositions/Form/Form";
import { ISearchForm } from "./certification.interface";
import { certificationSchema } from "./certification.schema";
import { useForm } from "hooks/useForm";

import "./Certification.scss";
import Timeline from "compositions/Timeline/Timeline";
import { TIMELINE_CONFIG } from "compositions/Timeline/timeline.config";
import {
  processFinishedJson,
  setManyStatus,
} from "components/TimelineItem/timeline.helper";
import LogContainer from "./components/LogContainer";
import FileCoverageContainer from "./components/FileCoverageContainer";
import {
  isAnyTaskFailure,
  getPlannedCertificationTaskCount,
} from "./Certification.helper";
import { useDelayedApi } from "hooks/useDelayedApi";
import Toast from "components/Toast/Toast";
import { exportObjectToJsonFile } from "../../utils/utils";
import DownloadIcon from "assets/images/download.svg";

const TIMEOFFSET = 60 * 1000;

const Certification = () => {
  const form: any = useForm({
    schema: certificationSchema,
    mode: "onChange",
  });

  const [submitting, setSubmitting] = useState(false);
  const [formSubmitted, setFormSubmitted] = useState(false);
  const [timelineConfig, setTimelineConfig] = useState(TIMELINE_CONFIG);
  const [finishedCertify, setFinishedCertify] = useState(false);
  const [githubLink, setGithubLink] = useState("");
  const [uid, setUid] = useState("");
  const [logData, setLogData] = useState<any>({});
  const [unitTestSuccess, setUnitTestSuccess] = useState(true); // assuming unit tests will pass
  const [errorToast, setErrorToast] = useState(false);
  const [runStatus, setRunStatus] = useState("");
  const [runState, setRunState] = useState("");
  const [refetchMin, setRefetchMin] = useState(1);
  const [fetchRunStatus, setFetchRunStatus] = useState(false);
  const [apiFetching, setApiFetching] = useState(false);

  const formHandler = (formData: ISearchForm) => {
    const { username, repoName, branch, commit } = formData;
    setSubmitting(true);

    // Reset to default process states
    if (formSubmitted) {
      setTimelineConfig(TIMELINE_CONFIG);
    }

    let githubBranchOrCommitHash = branch || commit;
    setGithubLink(
      "https://github.com/" + username + "/" + repoName + "/tree/" + githubBranchOrCommitHash
    );
    
    const triggerAPI = async () => {
      try {
        const response = await postData.post(
          "/run",
          "github:" + [username, repoName, githubBranchOrCommitHash].join("/")
        );

        /** For mock */ 
        // const response = await postData.get('static/data/run')
        
        setUid(response.data);
      } catch (e) {
        handleErrorScenario();
        console.log(e);
      }
    };
    triggerAPI();
  };

  const triggerFetchRunStatus = async () => {
    let config = timelineConfig;
    try {
      const res = await fetchData.get("/run/" + uid);
      /** For mock */ 
      // const res = await fetchData.get("static/data/certifying.json")
      const status = res.data.status;
      const state = res.data.hasOwnProperty("state") ? res.data.state : "";
      setRunStatus(status);
      setRunState(state);
      setFetchRunStatus(state === "running" || state === "passed");
      config = config.map((item, index) => {
        if (item.status === status) {
          const currentState =
            status === "finished" ? "passed" : state || "running";
          let returnObj = { ...item, state: currentState };
          if (
            status === "certifying" &&
            currentState === "running" &&
            res.data.progress &&
            res.data.plan
          ) {
            returnObj["progress"] = Math.trunc(
              (res.data.progress["finished-tasks"].length /
                getPlannedCertificationTaskCount(res.data.plan)) *
                100
            );
          }
          return returnObj;
        }
        // Set the previously executed states as passed
        return setManyStatus(index, config, item, status, "passed");
      });
      if (status === "finished") {
        const unitTestResult = processFinishedJson(res.data.result);
        setFinishedCertify(true); // to hide form even when UT-Failure
        setUnitTestSuccess(unitTestResult);
        setLogData(res.data.result);
      }
      if (state === "failed" || status === "finished") {
        setSubmitting(false);
      }
      setTimelineConfig(config);
    } catch (e) {
      handleErrorScenario();
      console.log(e);
    }
  };

  const handleErrorScenario = () => {
    // show an api error toast
    setErrorToast(true);
    form.reset();
    setTimeout(() => {
      setErrorToast(false);
      // TBD - blur out of input fields 
    }, 5000); // hide after 5 seconds
    setSubmitting(false);
    setFormSubmitted(false);
    setTimelineConfig(TIMELINE_CONFIG);
  };

  const handleDownloadLogData = (logData: any) => {
    exportObjectToJsonFile(logData);
  };

  useEffect(() => {
    if (uid.length) {
      triggerFetchRunStatus();
    }
    // eslint-disable-next-line
  }, [uid]);

  useEffect(() => {
    runStatus === "certifying" ? setRefetchMin(0.2) : setRefetchMin(1);
    if (
      runStatus === "certifying" ||
      runStatus === "building" ||
      runStatus === "preparing" ||
      runStatus === "queued" ||
      (runStatus === "finished" && runState === "running")
    ) {
      setApiFetching(true);
    } else {
      setApiFetching(false);
    }
  }, [runStatus, runState]);

  useDelayedApi(
    async () => {
      setFetchRunStatus(false); // to clear timeout until api response
      triggerFetchRunStatus();
    },
    refetchMin * TIMEOFFSET, // delay in milliseconds
    fetchRunStatus // set to false to stop polling
  );

  return (
    <>
      <div
        id="searchContainer"
        className={classNames({ hidden: finishedCertify })}
      >
        <h2>
          Enter Github repository details of your Dapp to start the
          certification process.
        </h2>
        <div className="search-form common-top">
          <Form form={form} onSubmit={formHandler}>
            <Input
              label="Owner"
              type="text"
              disabled={submitting}
              {...form.register("username")}
            />

            <Input
              label="Repository"
              type="text"
              disabled={submitting}
              {...form.register("repoName")}
            />

            <Input
              label="Commit Hash"
              type="text"
              id="commit"
              disabled={submitting || form.watch("branch")?.length}
              {...form.register("commit")}
            />
            <div className="or-separator-text">Or</div>
            <Input
              label="Branch"
              type="text"
              id="branch"
              disabled={submitting || form.watch("commit")?.length}
              {...form.register("branch")}
            />
            {false && apiFetching ? (
              <Button
                className="btn btn-abort"
                buttonLabel="Abort"
                // onClick={(_) => abortApi()}
              />
            ) : (
              <Button
                type="submit"
                className="btn btn-primary"
                buttonLabel="Start Testing"
                disabled={!form.formState.isValid || submitting}
                onClick={(_) => setFormSubmitted(true)}
              />
            )}
          </Form>
        </div>
      </div>

      {formSubmitted && (
        <>
          <div id="resultContainer">
            <h2
              id="breadcrumb"
              className={classNames({ hidden: !finishedCertify })}
            >
              <a target="_blank" rel="noreferrer" href={githubLink}>
                {form.getValues("username")}/{form.getValues("repoName")}
              </a>
              {Object.keys(logData).length ? (
                <>
                  <Button
                    className="report-download"
                    onClick={(e) => handleDownloadLogData(logData)}
                    buttonLabel="Download Report"
                    iconUrl={DownloadIcon}
                  />
                </>
              ) : null}
            </h2>

            <Timeline
              statusConfig={timelineConfig}
              unitTestSuccess={unitTestSuccess}
              hasFailedTasks={isAnyTaskFailure(logData)}
            />
          </div>
          {unitTestSuccess === false && Object.keys(logData).length ? (
            <>
              <LogContainer unitTestSuccess={unitTestSuccess} result={logData} />
            </>
          ) : null}

          {unitTestSuccess && Object.keys(logData).length ? (
            <>
              <FileCoverageContainer githubLink={githubLink} result={logData} />
              <LogContainer result={logData} />
            </>
          ) : null}
        </>
      )}

      {errorToast ? <Toast /> : null}
    </>
  );
};

export default Certification;
