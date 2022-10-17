import React, { useState } from "react";
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
import Toast from "components/Toast/Toast";

const Certification = () => {
  const form = useForm({
    schema: certificationSchema,
    mode: "onChange",
  });

  const [submitting, setSubmitting] = useState(false);
  const [formSubmitted, setFormSubmitted] = useState(false);
  const [timelineConfig, setTimelineConfig] = useState(TIMELINE_CONFIG);
  const [finishedCertify, setFinishedCertify] = useState(false);
  const [githubLink, setGithubLink] = useState("");
  const [logData, setLogData] = useState<any>({});
  const [unitTestSuccess, setUnitTestSuccess] = useState(true); // assuming unit tests will pass
  const [errorToast, setErrorToast] = useState(false);

  let timeout: any;

  const formHandler = (formData: ISearchForm) => {
    const { username, repoName, branch } = formData;
    setSubmitting(true);
    let config = timelineConfig;

    // Reset to default process states
    if (formSubmitted) {
      setTimelineConfig(TIMELINE_CONFIG);
    }

    setGithubLink(
      "https://github.com/" + username + "/" + repoName + "/tree/" + branch
    );

    const handleErrorScenario = () => {
      // show an api error toast
      setErrorToast(true);
      setTimeout(() => {
        setErrorToast(false);
        form.reset();
      }, 5000); // hide after 5 seconds
      setSubmitting(false);
      setFormSubmitted(false);
      setTimelineConfig(TIMELINE_CONFIG);
    };
    
    const triggerAPI = () => {
      postData
        .post("/run", "github:" + [username, repoName, branch].join("/"))
        .then((response) => response.data)
        .then((uid) => {
          const triggerFetchRunStatus = async () => {
            await fetchData
              .get("/run/" + uid)
              .then((res) => {
                const status = res.data.status,
                  state = res.hasOwnProperty("state") ? res.data.state : "";

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
                  setFinishedCertify(unitTestResult);
                  setUnitTestSuccess(unitTestResult);
                  setLogData(res.data.result);
                }
                if (state === "failed" || status === "finished") {
                  setSubmitting(false);
                }
                setTimelineConfig(config);

                if (state === "running" || state === "passed") {
                  const timeOffset = 60 * 1000;
                  let refetchMins = 1;
                  if (status === "certifying") {
                    refetchMins = 0.2;
                  }
                  timeout = setTimeout(() => {
                    clearTimeout(timeout);
                    triggerFetchRunStatus();
                  }, refetchMins * timeOffset);
                }
              })
              .catch((error) => {
                handleErrorScenario();
                console.log(error);
              });
          };
          triggerFetchRunStatus();
        })
        .catch((error) => {
          handleErrorScenario();
          console.log(error);
        });
    };
    triggerAPI();
     
    /**
    const fetchMockData = () => {
      postData
        .get("static/data/run")
        .then((response) => response.data)
        .then((uuid) => {
          // await fetchData.get("/run/" + uid)
          fetchData.get("static/data/finished-escrow-fail.json").then((res) => {

            const status = res.data.status,
              state = res.data.hasOwnProperty("state") ? res.data.state : "";

            // TBD -- save this state into useState and based on state changes useEffect and handle the logic there
            
            config = config.map((item, index) => {
              if (item.status === status) {
                const currentState = status === "finished" ? "passed" : (state || "running")
                let returnObj = { ...item, state: currentState };
                if (status === 'certifying' && currentState === 'running' && res.data.progress && res.data.plan) {
                  returnObj['progress'] = Math.trunc((res.data.progress['finished-tasks'].length / getPlannedCertificationTaskCount(res.data.plan)) * 100)
                }
                return returnObj;
              }
              // Set the previously executed states as passed
              return setManyStatus(index, config, item, status, "passed");
            });
            if (status === 'finished') {
              const unitTestResult = processFinishedJson(res.data.result)
              setFinishedCertify(unitTestResult);
              setUnitTestSuccess(unitTestResult);
              setLogData(res.data.result);
            }
            if (state === 'failed' || status === 'finished') {
              setSubmitting(false);
            }
            setTimelineConfig(config);
          });
        });
    };
    fetchMockData();
    */
  };

  /**
  const resetForm = (evt: Event) => {
    evt.stopImmediatePropagation();
    form.reset();
    clearTimeout(timeout);
  }
  */

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
              label="Github Username"
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
              label="Branch"
              type="text"
              id="branch"
              disabled={submitting}
              {...form.register("branch")}
            />
            <Button
              type="submit"
              className="btn btn-primary"
              buttonLabel="Start Testing"
              disabled={!form.formState.isValid || submitting}
              onClick={(_) => setFormSubmitted(true)}
            />
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
            </h2>

            <Timeline
              statusConfig={timelineConfig}
              unitTestSuccess={unitTestSuccess}
              hasFailedTasks={isAnyTaskFailure(logData)}
            />
          </div>

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
