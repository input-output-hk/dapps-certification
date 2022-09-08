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
  indexOfExecutingProcess,
  processFinishedJson,
  setManyStatus,
} from "components/TimelineItem/timeline.helper";

const Certification = () => {
  const form = useForm({
    schema: certificationSchema,
    mode: "onChange"
  });

  const [submitting, setSubmitting] = useState(false);
  const [formSubmitted, setFormSubmitted] = useState(false);
  const [timelineConfig, setTimelineConfig] = useState(TIMELINE_CONFIG);
  
  let timeout: any;

  const formHandler = (formData: ISearchForm) => {
    const { username, repoName, branch } = formData;
    setSubmitting(true);
    let config = timelineConfig;

    // Reset to default process states
    if (formSubmitted) {
      setTimelineConfig(TIMELINE_CONFIG)
    }

    const processStateUpdate = (config: any) => {
      setTimelineConfig(config);
      setSubmitting(false);
    };

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

              if (status !== "finished") {
                // Update current status of process
                config = config.map((cfg, index) => {
                  if (cfg.status === status) {
                    return { ...cfg, state: state || "running" };
                  }
                  // Set the previously executed states as passed
                  return setManyStatus(index, config, cfg, status, "passed");
                });
                setTimelineConfig(config);

                const timeOffset = 60 * 1000, refetchMins = 2;
                timeout = setTimeout(() => {
                  clearTimeout(timeout);
                  triggerFetchRunStatus();
                }, refetchMins * timeOffset);
              } else {
                config = config.map((cfg, index) => {
                  if (cfg.status === status) {
                    return { ...cfg, state: "passed" };
                  }
                  // Set the previously executed states as passed
                  return setManyStatus(index, config, cfg, status, "passed");
                });
                processFinishedJson(res.data.result);
                processStateUpdate(config);
              }
            })
            .catch((error) => {
              config[indexOfExecutingProcess(config, "outline")].state = "failed";
              processStateUpdate(config);
              console.log(error);
            });
        };
        triggerFetchRunStatus();
      })
      .catch((error) => {
        config[indexOfExecutingProcess(config, "queued")].state = "failed";
        processStateUpdate(config);
        console.log(error);
      });
  };

  const resetForm = (evt: Event) => {
    evt.stopImmediatePropagation();
    form.reset();
    clearTimeout(timeout);
  }

  return (
    <>
      <div id="searchContainer" className={classNames({ hidden: formSubmitted})}>
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
            {/* {!submitting ?
            <Button
              type="submit"
              className="btn btn-primary"
              buttonLabel="Start Certification"
              disabled={!form.formState.isValid}
              onClick={(_) => setFormSubmitted(true)}
            /> 
            :
            <Button
              type="reset"
              className="btn btn-primary"
              buttonLabel="Abort"
              onClick={(e) => resetForm(e)}
            />
            } */}
            <Button
              type="submit"
              className="btn btn-primary"
              buttonLabel="Start Certification"
              disabled={!form.formState.isValid || submitting}
              onClick={(_) => setFormSubmitted(true)}
            /> 
          </Form>
        </div>
      </div>

      {formSubmitted && (
        <>
          <div id="resultContainer">

            <h2 id="breadcrumb">
              <a href="https://github.com/shlevy/plutus-apps/tree/certification-test">
              {form.getValues("username")}/{form.getValues("repoName")}</a>
            </h2>

            <Timeline statusConfig={timelineConfig} />
          </div>
          <div id="logContainer"></div>
        </>
      )}
    </>
  );
};

export default Certification;