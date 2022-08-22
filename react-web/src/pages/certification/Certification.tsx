import React, { useState } from "react";

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
import { processFinishedJson, setManyStatus } from "components/TimelineItem/timeline.helper";

const Certification = () => {
  const form = useForm({
    schema: certificationSchema,
  });

  const [submitting, setSubmitting] = useState(false);
  const [timelineConfig, setTimelineConfig] = useState(TIMELINE_CONFIG);

  const formHandler = (formData: ISearchForm) => {
    const { username, repoName, branch } = formData;
    setSubmitting(true);
    let config = timelineConfig;

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

                const timeOffset = 60 * 1000,
                  refetchMins = 2;
                const timeout = setTimeout(() => {
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
                setTimelineConfig(config);
                processFinishedJson(res.data.result);
                setSubmitting(false);
              }
            })
            .catch((error) => {
              console.log("Oops 2", error);
            });
        };
        triggerFetchRunStatus();
      })
      .catch((error) => {
        config = config.map((config) => {
          if (config.status === "queued") {
            return { ...config, state: "failed" };
          }
          return config;
        });
        setTimelineConfig(config);
        console.log("Oops 1", error);
      });
  };

  return (
    <>
      <div id="searchContainer">
        <h2>
          Enter Github repository details of your Dapp to start the
          certification process.
        </h2>
        <div className="search-form common-top">
          <Form form={form} onSubmit={formHandler}>
            <Input
              label="Github Username"
              type="text"
              {...form.register("username")}
            />

            <Input
              label="Repository"
              type="text"
              {...form.register("repoName")}
            />

            <Input
              label="Branch"
              type="text"
              id="branch"
              {...form.register("branch")}
            />

            <Button
              type="submit"
              className="btn btn-primary"
              buttonLabel="Start Certification"
              isLoading={submitting}
            />
          </Form>
        </div>
      </div>

      {submitting && timelineConfig.length && (
        <>
          <div id="resultContainer">
            <Timeline statusConfig={timelineConfig} />
          </div>
          <div id="logContainer"></div>
        </>
      )}
    </>
  );
};

export default Certification;
