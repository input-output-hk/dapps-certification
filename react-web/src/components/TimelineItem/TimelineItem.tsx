import React, { FC } from "react";

export interface ITimelineItem {
  config: {
    status: string;
    text: string;
    state?: string;
    progress?: any;
  }
  unitTestSuccess?: boolean,
  hasFailedTasks?: boolean
}

const TimelineItem: FC<ITimelineItem> = ({
  unitTestSuccess,
  config: { status, text, state, progress },
  hasFailedTasks
}) => {

  // TBD - useCallback to cache
  const getURLFor = (state: string = "outline") => {
    if (hasFailedTasks && state === "passed" && status === "finished") {
      state += '-error' // load gray check
    } else if (!unitTestSuccess && (status === "finished" || status === 'certifying')) {
      state = 'failed'
    }
    return "images/" + state + ".svg";
  };

  const renderProgressPercentage = () => {
    if (state === "running" && status === "certifying") {
      return <span className="progress-percentage">{progress}%</span>
    }
  };

  return (
    <li
      data-value={status}
      data-testid={status}
      className={state === "running" ? "active" : ""}
    >
      <img
        className={`image ${state==="running" ? "anim-rotate" : ""} ${status==="certifying" ? "certifying" : ""}`}
        data-testid={state}
        src={getURLFor(state)}
        alt={state}
      />
      {renderProgressPercentage()}
      <span className="text" data-testid={text}>
        {text}
      </span>
    </li>
  );
};

export default TimelineItem;
