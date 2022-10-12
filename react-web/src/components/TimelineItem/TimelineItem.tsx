import React, { FC } from "react";
import classNames from "classnames";
// TBD -  import PassedImg from 'assets/images/passed.svg' - while moving to assets instead of public

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
      state += '-error' // load grey check
    } else if (!unitTestSuccess && (status === "finished" || status === 'certifying')) {
      state = 'failed'
    }
    // TBD - return PassedImg
    return "images/" + state + ".svg";
  };

  return (
    <li
      data-value={status}
      data-testid={status}
      className={classNames({
        active:
          state === "running" || state === "failed" || state === "passed",
      })}
    >
      <img
        className={classNames({
          image: true,
          "anim-rotate": state === "running",
          "certifying": status === 'certifying'
        })}
        data-testid={state}
        src={getURLFor(state)}
        alt={state}
      />
      {
        (state === 'running' && status === 'certifying') ?
        <span className="progress-percentage">{progress}%</span>
        : null
      }
      <span className="text" data-testid={text}>
        {text}
      </span>
    </li>
  );
};

export default TimelineItem;
