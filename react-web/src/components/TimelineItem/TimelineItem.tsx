import React, { FC } from "react";

import {
  FailedSvg,
  OutlineSvg,
  PassedSvg,
  RunningSvg,
} from "./timeline.helper";

export interface ITimelineItem {
  config: {
    status: string;
    text: string;
    state?: string;
  };
}

const fetchSVG = (state = "outline") => {
  switch (state) {
    case "outline":
      return <OutlineSvg />;
    case "passed":
      return <PassedSvg />;
    case "failed":
      return <FailedSvg />;
    case "running":
      return <RunningSvg />;
    default:
      return <OutlineSvg />;
  }
};

const TimelineItem: FC<ITimelineItem> = ({
  config: { status, text, state },
}) => {
  return (
    <li data-value={status} data-testid={status}>
      <span className="image" data-testid={state}>{fetchSVG(state)}</span>
      <span className="text" data-testid={text}>{text}</span>
    </li>
  );
};

export default TimelineItem;
