import React from "react";

import TimelineItem from "components/TimelineItem/TimelineItem";

import "./Timeline.scss";
import { useAppSelector } from "store/store";

const Timeline = (props: any) => {
  const { statusConfig, unitTestSuccess, hasFailedTasks } = props;
  const { buildInfo } = useAppSelector((state) => state.runTime);

  return (
    <div id="statusTimeline" data-testid="statusTimeline">
      <ul>
        {statusConfig.map(
          (config: any, index: React.Key | null | undefined) => {
            if (buildInfo.runState === config.status) {
              config.runTimeTaken = buildInfo.runTime
            }
            return (
            <TimelineItem key={index} config={config} unitTestSuccess={unitTestSuccess} hasFailedTasks={hasFailedTasks}/>
            )
          }
        )}
      </ul>
    </div>
  );
};

export default Timeline;
