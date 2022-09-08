import React from "react";

import TimelineItem from "components/TimelineItem/TimelineItem";

import "./Timeline.scss";

const Timeline = (props: any) => {
  const { statusConfig } = props;

  return (
    <div id="statusTimeline" data-testid="statusTimeline">
      <ul>
        {statusConfig.map(
          (config: any, index: React.Key | null | undefined) => (
            <TimelineItem key={index} config={config} />
          )
        )}
      </ul>
    </div>
  );
};

export default Timeline;
