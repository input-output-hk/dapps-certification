import React from "react";
import Tooltip from "@mui/material/Tooltip";

const ArrowTooltip: React.FC<{
  children?: any;
  title: string;
}> = ({ title, children }) => {
  return (
    <div className="absolute tooltip-wrapper">
      <Tooltip title={title} arrow placement="bottom-end">
        {/* Content on which tooltip appears */}
        <div className="tooltip-content pointer">{children}</div>
      </Tooltip>
    </div>
  );
};

export default ArrowTooltip;
