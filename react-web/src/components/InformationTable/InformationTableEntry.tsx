import React, { FC, memo } from "react";

import "./InformationTable.scss";

const InformationTableEntry: FC<{time: any, log: string}> = ({ time, log }) => {
    return (
        <>
            <div className="log">
                <span className="log-time">{time}</span><span>{log}</span>
            </div>
        </>
    )
};

export default memo(InformationTableEntry);