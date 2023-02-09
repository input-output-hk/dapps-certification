import React, { FC, useState, useEffect, useRef } from "react";

import "./InformationTable.scss";
import InformationTableEntry from "./InformationTableEntry";

const InformationTable: FC<{logs: any, }> = ({ logs, }) => {
    const [showLogs, setShowLogs] = useState(false);
    const bottomRef = useRef<HTMLDivElement>(null);
    const logContentRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
        bottomRef.current?.scrollIntoView({behavior: 'smooth'}); // scroll to bottom 
        logContentRef.current?.scrollIntoView({behavior: 'smooth'})
    }, [logs])

    const showLogView = () => {
        setShowLogs(true);
        const timeout = setTimeout(() => {
            clearTimeout(timeout)
            bottomRef.current?.scrollIntoView({behavior: 'smooth'}); // scroll to bottom 
            logContentRef.current?.scrollIntoView({behavior: 'smooth'})
        }, 2);
    }

    const hideLogView = () => {
        setShowLogs(false)
    }

    return (
        <>
            <div id="logContainer">
                <span 
                    id="viewLogsBtn"
                    className={`link text-right ${showLogs ? "hidden" : ""}`} 
                    onClick={showLogView}>
                        View logs
                </span>
                <section className={`log-information ${showLogs ? "" : "hidden"}`}>
                    <div className="log-header">
                        <h6>Logs</h6>
                        <span 
                            className="minimize-btn text-right"
                            onClick={hideLogView}>
                                <i>-</i>
                        </span>
                    </div>
                    <div className="log-content" ref={logContentRef}>
                        {logs.map((item: any, index: number) => {
                            let logData = ''
                            try {
                                const data = JSON.parse(item.Text)
                                const attr = data[Object.keys(data)[0]]
                                if (attr?.fields?.hasOwnProperty('launch-config')) {
                                    logData = attr['fields']['launch-config']
                                }
                                if (attr?.fields?.hasOwnProperty('chunk-data')) {
                                    logData = attr['fields']['chunk-data']
                                }
                            } catch(e) {
                                // do nothing
                                if (typeof item.Text == 'string' && item.Text.length) {
                                    logData = item.Text
                                }
                            }
                            logData = !logData.length ? item.Text : logData
                            return logData.length ? (
                                <InformationTableEntry key={index} time={item.Time} log={logData} />
                            ) : null
                        })}
                        <div className="empty-element" ref={bottomRef}></div>
                    </div>
                </section>
            </div>
        </>
    );
};

export default InformationTable;
