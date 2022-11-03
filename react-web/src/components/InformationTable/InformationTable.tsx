import React, { FC, useState, useEffect, useRef } from "react";

import "./InformationTable.scss";
import InformationTableEntry from "./InformationTableEntry";

const InformationTable: FC<{logs: any, emitLastLogTimestamp?: (e: any) => any;}> = ({ logs, emitLastLogTimestamp }) => {
    const [showLogs, setShowLogs] = useState(false);
    const [dataCollection, setDataCollection] = useState<any>([])
    const bottomRef = useRef<HTMLDivElement>(null)

    useEffect(() => {
        setDataCollection((prevState: any) => {
            if (prevState.length) {
                emitLastLogTimestamp && emitLastLogTimestamp(prevState[prevState.length - 1]['Time'])
            }
            return [...prevState, ...logs]
        })
    // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [logs])

    useEffect(() => {
        // scroll to bottom 
        bottomRef.current?.scrollIntoView({behavior: 'smooth'});
    }, [dataCollection])

    const showLogView = () => {
        setShowLogs(true)
        // scroll to bottom once visible
        const timeout = setTimeout(()=> {
            clearTimeout(timeout)
            bottomRef.current?.scrollIntoView({behavior: 'smooth'});
        }, 0)
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
                    <div className="log-content">
                        {dataCollection.map((item: any, index: number) => {
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
                            }
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