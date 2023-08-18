import { useEffect, useState } from "react";
import { useAppSelector } from "store/store";
import { useDispatch } from "react-redux";
import { useDelayedApi } from "hooks/useDelayedApi";

import { fetchData } from "api/api";
import { Log } from '../pages/certification/Certification.helper'
import { setStates, setEnded, setBuildInfo } from "pages/certification/slices/logRunTime.slice";

const TIME_OFFSET = 1000;

export const useLogs = (
    uuid: string,
    testEnded: boolean,
    handleErrorScenario: () => void
) => {
    const dispatch = useDispatch()
    const [logInfo, setLogInfo] = useState<Log[]>([])
    const [fetchingLogs, setFetchingLogs] = useState(false);
    const [refetchLogsOffset] = useState(1);

    const { startTime, endTime, runState, ended } = useAppSelector((state) => state.runTime)

    const enabled = !fetchingLogs && !(ended > 1) && !!uuid

    useEffect(() => {
        if (testEnded) { 
            dispatch(setEnded(ended+1)); 
        }
    // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [testEnded])

    //reset log if uuid changed
    useEffect(()=>{
        setLogInfo([])
        dispatch(setEnded(0))
    },[uuid])

    const captureRunTime = (sTime: string, eTime: string, state: string) => {
        if (sTime && eTime && state) { 
            dispatch(setStates({startTime: sTime, endTime: eTime, runState: state}))
            dispatch(setBuildInfo())
        }
    }

    const computeRunState = (state: string) => {
        if (state.indexOf('build-flake') !== -1) { 
            return 'building'
        } else if (state.indexOf('generate-flake') !== -1) {
            return 'preparing'
        } else if (state.indexOf('run-certify') !== -1) {
            return 'certifying'
        } else {
            return ''
        }
    }

    const computeTimeForRunState = (currentLogs: Log[]) => {
        let sTime: string = startTime, eTime: string = endTime, state: string = runState;
        let triggeredStateChange = false
        let lastLogItem = false;
        const logSetLength = currentLogs.length - 1;
        currentLogs.forEach((log, idx) => {
            lastLogItem = idx === logSetLength
            const currentRunState: string = computeRunState(log.Source)
            if (!sTime) {
                sTime = log.Time
                eTime = log.Time
                state = currentRunState
            } else {
                const lastLogEntry = ended === 1 && lastLogItem
                if (state === currentRunState && !lastLogEntry) {
                    eTime = log.Time
                    if (lastLogItem) {
                        triggeredStateChange = false;
                    }
                } else if (state && (state !== currentRunState || lastLogEntry)) {
                    captureRunTime(sTime, eTime, state)
                    state = currentRunState
                    sTime = log.Time
                    eTime = log.Time
                    triggeredStateChange = true
                }
            }
        })
        if (!triggeredStateChange) {
            // save to useState to use in next logs
            dispatch(setStates({startTime: sTime, endTime: eTime, runState: state}))
        }
    }


    const fetchLog = async ()=>{
        if(!uuid) return
        const lastLogTimestamp = logInfo[logInfo.length - 1]?.Time
        setFetchingLogs(true)
        let fetchApi = false
        if (testEnded && ended <= 1) { 
            fetchApi = true
            dispatch(setEnded(ended+1));
        }
        try {
            const queryAfter = lastLogTimestamp ? '?after=' + encodeURIComponent(lastLogTimestamp) : '';
            const res: any = await fetchData.get("/run/" + uuid + "/logs" + queryAfter);
            /** For mock */
            // const res: any = await fetchData.get("static/data/build-logs.json")
            if(res.data.length) {
                await computeTimeForRunState(res.data)
                setLogInfo((prev)=> prev.concat(res.data))
            } else if (fetchApi) {
                // capture whatever is the last stored state
                dispatch(setBuildInfo())
            }
        } catch(e) {
            handleErrorScenario();
            console.log(e);
        } finally{
            setFetchingLogs(false)
        }
    }

    useDelayedApi(
        fetchLog,
        refetchLogsOffset * TIME_OFFSET,
        enabled
    )
    return { logInfo,fetchingLogs}
}
