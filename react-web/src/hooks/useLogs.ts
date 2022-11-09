import React, { useEffect, useState } from "react";
import { fetchData,} from "api/api";
import { useDelayedApi } from "hooks/useDelayedApi";

type Log = {
    Time: string,
}

const TIMEOFFSET = 60 * 1000;

export const useLogs = (
    uuid: string,
    testEnded: boolean,
    handleErrorScenario: () => void
) => {
    const [logInfo, setLogInfo] = useState<Log[]>([])
    const [fetchingLogs, setFetchingLogs] = useState(false);
    const [refetchLogsOffset] = useState(0.1);
    //reset log if uuid changed
    useEffect(()=>{
        setLogInfo([])
    },[uuid])

    const fetchLog = React.useCallback(async ()=>{
        if(!uuid) return
        const lastLogTimestamp = logInfo[logInfo.length - 1]?.Time
        setFetchingLogs(true)
        try {
            const queryAfter = lastLogTimestamp ? '?after=' + encodeURIComponent(lastLogTimestamp) : '';
            const res: any = await fetchData.get("/run/" + uuid + "/logs" + queryAfter);
            /** For mock */
            // const res: any = await fetchData.get("static/data/build-logs.json")
            if(res.data.length) setLogInfo((prev)=> prev.concat(res.data))
        } catch(e) {
            handleErrorScenario();
            console.log(e);
        } finally{
            setFetchingLogs(false)
        }
    },[
        logInfo,
        uuid,
        handleErrorScenario,
    ])

  const enabled = !fetchingLogs && !testEnded && !!uuid
  useDelayedApi(
      fetchLog,
      refetchLogsOffset * TIMEOFFSET,
      enabled
  )
  return { logInfo,fetchingLogs}
}
