import React, { useEffect, useState } from "react";
import { fetchData,} from "api/api";
import { useDelayedApi } from "hooks/useDelayedApi";

type Log = {
    Time: string,
}

export const useLogs = (
    uid: string,
    finishedCertify: boolean,
    handleErrorScenario: () => void,
    timeOffset: number,
) => {
    const [logInfo, setLogInfo] = useState<Log[]>([])
    const [fetchingLogs, setFetchingLogs] = useState(false);
    const [refetchLogsOffset] = useState(0.1);
    //reset log if uid changed
    useEffect(()=>{
        setLogInfo([])
    },[uid])

    const fetchLog = React.useCallback(async ()=>{
        if(!uid) return
        const lastLogTimestamp = logInfo[logInfo.length - 1]?.Time
        setFetchingLogs(true)
        try {
            const queryAfter = lastLogTimestamp ? '?after=' + encodeURIComponent(lastLogTimestamp) : '';
            const res: any = await fetchData.get("/run/" + uid + "/logs" + queryAfter);
            /** For mock */
            // const res: any = await fetchData.get("static/data/build-logs.json")
            //setFetchLogs(!finishedCertify || !submitting);
            if(res.data.length) setLogInfo((prev)=> prev.concat(res.data))
        } catch(e) {
            handleErrorScenario();
            console.log(e);
        } finally{
            setFetchingLogs(false)
        }
    },[
        logInfo,
        uid,
        handleErrorScenario,
    ])

  const enabled = !fetchingLogs && !finishedCertify && !!uid
  useDelayedApi(
      fetchLog,
      refetchLogsOffset * timeOffset,
      enabled
  )
  return { logInfo,fetchingLogs}
}
