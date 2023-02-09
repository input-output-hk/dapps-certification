import { useEffect, useRef } from "react";

export const useDelayedApi = (
  callback: Function,
  delay: number,
  enabled: boolean
) => {
  const savedCallback = useRef<Function>(() => {});
  useEffect(() => {
    savedCallback.current = callback;
  }, [callback]);

  useEffect(() => {
    let timeout: any;
    if (enabled && delay !== null) {
      timeout = setTimeout(() => {clearTimeout(timeout); savedCallback.current() }, delay);
    } else {
      clearTimeout(timeout);
    }
    return () => {
      clearTimeout(timeout);
    };
  }, [enabled, delay]);
};