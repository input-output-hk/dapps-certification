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
    const tick = () => {
      return savedCallback.current;
    };
    let id: any;
    if (enabled && delay !== null) {
      id = setTimeout(tick, delay);
    }
    return () => {
      clearTimeout(id);
    };
  }, [enabled, delay]);
};