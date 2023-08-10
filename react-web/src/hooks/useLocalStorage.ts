/* eslint-disable no-useless-escape */
import { Dispatch, SetStateAction, useEffect, useState } from "react";

function isValidJSON(text: string) {
  try {
    const result = JSON.parse(text);
    return (typeof result === 'object' && result !== null);
  } catch (e) {
    return false;
  }
}

function useLocalStorage<T>(
  key: string,
  initialValue: T
): [T, Dispatch<SetStateAction<T>>] {
  // Get from local storage then
  // parse stored json or return initialValue
  const readValue = () => {
    // Prevent build error "window is undefined" but keep keep working
    if (typeof window === "undefined") {
      return initialValue;
    }

    try {
      const item = window.localStorage.getItem(key);
      // eslint-disable-next-line no-nested-ternary
      return item
        ? isValidJSON(item)
          ? JSON.parse(item)
          : item
        : initialValue;
    } catch (error) {
      return initialValue;
    }
  };

  // State to store our value
  // Pass initial state function to useState so logic is only executed once
  const [storedValue, setStoredValue] = useState<T>(readValue);

  // Return a wrapped version of useState's setter function that ...
  // ... persists the new value to localStorage.
  const setValue: Dispatch<SetStateAction<T>> = (value) => {
    // Prevent build error "window is undefined" but keeps working
    // eslint-disable-next-line eqeqeq
    if (typeof window === "undefined") {
      console.warn(
        `Tried setting localStorage key “${key}” even though environment is not a client`
      );
    }

    try {
      // Allow value to be a function so we have the same API as useState
      const newValue = value instanceof Function ? value(storedValue) : value;

      // Save to local storage
      try {
        const serializedValue = JSON.stringify(newValue);
        window.localStorage.setItem(key, serializedValue);
      } catch (error) {
        console.warn(`Error serializing localStorage key “${key}”:`, error);
      }
      
      // Save state
      setStoredValue(newValue);

      // We dispatch a custom event so every useLocalStorage hook are notified
      window.dispatchEvent(new Event("local-storage"));
    } catch (error) {
      console.warn(`Error setting localStorage key “${key}”:`, error);
    }
  };

  useEffect(() => {
    setStoredValue(readValue());

    const handleStorageChange = () => {
      setStoredValue(readValue());
    };

    // this only works for other documents, not the current one
    window.addEventListener("storage", handleStorageChange);

    // this is a custom event, triggered in writeValueToLocalStorage
    window.addEventListener("local-storage", handleStorageChange);

    return () => {
      window.removeEventListener("storage", handleStorageChange);
      window.removeEventListener("local-storage", handleStorageChange);
    };
    // eslint-disable-next-line
  }, []);

  return [storedValue, setValue];
}

export default useLocalStorage;