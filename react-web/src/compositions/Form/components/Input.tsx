import React, { ComponentProps, forwardRef, useEffect, useState } from "react";

import { FieldError } from "./FieldError";
import "./Input.scss";
import { useFormContext } from "react-hook-form";
import { getObjectByPath } from "utils/utils";

interface InputProps extends ComponentProps<"input"> {
  label: string;
  disabled?: boolean;
  disablefocus?: boolean;
  name: string;
  required?: boolean;
}

export const Input = forwardRef<HTMLInputElement, InputProps>(function Input(
  {
    disabled = false,
    label,
    type = "text",
    className = "",
    disablefocus = false,
    name,
    required = false,
    value,
    id = "",
    ...props
  },
  ref
) {
  const {
    formState: { errors },
  } = useFormContext();

  useEffect(() => {
    disablefocus && setActive(true);
  }, [disablefocus]);

  const [active, setActive] = useState(false);
  const [error, setError] = useState("");

  useEffect(() => {
    setError(getObjectByPath(errors, name)?.message);
    // eslint-disable-next-line
  }, [errors]);

  useEffect(() => {
    value ? setActive(true) : setActive(false);
  }, [value]);

  return (
    <div
      className={`input-wrapper ${className}`}
      onBlur={(e: any) => !e.target.value && setActive(false)}
      onClick={(e: any) => setActive(true)}
    >
      <div
        className={`input ${active ? "active" : ""} ${error ? "error" : ""} ${
          disabled ? "disabled" : ""
        }`}
        onClick={(_) => {
          setActive(true);
          document.getElementById(id || name || "")?.focus();
        }}
      >
        <label>
          {label} {required ? <span style={{ color: "red" }}>*</span> : null}
        </label>
        <input
          type={type}
          ref={ref}
          {...props}
          name={name}
          id={id}
          data-testid={name}
          value={value}
          onFocusCapture={() => setActive(true)}
          disabled={disabled}
        />
      </div>

      {error ? <FieldError message={error} /> : <></>}
    </div>
  );
});
