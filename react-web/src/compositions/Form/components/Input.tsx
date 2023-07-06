import React, { ComponentProps, forwardRef, useEffect, useState } from "react";

import { FieldError } from "./FieldError";
import "./Input.scss";
import { useFormContext } from "react-hook-form";
import { getObjectByPath } from "utils/utils";
import HelperText from "components/HelperText/HelperText";

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
    getValues
  } = useFormContext();

  const [active, setActive] = useState(false);
  const [error, setError] = useState("");

  useEffect(() => {
    setError(getObjectByPath(errors, name)?.message);
    // eslint-disable-next-line
  }, [errors]);

  useEffect(() => {
    if (getValues(name)) {
      // field has values
      setActive(true);
    } else {
      // set field active if value empty and iff not on focus
      if (
        document.activeElement !== document.getElementById(id || name || "")
      ) {
        setActive(false);
      }
    }

    // catch deeply nested form errors onChange
    setError(getObjectByPath(errors, name)?.message);

    // eslint-disable-next-line
  }, [getValues(name)]);

  return (
    <div
      className={`input-wrapper ${className}`}
      onBlur={(e: any) => !e.target.value && setActive(false)}
      onClick={() => setActive(true)}
    >
      <div
        className={`input ${active ? "active" : ""} ${
          errors?.[name] || error ? "error" : ""
        } ${disabled ? "disabled" : ""}`}
        onClick={(_) => {
          setActive(true);
          document.getElementById(id || name || "")?.focus();
        }}
      >
        <label htmlFor={name}>
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

      {errors?.[name] && (
        <div style={{ marginTop: "4px" }}>
          <HelperText
            type="error"
            value={errors[name]?.message as string}
            showInfoIcon={false}
          />
        </div>
      )}

      {/* Deeply nested errors */}
      {error && !errors?.[name] ? <FieldError message={error} /> : <></>}
    </div>
  );
});
