import { ComponentProps, forwardRef, useEffect, useState } from "react";

import "./Input.scss";
import { useFormContext } from "react-hook-form";
import HelperText from "components/HelperText/HelperText";

interface InputProps extends ComponentProps<"input"> {
  label: string;
  disabled?: boolean;
  disablefocus?: boolean;
  name: string;
  required?: boolean;
  error?: string;
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
    error,
    ...props
  },
  ref
) {
  const {
    formState: { errors },
    getValues
  } = useFormContext();

  const [active, setActive] = useState(false);

  useEffect(() => {
    if (getValues(name)) {
      // field has values
      setActive(true);
    } else {
      // set field active if value empty and if not on focus
      if (
        document.activeElement !== document.getElementById(id || name || "")
      ) {
        setActive(false);
      }
    }

    // eslint-disable-next-line
  }, [getValues(name)]);

  return (
    <div
      className={`input-wrapper ${className}`}
      onBlur={(e: any) => !e.target.value && setActive(false)}
      onClick={() => setActive(true)}
      data-testid={`${name}-wrapper`}
    >
      <div
        className={`input ${active ? "active" : ""} ${
          errors?.[name] || error ? "error" : ""
        } ${disabled ? "disabled" : ""}`}
        onClick={(_) => {
          setActive(true);
          document.getElementById(id || name || "")?.focus();
        }}
        data-testid={`${name}-container`}
      >
        <label>
          {label} {required ? <span style={{ color: "red" }}>*</span> : null}
        </label>
        <input
          type={type}
          ref={ref}
          {...props}
          name={name}
          id={id || name}
          data-testid={name}
          value={value}
          onFocusCapture={() => setActive(true)}
          disabled={disabled}
        />
      </div>

      {(errors?.[name] || error) && (
        <div style={{ marginTop: "4px" }}>
          {errors?.[name] && (
            <HelperText
              type="error"
              value={errors[name]?.message as string}
              showInfoIcon={false}
            />
          )}
          {error && (
            <HelperText
              type="error"
              value={error as string}
              showInfoIcon={false}
            />
          )}
        </div>
      )}
    </div>
  );
});
