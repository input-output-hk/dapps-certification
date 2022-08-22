import React, { ComponentProps, forwardRef, useEffect, useState } from "react";
import classNames from "classnames";

import { FieldError } from "./FieldError";
import "./Input.scss";
import { useFormContext } from "react-hook-form";

interface InputProps extends ComponentProps<"input"> {
  label: string;
}

export const Input = forwardRef<HTMLInputElement, InputProps>(function Input(
  { label, type = "text", className = "", name, ...props },
  ref
) {
  const {
    formState: { errors },
  } = useFormContext();

  const [active, setActive] = useState(false);
  const [errorMsg, setErrorMessage] = useState("");

  useEffect(() => {
    if (errors && name && errors[name]) {
      setErrorMessage(errors[name]?.message as string);
    } else setErrorMessage("");

    //eslint-disable-next-line
  }, [errors]);

  return (
    <div
      className={classNames("input-wrapper", className)}
      onBlur={(e: any) => !e.target.value && setActive(false)}
      onClick={(e: any) => setActive(true)}
    >
      <div
        className={classNames("input", { active: active, error: errorMsg })}
        onClick={(_) => {
          setActive(true);
          document.getElementById(name || "")?.focus();
        }}
      >
        <label>{label}</label>
        <input type={type} ref={ref} {...props} name={name} id={name} />
      </div>

      {errorMsg && <FieldError message={errorMsg} />}
    </div>
  );
});
