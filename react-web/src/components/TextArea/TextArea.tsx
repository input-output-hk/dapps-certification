import React, { ComponentProps, forwardRef, useState } from "react";
import TextAreaAutoSize from "@mui/material/TextareaAutosize";
import classNames from "classnames";

import Icons from "components/Icons/Icons";
import ArrowTooltip from "components/Tooltip/Tooltip";
import HelperText, { HelperTextType } from "../HelperText/HelperText";

import "./TextArea.scss";
import { useFormContext } from "react-hook-form";

export interface TextAreaProps extends ComponentProps<"textarea"> {
  minRows?: number;
  maxRows?: number;
  tooltipText?: string;
  showInfoIcon?: boolean;
}

const TextArea = forwardRef<HTMLTextAreaElement, TextAreaProps>(
  function TextArea(
    {
      name = "",
      value,
      onChange,
      placeholder,
      minRows = 2,
      maxRows = 5,
      className,
      tooltipText = "",
      required = false,
      showInfoIcon = false,
    },
    ref
  ) {
    const {
      formState: { errors },
    } = useFormContext();

    const [active, setActive] = useState(false);

    return (
      <div
        className={classNames("text-area-wrapper relative", {
          error: errors?.[name],
          active: active,
        })}
        onClick={(_) => {
          setActive(true);
          document.getElementById(name || "")?.focus();
        }}
        onBlur={(e: any) => !e.target.value && setActive(false)}
      >
        <div className="relative">
          <span className="label absolute">
            {placeholder}
            {required ? <span style={{ color: "red" }}>*</span> : null}
          </span>

          <TextAreaAutoSize
            id={name}
            name={name}
            value={value}
            onChange={(e) => {
              setActive(true);
              onChange && onChange(e);
            }}
            minRows={minRows}
            maxRows={maxRows}
            className={classNames("text-area", className)}
            ref={ref}
          />

          {errors?.[name] && (
            <HelperText
              type="error"
              value={errors[name]?.message as string}
              showInfoIcon={showInfoIcon}
            />
          )}
        </div>

        {tooltipText ? (
          <ArrowTooltip title={tooltipText}>
            <Icons type="question" color="grey" />
          </ArrowTooltip>
        ) : null}
      </div>
    );
  }
);

export default TextArea;
