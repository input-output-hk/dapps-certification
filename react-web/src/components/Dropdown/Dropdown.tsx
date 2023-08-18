import React, { forwardRef, useState } from "react";
import classNames from "classnames";
import Button from "@mui/material/Button/Button";
import Checkbox from "@mui/material/Checkbox/Checkbox";
import CircleCheckedFilled from "@mui/icons-material/CheckCircle";
import CircleUnchecked from "@mui/icons-material/RadioButtonUnchecked";

import ArrowTooltip from "components/Tooltip/Tooltip";
import Icons, { IconTypes } from "components/Icons/Icons";
import { useComponentVisible } from "hooks/useComponentVisible";

import "./Dropdown.scss";
import HelperText from "components/HelperText/HelperText";
import { useFormContext } from "react-hook-form";

export const emptyOption = { label: "", value: "" };

export type Option = {
  label: string;
  value: string | number;
  icon?: string;
  path?: string;
};

export interface DropdownProps {
  options: Option[];
  wrapperClassName?: string;
  menuItemClassName?: string;
  showCheckBox?: boolean;
  tooltipText?: string;
  onOptionSelect: (item: Option) => void;
  showInfoIcon?: boolean;
  name: string;
  required?: boolean;
  className?: string;
  placeholder?: string;
}

const Dropdown: React.FC<DropdownProps> = forwardRef<
  HTMLSpanElement,
  DropdownProps
>(function Dropdown(
  {
    options,
    onOptionSelect,
    wrapperClassName = "",
    menuItemClassName = "",
    placeholder = "Choose an option",
    showCheckBox = false,
    tooltipText = "",
    required = false,
    showInfoIcon = false,
    name = "",
    className = "",
  },
  ref
) {
  const {
    ref: compRef,
    isComponentVisible,
    setIsComponentVisible,
  } = useComponentVisible(false);
  const [selected, setSelected] = useState<Option>(emptyOption);

  const {
    formState: { errors },
  } = useFormContext();

  const handleClick = (item: Option) => {
    setIsComponentVisible(false);
    setSelected(item);
    onOptionSelect(item);
  };

  return (
    <div className="dropdown-wrapper text-gray relative" ref={compRef}>
      <div
        onClick={() => setIsComponentVisible(!isComponentVisible)}
        className={classNames("dropdown flex col relative", {
          [wrapperClassName]: wrapperClassName,
          active: isComponentVisible || selected.label,
          "has-value": selected.label,
          error: errors?.[name],
        })}
      >
        <span className="label absolute vertical-align">
          {placeholder}
          {required ? <span style={{ color: "red" }}> *</span> : null}
        </span>

        {selected?.label && (
          <span ref={ref} id={name} data-testid={name} className="option-value">
            {selected?.label}
          </span>
        )}

        <Icons
          type={IconTypes.chevron}
          className={classNames("chevron absolute vertical-align", {
            open: isComponentVisible,
          })}
        />

        {selected?.label ? (
          <Icons
            className={classNames("close-btn absolute vertical-align pointer", {
              hidden: !selected,
            })}
            type={IconTypes.close}
            onClick={() => {
              setSelected(emptyOption);
              onOptionSelect(emptyOption);
            }}
          />
        ) : null}
      </div>

      {isComponentVisible && (
        <div
          className={classNames("options-wrapper flex col absolute", className)}
        >
          {options.map((option) => (
            <Button
              type="button"
              variant="text"
              color="inherit"
              key={option.label}
              fullWidth
              onClick={() => handleClick(option)}
              className={classNames(
                "option-label justify-start",
                menuItemClassName
              )}
            >
              <div className="flex text-gray-dark">
                <span className="flex">
                  {option.icon ? (
                    <Icons type={option.icon} />
                  ) : showCheckBox ? (
                    <Checkbox
                      checked={option.value === selected.value}
                      disableRipple
                      color="success"
                      icon={<CircleUnchecked />}
                      checkedIcon={<CircleCheckedFilled />}
                    />
                  ) : null}
                  {option.value === selected.value ? (
                    <span className="text-bold">{option.label}</span>
                  ) : (
                    option.label
                  )}
                </span>
              </div>
            </Button>
          ))}
        </div>
      )}

      {errors?.[name] && (
        <HelperText
          type="error"
          value={errors[name]?.message as string}
          showInfoIcon={showInfoIcon}
        />
      )}

      {tooltipText ? (
        <ArrowTooltip title={tooltipText}>
          <Icons type="question" color="grey" />
        </ArrowTooltip>
      ) : null}
    </div>
  );
});

export default Dropdown;