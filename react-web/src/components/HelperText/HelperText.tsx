import { FC } from "react";
import classNames from "classnames";

import Icons, { IconTypes } from "components/Icons/Icons";
import "./HelperText.scss";

export type HelperTextType =
  | "info"
  | "warning"
  | "error"
  | "success"
  | undefined;

export interface HelperTextProps {
  value: string;
  showInfoIcon?: boolean;
  className?: string;
  type?: HelperTextType;
}

export const fetchHelperTextColor = (type: HelperTextType) => {
  switch (type) {
    case IconTypes.info:
      return "text-info";
    case IconTypes.warning:
      return "text-warn";
    case IconTypes.success:
      return "text-success";
    case IconTypes.error:
      return "text-error";
    default:
      return "";
  }
};

// Component to show different type helper texts
const HelperText: FC<HelperTextProps> = ({
  className = "",
  type = "info",
  value,
  showInfoIcon = false,
}: HelperTextProps) => {
  return (
    <div className={classNames("helper-text-wrapper", className)}>
      {showInfoIcon ? (
        <span className="icon-wrapper">
          <Icons type={type} />
        </span>
      ) : null}
      <span className={classNames("helper-text", fetchHelperTextColor(type))}>
        {value}
      </span>
    </div>
  );
};

export default HelperText;
