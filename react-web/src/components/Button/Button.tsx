import React, { FC } from "react";
import classNames from "classnames";
import "./Button.scss";
import ButtonLoader from "components/ButtonLoader/ButtonLoader";

export interface IButtonProps {
  className?: string;
  buttonLabel?: string;
  isLoading?: boolean;
  type?: "button" | "reset" | "submit" | undefined;
  onClick?: (e: any) => any;
}

const Button: FC<IButtonProps> = ({
  className = "",
  buttonLabel = "Submit",
  isLoading = false,
  type = "submit",
  onClick,
}) => {
  return (
    <button
      className={classNames("btn", className, { "is-loading": isLoading })}
      type={type}
      onClick={onClick}
    >
      <span className="button-label">{buttonLabel}</span>
      {isLoading && <ButtonLoader />}
    </button>
  );
};

export default Button;
