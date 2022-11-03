import React, { FC } from "react";
import "./Button.scss";

export interface IButtonProps {
  disabled?: boolean;
  className?: string;
  buttonLabel?: string;
  isLoading?: boolean;
  iconUrl?: any;
  type?: "button" | "reset" | "submit" | undefined;
  onClick?: (e: any) => any;
}

const Button: FC<IButtonProps> = ({
  disabled = false,
  className = "",
  buttonLabel = "Submit",
  isLoading = false,
  type = "submit",
  iconUrl,
  onClick,
}) => {
  return (
    <button
      className={`btn ${className} ${isLoading ? "is-loading" : ""}`}
      type={type}
      onClick={onClick}
      disabled={disabled}
    >
      {iconUrl && <img className="icon-image" src={iconUrl} alt="icon-img" />}
      <span className="button-label">{buttonLabel}</span>
    </button>
  );
};

export default Button;
