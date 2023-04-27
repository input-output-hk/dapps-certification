import React, { FC } from "react";
import "./Button.scss";
import Loader from "components/Loader/Loader";

export interface IButtonProps {
  disabled?: boolean;
  className?: string;
  buttonLabel?: any;
  isLoading?: boolean;
  iconUrl?: any;
  showLoader?: boolean;
  size?: 'small' | 'medium';
  type?: "button" | "reset" | "submit" | undefined;
  displayStyle?: "primary" | "primary-outline" | "secondary" | "gradient";
  onClick?: (e: any) => any;
}

const Button: FC<IButtonProps> = ({
  disabled = false,
  className = "",
  buttonLabel = "Submit",
  isLoading = false,
  type = "submit",
  iconUrl,
  size = "medium",
  showLoader = false,
  displayStyle = "primary",
  onClick,
}) => {
  return (
    <button
      className={`btn ${className} ${isLoading ? "is-loading" : ""} ${"btn-" + displayStyle} ${"btn-" + size}`}
      type={type}
      onClick={onClick}
      disabled={disabled}
    >
      {showLoader ? (
        <Loader />
      ) : (
        <>
          {iconUrl && (
            <img data-testid="btn-icon" className="icon-image" src={iconUrl} alt="icon-img" />
          )}
          <span className="button-label">{buttonLabel}</span>
        </>
      )}
    </button>
  );
};

export default Button;
