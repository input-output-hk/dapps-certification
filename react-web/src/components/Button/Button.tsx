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
  type?: "button" | "reset" | "submit" | undefined;
  displayStyle?: "primary" | "secondary" | "gradient";
  onClick?: (e: any) => any;
}

const Button: FC<IButtonProps> = ({
  disabled = false,
  className = "",
  buttonLabel = "Submit",
  isLoading = false,
  type = "submit",
  iconUrl,
  showLoader = false,
  displayStyle = "primary",
  onClick,
}) => {
  return (
    <button
      className={`btn ${className} ${isLoading ? "is-loading" : ""} ${"btn-" + displayStyle}`}
      type={type}
      onClick={onClick}
      disabled={disabled}
    >
      {showLoader ? (
        <Loader />
      ) : (
        <>
          {iconUrl && (
            <img className="icon-image" src={iconUrl} alt="icon-img" />
          )}
          <span className="button-label">{buttonLabel}</span>
        </>
      )}
    </button>
  );
};

export default Button;
