import React from "react";
import { Link } from "react-router-dom";
import { logout } from "store/slices/auth.slice";
import { useAppDispatch } from "store/store";
import "./avatar.scss";

const AvatarDropDown = () => {
  const dispatch = useAppDispatch();
  const logOut = () => {
    dispatch(logout());
  };
  return (
    <div className="wrapper">
      <label htmlFor="toggler">
        <img
          className="avatar"
          src="images/avatar.svg"
          alt="Profile"
        />
      </label>
      <input id="toggler" type="checkbox" />
      <div className="dropdown">
        <Link to="profile">User Profile</Link>
        <button className="logout-btn link" onClick={logOut}>
          Logout
        </button>
      </div>
    </div>
  );
};

export default AvatarDropDown;