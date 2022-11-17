import React, { useEffect, useState } from "react";
import { useNavigate, Link } from "react-router-dom";
import { login } from "store/slices/auth.slice";
import { useAppDispatch, useAppSelector } from "store/store";
import "./Header.scss";

import AvatarDropDown from "components/AvatarDropdown/AvatarDropdown";
import Button from "components/Button/Button";

const Header = () => {
  const { isLoggedIn } = useAppSelector((state) => state.auth);
  const [isActive, setIsActive] = useState(false);
  const dispatch = useAppDispatch();
  const navigate = useNavigate();
  const onLogin = () => {
    dispatch(login());
  };

  useEffect(() => {
    if (isLoggedIn) {
      navigate("");
    }
  }, [isLoggedIn]);

  const renderNoAuthMenu = () => {
    return (
      <>
        <li>
          <a href="#">Community</a>
        </li>
        <li>
          <a href="#">Pricing</a>
        </li>
        <li>
          <a href="#">Support</a>
        </li>
        <li className="button-wrap">
          <Button
            type="button"
            className="btn btn-primary-outline"
            buttonLabel={"Connect Wallet"}
            onClick={(_) => onLogin()}
          />
        </li>
      </>
    );
  };
  const renderAuthenticatedMenu = () => {
    return (
      <>
        {/*
        <li>
          <Link to="support">Support</Link>
        </li>
        <li>
          <Link to="subscription">Subscription</Link>
        </li>
        <li>
          <Link to="test">Test History</Link>
        </li>
        */}
        <li>
          <AvatarDropDown />
        </li>
      </>
    );
  };
  return (
    <header className="header">
      <input
        className="menu-btn"
        type="checkbox"
        id="menu-btn"
        onChange={(e) => setIsActive(!isActive)}
      />
      <label className="menu-icon" htmlFor="menu-btn">
        <span className="navicon"></span>
      </label>
      <ul className={`menu ${isActive ? "active-ul" : ""}`}>
        {isLoggedIn ? renderAuthenticatedMenu() : renderNoAuthMenu()} 
      </ul>
    </header>
  );
};

export default Header;