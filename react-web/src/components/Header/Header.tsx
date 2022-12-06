import React, { useEffect, useState } from "react";
import { useNavigate, Link } from "react-router-dom";
import { useAppSelector } from "store/store";
import "./Header.scss";

import AvatarDropDown from "components/AvatarDropdown/AvatarDropdown";
import ConnectWallet from "components/ConnectWallet/ConnectWallet";

const Header = () => {
  const { isLoggedIn } = useAppSelector((state) => state.auth);
  const [isActive, setIsActive] = useState(false);
  const navigate = useNavigate();
  

  useEffect(() => {
    if (isLoggedIn) {
      navigate("/");
    }
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [isLoggedIn]);

  const renderNoAuthMenu = () => {
    return (
      <>
        <li>
          <Link to="community">Community</Link>
        </li>
        <li>
          <Link to="pricing">Pricing</Link>
        </li>
        <li>
          <Link to="support">Support</Link>
        </li>
        <li className="button-wrap">
          <ConnectWallet />
        </li>
      </>
    );
  };
  const renderAuthenticatedMenu = () => {
    return (
      <>
        <li>
          <Link to="support">Support</Link>
        </li>
        <li>
          <Link to="subscription">Subscription</Link>
        </li>
        <li>
          <Link to="history">Test History</Link>
        </li>
        <li>
          <AvatarDropDown />
        </li>
      </>
    );
  };

  return (
    <header className="header">

      <Link to="/">
        <img src="images/logo.png" alt="IOHK logo" style={{width: '82px', padding: '10px'}}/>
      </Link>

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