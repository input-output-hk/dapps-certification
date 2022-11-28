import React, { useEffect, useState } from "react";
import { useNavigate, Link } from "react-router-dom";
import { useAppSelector } from "store/store";
import "./Header.scss";

import AvatarDropDown from "components/AvatarDropdown/AvatarDropdown";
import Button from "components/Button/Button";
import ConnectWalletModal from "components/ConnectWalletModal/ConnectWalletModal";

const Header = () => {
  const { isLoggedIn } = useAppSelector((state) => state.auth);
  const [isActive, setIsActive] = useState(false);
  const navigate = useNavigate();
  

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
            displayStyle="gradient"
            buttonLabel={"Connect Wallet"}
            onClick={(_) => openConnectWallet()}
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


  const [connectToWallet, setConnectToWallet] = useState(false)
  const openConnectWallet = () => {
    setConnectToWallet(true)
  }

  return (
    <header className="header">

        <a href="#"><img src="images/logo.png" alt="IOHK logo" style={{width: '82px', padding: '10px'}}/></a>
        
      <ConnectWalletModal open={connectToWallet}/>

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