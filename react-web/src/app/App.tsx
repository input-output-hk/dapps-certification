import React, { useState } from "react";
import { Routes, Route, Outlet } from "react-router-dom";
import { BASE_URL } from "constants/route";

import "./App.scss";
import Certification from "pages/certification/Certification";
import Button from "components/Button/Button";
import ConnectWalletModal from "components/ConnectWalletModal/ConnectWalletModal";

const PageLayout = () => {

  const [connectToWallet, setConnectToWallet] = useState(false)
  const openConnectWallet = () => {
    setConnectToWallet(true)
  }
  
  return (
    <>
      <header>
        <a href="#"><img src="images/logo.png" alt="IOHK logo" style={{width: '82px', padding: '10px'}}/></a>
        <Button
            type="button"
            displayStyle="gradient"
            buttonLabel={"Connect Wallet"}
            onClick={(_) => openConnectWallet()}
          />
      </header>

      {/* Load page content here */}
      <section data-testid="contentWrapper" id="contentWrapper">
        <Outlet />
      </section>

      <ConnectWalletModal open={connectToWallet}/>
    </>
  );
};

const App = () => {
  return (
    <Routes>
      <Route path={BASE_URL} element={<PageLayout />}>
        <Route index element={<Certification />}></Route>
      </Route>
    </Routes>
  );
};

export default App;
