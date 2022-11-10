import React, { useState } from "react";
import { Routes, Route, Outlet } from "react-router-dom";
import { BASE_URL } from "constants/route";

import "./App.scss";
import Certification from "pages/certification/Certification";
import Button from "components/Button/Button";
import ConnectWallet from "components/ConnectWallet/ConnectWallet";

const PageLayout = () => {

  const [connectToWallet, setConnectToWallet] = useState(false)
  const openConnectWallet = () => {
    setConnectToWallet(true)
  }
  
  return (
    <>
      <header>
        <Button
            type="button"
            className="btn btn-primary"
            buttonLabel={"Connect Wallet"}
            onClick={(_) => openConnectWallet()}
          />
      </header>

      {/* Load page content here */}
      <section data-testid="contentWrapper" id="contentWrapper">
        <Outlet />
      </section>

      {connectToWallet ? <>
        <ConnectWallet />
      </> : null}
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
