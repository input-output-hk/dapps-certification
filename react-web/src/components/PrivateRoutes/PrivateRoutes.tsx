import React from "react";
import { Outlet, useLocation } from "react-router-dom";

import { useAppSelector } from "store/store";

import NotAuthorized from "components/NotAuthorized/NotAuthorized";

const PrivateRoutes = () => {
  const location = useLocation();
  const renderNoAuthPage = () => {
    if (location.pathname === "/") {
      return <h1 style={{textAlign: "center"}}>Welcome to testing tool</h1>;
    } else {
      return <NotAuthorized />;
    }
  };
  const { isLoggedIn } = useAppSelector((state) => state.auth);
  return isLoggedIn ? <Outlet /> : renderNoAuthPage();
}

export default PrivateRoutes;