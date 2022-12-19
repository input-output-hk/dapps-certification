import React from "react";
import { Outlet, useLocation, Navigate } from "react-router-dom";


import { useAppSelector } from "store/store";

import NotAuthorized from "components/NotAuthorized/NotAuthorized";

const PrivateRoutes = () => {
  const location = useLocation();
  const renderNoAuthPage = () => {
    if (location.pathname === "/") {
      return <></>;
    } else {
      return <NotAuthorized />;
    }
  };
  const renderOutlets = (userDetails: any) => {
    if (location.pathname !== "/profile" && (!userDetails.dappOwner || !userDetails.dappRepository)) {
      return <Navigate replace to="/profile" />;
    } else {
      return <Outlet />
    }
  }
  const { isLoggedIn, userDetails } = useAppSelector((state) => state.auth);
  return isLoggedIn ? renderOutlets(userDetails) : renderNoAuthPage();
}

export default PrivateRoutes;